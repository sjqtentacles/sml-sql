(* sql.sml - pure in-memory relational query engine over a small SQL subset *)

structure Sql :> SQL =
struct
  (* ============================ Values ============================ *)

  datatype value =
      Int  of int
    | Real of real
    | Text of string
    | Bool of bool
    | Null

  datatype colType = TInt | TReal | TText | TBool

  type column = { name : string, ty : colType }
  type row    = value list

  type table  = { name : string, columns : column list, rows : row list }
  type database = table list

  exception ParseError of string
  exception TypeError  of string
  exception RuntimeError of string

  (* ============================ Rendering ============================ *)

  fun valueToString v =
    case v of
        Int n  => Int.toString n
      | Real r => Real.toString r
      | Text s => "'" ^ s ^ "'"
      | Bool b => Bool.toString b
      | Null   => "NULL"

  fun rowToString r = "(" ^ String.concatWith ", " (List.map valueToString r) ^ ")"

  (* ============================ Database API ============================ *)

  val empty : database = []
  fun tables (db : database) = db

  fun getTable (db : database) name =
    case List.find (fn (t : table) => #name t = name) db of
        SOME t => t
      | NONE => raise RuntimeError ("unknown table: " ^ name)

  (* ============================ Tokenizer ============================ *)

  datatype token =
      TIdent   of string
    | TNum     of string
    | TStr     of string
    | TSymbol  of string
    | TKeyword of string

  val keywords =
    [ "SELECT","FROM","WHERE","INSERT","INTO","VALUES","CREATE","TABLE"
    , "JOIN","ON","ORDER","BY","ASC","DESC","LIMIT","AND","OR","NOT"
    , "TRUE","FALSE","NULL","COUNT","SUM","MIN","MAX","AVG"
    , "INT","INTEGER","REAL","FLOAT","TEXT","BOOL","BOOLEAN" ]

  fun isKeyword s = List.exists (fn k => k = s) keywords

  fun isIdentStart c = Char.isAlpha c orelse c = #"_"
  fun isIdentChar  c = Char.isAlphaNum c orelse c = #"_"

  (* tokenize a SQL string into a token list *)
  fun tokenize s =
    let
      val n = String.size s
      fun peek i = if i < n then SOME (String.sub (s, i)) else NONE

      fun lexString (i, acc) =
        (case peek i of
             NONE => raise ParseError "unterminated string literal"
           | SOME #"'" =>
               (* support doubled '' as an escaped quote *)
               (case peek (i + 1) of
                    SOME #"'" => lexString (i + 2, acc ^ "'")
                  | _ => (acc, i + 1))
           | SOME c => lexString (i + 1, acc ^ String.str c))

      fun lexNumber (i, acc) =
        (case peek i of
             SOME c =>
               if Char.isDigit c orelse c = #"." then lexNumber (i + 1, acc ^ String.str c)
               else (acc, i)
           | NONE => (acc, i))

      fun lexIdent (i, acc) =
        (case peek i of
             SOME c =>
               if isIdentChar c then lexIdent (i + 1, acc ^ String.str c)
               else (acc, i)
           | NONE => (acc, i))

      fun go (i, acc) =
        (case peek i of
             NONE => List.rev acc
           | SOME c =>
               if Char.isSpace c then go (i + 1, acc)
               else if c = #"'" then
                 let val (str, j) = lexString (i + 1, "")
                 in go (j, TStr str :: acc) end
               else if Char.isDigit c then
                 let val (num, j) = lexNumber (i, "")
                 in go (j, TNum num :: acc) end
               else if isIdentStart c then
                 let
                   val (id, j) = lexIdent (i, "")
                   val up = String.map Char.toUpper id
                   val tok = if isKeyword up then TKeyword up else TIdent id
                 in go (j, tok :: acc) end
               else
                 (* operators and punctuation *)
                 (case c of
                      #"<" =>
                        (case peek (i + 1) of
                             SOME #"=" => go (i + 2, TSymbol "<=" :: acc)
                           | SOME #">" => go (i + 2, TSymbol "<>" :: acc)
                           | _ => go (i + 1, TSymbol "<" :: acc))
                    | #">" =>
                        (case peek (i + 1) of
                             SOME #"=" => go (i + 2, TSymbol ">=" :: acc)
                           | _ => go (i + 1, TSymbol ">" :: acc))
                    | #"!" =>
                        (case peek (i + 1) of
                             SOME #"=" => go (i + 2, TSymbol "<>" :: acc)
                           | _ => raise ParseError "unexpected '!'")
                    | #"=" => go (i + 1, TSymbol "=" :: acc)
                    | #"(" => go (i + 1, TSymbol "(" :: acc)
                    | #")" => go (i + 1, TSymbol ")" :: acc)
                    | #"," => go (i + 1, TSymbol "," :: acc)
                    | #"*" => go (i + 1, TSymbol "*" :: acc)
                    | #"." => go (i + 1, TSymbol "." :: acc)
                    | _ => raise ParseError ("unexpected character: " ^ String.str c)))
    in
      go (0, [])
    end

  (* ============================ AST ============================ *)

  datatype expr =
      ECol of string option * string   (* optional table qualifier, column *)
    | ELit of value
    | EBin of string * expr * expr     (* comparison operator *)
    | EAnd of expr * expr
    | EOr  of expr * expr
    | ENot of expr

  datatype selItem =
      SStar
    | SExpr of expr
    | SAgg of string * aggArg          (* COUNT/SUM/MIN/MAX, argument *)
  and aggArg = AStar | ACol of string option * string

  datatype order = OrderBy of (string option * string) * bool (* col, asc? *)

  type selectStmt =
    { items   : selItem list
    , from    : string
    , join    : (string * expr) option   (* joined table, ON condition *)
    , wherePred : expr option
    , orderBy : order option
    , limit   : int option }

  datatype stmt =
      SCreate of string * column list
    | SInsert of string * string list option * value list
    | SSelect of selectStmt

  datatype result =
      Updated of database
    | Rows of { columns : string list, rows : row list }

  (* ============================ Parser ============================ *)
  (* A recursive-descent parser over the token list. We thread the remaining
     token list explicitly. *)

  fun expect (toks, t, what) =
    case toks of
        x :: rest => if x = t then rest
                     else raise ParseError ("expected " ^ what)
      | [] => raise ParseError ("expected " ^ what ^ " but reached end")

  fun expectKw (toks, kw) = expect (toks, TKeyword kw, kw)
  fun expectSym (toks, sy) = expect (toks, TSymbol sy, sy)

  fun parseColType (toks : token list) : colType * token list =
    case toks of
        TKeyword "INT" :: r => (TInt, r)
      | TKeyword "INTEGER" :: r => (TInt, r)
      | TKeyword "REAL" :: r => (TReal, r)
      | TKeyword "FLOAT" :: r => (TReal, r)
      | TKeyword "TEXT" :: r => (TText, r)
      | TKeyword "BOOL" :: r => (TBool, r)
      | TKeyword "BOOLEAN" :: r => (TBool, r)
      | _ => raise ParseError "expected a column type (INT/REAL/TEXT/BOOL)"

  (* a literal value from tokens *)
  fun parseLiteral (toks : token list) : value * token list =
    case toks of
        TNum num :: r =>
          if CharVector.exists (fn c => c = #".") num then
            (case Real.fromString num of
                 SOME x => (Real x, r)
               | NONE => raise ParseError ("bad real: " ^ num))
          else
            (case Int.fromString num of
                 SOME n => (Int n, r)
               | NONE => raise ParseError ("bad int: " ^ num))
      | TStr s :: r => (Text s, r)
      | TKeyword "TRUE" :: r => (Bool true, r)
      | TKeyword "FALSE" :: r => (Bool false, r)
      | TKeyword "NULL" :: r => (Null, r)
      | _ => raise ParseError "expected a literal value"

  (* a possibly-qualified column reference: ident or ident.ident *)
  fun parseColRef (toks : token list) : (string option * string) * token list =
    case toks of
        TIdent a :: TSymbol "." :: TIdent b :: r => ((SOME a, b), r)
      | TIdent a :: r => ((NONE, a), r)
      | _ => raise ParseError "expected a column reference"

  (* primary expression: literal | colref *)
  fun parsePrimary (toks : token list) : expr * token list =
    case toks of
        TNum _ :: _ => let val (v, r) = parseLiteral toks in (ELit v, r) end
      | TStr _ :: _ => let val (v, r) = parseLiteral toks in (ELit v, r) end
      | TKeyword "TRUE" :: _ => (ELit (Bool true), List.tl toks)
      | TKeyword "FALSE" :: _ => (ELit (Bool false), List.tl toks)
      | TKeyword "NULL" :: _ => (ELit Null, List.tl toks)
      | _ => let val (cr, r) = parseColRef toks in (ECol cr, r) end

  val cmpOps = ["=","<>","<","<=",">",">="]

  (* comparison: primary (op primary)? *)
  fun parseCmp (toks : token list) : expr * token list =
    let val (lhs, r1) = parsePrimary toks
    in case r1 of
           TSymbol op' :: r2 =>
             if List.exists (fn o' => o' = op') cmpOps then
               let val (rhs, r3) = parsePrimary r2
               in (EBin (op', lhs, rhs), r3) end
             else (lhs, r1)
         | _ => (lhs, r1)
    end

  (* NOT binds tighter than AND/OR *)
  fun parseNot (toks : token list) : expr * token list =
    case toks of
        TKeyword "NOT" :: r =>
          let val (e, r2) = parseNot r in (ENot e, r2) end
      | _ => parseCmp toks

  fun parseAnd (toks : token list) : expr * token list =
    let
      val (lhs, r1) = parseNot toks
      fun loop (acc, ts) =
        case ts of
            TKeyword "AND" :: r =>
              let val (rhs, r2) = parseNot r in loop (EAnd (acc, rhs), r2) end
          | _ => (acc, ts)
    in loop (lhs, r1) end

  fun parseOr (toks : token list) : expr * token list =
    let
      val (lhs, r1) = parseAnd toks
      fun loop (acc, ts) =
        case ts of
            TKeyword "OR" :: r =>
              let val (rhs, r2) = parseAnd r in loop (EOr (acc, rhs), r2) end
          | _ => (acc, ts)
    in loop (lhs, r1) end

  val parseExpr = parseOr

  (* one SELECT item *)
  fun parseSelItem (toks : token list) : selItem * token list =
    case toks of
        TSymbol "*" :: r => (SStar, r)
      | TKeyword agg :: TSymbol "(" :: r =>
          if List.exists (fn a => a = agg) ["COUNT","SUM","MIN","MAX"] then
            (case r of
                 TSymbol "*" :: r2 =>
                   let val r3 = expectSym (r2, ")")
                   in (SAgg (agg, AStar), r3) end
               | _ =>
                   let
                     val (cr, r2) = parseColRef r
                     val r3 = expectSym (r2, ")")
                   in (SAgg (agg, ACol cr), r3) end)
          else raise ParseError ("unknown aggregate: " ^ agg)
      | _ => let val (cr, r) = parseColRef toks in (SExpr (ECol cr), r) end

  fun parseSelItems (toks : token list) : selItem list * token list =
    let
      val (item, r1) = parseSelItem toks
      fun loop (acc, ts) =
        case ts of
            TSymbol "," :: r =>
              let val (it, r2) = parseSelItem r in loop (it :: acc, r2) end
          | _ => (List.rev acc, ts)
    in loop ([item], r1) end

  fun identName (toks, what) =
    case toks of
        TIdent x :: r => (x, r)
      | _ => raise ParseError ("expected " ^ what)

  fun parseSelect (toks : token list) : selectStmt * token list =
    let
      val r0 = expectKw (toks, "SELECT")
      val (items, r1) = parseSelItems r0
      val r2 = expectKw (r1, "FROM")
      val (fromT, r3) = identName (r2, "table name after FROM")
      (* optional JOIN t ON expr *)
      val (join, r4) =
        case r3 of
            TKeyword "JOIN" :: r =>
              let
                val (jt, r') = identName (r, "table name after JOIN")
                val r'' = expectKw (r', "ON")
                val (cond, r''') = parseExpr r''
              in (SOME (jt, cond), r''') end
          | _ => (NONE, r3)
      (* optional WHERE *)
      val (whereE, r5) =
        case r4 of
            TKeyword "WHERE" :: r =>
              let val (e, r') = parseExpr r in (SOME e, r') end
          | _ => (NONE, r4)
      (* optional ORDER BY col [ASC|DESC] *)
      val (orderB, r6) =
        case r5 of
            TKeyword "ORDER" :: r =>
              let
                val r' = expectKw (r, "BY")
                val (cr, r'') = parseColRef r'
              in
                case r'' of
                    TKeyword "ASC" :: r3 => (SOME (OrderBy (cr, true)), r3)
                  | TKeyword "DESC" :: r3 => (SOME (OrderBy (cr, false)), r3)
                  | _ => (SOME (OrderBy (cr, true)), r'')
              end
          | _ => (NONE, r5)
      (* optional LIMIT n *)
      val (lim, r7) =
        case r6 of
            TKeyword "LIMIT" :: TNum num :: r =>
              (case Int.fromString num of
                   SOME n => (SOME n, r)
                 | NONE => raise ParseError ("bad LIMIT: " ^ num))
          | TKeyword "LIMIT" :: _ => raise ParseError "expected integer after LIMIT"
          | _ => (NONE, r6)
    in
      ({ items = items, from = fromT, join = join, wherePred = whereE
       , orderBy = orderB, limit = lim }, r7)
    end

  (* comma-separated literal values inside parens *)
  fun parseValueList (toks : token list) : value list * token list =
    let
      val r0 = expectSym (toks, "(")
      fun loop (acc, ts) =
        let
          val (v, r1) = parseLiteral ts
        in
          case r1 of
              TSymbol "," :: r2 => loop (v :: acc, r2)
            | TSymbol ")" :: r2 => (List.rev (v :: acc), r2)
            | _ => raise ParseError "expected , or ) in value list"
        end
    in loop ([], r0) end

  fun parseIdentList (toks : token list) : string list * token list =
    let
      val r0 = expectSym (toks, "(")
      fun loop (acc, ts) =
        let val (nm, r1) = identName (ts, "column name") in
          case r1 of
              TSymbol "," :: r2 => loop (nm :: acc, r2)
            | TSymbol ")" :: r2 => (List.rev (nm :: acc), r2)
            | _ => raise ParseError "expected , or ) in column list"
        end
    in loop ([], r0) end

  fun parseColumnDefs (toks : token list) : column list * token list =
    let
      val r0 = expectSym (toks, "(")
      fun loop (acc, ts) =
        let
          val (nm, r1) = identName (ts, "column name")
          val (ty, r2) = parseColType r1
          val col = { name = nm, ty = ty }
        in
          case r2 of
              TSymbol "," :: r3 => loop (col :: acc, r3)
            | TSymbol ")" :: r3 => (List.rev (col :: acc), r3)
            | _ => raise ParseError "expected , or ) in column definitions"
        end
    in loop ([], r0) end

  fun parseStmt (toks : token list) : stmt =
    case toks of
        TKeyword "CREATE" :: r =>
          let
            val r1 = expectKw (r, "TABLE")
            val (nm, r2) = identName (r1, "table name")
            val (cols, r3) = parseColumnDefs r2
          in if r3 <> [] then raise ParseError "trailing tokens after CREATE TABLE"
             else SCreate (nm, cols)
          end
      | TKeyword "INSERT" :: r =>
          let
            val r1 = expectKw (r, "INTO")
            val (nm, r2) = identName (r1, "table name")
            val (colsOpt, r3) =
              case r2 of
                  TSymbol "(" :: _ =>
                    let val (cs, r') = parseIdentList r2 in (SOME cs, r') end
                | _ => (NONE, r2)
            val r4 = expectKw (r3, "VALUES")
            val (vals, r5) = parseValueList r4
          in if r5 <> [] then raise ParseError "trailing tokens after INSERT"
             else SInsert (nm, colsOpt, vals)
          end
      | TKeyword "SELECT" :: _ =>
          let val (s, rest) = parseSelect toks
          in if rest <> [] then raise ParseError "trailing tokens after SELECT"
             else SSelect s
          end
      | _ => raise ParseError "expected CREATE, INSERT, or SELECT"

  fun parse s = parseStmt (tokenize s)

  (* ============================ Evaluator ============================ *)

  fun typeName ty =
    case ty of TInt => "INT" | TReal => "REAL" | TText => "TEXT" | TBool => "BOOL"

  (* coerce/validate a literal against a declared column type *)
  fun checkType (ty, v) =
    case (ty, v) of
        (_, Null) => Null
      | (TInt, Int n) => Int n
      | (TReal, Real r) => Real r
      | (TReal, Int n) => Real (Real.fromInt n)   (* widen int literal to real *)
      | (TText, Text s) => Text s
      | (TBool, Bool b) => Bool b
      | _ => raise TypeError ("expected " ^ typeName ty ^ " but got " ^ valueToString v)

  fun replaceTable (db : database, t : table) : database =
    let val without = List.filter (fn (x : table) => #name x <> #name t) db
    in without @ [t] end

  fun colIndex (cols : column list, name) =
    let
      fun go (_, []) = NONE
        | go (i, (c : column) :: cs) =
            if #name c = name then SOME i else go (i + 1, cs)
    in go (0, cols) end

  fun doCreate (db, name, cols) =
    (case List.find (fn (t : table) => #name t = name) db of
         SOME _ => raise RuntimeError ("table already exists: " ^ name)
       | NONE => replaceTable (db, { name = name, columns = cols, rows = [] }))

  fun doInsert (db, name, colsOpt, vals) =
    let
      val t = getTable db name
      val cols = #columns t
      val ncols = List.length cols
      val newRow =
        case colsOpt of
            NONE =>
              (* positional: arity must match exactly *)
              if List.length vals <> ncols then
                raise TypeError ("INSERT expects " ^ Int.toString ncols
                                 ^ " values, got " ^ Int.toString (List.length vals))
              else
                ListPair.map (fn (c, v) => checkType (#ty c, v)) (cols, vals)
          | SOME names =>
              if List.length names <> List.length vals then
                raise TypeError "INSERT column/value count mismatch"
              else
                List.map
                  (fn (c : column) =>
                     case ListPair.foldl
                            (fn (nm, v, acc) =>
                               if nm = #name c then SOME v else acc)
                            NONE (names, vals) of
                         SOME v => checkType (#ty c, v)
                       | NONE => Null)
                  cols
      val t' = { name = #name t, columns = cols, rows = #rows t @ [newRow] }
    in replaceTable (db, t') end

  (* ---- SELECT evaluation ---- *)

  (* A "scope" pairs a synthetic header (qualified column descriptors) with a
     concrete row, so column references (qualified or not) can be resolved. *)
  type hcol = { table : string, name : string }

  fun resolveCol (header : hcol list, row : row, qual, name) : value =
    let
      val idxs = List.tabulate (List.length header, fn i => i)
      val matches =
        List.filter
          (fn i =>
             let val h = List.nth (header, i) in
               #name h = name andalso
               (case qual of NONE => true | SOME q => #table h = q)
             end)
          idxs
    in
      case matches of
          [i] => List.nth (row, i)
        | [] => raise RuntimeError ("unknown column: "
                  ^ (case qual of SOME q => q ^ "." | NONE => "") ^ name)
        | _ => raise RuntimeError ("ambiguous column: " ^ name)
    end

  (* compare two values for ordering; returns order *)
  fun cmpValue (a, b) =
    case (a, b) of
        (Int x, Int y) => Int.compare (x, y)
      | (Real x, Real y) => Real.compare (x, y)
      | (Int x, Real y) => Real.compare (Real.fromInt x, y)
      | (Real x, Int y) => Real.compare (x, Real.fromInt y)
      | (Text x, Text y) => String.compare (x, y)
      | (Bool x, Bool y) =>
          if x = y then EQUAL else if x then GREATER else LESS
      | (Null, Null) => EQUAL
      | (Null, _) => LESS
      | (_, Null) => GREATER
      | _ => raise TypeError "cannot compare values of different types"

  fun evalExpr (header : hcol list, row : row) e : value =
    case e of
        ELit v => v
      | ECol (qual, name) => resolveCol (header, row, qual, name)
      | ENot e1 =>
          (case evalExpr (header, row) e1 of
               Bool b => Bool (not b)
             | Null => Null
             | _ => raise TypeError "NOT expects a boolean")
      | EAnd (a, b) => boolBin (header, row, a, b, fn (x, y) => x andalso y)
      | EOr (a, b) => boolBin (header, row, a, b, fn (x, y) => x orelse y)
      | EBin (oper, a, b) =>
          let
            val va = evalExpr (header, row) a
            val vb = evalExpr (header, row) b
          in
            case (va, vb) of
                (Null, _) => Bool false
              | (_, Null) => Bool false
              | _ =>
                let val c = cmpValue (va, vb) in
                  Bool (case oper of
                            "=" => c = EQUAL
                          | "<>" => c <> EQUAL
                          | "<" => c = LESS
                          | "<=" => c <> GREATER
                          | ">" => c = GREATER
                          | ">=" => c <> LESS
                          | _ => raise ParseError ("bad operator: " ^ oper))
                end
          end
  and boolBin (header, row, a, b, f) =
    let
      fun asBool v = case v of Bool x => x
                             | Null => false
                             | _ => raise TypeError "expected boolean in logical expression"
    in Bool (f (asBool (evalExpr (header, row) a), asBool (evalExpr (header, row) b))) end

  fun evalPred (header, row) eopt =
    case eopt of
        NONE => true
      | SOME e =>
          (case evalExpr (header, row) e of
               Bool b => b
             | Null => false
             | _ => raise TypeError "WHERE predicate must be boolean")

  fun headerOf (t : table) : hcol list =
    List.map (fn (c : column) => { table = #name t, name = #name c }) (#columns t)

  (* a stable merge sort; cmp returns order *)
  fun stableSort cmp xs =
    let
      fun merge ([], ys) = ys
        | merge (xs, []) = xs
        | merge (x :: xs, y :: ys) =
            (case cmp (x, y) of
                 GREATER => y :: merge (x :: xs, ys)
               | _ => x :: merge (xs, y :: ys))   (* keep stable on EQUAL/LESS *)
      fun split [] = ([], [])
        | split [a] = ([a], [])
        | split (a :: b :: rest) =
            let val (l, r) = split rest in (a :: l, b :: r) end
      fun sort [] = []
        | sort [a] = [a]
        | sort lst =
            let val (l, r) = split lst
            in merge (sort l, sort r) end
    in sort xs end

  fun doSelect (db, s : selectStmt) : { columns : string list, rows : row list } =
    let
      val baseT = getTable db (#from s)
      (* build the working set: header + rows, applying JOIN if present *)
      val (header, rows0) =
        case #join s of
            NONE => (headerOf baseT, #rows baseT)
          | SOME (jname, cond) =>
              let
                val jt = getTable db jname
                val hdr = headerOf baseT @ headerOf jt
                val combined =
                  List.concat
                    (List.map
                       (fn lr =>
                          List.mapPartial
                            (fn rr =>
                               let val full = lr @ rr in
                                 if (case evalExpr (hdr, full) cond of
                                         Bool b => b | _ => false)
                                 then SOME full else NONE
                               end)
                            (#rows jt))
                       (#rows baseT))
              in (hdr, combined) end

      (* WHERE *)
      val filtered = List.filter (fn r => evalPred (header, r) (#wherePred s)) rows0

      (* ORDER BY *)
      val ordered =
        case #orderBy s of
            NONE => filtered
          | SOME (OrderBy ((q, nm), asc)) =>
              let
                fun key r = resolveCol (header, r, q, nm)
                fun cmp (r1, r2) =
                  let val c = cmpValue (key r1, key r2)
                  in if asc then c else
                       (case c of LESS => GREATER | GREATER => LESS | EQUAL => EQUAL)
                  end
              in stableSort (fn (a, b) => cmp (a, b)) filtered end

      (* detect aggregate query *)
      val isAgg = List.exists (fn SAgg _ => true | _ => false) (#items s)
    in
      if isAgg then
        let
          fun evalAgg (name, arg) =
            let
              val vals =
                case arg of
                    AStar => List.map (fn _ => Int 1) ordered
                  | ACol (q, nm) => List.map (fn r => resolveCol (header, r, q, nm)) ordered
              val nonNull = List.filter (fn Null => false | _ => true) vals
            in
              case name of
                  "COUNT" => Int (List.length nonNull)
                | "SUM" =>
                    List.foldl
                      (fn (Int n, Int acc) => Int (acc + n)
                        | (Real x, Real acc) => Real (acc + x)
                        | (Real x, Int acc) => Real (x + Real.fromInt acc)
                        | (Int n, Real acc) => Real (Real.fromInt n + acc)
                        | _ => raise TypeError "SUM on non-numeric column")
                      (Int 0) nonNull
                | "MIN" =>
                    (case nonNull of
                         [] => Null
                       | x :: xs => List.foldl
                           (fn (v, acc) => if cmpValue (v, acc) = LESS then v else acc) x xs)
                | "MAX" =>
                    (case nonNull of
                         [] => Null
                       | x :: xs => List.foldl
                           (fn (v, acc) => if cmpValue (v, acc) = GREATER then v else acc) x xs)
                | _ => raise ParseError ("unknown aggregate: " ^ name)
            end
          fun aggLabel (name, arg) =
            name ^ "(" ^ (case arg of AStar => "*"
                                    | ACol (_, nm) => nm) ^ ")"
          val pairs =
            List.map
              (fn SAgg (n, a) => (aggLabel (n, a), evalAgg (n, a))
                | _ => raise ParseError "cannot mix aggregates with plain columns")
              (#items s)
        in
          { columns = List.map #1 pairs, rows = [ List.map #2 pairs ] }
        end
      else
        let
          (* projection *)
          val (colNames, project) =
            case #items s of
                [SStar] =>
                  (List.map #name header, fn r => r)
              | items =>
                  let
                    val refs =
                      List.map
                        (fn SExpr (ECol (q, nm)) => (q, nm)
                          | SStar => raise ParseError "cannot mix * with columns"
                          | _ => raise ParseError "unsupported select item")
                        items
                    val names = List.map #2 refs
                    fun proj r = List.map (fn (q, nm) => resolveCol (header, r, q, nm)) refs
                  in (names, proj) end
          val projected = List.map project ordered
          (* LIMIT *)
          val limited =
            case #limit s of
                NONE => projected
              | SOME n => List.take (projected, Int.min (n, List.length projected))
        in
          { columns = colNames, rows = limited }
        end
    end

  fun exec (db : database) (sql : string) : result =
    case parse sql of
        SCreate (nm, cols) => Updated (doCreate (db, nm, cols))
      | SInsert (nm, colsOpt, vals) => Updated (doInsert (db, nm, colsOpt, vals))
      | SSelect s => Rows (doSelect (db, s))

  fun query (db : database) (sql : string) =
    case exec db sql of
        Rows r => r
      | Updated _ => raise RuntimeError "query: statement was not a SELECT"

end
