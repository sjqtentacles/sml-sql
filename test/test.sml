(* test.sml - tests for sml-sql (written before implementation: strict TDD) *)

structure Tests =
struct
  open Harness
  open Sql

  (* ---- readable row comparison helper ---- *)

  fun rowsToString (rows : row list) : string =
    "[" ^ String.concatWith "; " (List.map Sql.rowToString rows) ^ "]"

  (* compare two row lists for structural equality and report readably.
     We compare via rendering because `value` contains a `real` (not an
     equality type), so structural `=` is unavailable. *)
  fun rowEq (a : row, b : row) = (Sql.rowToString a = Sql.rowToString b)
  fun rowsEq (xs, ys) =
    List.length xs = List.length ys
    andalso ListPair.all rowEq (xs, ys)

  fun checkRows name (expected : row list, actual : row list) =
    check (name ^ "  (expected " ^ rowsToString expected
           ^ " got " ^ rowsToString actual ^ ")")
          (rowsEq (expected, actual))

  fun checkValue name (expected : value, actual : value) =
    check (name ^ "  (expected " ^ Sql.valueToString expected
           ^ " got " ^ Sql.valueToString actual ^ ")")
          (Sql.valueToString expected = Sql.valueToString actual)

  (* run a SELECT and grab its rows *)
  fun rowsOf db sql = #rows (Sql.query db sql)

  (* run a list of statements, threading the database; returns final db *)
  fun run' db [] = db
    | run' db (s :: ss) =
        (case Sql.exec db s of
             Updated db' => run' db' ss
           | Rows _ => run' db ss)

  (* a small fixture database *)
  fun fixture () =
    run' Sql.empty
      [ "CREATE TABLE users (id INT, name TEXT, age INT, active BOOL)"
      , "INSERT INTO users VALUES (1, 'alice', 30, true)"
      , "INSERT INTO users VALUES (2, 'bob', 25, false)"
      , "INSERT INTO users VALUES (3, 'carol', 40, true)"
      ]

  (* ---------------------------------------------------------------- *)

  fun testTokenizer () =
    let
      val () = section "tokenizer"
      val toks = Sql.tokenize "SELECT * FROM t WHERE x >= 3"
      val () = checkInt "token count" (8, List.length toks)
      val () = check "first is SELECT keyword"
                 (case toks of (TKeyword "SELECT") :: _ => true | _ => false)
      val () = check "star symbol present"
                 (List.exists (fn t => t = TSymbol "*") toks)
      val () = check ">= operator tokenized as one symbol"
                 (List.exists (fn t => t = TSymbol ">=") toks)
      val () = check "ident t present"
                 (List.exists (fn t => t = TIdent "t") toks)
      val () = check "number 3 present"
                 (List.exists (fn t => t = TNum "3") toks)

      val strToks = Sql.tokenize "INSERT INTO t VALUES ('hello world', 3.14)"
      val () = check "string literal contents extracted"
                 (List.exists (fn t => t = TStr "hello world") strToks)
      val () = check "real literal lexeme"
                 (List.exists (fn t => t = TNum "3.14") strToks)
      val () = check "keywords are case-insensitive -> upper"
                 (case Sql.tokenize "select" of [TKeyword "SELECT"] => true | _ => false)
    in () end

  fun testCreateInsertSelectStar () =
    let
      val () = section "create / insert / select *"
      val db = fixture ()
      val res = Sql.query db "SELECT * FROM users"
      val () = checkStringList "column names"
                 (["id","name","age","active"], #columns res)
      val () = checkInt "row count" (3, List.length (#rows res))
      val () = checkRows "all rows in insertion order"
                 ([ [Int 1, Text "alice", Int 30, Bool true]
                  , [Int 2, Text "bob",   Int 25, Bool false]
                  , [Int 3, Text "carol", Int 40, Bool true] ],
                  #rows res)
    in () end

  fun testInsertWithColumns () =
    let
      val () = section "insert with explicit columns"
      val db = run' Sql.empty
                 [ "CREATE TABLE p (a INT, b TEXT, c INT)"
                 , "INSERT INTO p (c, a) VALUES (99, 7)" ]
      val () = checkRows "unspecified column b is null"
                 ([ [Int 7, Null, Int 99] ], rowsOf db "SELECT * FROM p")
    in () end

  fun testProjection () =
    let
      val () = section "projection"
      val db = fixture ()
      val res = Sql.query db "SELECT name, age FROM users"
      val () = checkStringList "projected columns" (["name","age"], #columns res)
      val () = checkRows "projected values"
                 ([ [Text "alice", Int 30]
                  , [Text "bob",   Int 25]
                  , [Text "carol", Int 40] ], #rows res)
    in () end

  fun testWhereComparisons () =
    let
      val () = section "where comparisons"
      val db = fixture ()
      val () = checkRows "="
                 ([ [Int 2, Text "bob", Int 25, Bool false] ],
                  rowsOf db "SELECT * FROM users WHERE id = 2")
      val () = checkRows "<>"
                 ([ [Int 1, Text "alice", Int 30, Bool true]
                  , [Int 3, Text "carol", Int 40, Bool true] ],
                  rowsOf db "SELECT * FROM users WHERE id <> 2")
      val () = checkRows "<"
                 ([ [Int 2, Text "bob", Int 25, Bool false] ],
                  rowsOf db "SELECT * FROM users WHERE age < 30")
      val () = checkRows "<="
                 ([ [Int 1, Text "alice", Int 30, Bool true]
                  , [Int 2, Text "bob", Int 25, Bool false] ],
                  rowsOf db "SELECT * FROM users WHERE age <= 30")
      val () = checkRows ">"
                 ([ [Int 3, Text "carol", Int 40, Bool true] ],
                  rowsOf db "SELECT * FROM users WHERE age > 30")
      val () = checkRows ">="
                 ([ [Int 1, Text "alice", Int 30, Bool true]
                  , [Int 3, Text "carol", Int 40, Bool true] ],
                  rowsOf db "SELECT * FROM users WHERE age >= 30")
      val () = checkRows "text equality"
                 ([ [Int 1, Text "alice", Int 30, Bool true] ],
                  rowsOf db "SELECT * FROM users WHERE name = 'alice'")
      val () = checkRows "bool equality"
                 ([ [Int 2, Text "bob", Int 25, Bool false] ],
                  rowsOf db "SELECT * FROM users WHERE active = false")
    in () end

  fun testLogical () =
    let
      val () = section "AND / OR / NOT"
      val db = fixture ()
      val () = checkRows "AND"
                 ([ [Int 1, Text "alice", Int 30, Bool true]
                  , [Int 3, Text "carol", Int 40, Bool true] ],
                  rowsOf db "SELECT * FROM users WHERE age > 25 AND active = true")
      val () = checkRows "OR"
                 ([ [Int 1, Text "alice", Int 30, Bool true]
                  , [Int 2, Text "bob", Int 25, Bool false] ],
                  rowsOf db "SELECT * FROM users WHERE id = 1 OR id = 2")
      val () = checkRows "NOT"
                 ([ [Int 2, Text "bob", Int 25, Bool false] ],
                  rowsOf db "SELECT * FROM users WHERE NOT active = true")
      val () = checkRows "precedence: AND binds tighter than OR"
                 ([ [Int 1, Text "alice", Int 30, Bool true]
                  , [Int 3, Text "carol", Int 40, Bool true] ],
                  rowsOf db "SELECT * FROM users WHERE id = 1 OR age > 25 AND active = true")
    in () end

  fun testOrderBy () =
    let
      val () = section "ORDER BY"
      val db = fixture ()
      val () = checkRows "ASC"
                 ([ [Text "bob",   Int 25]
                  , [Text "alice", Int 30]
                  , [Text "carol", Int 40] ],
                  rowsOf db "SELECT name, age FROM users ORDER BY age ASC")
      val () = checkRows "DESC"
                 ([ [Text "carol", Int 40]
                  , [Text "alice", Int 30]
                  , [Text "bob",   Int 25] ],
                  rowsOf db "SELECT name, age FROM users ORDER BY age DESC")
      val () = checkRows "default ASC"
                 ([ [Text "alice"], [Text "bob"], [Text "carol"] ],
                  rowsOf db "SELECT name FROM users ORDER BY name")
    in () end

  fun testLimit () =
    let
      val () = section "LIMIT"
      val db = fixture ()
      val () = checkRows "limit 2 after order"
                 ([ [Text "carol", Int 40], [Text "alice", Int 30] ],
                  rowsOf db "SELECT name, age FROM users ORDER BY age DESC LIMIT 2")
      val () = checkInt "limit 0 -> empty"
                 (0, List.length (rowsOf db "SELECT * FROM users LIMIT 0"))
      val () = checkInt "limit larger than rows"
                 (3, List.length (rowsOf db "SELECT * FROM users LIMIT 100"))
    in () end

  fun testJoin () =
    let
      val () = section "inner JOIN"
      val db = run' Sql.empty
                 [ "CREATE TABLE users (id INT, name TEXT)"
                 , "INSERT INTO users VALUES (1, 'alice')"
                 , "INSERT INTO users VALUES (2, 'bob')"
                 , "CREATE TABLE orders (uid INT, item TEXT)"
                 , "INSERT INTO orders VALUES (1, 'book')"
                 , "INSERT INTO orders VALUES (1, 'pen')"
                 , "INSERT INTO orders VALUES (2, 'cup')"
                 , "INSERT INTO orders VALUES (3, 'ghost')" ]
      val res = Sql.query db
                  "SELECT users.name, orders.item FROM users JOIN orders ON users.id = orders.uid ORDER BY orders.item"
      val () = checkRows "joined rows (only matching uids)"
                 ([ [Text "alice", Text "book"]
                  , [Text "bob",   Text "cup"]
                  , [Text "alice", Text "pen"] ],
                  #rows res)
      val () = checkInt "no row for uid=3 ghost order" (3, List.length (#rows res))
    in () end

  fun testAggregates () =
    let
      val () = section "aggregates"
      val db = fixture ()
      val () = checkValue "COUNT(*)"
                 (Int 3, hd (hd (rowsOf db "SELECT COUNT(*) FROM users")))
      val () = checkValue "COUNT(col)"
                 (Int 3, hd (hd (rowsOf db "SELECT COUNT(id) FROM users")))
      val () = checkValue "SUM(age)"
                 (Int 95, hd (hd (rowsOf db "SELECT SUM(age) FROM users")))
      val () = checkValue "MIN(age)"
                 (Int 25, hd (hd (rowsOf db "SELECT MIN(age) FROM users")))
      val () = checkValue "MAX(age)"
                 (Int 40, hd (hd (rowsOf db "SELECT MAX(age) FROM users")))
      val () = checkValue "COUNT(*) with WHERE"
                 (Int 2, hd (hd (rowsOf db "SELECT COUNT(*) FROM users WHERE active = true")))
    in () end

  fun testRendering () =
    let
      val () = section "rendering helpers"
      val () = checkString "int" ("1", Sql.valueToString (Int 1))
      val () = checkString "text quoted" ("'hi'", Sql.valueToString (Text "hi"))
      val () = checkString "bool" ("true", Sql.valueToString (Bool true))
      val () = checkString "null" ("NULL", Sql.valueToString Null)
    in () end

  fun testErrors () =
    let
      val () = section "error cases"
      val db = fixture ()
      val () = checkRaises "unknown table"
                 (fn () => Sql.query db "SELECT * FROM nope")
      val () = checkRaises "unknown column"
                 (fn () => Sql.query db "SELECT nope FROM users")
      val () = checkRaises "type mismatch on insert"
                 (fn () => Sql.exec db "INSERT INTO users VALUES ('x', 'y', 'z', 'w')")
      val () = checkRaises "parse error: garbage"
                 (fn () => Sql.exec db "SELEKT * FROM users")
      val () = checkRaises "parse error: incomplete"
                 (fn () => Sql.exec db "SELECT * FROM")
      val () = checkRaises "insert wrong arity"
                 (fn () => Sql.exec db "INSERT INTO users VALUES (1)")
      val () = checkRaises "getTable unknown"
                 (fn () => Sql.getTable db "missing")
    in () end

  fun runAll () =
    ( testTokenizer ()
    ; testCreateInsertSelectStar ()
    ; testInsertWithColumns ()
    ; testProjection ()
    ; testWhereComparisons ()
    ; testLogical ()
    ; testOrderBy ()
    ; testLimit ()
    ; testJoin ()
    ; testAggregates ()
    ; testRendering ()
    ; testErrors ()
    )

  fun run () = (reset (); runAll (); Harness.run ())
end
