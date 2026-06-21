(* sql.sig - a pure in-memory relational query engine over a small SQL subset *)

signature SQL =
sig
  (* ---- Values and types ---- *)

  datatype value =
      Int  of int
    | Real of real
    | Text of string
    | Bool of bool
    | Null

  datatype colType = TInt | TReal | TText | TBool

  (* ---- Schema, tables, database ---- *)

  type column = { name : string, ty : colType }
  type row    = value list

  type table  = { name : string, columns : column list, rows : row list }
  type database

  (* ---- Errors ---- *)

  exception ParseError of string
  exception TypeError  of string
  exception RuntimeError of string

  (* ---- Constructor API (for tests / programmatic use) ---- *)

  val empty   : database
  val tables  : database -> table list
  val getTable : database -> string -> table

  (* ---- Tokenizer (exposed for testing) ---- *)

  datatype token =
      TIdent   of string
    | TNum     of string         (* raw numeric lexeme, e.g. "42" or "3.14" *)
    | TStr     of string         (* string literal contents, unescaped *)
    | TSymbol  of string         (* punctuation/operators: ( ) , * = <> < <= > >= . *)
    | TKeyword of string         (* upper-cased SQL keyword *)

  val tokenize : string -> token list

  (* ---- Execution ---- *)

  (* A statement either updates the database (CREATE/INSERT) or produces a
     result set (SELECT). *)
  datatype result =
      Updated of database
    | Rows of { columns : string list, rows : row list }

  val exec  : database -> string -> result

  (* Convenience: run a SELECT and return its rows, raising if the
     statement was not a query. *)
  val query : database -> string -> { columns : string list, rows : row list }

  (* ---- Rendering helpers ---- *)

  val valueToString : value -> string
  val rowToString   : row -> string
end
