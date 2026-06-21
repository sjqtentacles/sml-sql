# sml-sql

A pure, in-memory relational query engine for a small SQL subset, written in
Standard ML with **no external dependencies**. It ships a hand-rolled tokenizer,
a recursive-descent parser, and an evaluator over in-memory tables. It is **not**
a binding to SQLite or any C library.

Builds and tests pass on both **MLton** and **Poly/ML**.

## Installation

```
smlpkg add github.com/sjqtentacles/sml-sql
smlpkg sync
```

Then add the library to your MLB file:

```
$(SML_LIB)/basis/basis.mlb
lib/github.com/sjqtentacles/sml-sql/sources.mlb
```

## Overview

Everything is exposed through the `SQL` signature and the
`structure Sql :> SQL`. The two entry points are:

- `Sql.exec : database -> string -> result` — run any statement. `CREATE` and
  `INSERT` return `Updated db'`; `SELECT` returns `Rows {columns, rows}`.
- `Sql.query : database -> string -> {columns, rows}` — run a `SELECT` and get
  its result set directly (raises if the statement was not a query).

Values are a single datatype:

```sml
datatype value = Int of int | Real of real | Text of string | Bool of bool | Null
```

## Supported SQL grammar

The engine understands the following subset (keywords are case-insensitive):

```
statement   ::= create | insert | select

create      ::= "CREATE" "TABLE" ident "(" coldef ("," coldef)* ")"
coldef      ::= ident type
type        ::= "INT" | "INTEGER" | "REAL" | "FLOAT" | "TEXT" | "BOOL" | "BOOLEAN"

insert      ::= "INSERT" "INTO" ident ["(" ident ("," ident)* ")"]
                "VALUES" "(" literal ("," literal)* ")"

select      ::= "SELECT" selitems
                "FROM" ident
                ["JOIN" ident "ON" expr]
                ["WHERE" expr]
                ["ORDER" "BY" colref ["ASC" | "DESC"]]
                ["LIMIT" integer]

selitems    ::= "*" | selitem ("," selitem)*
selitem     ::= colref | aggregate
aggregate   ::= ("COUNT" "(" ("*" | colref) ")")
              | (("SUM" | "MIN" | "MAX") "(" colref ")")

expr        ::= orExpr
orExpr      ::= andExpr ("OR" andExpr)*
andExpr     ::= notExpr ("AND" notExpr)*
notExpr     ::= "NOT" notExpr | cmpExpr
cmpExpr     ::= primary [ ("=" | "<>" | "<" | "<=" | ">" | ">=") primary ]
primary     ::= literal | colref
colref      ::= ident | ident "." ident
literal     ::= integer | real | string | "TRUE" | "FALSE" | "NULL"
```

Notes and limitations (it is a deliberate subset):

- Inner `JOIN ... ON` only (no outer joins). At most one join per query.
- `WHERE` predicates compose comparisons with `AND`/`OR`/`NOT`. `AND` binds
  tighter than `OR`; `NOT` binds tightest. Comparisons against `NULL` are false.
- Aggregates (`COUNT(*)`, `COUNT(col)`, `SUM`, `MIN`, `MAX`) operate over the
  whole (optionally filtered) result. There is no `GROUP BY`.
- String literals use single quotes; `''` is an escaped quote.
- An integer literal inserted into a `REAL` column is widened automatically.

## Usage example

```sml
val db0 = Sql.empty

(* thread statements through exec *)
fun run db sql = case Sql.exec db sql of Sql.Updated db' => db' | _ => db

val db =
  List.foldl (fn (s, d) => run d s) db0
    [ "CREATE TABLE users (id INT, name TEXT, age INT, active BOOL)"
    , "INSERT INTO users VALUES (1, 'alice', 30, true)"
    , "INSERT INTO users VALUES (2, 'bob', 25, false)"
    , "INSERT INTO users VALUES (3, 'carol', 40, true)" ]

(* projection + filter + order + limit *)
val res = Sql.query db
  "SELECT name, age FROM users WHERE age >= 30 ORDER BY age DESC LIMIT 1"
(* res = { columns = ["name","age"], rows = [[Text "carol", Int 40]] } *)

(* aggregates *)
val total = Sql.query db "SELECT COUNT(*), SUM(age) FROM users"
(* total.rows = [[Int 3, Int 95]] *)

(* inner join *)
val db2 = run (run db "CREATE TABLE orders (uid INT, item TEXT)")
              "INSERT INTO orders VALUES (1, 'book')"
val joined = Sql.query db2
  "SELECT users.name, orders.item FROM users JOIN orders ON users.id = orders.uid"
```

`Sql.valueToString` and `Sql.rowToString` render values and rows for display.
The tokenizer is also exposed as `Sql.tokenize : string -> token list`.

Errors are raised as `Sql.ParseError`, `Sql.TypeError`, or `Sql.RuntimeError`
(e.g. unknown table/column, arity or type mismatch, malformed SQL).

## Testing

```
make test       # MLton
make test-poly  # Poly/ML
```

Both run the same suite (52 checks) covering the tokenizer, CREATE/INSERT/SELECT,
projection, every comparison operator, AND/OR/NOT, ORDER BY ASC/DESC, LIMIT,
inner JOIN, aggregates, and error cases.

## License

MIT
