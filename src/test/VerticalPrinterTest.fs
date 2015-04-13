module VerticalPrinterTest

open NUnit.Framework
open CoreParser
open VerticalPrinter

[<TestCase ("1+2", Result="+\n1\n2\n")>]
[<TestCase ("5 * (2 - 1)", Result="*\n5\n-\n2\n1\n")>]
[<TestCase ("1 + 2 + 3", Result="+\n+\n1\n2\n3\n")>]
let exprTest s =
  let expr = (&s) |> Expr.Parser.parse () |> List.head |> fst
  exprPrinter expr

[<TestCase ("x := 5; y:= 1 - x", Result=";\n:=\nx\n5\n:=\ny\n-\n1\nx\n")>]
[<TestCase ("read(x); write(x + 1)", Result=";\nread\nx\nwrite\n+\nx\n1\n")>]
[<TestCase ("if (x) {write(x + 1)} else {write(x)}", Result="if\nx\nwrite\n+\nx\n1\nwrite\nx\n")>]
let stmtTest s =
  let stmt = (&s) |> Stmt.Parser.parse () |> List.head |> fst
  printer stmt