module ExprTest

open Expr
open Expr.Parser
open CoreParser

open NUnit.Framework

[<TestCase ("1+2", Result=3)>]
[<TestCase ("5 * (2 - 1)", Result=5)>]
[<TestCase ("1 - 2 - 3", Result=(-4))>]
[<TestCase ("1 * 2 - 3", Result=(-1))>]
[<TestCase ("1 - 2 * 3", Result=(-5))>]
[<TestCase ("1 * 2 + 3", Result=5)>]
[<TestCase ("2 ^ 2 ", Result=4)>]
[<TestCase ("3 ^ 1 ^ 2", Result=3)>]
[<TestCase ("(3 ^ 1) ^ 2", Result=9)>]
let calcTest s =
  let expr = (&s) |> parse () |> List.head |> fst
  let someRes = calc (fun _ -> None) expr
  let (Some res) = someRes
  res