module Stmt

type t = 
       | Read   of string
       | Write  of Expr.t
       | Assign of string * Expr.t
       | Seq    of t * t
       | If     of Expr.t * t * t
       | While  of Expr.t * t

module Parser =
  open CoreParser
  let readP   =
    parser {
      let! _ = symbol &"read"
      return! paren' (map (fun x -> Read x) word)
    }
  let writeP  =
    parser {
      let! _ = symbol &"write"
      return! paren' (map Write (Expr.Parser.parse ()))
    }
  let assignP =
    parser {
      let! x = (sp word << sp (symbol &":="))
      return! map (fun e -> Assign (x, e)) (Expr.Parser.parse ())
    }

  let ifP p =
    parser {
      let! _   = (symbol &"if" |> sp)
      let! e   = (Expr.Parser.parse () |> sp)
      let! op1 = p() |> cparen'
      let! _   = (symbol &"else" |> sp)
      return! p() |> cparen' |> map (fun op2 -> If (e, op1, op2))
    }
  let whileP p =
    parser {
      let! _ = (symbol &"while" |> sp)
      let! e = (Expr.Parser.parse () |> sp)
      return! p() |> cparen' |> map (fun op -> While (e, op))
    }

  let term p =
    parser {
      return! readP
      return! writeP
      return! assignP
      return! ifP p
      return! whileP p
    }

  let seqP p =
    parser {
      let! leftOp = (term p << sp (char ';'))
      return! p() |> sp |> map (fun rightOp -> Seq (leftOp, rightOp))
    }

  let rec parse _: t CoreParser.t =
    seqP parse <|> term parse