module Expr

type t = Num of int
       | Var of string
       | BinOp of char * t * t

exception NotOperation 
let op s =
  match s with
  | '+' -> (+)
  | '-' -> (-)
  | '*' -> ( * )
  | '/' -> (/)
  | '%' -> ( % )
  | '^' -> pown
  | _   -> raise NotOperation

module Parser =
  open CoreParser

  exception EmptyList
  let fold1 f s =
    match s with
    | h :: s -> List.fold f h s
    | [] -> raise EmptyList

  let rec pLeftAssoc' opParser pNext =
    parser {
      let! op = opParser
      let!  r = pNext
      return (fun l -> BinOp (op, l, r))
    }
  let rec pLeftAssoc opParser pNext =
    let app f x = f x 
    parser {
      let! l = pNext
      return! fold app l (pLeftAssoc' opParser pNext)
    }
  let rec pRightAssoc opParser pNext =
    parser {
      let! leftArg = pNext
      let  l =
        parser {
          let! op = opParser
          let! s  = (pRightAssoc opParser pNext)
          return BinOp (op, leftArg, s)
        }
      return! (l <|> mreturn leftArg)

      (*
      let! op = opParser
      let! s  = pRightAssoc opParser pNext
      return BinOp (op, leftArg, s)
      return leftArg
      *)
    }

  let rec parse _ : t CoreParser.t =
    let op1Parser = char '^' |> sp
    let op2Parser = char '*' <|> char '/' <|> char '%' |> sp
    let op3Parser = char '+' <|> char '-' |> sp
    let term = (map Num number) <|> (map Var word) |> sp
    let baseParser = term <|> paren parse
    pLeftAssoc op3Parser (pLeftAssoc op2Parser (pRightAssoc op1Parser baseParser))

type OptionBuilder () =
  member this.Return(a) = Some a
  member this.Bind(p, f) =
    match p with
    | None   -> None
    | Some a -> f a
let option = OptionBuilder ()

let rec calc env e =
  match e with
  | Num n -> Some n
  | Var x -> env  x
  | BinOp (o, x, y) ->
    option {
      let! vx = calc env x
      let! vy = calc env y
      return (op o) vx vy
    }
