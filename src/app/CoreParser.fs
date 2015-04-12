module CoreParser

type 'a t = char list -> ('a * char list) list

type ParserMonad () =
  member this.Return (r : 'a) = fun s -> [(r, s)]
  member this.Bind (p : 'a t, f : 'a -> 'b t) = fun s ->
    List.concat [for (r, s') in p s -> (f r) s']
  member this.ReturnFrom (x) = x
  member this.Combine (p, q) = fun s ->
    match p s with
    | [] -> q s
    | rs -> rs
  member this.Delay (f) = f ()
let parser = new ParserMonad ()

let (>>=) (p: 'a t) (f: 'a -> 'b t) = fun s ->
  List.concat [for (r, s') in p s -> (f r) s']

let mreturn r = fun s -> [(r, s)]
let lambda    = fun s -> []
let item      = fun s -> match s with [] -> [] | h :: s -> [(h, s)]
let sat cond  = item >>= fun c -> if cond c then mreturn c else lambda

let (>>) p q = p >>= fun _ -> q
let (<<) p q = p >>= fun rs -> q >> mreturn rs

let char c    = sat ((=) c)
let digit     = sat (fun c -> (List.tryFind ((=) c) ['0'..'9']).IsSome) 
let alpha     = sat (fun c ->
  (List.tryFind ((=) c) (List.append ['a'..'z'] ['A'..'Z'])).IsSome)

let (<|>) p q = fun s ->
  match p s with
  | [] -> q s
  | rs -> rs  
let (++) p q = fun s -> List.append (p s) (q s)

let rec many0 p =
  parser {
    return! many1 p
    return  []
  }
and many1 p =
  parser {
    let! r  = p
    let! rs = many0 p
    return (r::rs)
  }

let rec symbol cs =
  match cs with
  | [] -> mreturn [] 
  | c::cs' ->
    parser {
      let! _ = (char c)
      let! _ = (symbol cs')
      return cs
    }

let (~&) (s: string   ) = s.ToCharArray() |> List.ofArray
let (~%) (l: char list) = new string(Array.ofList l)

let map (f: 'a -> 'b) (p: 'a t): 'b t = fun s ->
  p s |> List.map (fun (e, s) -> (f e, s)) 

let rec fold (f: 'a -> 'b -> 'b) (init : 'b) (p: 'a t): 'b t =
  (p >>= fun pRes -> fold f (f pRes init) p) <|> mreturn init

let number = map (fun s -> %s |> System.Int32.Parse) (many1 digit) 
let word   = map (~%) (many1 alpha)
let space =
  parser {
    return! char ' '
    return! char '\t'
    return! char '\010'
    let! _ = symbol &"\r\n"
    return '\n'
    return! char '\n'
  }
let spaces = many0 space
let sp   f = spaces >> f << spaces 

let gparen lparen rparen p =
  parser {
    let! _ = sp lparen
    return! p() << sp rparen
  }

let paren  p = gparen (char '(') (char ')') p
let paren' p = paren (fun _ -> p)  

let cparen  p = gparen (char '{') (char '}') p
let cparen' p = cparen (fun _ -> p) 