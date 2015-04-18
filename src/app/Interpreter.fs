module Interpreter
  
open Stmt
open Expr
open Expr.Parser
open CoreParser
open System

let updateEnv env v e = fun x -> if x = v then e else env x


let readS env v = printf "Read(%s): " v
                  let e = try 
                            Console.ReadLine() |> System.Int32.Parse |> Some
                          with
                          | _ -> None
                  updateEnv env v e
let writeS env e = calc env e |> printfn "%A" 
                   env
let assignS env v e = updateEnv env v (calc env e)

exception UndefinedValues
let rec bs (env: string -> int option) (op: Stmt.t) =
  match op with
  | Read v  -> readS env v
  | Write e -> writeS env e 
  | Assign (v, e) -> assignS env v e 
  | Seq (op1, op2) -> let nenv = bs env op1
                      bs nenv op2
  | If (e, op1, op2) -> match calc env e with
                        | None -> raise UndefinedValues
                        | Some 0 -> bs env op2
                        | Some _ -> bs env op1
  | While (e, body) -> match calc env e with
                       | None -> raise UndefinedValues
                       | Some 0 -> env
                       | Some _ -> let nenv = bs env body
                                   bs nenv op

let rec ss (env: string -> int option) (op: Stmt.t) =
  match op with 
  | Read v  -> readS env v, None
  | Write e -> writeS env e, None
  | Assign (v, e) -> assignS env v e, None
  | Seq (op1, op2) -> let (nenv, nop1) = ss env op1
                      match nop1 with
                      | None -> nenv, Some op2
                      | Some op -> nenv, Some (Seq (op, op2))
  | If (e, op1, op2) -> match calc env e with
                        | None -> raise UndefinedValues
                        | Some 0 -> env, Some op2
                        | Some _ -> env, Some op1
  | While (e, body) -> match calc env e with
                       | None -> raise UndefinedValues
                       | Some 0 -> env, None
                       | Some _ -> env, Some (Seq (body, op))