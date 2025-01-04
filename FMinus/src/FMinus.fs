module FMinus
open AST
open Types
// Evaluate expression into a value, under the given environment.
let rec evalExp (exp: Exp) (env: Env) : Val =
  match exp with
  | Num i -> Int i | True -> Bool true | False -> Bool false
  | Var x -> 
    match Map.tryFind x env with
    | Some i -> i | None -> raise UndefinedSemantics
  | Neg e ->
    match (evalExp e env) with
    | Int i -> Int (-1*i)
    | _ -> raise UndefinedSemantics
  | Add (a1, a2) ->
    match (evalExp a1 env, evalExp a2 env) with
    | (Int f, Int s) -> Int (f+s)
    | _ -> raise UndefinedSemantics
  | Sub (a1, a2) ->
    match (evalExp a1 env, evalExp a2 env) with
    | (Int f, Int s) -> Int (f-s)
    | _ -> raise UndefinedSemantics
  | LessThan (a1, a2) ->
    match (evalExp a1 env, evalExp a2 env) with
    | (Int f, Int s) -> Bool(f<s)
    | _ -> raise UndefinedSemantics
  | GreaterThan (a1, a2) ->
    match (evalExp a1 env, evalExp a2 env) with
    | (Int f, Int s) -> Bool(f>s)
    | _ -> raise UndefinedSemantics
  | Equal (a1, a2) ->
    match (evalExp a1 env, evalExp a2 env) with
    | (Int f, Int s) -> Bool(f=s)
    | (Bool f, Bool s) -> Bool(f=s)
    | _ -> raise UndefinedSemantics
  | NotEq (a1, a2) ->
    match (evalExp a1 env, evalExp a2 env) with
    | (Int f, Int s) -> Bool(f<>s)
    | (Bool f, Bool s) -> Bool(f<>s)
    | _ -> raise UndefinedSemantics
  | IfThenElse (c, e1, e2) ->
    match (evalExp c env) with
    | Bool true -> evalExp e1 env
    | Bool false -> evalExp e2 env
    | _ -> raise UndefinedSemantics
  | LetIn (s, e1, e2) ->
    let newenv = Map.add s (evalExp e1 env) env
    evalExp e2 newenv
  | LetFunIn (s,arg,e1,e2) ->
    let funenv = Map.add s (Val.Func (arg,e1,env)) env
    evalExp e2 funenv
  | LetRecIn (s,arg,e1,e2) ->
    let recfunenv = Map.add s (Val.RecFunc (s,arg,e1,env)) env
    evalExp e2 recfunenv
  | Fun (arg, e1) ->
    Func (arg,e1,env)
  | App (f,e) ->
    let xarg = evalExp e env
    let fn = evalExp f env
    match fn with
    | Func (arg, e1, envf) ->
      let envfapply = Map.add arg xarg envf
      evalExp e1 envfapply
    | RecFunc (f, arg, e1, envf) ->
      let envfapply = Map.add arg xarg envf
      let envrecfapply = Map.add f fn envfapply
      evalExp e1 envrecfapply
    | _ -> raise UndefinedSemantics
let run (prog: Program) : Val =
  evalExp prog Map.empty
