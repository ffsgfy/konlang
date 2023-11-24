module Interp

type Value =
    | VUnit
    | VInt of int64
    | VStr of string
    | VBool of bool
    | VLazy of Lazy
    | VFunc of Func
    | VFuncR of Func * string
    | VCtrl of (Scope * Value -> Scope * Value)

and Lazy = Scope * Ast.Node

and Func = Lazy * string list

and Scope = Map<string, Value>

let rec eval scope ast =
    match ast with
    | Ast.Int value -> (scope, VInt value)
    | Ast.Str value -> (scope, VStr value)
    | Ast.Ident ident -> (scope, Map.find ident scope)
    | Ast.Lazy inner -> (scope, VLazy (scope, inner))
    | Ast.Chain ([], None) -> (scope, VUnit)
    | Ast.Chain ([], Some result) -> eval scope result
    | Ast.Chain (head :: rest, result) -> eval (eval scope head |> fst) (Ast.Chain (rest, result))
    | Ast.Call (func, arg) ->
        let (scope', funcv) = eval scope func
        let (scope'', argv) = eval scope' arg
        call scope funcv argv

and call scope func arg =
    match func with
    | VFunc ((lscope, ast), [par]) -> (scope, eval (Map.add par arg lscope) ast |> snd)
    | VFunc ((lscope, ast), par :: rest) -> (scope, VFunc ((Map.add par arg lscope, ast), rest))
    | VFuncR (((lscope, ast), pars), rname) -> call scope (VFunc ((Map.add rname func lscope, ast), pars)) arg
    | VCtrl ctrl -> ctrl (scope, arg)
    | _ -> raise (System.Exception "Call target is not a valid function")

let wrapCtrl ctrl (scope, arg) = (scope, ctrl arg)

let ctrlBind argName =
    match argName with
    | VStr name -> VCtrl (fun (scope, argValue) -> (Map.add name argValue scope, VUnit))
    | _ -> raise (System.Exception "Argument must be a string")

let ctrlBindRec argName =
    match argName with
    | VStr name ->
        VCtrl (fun (scope, argFunc) ->
            match argFunc with
            | VFunc func -> (Map.add name (VFuncR (func, name)) scope, VUnit)
            | _ -> raise (System.Exception "Argument must be a function")
        )
    | _ -> raise (System.Exception "Argument must be a string")

let ctrlBindLazy argName =
    match argName with
    | VStr name ->
        VCtrl <| wrapCtrl (fun argValue ->
            VCtrl <| wrapCtrl (fun argLazy ->
                match argLazy with
                | VLazy (lscope, ast) -> VLazy (Map.add name argValue lscope, ast)
                | _ -> raise (System.Exception "Argument must be a lazy expression")
            )
        )
    | _ -> raise (System.Exception "Argument must be a string")

let ctrlEval argLazy =
    match argLazy with
    | VLazy (lscope, ast) -> eval lscope ast |> snd
    | _ -> raise (System.Exception "Argument must be a lazy expression")

let rec ctrlFunc pars arg =
    match arg with
    | VStr par -> VCtrl <| wrapCtrl (ctrlFunc (par :: pars))
    | VLazy inner ->
        match pars with
        | [] -> raise (System.Exception "Function must accept at least one parameter")
        | _ -> VFunc (inner, List.rev pars)
    | _ -> raise (System.Exception "Argument must be a string or a lazy expression")

let ctrlIf argCond =
    match argCond with
    | VBool cond ->
        VCtrl <| wrapCtrl (fun argTrueBranch ->
            VCtrl <| wrapCtrl (fun argFalseBranch ->
                ctrlEval (if cond then argTrueBranch else argFalseBranch)
            )
        )
    | _ -> raise (System.Exception "Argument must be a boolean")

let ctrlAdd argLhs =
    VCtrl <| wrapCtrl (fun argRhs ->
        match (argLhs, argRhs) with
        | (VInt lhs, VInt rhs) -> VInt (lhs + rhs)
        | _ -> raise (System.Exception "Arguments must be integers")
    )

let ctrlSub argLhs =
    VCtrl <| wrapCtrl (fun argRhs ->
        match (argLhs, argRhs) with
        | (VInt lhs, VInt rhs) -> VInt (lhs - rhs)
        | _ -> raise (System.Exception "Arguments must be integers")
    )

let ctrlMul argLhs =
    VCtrl <| wrapCtrl (fun argRhs ->
        match (argLhs, argRhs) with
        | (VInt lhs, VInt rhs) -> VInt (lhs * rhs)
        | _ -> raise (System.Exception "Arguments must be integers")
    )

let ctrlDiv argLhs =
    VCtrl <| wrapCtrl (fun argRhs ->
        match (argLhs, argRhs) with
        | (VInt lhs, VInt rhs) -> VInt (lhs / rhs)
        | _ -> raise (System.Exception "Arguments must be integers")
    )

let ctrlMod argLhs =
    VCtrl <| wrapCtrl (fun argRhs ->
        match (argLhs, argRhs) with
        | (VInt lhs, VInt rhs) -> VInt (lhs % rhs)
        | _ -> raise (System.Exception "Arguments must be integers")
    )

let ctrlEq argLhs =
    VCtrl <| wrapCtrl (fun argRhs ->
        match (argLhs, argRhs) with
        | (VUnit, VUnit) -> VBool true
        | (VInt lhs, VInt rhs) -> VBool (lhs = rhs)
        | (VStr lhs, VStr rhs) -> VBool (lhs = rhs)
        | (VBool lhs, VBool rhs) -> VBool (lhs = rhs)
        | _ -> VBool false
    )

let ctrlLt argLhs =
    VCtrl <| wrapCtrl (fun argRhs ->
        match (argLhs, argRhs) with
        | (VInt lhs, VInt rhs) -> VBool (lhs < rhs)
        | _ -> raise (System.Exception "Arguments must be integers")
    )

let ctrlLe argLhs =
    VCtrl <| wrapCtrl (fun argRhs ->
        match (argLhs, argRhs) with
        | (VInt lhs, VInt rhs) -> VBool (lhs <= rhs)
        | _ -> raise (System.Exception "Arguments must be integers")
    )

let ctrlGt argLhs =
    VCtrl <| wrapCtrl (fun argRhs ->
        match (argLhs, argRhs) with
        | (VInt lhs, VInt rhs) -> VBool (lhs > rhs)
        | _ -> raise (System.Exception "Arguments must be integers")
    )

let ctrlGe argLhs =
    VCtrl <| wrapCtrl (fun argRhs ->
        match (argLhs, argRhs) with
        | (VInt lhs, VInt rhs) -> VBool (lhs >= rhs)
        | _ -> raise (System.Exception "Arguments must be integers")
    )

let ctrlOr argLhs =
    VCtrl <| wrapCtrl (fun argRhs ->
        match (argLhs, argRhs) with
        | (VBool lhs, VBool rhs) -> VBool (lhs || rhs)
        | _ -> raise (System.Exception "Arguments must be booleans")
    )

let ctrlAnd argLhs =
    VCtrl <| wrapCtrl (fun argRhs ->
        match (argLhs, argRhs) with
        | (VBool lhs, VBool rhs) -> VBool (lhs && rhs)
        | _ -> raise (System.Exception "Arguments must be booleans")
    )

let ctrlNot arg =
    match arg with
    | VBool value -> VBool (not value)
    | _ -> raise (System.Exception "Argument must be a boolean")

let ctrlPrint arg = printfn "%A" arg; VUnit

let builtins = Map.ofList [
    "bind", VCtrl <| wrapCtrl ctrlBind;
    "bindr", VCtrl <| wrapCtrl ctrlBindRec;
    "bindl", VCtrl <| wrapCtrl ctrlBindLazy;
    "eval", VCtrl <| wrapCtrl ctrlEval;
    "func", VCtrl <| wrapCtrl (ctrlFunc []);
    "if", VCtrl <| wrapCtrl ctrlIf;
    "add", VCtrl <| wrapCtrl ctrlAdd;
    "sub", VCtrl <| wrapCtrl ctrlSub;
    "mul", VCtrl <| wrapCtrl ctrlMul;
    "div", VCtrl <| wrapCtrl ctrlDiv;
    "mod", VCtrl <| wrapCtrl ctrlMod;
    "eq", VCtrl <| wrapCtrl ctrlEq;
    "lt", VCtrl <| wrapCtrl ctrlLt;
    "le", VCtrl <| wrapCtrl ctrlLe;
    "gt", VCtrl <| wrapCtrl ctrlGt;
    "ge", VCtrl <| wrapCtrl ctrlGe;
    "or", VCtrl <| wrapCtrl ctrlOr;
    "and", VCtrl <| wrapCtrl ctrlAnd;
    "not", VCtrl <| wrapCtrl ctrlNot;
    "print", VCtrl <| wrapCtrl ctrlPrint;

    "true", VBool true;
    "false", VBool false;
]
