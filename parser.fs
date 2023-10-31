module Parser

// This parser uses the Generalized LL (GLL) parsing algorithm
// implemented in asynchronous continuation-passing style

type Node =
    | Token of string * string
    | Symbol of string * Node
    | Group of Node list
    | Epsilon

type Expr =
    | Tok of string
    | Sym of string
    | Opt of Expr
    | Alt of Expr list
    | Cat of Node list * Expr list
    | Rep of Node list * Expr

type Key = int * string // int = index of the first token
type Value = Node * int // int = index of the next token
type State = {
    input: (string * string) list
    cache: Map<Key, Callback list * Value list>
    rules: Map<string, Expr list>
}
and Callback = Value -> Exec.Task<State>

let rec matchExpr offset expr cont ctx =
    match expr with
    | Tok name ->
        matchToken offset name cont ctx
    | Sym name ->
        matchSymbol offset name cont ctx
    | Opt opt ->
        Exec.scheduleTask (cont (Epsilon, offset)) ctx |> matchExpr offset opt cont
    | Alt opts ->
        Exec.scheduleTasks (Seq.map (matchExpr offset >> (|>) cont) opts) ctx
    | Cat (nodes, exprs) ->
        match exprs with
        | head :: rest ->
            matchExpr offset head (fun (node, ptr) newctx ->
                matchExpr ptr (Cat (node :: nodes, rest)) cont newctx
            ) ctx
        | [] ->
            cont (Group nodes, offset) ctx
    | Rep (nodes, opt) ->
        Exec.scheduleTask (cont (Group nodes, offset)) ctx
        |> matchExpr offset opt (fun (node, ptr) newctx ->
            matchExpr ptr (Rep (node :: nodes, opt)) cont newctx
        )

and matchRule offset name expr =
    matchExpr offset expr (fun (node, ptr) ctx ->
        triggerCallbacks (offset, name) (Symbol (name, node), ptr) ctx
    )

and matchToken offset name callback (ctx: Exec.Context<State>) =
    match List.tryItem offset ctx.state.input with
    | Some token when fst token = name -> callback (Token token, offset + 1) ctx
    | _ -> ctx

and matchSymbol offset name callback (ctx: Exec.Context<State>) =
    let key = (offset, name)
    match Map.tryFind key ctx.state.cache with
    | Some (callbacks, history) ->
        let cache = Map.add key (callback :: callbacks, history) ctx.state.cache
        Exec.updateState { ctx.state with cache = cache } ctx
        |> Exec.scheduleTasks (Seq.map callback history)
    | None ->
        let cache = Map.add key ([callback], []) ctx.state.cache
        Exec.updateState { ctx.state with cache = cache } ctx
        |> Exec.scheduleTasks (Map.find name ctx.state.rules |> Seq.map (matchRule offset name))

and triggerCallbacks key value (ctx: Exec.Context<State>) =
    match Map.tryFind key ctx.state.cache with
    | Some (callbacks, history) ->
        let cache = Map.add key (callbacks, value :: history) ctx.state.cache
        Exec.updateState { ctx.state with cache = cache } ctx
        |> Exec.scheduleTasks (Seq.map ((|>) value) callbacks)
    | None -> ctx

let parse rulemap input start =
    let mutable result = List.empty
    let state = { input = input; cache = Map.empty; rules = rulemap }
    let callback value ctx =
        result <- value :: result
        ctx

    Exec.runTask (matchSymbol 0 start callback) state |> ignore
    result
