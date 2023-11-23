let parensMap = Map.ofList [
    '(', ')';
    '{', '}';
]

let rec checkParensBalance parens chars =
    if Seq.isEmpty chars then
        if List.length parens = 0 then true else false
    else
        let c = Seq.head chars
        let cRest = Seq.tail chars

        match parens with
        | p :: pRest when p = c -> checkParensBalance pRest cRest
        | _ ->
            match Map.tryFind c parensMap with
            | None -> checkParensBalance parens cRest
            | Some p -> checkParensBalance (p :: parens) cRest

let printAsComment (text: string) =
    let text' = text.ReplaceLineEndings ()
    for line in text'.Split System.Environment.NewLine do
        printfn "# %s" line

let rec runText scope text symbol =
    let tokens = Lexer.tokenize Ast.lexerRules Ast.lexerIgnores text
    let csts = Parser.parse Ast.parserRules symbol tokens

    match csts with
    | [cst] -> Ok <| Interp.eval scope (Ast.transform cst)
    | [] -> Error "Invalid input"
    | _ -> Error "Ambiguous input"

let runFile scope path =
    match runText scope (System.IO.File.ReadAllText path) Ast.parserStart with
    | Ok (_, result) -> printfn "%A" result
    | Error err -> printfn "Error: %s" err

let rec runRepl scope lines =
    if (List.length lines > 0) && (checkParensBalance [] (Seq.rev lines |> Seq.concat)) then
        try
            match runText scope (Seq.rev lines |> String.concat "\n") Ast.parserStart with
            | Ok (scope', result) ->
                match result with
                | Interp.VUnit -> ()
                | _ ->
                    printAsComment (sprintf "%A" result)
                    printfn ""
                runRepl scope' []
            | Error err ->
                printAsComment (sprintf "Error: %s" err)
                printfn ""
                runRepl scope []
        with e ->
            printAsComment (sprintf "Error: %A" e)
            printfn ""
            runRepl scope []
    else
        let line = System.Console.ReadLine ()
        if not (isNull line) then
            runRepl scope (line :: lines)

[<EntryPoint>]
let main args =
    match Array.length args with
    | 0 ->
        printAsComment "Welcome to the Kon REPL!"
        printfn ""
        runRepl Interp.builtins []
    | 1 -> runFile Interp.builtins args[0]
    | _ -> raise (System.Exception "Too many arguments")

    0
