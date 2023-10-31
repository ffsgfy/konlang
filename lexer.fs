module Lexer

let rec shift (states: ((string * Regex.Expr) * Regex.State) list) input offset first =
    if offset < String.length input then
        for (name, expr), state in states do
            if first || not state.broken then
                Regex.shift state 0 first input[offset] expr |> ignore

        let accepting = states |> List.filter (fun x -> (snd x).getFinal 0)
        let result = shift states input (offset + 1) false

        match result with
        | Some _ -> result
        | None ->
            if List.length accepting > 0 then
                Some (List.head accepting |> fst |> fst, offset)
            else
                None
    else
        None

let rec step (states: ((string * Regex.Expr) * Regex.State) list) input offset output =
    if offset < String.length input then
        for _, state in states do
            state.reset ()

        match shift states input offset true with
        | Some (name, ending) -> step states input (ending + 1) ((name, input[offset..ending]) :: output)
        | None -> raise (System.Exception (sprintf "Failed to recognize token at offset %i" offset))
    else
        output

let tokenize exprs ignores input =
    let states = List.zip exprs (List.map (snd >> Regex.prepare) exprs)
    let output = step states input 0 List.empty
    List.filter (fst >> List.contains >> (|>) ignores >> not) output |> List.rev
