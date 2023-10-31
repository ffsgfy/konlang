module Regex

// Regular expressions here are matched using an implicit nondeterministic Glushkov automaton
// where the next state is computed on the fly from the previous state and the next input symbol
// The implementation is loosely based on "A Play on Regular Expressions" by S. Fischer et al.

type Expr =
    | Any
    | Chr of char
    | Rng of char * char
    | Opt of Expr
    | Alt of Expr list
    | Cat of Expr list
    | Rep of Expr

let onePlus expr = Cat [expr; Rep expr]

type State =
    {
        empty: bool array
        final: bool array
        mutable broken: bool // true when all elements in final are false
    }

    member this.getEmpty index = Array.get this.empty index
    member this.setEmpty index value = Array.set this.empty index value
    member this.getFinal index = Array.get this.final index
    member this.setFinal index value = Array.set this.final index value
    member this.reset () = Array.fill this.final 0 (Array.length this.final) false

let prepare expr =
    let rec getSize = function
        | Any | Chr _ | Rng _ -> 1
        | Opt item | Rep item -> 1 + getSize item
        | Alt items | Cat items -> 1 + Seq.sumBy getSize items

    let size = getSize expr
    let state = { empty = Array.zeroCreate size; final = Array.zeroCreate size; broken = true }

    let rec setEmpty index = function
        | Any | Chr _ | Rng _ -> 1
        | Opt item | Rep item -> state.setEmpty index true; 1 + setEmpty (index + 1) item
        | Alt items -> setEmptyItems (||) (items.Length = 0) index items
        | Cat items -> setEmptyItems (&&) true index items

    and setEmptyItems op initial index items =
        let mutable size = 1
        let mutable empty = initial

        for item in items do
            let delta = setEmpty (index + size) item
            empty <- op empty (state.getEmpty (index + size))
            size <- size + delta

        state.setEmpty index empty
        size

    setEmpty 0 expr |> ignore
    state

let rec shift (state: State) index mark chr expr =
    if index = 0 then
        state.broken <- true

    match expr with
    | Any ->
        let final = mark
        if final then
            state.broken <- false

        state.setFinal index final
        1

    | Chr c ->
        let final = mark && c = chr
        if final then
            state.broken <- false

        state.setFinal index final
        1

    | Rng (ca, cb) ->
        let final = mark && ca <= chr && chr <= cb
        if final then
            state.broken <- false

        state.setFinal index final
        1

    | Opt item ->
        let size = 1 + shift state (index + 1) mark chr item
        state.setFinal index (state.getFinal (index + 1))
        size

    | Alt items ->
        let mutable size = 1
        let mutable final = false

        for item in items do
            let delta = shift state (index + size) mark chr item
            final <- final || state.getFinal (index + size)
            size <- size + delta

        state.setFinal index final
        size

    | Cat items -> 
        let mutable size = 1
        let mutable empty = true
        let mutable oldfinal = false
        let mutable newfinal = false

        for item in items do
            let wasfinal = state.getFinal (index + size)
            let delta = shift state (index + size) (mark && empty || oldfinal) chr item

            if not (state.getEmpty (index + size)) then
                empty <- false
                oldfinal <- false
                newfinal <- false

            oldfinal <- oldfinal || wasfinal
            newfinal <- newfinal || state.getFinal (index + size)
            size <- size + delta

        state.setFinal index newfinal
        size

    | Rep item ->
        let size = 1 + shift state (index + 1) (mark || state.getFinal (index + 1)) chr item
        state.setFinal index (state.getFinal (index + 1))
        size

let test expr str =
    let state = prepare expr
    if String.length str = 0 then
        state.getEmpty 0
    else
        Seq.iteri (fun i chr -> shift state 0 (i = 0) chr expr |> ignore) str
        state.getFinal 0
