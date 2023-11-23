module Ast

type Node =
    | Int of int64
    | Str of string
    | Ident of string
    | Lazy of Node
    | Chain of Node list * Node option
    | Call of Node * Node

let lexerRules = [
    "whitespace", Regex.onePlus (Regex.oneOf " \t\f\r\n");
    "ident", Regex.Cat [
        Regex.Alt [Regex.Rng ('a', 'z'); Regex.Rng ('A', 'Z'); Regex.Chr '_'];
        Regex.Alt [Regex.Rng ('a', 'z'); Regex.Rng ('A', 'Z'); Regex.Rng ('0', '9'); Regex.oneOf "_-"] |> Regex.Rep;
    ];
    "int", Regex.Alt [
        Regex.Chr '0';
        Regex.Cat [
            Regex.Opt (Regex.Chr '-');
            Regex.Rng ('1', '9');
            Regex.Rng ('0', '9') |> Regex.Rep;
        ];
    ];
    "str", Regex.Cat [
        Regex.Chr '"';
        Regex.Rep (Regex.Alt [
            Regex.Nch '"';
            Regex.Cat [Regex.Chr '\\'; Regex.Chr '"'];
        ]);
        Regex.Chr '"';
    ];
    "comment", Regex.Cat [Regex.Chr '#'; Regex.Rep (Regex.Nch '\n')];
    ";", Regex.Chr ';';
    "{", Regex.Chr '{';
    "}", Regex.Chr '}';
    "(", Regex.Chr '(';
    ")", Regex.Chr ')';
]

let lexerIgnores = ["whitespace"; "comment"]

let parserRules = Map.ofList [
    "int", [Parser.Tok "int"];
    "str", [Parser.Tok "str"];
    "ident", [Parser.Tok "ident"];
    "lazy", [Parser.Cat ([], [Parser.Tok "{"; Parser.Sym "chain"; Parser.Tok "}"])];
    "parens", [Parser.Cat ([], [Parser.Tok "("; Parser.Sym "chain"; Parser.Tok ")"])];
    "call", [Parser.Cat ([], [Parser.Sym "expr"; Parser.Sym "expri"])];
    "chain", [Parser.Cat ([], [
        Parser.Rep ([], (Parser.Cat ([], [Parser.Sym "expr"; Parser.Tok ";"])));
        Parser.Opt (Parser.Sym "expr")
    ])];
    "expri", [
        Parser.Sym "int";
        Parser.Sym "str";
        Parser.Sym "ident";
        Parser.Sym "lazy";
        Parser.Sym "parens";
    ];
    "expr", [
        Parser.Sym "expri";
        Parser.Sym "call";
    ]
]

let parserStart = "chain"

let defaultException = System.Exception ()

let rec transform cst =
    match cst with
    | Parser.Symbol ("int", x) -> transformInt x
    | Parser.Symbol ("str", x) -> transformStr x
    | Parser.Symbol ("ident", x) -> transformIdent x
    | Parser.Symbol ("lazy", x) -> transformLazy x
    | Parser.Symbol ("parens", x) -> transformParens x
    | Parser.Symbol ("call", x) -> transformCall x
    | Parser.Symbol ("chain", x) -> transformChain x
    | Parser.Symbol ("expri", x) -> transformExpri x
    | Parser.Symbol ("expr", x) -> transformExpr x
    | _ -> raise (defaultException)

and transformInt cst =
    match cst with
    | Parser.Token (_, str) -> Int (int64 str)
    | _ -> raise (defaultException)

and transformStr cst =
    match cst with
    | Parser.Token (_, str) -> Str str[1 .. String.length str - 2]
    | _ -> raise (defaultException)

and transformIdent cst =
    match cst with
    | Parser.Token (_, str) -> Ident str
    | _ -> raise (defaultException)

and transformLazy cst =
    match cst with
    | Parser.Group items ->
        match items with
        | [_; Parser.Symbol (_, chain); _] -> Lazy (transformChain chain)
        | _ -> raise (defaultException)
    | _ -> raise (defaultException)

and transformParens cst =
    match cst with
    | Parser.Group [_; Parser.Symbol (_, chain); _] -> transformChain chain
    | _ -> raise (defaultException)

and transformCall cst =
    match cst with
    | Parser.Group items ->
        match items with
        | [Parser.Symbol (_, expr); Parser.Symbol (_, expri)] -> Call (transformExpr expr, transformExpri expri)
        | _ -> raise (defaultException)
    | _ -> raise (defaultException)

and transformChain cst =
    match cst with
    | Parser.Group [Parser.Group items; item] ->
        Chain (
            items |> List.map (fun x ->
                match x with
                | Parser.Group [Parser.Symbol (_, expr); _] -> transformExpr expr
                | _ -> raise (defaultException)
            ),
            match item with
            | Parser.Symbol (_, expr) -> Some (transformExpr expr)
            | Parser.Epsilon -> None
            | _ -> raise (defaultException)
        )
    | _ -> raise (defaultException)

and transformExpri cst = transform cst 

and transformExpr cst = transform cst
