#I @"/Users/mtraylor/Projects/research/delos/packages"
#r @"/Users/mtraylor/Projects/research/delos/packages/FParsec-Big-Data-Edition/lib/net45/FParsecCS.dll"
#r @"/Users/mtraylor/Projects/research/delos/packages/FParsec-Big-Data-Edition/lib/net45/FParsec.dll"

type T =
    | Atom of string
    | List of T list

open FParsec

let test parser txt =
    match (run parser txt) with
    | Success (result,_,_) -> printfn "Success: %A" result
    | Failure (_,error,_) -> printfn "Failure: %A" error

let (expr: Parser<T,unit>), impl = createParserForwardedToRef ()

let start = skipString "("
let stop = skipString ")"

let func: Parser<string,unit> =
    let valid c = isLetter c || isDigit c || c = '-'
    many1Satisfy2L isLetter valid "function name"
let cstr =
    let norm = satisfy (fun c -> c <> '\\' && c <> '"')
    let unes =
        function
        | 'n' -> '\n'
        | 'r' -> '\r'
        | 't' -> '\t'
        | x   -> x
    let escp =
        pstring "\\" >>. (anyOf "\\nrt\"" |>> unes)
    between
        (skipString "\"")
        (skipString "\"")
        (manyChars (norm <|> escp))
let atom = func <|> cstr |>> Atom
let lexp =
    between start stop
            (sepBy1 expr (pstring " ") |>> List)

do impl := atom <|> lexp

let test2 = """(f "hello" (g (h "to" "the") "world"))"""
let tests =
    """(((in ("a" "b")) (dwelling (in ("SFR" "CONDO"))) (size ("5000" "1000"))) ((in ("3" "4" "5")) (dwelling (in ("MOBI" "PLEX"))) (size ("1" "5000")) (age ("2" "10"))))"""

type Value =
    | Int       of int
    | Bool      of bool
    | F         of (Expr list -> Expr)
    | Or
    | And
and Expr =
    | Const     of Value
    | Apply     of Expr * Expr list

let op =
    function
    | Or ->
        fun [Bool p; Bool q] -> (Bool (p || q))
    | And ->
        fun [Bool p; Bool q] -> (Bool (p && q))

let (|Binary|_|) =
    function
    | Or -> Some Or
    | And -> Some And
    | _ -> None

let rec eval =
    function
    | Const x -> x
    | Apply (f,x) ->
        match eval f with
        | F f -> eval (f x)
        | Binary f -> op f (List.map eval x)

let t = Apply (Const Or, [ Const (Bool false); Apply (Const (F (List.head)), [Const (Bool false)]) ])

open System

// let read () = Console.In.ReadLine ()
// let node = function (x,'.') -> Some x | _ -> None
// let crds y = Option.map (fun x -> (x,y))
// let read' w h =
//     Seq.init (h - 1)
//         (fun y ->
//             read () |> Seq.indexed |> Seq.choose (node >> crds y))
//     |> Seq.concat

let node x = function (y,'.') -> Some (x,y) | _ -> None
let data w h =
    Seq.init h
        (fun y -> Console.ReadLine () |> Seq.indexed |> Seq.choose (node y))
    |> Seq.toList |> printfn "%A"

let data' h =
    let a = Array.init h
                (fun y -> Console.ReadLine () |> Array.ofSeq)

    [| for y in 0 .. h - 1 do
        let w = a.[y].Length
        for x in 0 .. w - 1 do
             let n = a.[y][x]
             if n = '.' then
                let r = a.[y] |> Array.tryFindIndex ((=) '.')
                let d = a.[y..(h-1)][w] |> Array.tryFindIndex ((=) '.')
                if r.IsSome the
                ((x,y), ) |]
    // [ for i in 0 .. (w * h) - 1 do
    //     let x,y = i / w, i % w

type B = L | N of (int * int) * B * B

let rec insert a =
    function
    | N (b, l, r) ->
        if (a < b) then
            N (b, l, insert a r) else
            N (b, insert a l, r)
    | _ ->
        N (a, L, L)
let tree (x,y) b n = if b = 1 then insert (x,y) n else n
let decode w bit =
    List.scan
        (fun (i,n) t ->
            (i + 1, tree (i / w, i % w) t n)) (0,L) bit
let bits = [ 1; 1
           ; 0; 1
           ; 1; 0 ]
decode 2 bits