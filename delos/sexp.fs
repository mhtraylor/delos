module Delos.Sexp

open System

type T =
    | Atom of string
    | List of T list

module Parser =

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

    let parse txt =
        run expr txt
  
   
