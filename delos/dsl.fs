module Delos.DSL

open System

type Expr =
    | And       of Expr * Expr
    | Or        of Expr * Expr
    | Not       of Expr
    | Less      of Expr * Expr
    | More      of Expr * Expr
    | Eq        of Expr * Expr
    | Constant  of Value
and Value =
    | Int       of int
    | Bool      of bool

let rec eval =
    function
    | And (p,q)         -> Bool (parseBool (eval p) && parseBool (eval q))
    | Or (p,q)          -> Bool (parseBool (eval p) || parseBool (eval q))
    | Not (p)           -> Bool (not ( parseBool (eval p)))
    | Less (p,q)        -> Bool (parseInt (eval p) < parseInt (eval q))
    | More (p,q)        -> Bool (parseInt (eval p) > parseInt (eval q))
    | Eq (p,q)          -> Bool (parseBool (eval p) = parseBool (eval q))
    | Constant (v)      -> v
and parseBool =
    function
    | Bool b -> b
    | _ -> failwith "Invalid expression"
and parseInt =
    function
    | Int i -> i
    | _ -> failwith "Invalide expression"


let test = Less (Constant (Int 0), Constant (Int 1))