// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.
#load "Prelude.fs"
#load "Span.fs"
open Spanner

let s = [1 .. 100] |> Span.ofSeq
let q = [-10 .. 1000] |> Span.ofSeq

let (||>) (v: 't) (f: 't -> 'u) =
    printf "%A\n" v
    f v


[23.0f .. -0.4f .. -56.0f]
||> Span.ofSeq
||> Span.bufferLo 5.0f
||> Span.translate -50.f


