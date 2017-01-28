namespace Spanner


type ComparisonResult = LessThan | Equal | GreaterThan

[<AutoOpen>]
module Prelude =

  let (|LessThan|Equal|GreaterThan|) = function
    | 1 -> GreaterThan
    | 0 -> Equal
    | -1 -> LessThan
    | v -> failwith <| (sprintf "Invalid comparison result %d" v)


module Option = 

    /// Returns the value if preset, otherwise the default
    let defaultTo value = function
    | Some x -> x
    | None -> value

    /// Returns the value if present, otherwise evaluates the default function
    let defaultWith f = function
    | Some x -> x
    | None -> f()

    // Combine two options 
    let zip optA optB =
        match optA, optB with
        | Some a, Some b ->
            Some (a, b)
        | _ ->
            None


    /// Compute the intersection of two options
    let intersect optA optB =
        zip optA optB |> Option.filter (fun (a,b) -> a = b)
    

    /// Apply an option of f to an option
    let apply fOpt opt = 
        zip fOpt opt
        |> Option.map (fun (f,x) -> f x)

    /// The minimum of two options
    let min optA optB = 
        match (optA, optB) with
        | a, None -> a
        | None, b -> b
        | Some a, Some b ->
            Some (if a < b then a else b)


    /// The Minimum of two options
    let max optA optB = 
        match (optA, optB) with
        | a, None -> a
        | None, b -> b
        | Some a, Some b ->
            Some (if a > b then a else b)
