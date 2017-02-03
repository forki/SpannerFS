namespace Spanner


type Span<'a> =
    | Empty
    | LowerBound of 'a
    | UpperBound of 'a
    | Point of 'a
    | Bounds of 'a * 'a
    | Unbounded


module Span =

    /// The empty span
    let empty = Empty

    /// Span of a single value
    let ofValue x = Point x

    /// <summary>
    /// Create a bounded span from
    /// two comparable points.  
    /// </summary>
    /// <param name="validate">causes this function to return the Empty span if an unordered tuple is provided</param>
    /// <returns>A new span</returns>

    let ofBounds validate (lo, hi) =

        match (compare lo hi) with

        | LessThan -> Bounds (lo, hi)

        | Equal -> Point lo    

        | GreaterThan when not validate -> Bounds (hi, lo)

        | _ -> Empty
        

    /// Return a span with the given lower bound
    /// and no upper bound
    let ofLowerBound x = LowerBound x

    /// Return a span with the given upper bound
    /// and no lower bound
    let ofUpperBound x = UpperBound x

    /// Returns an unbounded Span
    let ofUnbounded = Unbounded

    /// Returns a Point if the value is present otherwise Empty span
    let ofOption x = (Option.map ofValue) >> (Option.defaultTo empty) <| x
    
    /// Create from two optionals
    /// TODO: consider better name
    let ofOptions options =
        match options with
        | Some lo, Some hi ->
            ofBounds false (lo, hi)

        | Some lo, None ->
            ofLowerBound lo

        | None, Some hi ->
            ofUpperBound hi

        | _ ->
            ofUnbounded


    /// Returns the lower bound of the Span
    let lower span =
        match span with 
        | Point lo | LowerBound lo | Bounds (lo, _) ->
            Some lo
        | _ -> 
            None

    /// Returns the upper bound of the Span
    let upper span =
        match span with
        | Point hi | UpperBound hi | Bounds (_, hi) ->
            Some hi
        | _ -> 
            None

    // Returns the lower bound of the Span or the given default
    let lowerOr value span = (lower span) |> Option.defaultTo value

    // Returns the upper bound of the Span or the given default
    let upperOr value span = (upper span) |> Option.defaultTo value

    /// Returns true if the Span empty
    let isEmpty span =
        match span with
        | Empty -> true
        | _ -> false

    /// Returns true if the Span non empty
    let nonEmpty span = not (isEmpty span)

    /// Returns true if the Span has a lower bound
    let hasLowerBound span = (lower span) |> Option.isSome

    /// Returns true if the Span has an upper bound
    let hasUpperBound span = (upper span) |> Option.isSome

    /// Returns the bounds of this Span if it is bounded on both ends
    let bounds span = Option.zip (lower span) (upper span)
    
    /// Apply one span to another
    let apply (fSpan: Span<'T -> 'U>) (span: Span<'T>) : Span<'U> =

        if (isEmpty span) then empty else
        
        // Apply to lower bound
        let lo = Option.apply (lower fSpan) (lower span)

        // Apply to upper bound
        let hi = Option.apply (upper fSpan) (upper span)

        ofOptions (lo, hi)
  

    /// Map the provided function to both bounds of the Span
    let map (f: 'T -> 'U) (span: Span<'T>) = span |> apply (Point f) 

    /// Map the provided function to the lower bound of the Span
    let mapLo (f: 'T -> 'U) (span: Span<'T>) = span |> apply (LowerBound f)

    /// Map the function to the upper bound of the Span
    let mapHi (f: 'T -> 'U) (span: Span<'T>) = span |> apply (UpperBound f)
    
    /// Computes the union of two spans
    let union (a: Span<'T>) (b: Span<'T>) : Span<'T> = 

        if (isEmpty a) then b
        else if (isEmpty b) then a
        else
        
        // Find the new upper and lower bounds
        let lo = Option.min (lower a) (lower b)
        let hi = Option.max (upper a) (upper b)
        
        ofOptions (lo, hi)

    
    let bind (f: 'T -> Span<'U>) span =
        match span with 
        | Empty -> Empty             
        | Point p -> f p
        | LowerBound lo -> f lo
        | UpperBound hi -> f hi
        | Bounds (lo, hi) -> union (f lo) (f hi)
        | Unbounded -> Unbounded


    /// Create a Span from the given sequence
    let ofSeq (x: seq<'T>) = (Seq.map ofValue) >> (Seq.fold union empty) <| x

    /// Create a Span from the given list
    let ofList (x: List<'T>) = (List.map ofValue) >> (List.fold union empty) <| x

    
    let inline bufferLo x = (+) x |> mapLo
    

    let inline bufferHi x = (+) x |> mapHi

    /// Translate the span by the given amount
    /// eg translate 10 Span(3,4) = Span(13,14)
    let inline translate x = (+) x |> map
    
    // Return the size of a span    
    let inline size span = 
        match span with
        | Bounds (lo, hi) -> hi - lo
        | _ -> LanguagePrimitives.GenericZero

    
        
   