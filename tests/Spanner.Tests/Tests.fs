module Spanner.Tests

open Span
open NUnit.Framework


[<Test>]
let ``empty unions are identity`` () =

  let a = Span.ofPair false (1, 100)
  let b = Span.Empty
  let c = Span.union a b
    
  Assert.AreEqual(a, c)


[<Test>]
let ``validating unordered pairs is empty`` () =

    let s = Span.ofPair true (10, -10)

    Assert.IsTrue( s |> Span.isEmpty )


[<TestCase(-100, 20)>]
[<TestCase(-0.1f, 2.0f)>]
[<TestCase("a", "f")>]
[<TestCase(-1023L, 20000L)>]
let ``ignoring validation is symetrical`` (a: 'T, b: 'T) =

    let createNoValidate = Span.ofPair false

    let forward = createNoValidate (a, b)
    let backward = createNoValidate (b, a)

    Assert.AreEqual(forward, backward)

