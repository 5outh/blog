---
title: Compressed Reading: Applicative Programming With Effects
author: Benjamin Kovach
tags: papers, applicative, functors
---

I have seen the paper [Applicatve Programming With Effects](http://www.soi.city.ac.uk/~ross/papers/Applicative.pdf)
by Conor McBride and Ross Patterson floating around as recommended reading for a while, but always thought it would come
off as redundant, since I already know about what Applicative Functors are. I finally decided that I was being silly
and gave it a read -- there's some good stuff here, even if you know what Applicative Functors are at a basic level.

The goal of this post is to provide a small outline of the paper, and detail some things I found particularly
interesting while reading it. I hope to accomplish two things in this post and subsequent posts summarizing other papers
I might come across:

1. Solidify my understanding of the material presented, and
2. Provide a second viewpoint of the material though my eyes.

It can be argued that the paper in question is pretty accessible as is; it isn't particularly dense and most of it
is focused directly on Haskell programming. However, there may be some things that I present slightly differently that
solidify knowledge for some readers. It doesn't happen often that I understand papers 100% after the first reading, so I suspect that many people have read the paper and missed a detail or two (I'm sure I did, as well). I may also cover denser papers in the future and hope to follow the same pattern.

Without further ado, let's dig into what this paper was about.

### Introductory examples

The paper starts by providing three examples of very similar looking computations. First, `sequence` is defined
as an operation to sequence monadic actions and their effects. Second, `transpose` is defined as an operation on lists of lists, which swaps the columns and rows. Finally, a simple expression evaluator is (partially) defined which makes use of the `S` and `K` combinators from lambda calculus. All three of these examples follow a similar pattern: They use some application function to glue structures together. It's pretty clear from these examples that we have some function that acts as an identity, and some composition function that glues structures together, and that this pattern is very general.

I found the transposition example particularly cool, since `repeat` is the identity operation for lists in that context.

### The Applicative Type Class

Here, `Applicative` is shown in its full glory, as we know it today, with `pure` and `<*>` (labeled as a circled star in the paper). All three of the structures in the examples above are given `Applicative` instances. New definitions for `sequence` and `transpose` look identical, disregarding the less general type signature of `sequence`.

Of particular interest (again), is how the `Applicative` instance for `[]` is defined:

```haskell
instance Applicative [] where
  pure = repeat
  (f:fs) <*> (x:xs) = f x : fs <*> xs
  _      <*> _      = []
```
  
I also think it's worth restating the intuition behind how `Applicative` works. With `Applicative` computations in the following form:

```haskell
f <*> u1 <*> ... <*> un
```

The computation has a fixed structure, given by the *pure* function `f`, which is then applied to effectful subcomputations in sequence, given by $u_k$.

### Traversals

Here, the notion of an *Applicative distributor* (for lists) is outlined. It looks like this:

```haskell
dist :: Applicative f => [f a] -> f [a]
dist [] = pure []
dist (x:xs) = (:) <$> x <*> dist xs
```
*Note: This is actually a specilized case of `sequenceA` for lists from `Data.Traversable`*

The definitions of `sequence` and `transpose` are actually exactly the same now, with `sequence = transpose = dist`. It is then shown that `dist` can be used along with `map` in order to map a function over elements while distributing and collect effects. The example given is a function, `flakyMap`, on the `Maybe` Applicative that fails entirely if any subcomputation fails. I'll add two more examples. In the list `Applicative` given in the paper, using the same definition of `dist`, we can do this:

```haskell
> let f x = [x, x^2, x^3]
> dist (fmap f [2, 3])
[[2,3],[4,9],[8,27]]
```

It's harder to explain in general what this does, but the above example should be clear enough. Using the typical definition of the list Applicative, we can do something similar:

```haskell
> let g x = [x, x^2, x^3]
> dist (fmap g [2, 3])
[[2,3],[2,9],[2,27],[4,3],[4,9],[4,27],[8,3],[8,9],[8,27]]
```

This gives all permutations of the input numbers, their squares, and their cubes. The "effect" in play here is clearly non-determinism.

In any case, the paper goes on to mention that, doing things in this way (with `map`), we traverse lists twice. It then defines the `Traversable` typeclass to allow mapping during traversal for more performance.

```haskell
class Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  dist :: Applicative f => t (f a) -> f (t a)
  dist = traverse id
```

A `Traversable` instance indicates that the data structure in question can have an `Applicative` computation "threaded through it" while it is being traversed. We could redefine `flakyMap = traverse` (and also the list functions I've defined above) and get better performance and more generality from them.

The paper goes on to show that the `Identity` Applicative (/Functor/Monad/Comonad/Whatever) can be used in order to recover `fmap` from `traverse`, showing that `Traversable` is strictly stronger than `Functor`. However, it is noted that not *all* Applicatives are Traversable (one example being `((->) env)`).

