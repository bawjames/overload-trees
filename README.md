# Overload Trees

## What are Overload Trees?

The fact is, I don't really know what they are either, so this is just a place to collect my thoughts and maybe even communicate the concept to others in some coherent manner.
We will attempt to form an extension to the Hindley-Milner type system to allow full type inference with overloaded functions (true ad-hoc polymorphism).

I'm working on an implementation in Haskell [here](https://github.com/bawjames/type-inference).

<details>
<summary>Notes for converting to LaTeX later</summary>

 - Replace `env` with `Γ` (uppercase gamma, `\Gamma`).
 - Replace fresh type variables (those prefixed with a tick) with lowercase greek letters.

</details>

## A brief introduction to type inference

To understand the rest of this document, it is important to first understand type inference.
If you have no prior understanding of type inference, read this section.
If you have prior understanding of type inference, please read this section and tell me how I can improve it.

### Notation

`env |- value : t -| C`

`env` is the environment, which is a set of mappings from variable names to their type schemes.
We will introduce type schemes later; until then, we will think of type schemes as identical to regular types.

`|-` and `-|` are separators, sometimes called turnstyles.

`C` is a set of equations, each in the form `a = b`, where `a` and `b` are types.
This grows as inference proceeds and is solved via unification, and is solved via unification at the end.

We use `<>` for set union, which may be used to combine constraint sets or environments.

Types begin with an uppercase letter if they are concrete (e.g., `String`, `Int`).
Type variables begin with a lowercase letter (e.g., `a`, `b`).
Fresh type variables (those introduced during inference) are written with a tick (e.g. `'t`, `'u`).

### Inferring constants

Constants have predefined types:
 - `"Hello world" : String`
 - `-1 : Integer`, `2 : Integer`, `1_000_000 : Integer`
 - `-1.0 : Real`, `2.71828 : Real`
 - `True : Bool`, `False : Bool`

For simplicity, we treat numeric literals as having monomorphic types here.
In Haskell, they are polymorphic with `Num` constraints.

### Inferring variables

To infer the type of a variable, we can simply look it up in the environment.

The notation for this is `x : a if x : a in env`.

Note that this is technically incorrect with type schemes, but until we learn to infer the types of let bindings, we will ignore this.

### Inferring lambdas

We write a lambda as `x. e`, equivelent to Haskell's `\x -> e`.
Only unary functions are allowed; polyadic functions are expressed via nesting `x. y. x + y`.

For the sake of simplicity, lambda calculus' `λx. e` becomes `x. e`, as seen above.

To infer the type of `x. e`:
 + Introduce a fresh type variable `'t` for the bound variable `x`.
 + Extend the environment with `x : 't`.
 + Infer the type of e in this extended environment, yielding `e : a -| C`
 + Finally, `env |- x. e : 't -> a -| C`

### Inferring application

To infer the type of `f e`:
 + Infer `f : a -| C_1`
 + Infer `e : b -| C_2`
 + Introduce a fresh type variable `'t` for the result.
 + Append the constraint `a = b -> 't`.
 + Finally, `env |- f e : 't -| C_1 <> C_2 <> { a = b -> 't }`

### Unification

To unify, apply any of the following reductions iteratively until no rules apply or until failure:
 + `t = t` => Remove the constraint.
 + `'t = a` where `'t` does not occur in `a` => Substitute `a` for `'t` everywhere and remove the constraint.
 + `a -> b = c -> d` => Replace with two new constraints `a = c` and `b = d`.
 + If `'t` occurs in `a` and `'t /= a`, unification fails.

## Back to overload trees: Notation/representation

Individual function types will be represented as `a -> ... -> z`, e.g. `id : a -> a` or `add : Int -> Int -> Int`.
These can be thought of as singly linked lists, where `->` delimits elements of the list.
More precisely, they are degenerate trees, meaning each parent node has only one child node.
Degenerate trees can be trivially concatenated.

<details>
<summary>Note</summary>

The trees here are not monoids because there is no identity element. The usual identity element for a tree is the empty tree, but it does not make any sense as a type, and is hence prohibited. They are instead (abelian) semigroups under the concatenation operation.

</details>

So, lets take two definitions for the `add` function:
  - `map : Int -> Int -> Int`
  - `map : Double -> Double -> Double`

It is possible to merge them into the following tree:
```
*┬Int─Int─Int
 ╰Double─Double─Double
```

<details>
<summary>Haskell</summary>

The following is the type of the tree in the Haskell implementation.
```haskell
data OverloadTree
  = Function (Map OverloadTree OverloadTree)
  | Concrete Primitive
  | Var Int

data Primitive
  = TInt Integer
  | TStr String
  {- ... -}
```

</details>

When a value is applied to the `add` function, we gain some information about which function to use.

```
add┬Int─Int─Int
   ╰Double─Double─Double

(add 500_000)─Int─Int
(add 3.14159)─Double─Double
```

```
map─(a -> b)┬(List a)─(List b)
            ╰(Array a)─(Array b)
```

Specifically, we take the children of the applied type as the type of the value of the application.

# Everything below here is a messy amalgamation of notes

## Modifications to type inference for overload trees

```
x y : a -> b

x :
  ├Bool─String
  ╰Int─String

y : Int

=> x y : String
```

```
-- POLYVARIADIC FUNCTIONS

take the following example
```

```haskell
f :: [a] -> a -> [a]
f xs x = g $ x : xs
  where g = id
```

and simply swapping id for head

```haskell
f :: [a] -> a -> a
f xs x = g $ x : xs
  where g = head
```

now the whole type signature is different.
the following code allows any signature of (g $ x : xs), but then g has to be an argument of f.


```haskell
f :: ([a] -> r) -> [a] -> a -> r
f g xs x = g $ x : xs
```

to solve this, we can use existentially qualified types.
therefore, type variables (lowercase letters `a` through `z`) are foralls,
ticked type variables (`'a` through `'z`) are exists.

so, `g : a -> 'b = g : forall a. exists b. a -> b`

now,

```
f : [a] -> a -> _
  = xs. x. g $ x :: xs
```

which will now infer based on the type signature of g. g could even have an arity of more than 1! its equivalent to having no type signature, and allowing the compiler to infer. i think this will be a necessary feature for a functional programming language with true ad-hoc polymorphism (like compile-time multiple dispatch), especially for that language to implement variadic functions.

Haskell has -XPartialTypeSignatures, which is EXACTLY what I need! except the following is still not implementable in haskell because it does not have true ad-hoc polymorphism

something like the following
```
process : Int -> _
  = x. if (some condition)
    then "string"
    else [x, x+1]
```
is disallowed in haskell and should be disallowed in our system.

we can then define

```
f : [a] -> [a]
  = id

f : [a] -> a -> _
  = xs. x. f $ x : xs
```

where the compiler will pick the function based on the second argument.

This allows polyvariadic functions

```
*─[a]┬[a]
     ╰a─_
```

can expand to

```
*─[a]┬[a] (Not selected)
     ╰a─┬[a]
        ╰a─_
```

which can expand to
```
*─[a]┬[a] (Not selected)
     ╰a─┬[a] (Not selected)
        ╰a─┬[a]
           ╰a─_
```
 which could happen boundlessly.
this stops happening when we run out of arguments, in this case there were 3 arguments including the initial list.

then we get
```
*─[a]┬[a] (Not selected)
     ╰a─┬[a] (Not selected)
        ╰a─┬[a]
           ╰a─_ (Not selected)
```

-- EXAMPLE

```
let f : [a] -> [a]
  = id

let f : [a] -> a -> _
  = xs. x. f $ x :: xs

let f' = f []
--  f' has the following overload tree
--  *┬a─_
--   ╰[a]

let x = f' 2 3
```
  
```
f [] 2 3
f : [a] -> 't
(f []) : 't
't = [a] | a -> 'r
(f [] 2) : 'r1        => (via application)  't = a -> 'r1
(f [] 2 3) : 'r2      => (via application) 'r1 = a -> 'r2
'r2 /= a -> 'r        => (via deduction)   'r2 = [a]
```
