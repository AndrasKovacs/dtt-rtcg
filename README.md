# dtt-rtcg

Runtime code generation with dependent types. This is a small demo which is only
mildly practical. In principle, we can extend any practical dependently typed
language (like Idris, Lean) with the features here.

Basic features:

- Dependent functions, records, type-in-type, builtin natural numbers.
- Convenience: Agda-style implicit arguments, overloaded record fields,
  opening records to bring their fields into scope.

Code generation features:

- Full type safety: generated code is always well-typed.
- Arbitrary number of stages. Generated code can also generate code.
- Generated code can reference any previous definition and any runtime value
  ("cross-stage persistence").
- Supports running open code (containing free variables).
- Two ways to run programs:
  1. Reference interpreter written in Haskell.
  2. Javascript backend that implements code generation with `eval`.

## Installation

1. Install the [Haskell Stack](https://docs.haskellstack.org/en/stable/).
2. Download or clone this repository.
2. Hit `stack install` in the project folder.
3. Make sure that you have the `stack` install directory on your PATH; if you
   don't, you will be reminded when `stack install` finishes.
5. Run `dtt-rtcg` to get an overview of command line usage.

## Overview

Runtime code generation is technically possible in many programming languages:
just build a string, feed it to an interpreter or compiler, and somehow link it
back to the program. However, the ergonomics of this is usually awful, in the
absence of native support by the language and the toolchains.

In traditionally JIT-ed languages such as Java, Javascript or C#, there is some
native support for code generation, but API-s are either highly verbose and
somewhat type safe, or just consist of evaluating strings.

- [BER MetaOcaml](https://okmij.org/ftp/ML/MetaOCaml.html) has probably the
  most advanced research and library corpus in relation to strongly typed
  staged programming.
- I haven't used Scala 3 [staging
  features](https://docs.scala-lang.org/scala3/reference/metaprogramming/index.html),
  but it looks comparable to MetaOCaml.

Limitations in MetaOcaml:

1. We can generate programs at given types, but can't generate types.
2. We can run generated code only if it's closed, i.e. contains no free
   variables. If we attempt to run open code, we get a code-generation time
   error but no type error.
3. We have structural restrictions on which variables and operations can be
   used in certain positions. Splices must appear in the lexical interior of a
   quote. Variables that are bound under N quotations can be only used under N
   or more quotations. These restrictions are also used to rule out open evaluation
   at code generation time. We can only pass a free variable as a piece of code
   to existing functions, we cannot pass it as an actual free variable that
   can block computations in the function body.

If we want to add runtime code generation to a dependent language like Idris or
Lean, and want to ensure full or almost full type safety, we need to lift all of
these restrictions.

For (1), since terms and types are both first-class and freely mixed, it makes
no sense to only support term generation.

For (2), tracking closedness is really only possible with a
[modality](https://dl.acm.org/doi/10.1145/3341711), but that's a rather heavy
feature and we'd prefer to not expose programmers to it. Alternatively, we could
just throw runtime errors on running open code, but I don't like that either.

For (3), dependent type theories need a well-behaved notion of typing and
definitional equality. Restricting variable usage is possible, but again, only
by using modalities. The smoothest user experience is obtained when our type
theory is purely structural, without modalities, and everything is enforced
purely by typing and definitional equality.

Fortunately, (1)-(3) can be all resolved with relative ease.

The thing about dependently typed languages is:

> They must all support open evaluation out of the box.

That's because type checking requires conversion checking, and conversion
checking requires evaluating terms containing free variables.

So why not just reuse open evaluation in code generation? The idea is not
new. Open evaluation is just a tiny step away from *normalization*, so usually
we get normalization as part of the package.

But we can't just normalize code and expect it to be faster. Normal forms tends
to blow up in size and contain massive duplication of computations. We need a
lot of fine control over what gets computed at code generation time.

Idris 1 [reused open evaluation for code
generation](https://dl.acm.org/doi/10.1145/1863543.1863587), providing extra
control in the form of [static
arguments](https://docs.idris-lang.org/en/latest/reference/partial-evaluation.html).
This was better than naive normalization but still kinda crude and ad-hoc. I do
not know of more attempts since then. The idea is important though, and we shall use
it here.

Closed evaluation is the standard operational model for pretty much everything
that's not dependently typed. For dependently typed languages, closed evaluation
is also the standard when we actually want to run programs instead of type
checking them. But the necessity of open evaluation is a *big* influence on the
language design; it's a massive effort to add open evaluation to a language
which wasn't designed to support it. Imagine coming up with a specification for
evaluating OCaml or Haskell programs that contain free variables!

Getting to the **basic setup** of `dtt-rtcg`:

1. Code can be run both in **open** and **closed** evaluation mode. This can be
   implemented by compiling programs *twice* for the two modes (as I do here in
   the JS backend), but we can choose between interpretation and compilation for
   either mode.
2. "Normal" runtime execution is **closed** and possibly **effectful**. There's
   a built-in monad for side effects.
3. Closed execution can trigger code generation. Code generation runs in
   **open** mode with **no side effects**.
4. Closed runtime values can be directly embedded into generated code. For
   example, if I have a closed runtime list, represented as a heap pointer, I
   can insert that pointer into generated machine code. I don't have to invent a
   syntactic name for the reference, nor do I have to "inline" the contents of
   the list into the code.

The API to code generation is the following:

- We have `U : U` as the universe of types.
- We have `□ : U → U`, where `□ A` means "code of A".
- We have `_~ : {A : U} → □ A → A` for "splicing".
- We have `<_> : {A : U} → A → □ A` for "quotation".
- We have `<~t> ≡ t` and `~<t>` as definitional equalities.

That's it. This means that the code generation API is, type-theoretically
speaking, **trivial**! As far as type checking is concerned, it's exactly the
same as having `□ A` as a record type with a single field, with both beta
and eta rules.

This should not be too surprising. If we reason up to definitional equality,
staging does nothing. This is also the case in [two-level type
theory](https://andraskovacs.github.io/pdfs/2ltt.pdf). The point of staging is
performance, and performance is *not invariant* under definitional equality.  We
use staging to exert control which is more fine-grained than definitional
equality. In other words, staging features are *transparent* to type checking
and conversion checking, and they start making a real difference when we run or
compile programs.

### Code examples

#### Non-staging features

A program is a single expression with type `Eff A`, where `Eff : U →
U` is the monad for side effects.
```
id {A : U}(x : A) = x;
do n ← readℕ;
do printℕ (id n);
return ()
```

- On the top-level, we can interleave pure definitions like `id` and effectful computations.
- The `do` is not the same as in Haskell; it parses as monadic binding, so we have to repeatedly
  write `do` for each action that we want to perform.
- `readℕ : Eff ℕ` reads a single natural number from stdin, crashing on ill-formed input.
- `printℕ` prints a natural number to stdout.
- `ℕ : U` is the type of natural numbers, supporting decimal literals, `suc : ℕ → ℕ` and
  the induction principle `ℕElim : {P : ℕ → U} → ({n} → P n → P (suc n)) → P 0 → (n : ℕ) → P n`.

If we hit `dtt-rtcg FILE interp`, we run `FILE` in the Haskell
interpreter. `dtt-rtcg FILE run` runs it in NodeJS instead.

We can use Agda-style implicit arguments. Function arguments can be written in Coq-style and
also in Agda-like style:
```
id : {A : U} → A → A =
  λ x. x;

id' {A : U}(x : A) : A = x;
```
Types of binders and return types can be often inferred:
```
id : {A} → A → A = λ x. x;
id' {A} (x : A) = x;
```
Holes and implicit applications follow Agda:
```
id   {A}(x : A) = x;
id'  {A}(x : A) = id {A} x;
id'' {A}(x : A) = id {_} x;
```
We have anonymous dependent records:
```
myPair : Σ(fst : ℕ, snd : ℕ) = (fst = 100, snd = 200);
foo : Σ(fst : ℕ, snd ℕ) → ℕ = λ x. x.fst;
```
Names of fields can be omitted in record construction when they're clear from the type:
```
myPair : Σ(fst : ℕ, snd : ℕ) = (100, 200)
foo : Σ(fst : ℕ, snd ℕ) → ℕ = λ x. x.fst;
```
Records are dependently typed:
```
myPair : Σ(A : U, a : A) = (ℕ → ℕ, λ x. suc (suc x));
```
The empty record serves as the unit type:
```
myAction : Eff Σ() =
  do printℕ 100;
  do printℕ 200;
  return ();
```
Records can be "opened", bringing their fields into scope:
```
Monoid (M : U) = Σ(
    empty : M
  , plus  : M → M → M
);

addition : Monoid ℕ = (
  empty = 0
, plus  = λ n m. ℕElim {λ _. ℕ} suc n m
);

open addition;
foo = plus 100 (plus 200 empty);
```
There is a builtin `$` operator for weakly binding applications, just for convenience.
```
foo = plus 100 $ plus 200 $ plus 300 400;
```
If we're not at the topmost level, we have to write `let` for local definitions.
We can also `open` locally.
```
let foo : ℕ =
   open addition;
   let x = plus 100 200;
   let y = plus x empty;
   plus x y;
```
Mutable references have the following API:

- `Ref   : U → U`
- `new   : {A} → A → Eff (Ref A)`
- `read  : {A} → Ref A → Eff A`
- `write : {A} → Ref A → A → Eff Σ()`

#### Staging features

A quotation creates an expression at runtime. Then, if we splice an expression
outside of any quotation, that causes code to be generated from the expression
and then immediately evaluated.
```
let one : □ ℕ = <1>;
let one' : ℕ = ~one;
return ()
```
If we run this with `dtt-rtcg FILE interp`, we get the following printed output:
```
CODE GENERATED AT:
(stdin):3:16:
  |
  | let one : □ ℕ = <1>;
3 | let one' : ℕ = ~one;
  |                ^

CODE:

1

RESULT:
()
```
The interpreter `interp` makes some effort at pretty printing
generated code, at also points you to where code generation was triggered.
In this case, the code is just the literal `1`. The `RESULT` is the value
returned by the whole program.

    Another example.
```
id : {A} → A → A = ~<λ x. x>;
return ()
```
Printed output:
```
CODE GENERATED AT:
(stdin):6:20:
  |
  |
6 | id : {A} → A → A = ~<λ x. x>;
  |                    ^

CODE:

λ A x. x

RESULT:
()
```
We quote the expression and immediately evaluate it with the splice.
Note that the implicit argument `A` becomes explicit in the printed version.
That's because the source program gets elaborated into code which doesn't have
implicitness anymore, and also replaces every type with an "erased" dummy value.

The `dtt-rtcg FILE zonk` command prints the erased version of the source
program.  Erased things are printed as `⊘`.
```
id : ⊘ =
  ~<λ A x. x>;

return {⊘} ()
```
The first argument of `return` still gets printed as implicit for no other reason than
me being lazy and not adjusting the printing of builtins.

We can combine expressions by splicing inside quotations:

```
compose {A B C}(f : B → C)(g : A → B) (a : A) : C =
  f (g a);

plus2code : □ ℕ → □ ℕ = λ x. <suc (suc ~x)>;
plus4code : □ ℕ → □ ℕ = compose plus2code plus2code;
plus4 : ℕ → ℕ = ~<λ n. ~(plus4code <n>)>;

return ()
```
This generates `λ n. suc (suc (suc (suc n)))` for `plus4`.

Note that code generation for splices **never happens at compile time**. In many cases,
it would be clearly better to splice code at compile time, but `dtt-rtcg` is not intended
to demonstrate anything like that.

This means that we need to be careful about splice placement. Let's assume that
we already have a function which generates code for list mapping.
```
map {A B}(f : □ A → □ B)(as : □ (List A)) : □ (List B)
```
If we want to generate code for incrementing numbers in a list, we do it like this:
```
mapSuc : List ℕ → List ℕ = ~<λ ns. ~(map (λ x. <suc x>) <ns>)>;
```
When evaluation gets to this point, we immediately generate the desired function.

On the other hand, if we have this
```
mapSuc : List ℕ → List ℕ = λ ns. ~(map (λ x. <suc x>) <ns>);
```
we generate code each time when `mapSuc` is applied, but not when it is defined!
That defeats the purpose of code specialization, since generating code can be fairly
expensive and can outweigh the performance gains on the function inlining.

TODO


<!-- -------------------------------------------------------------------------------- -->
