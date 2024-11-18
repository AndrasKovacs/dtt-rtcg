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
- Supports running open code (code containing free variables).
- Two ways to run programs:
  1. Reference interpreter written in Haskell.
  2. Javascript backend that implements code generation with `eval`.

## 1. Installation

1. Install the [Haskell Stack](https://docs.haskellstack.org/en/stable/).
2. Download or clone this repository.
2. Hit `stack install` in the project folder.
3. Make sure that you have the `stack` install directory on your PATH; if you
   don't, you will be reminded when `stack install` finishes.
5. Run `dtt-rtcg` to get an overview of command line usage.

## 2. Overview

Runtime code generation is technically possible in many programming languages:
just build a string, feed it to an interpreter or compiler, and somehow link it
back to the program. However, the ergonomics of this is usually awful, in the
absence of native support by the language.

In traditionally JIT-ed languages such as Java, Javascript or C#, there is some
native support for code generation, but API-s are either highly verbose and
somewhat type safe, or just consist of evaluating strings. Some of the nicer
API-s:

- [BER MetaOcaml](https://okmij.org/ftp/ML/MetaOCaml.html) has accummulated a
  substantial amount of research materials and library code, in a strongly-typed
  flavor that's similar what `dtt-rtcg` offers.
- I haven't used Scala 3 [staging
  features](https://docs.scala-lang.org/scala3/reference/metaprogramming/index.html),
  but it looks comparable to MetaOCaml.

Limitations in MetaOcaml:

1. We can't generate types. We can only generate programs at given types.
2. We can run generated code only if it's closed, i.e. contains no free
   variables. If we attempt to run open code, we get an error at code generation
   time, but no type error.
3. We have structural restrictions on which variables and operations can be used
   in certain positions. Splices must appear in the lexical interior of a
   quote. Variables that are bound under N quotations can be only used under N
   or more quotations. The goal of these restrictions is again to rule out open
   evaluation at code generation time. We can only pass a free variable as a
   piece of code to existing functions, we cannot pass it as an actual free
   variable that can block computations in the function body.

If we want to add runtime code generation to a dependent language like Idris or
Lean, and want to ensure full or almost full type safety, it looks like a good
idea to remove these restrictions.

For (1), since terms and types are both first-class and freely mixed, it makes
no sense to only support term generation.

For (2), tracking closedness is really only possible with a
[modality](https://dl.acm.org/doi/10.1145/3341711), but that's a rather heavy
feature and we'd prefer to not expose programmers to it. Alternatively, we could
just throw runtime errors on running open code, but I don't like that either.

For (3), dependent type theories need a well-behaved notion of typing and
definitional equality. Restricting variable usage is possible, but again, only
by using modalities. The smoothest user experience is obtained when our type
theory is purely structural, without modalities, and everything is enforced by
purely by typing.

Fortunately, all of (1)-(3) can be handled relatively easily.

The thing about dependently typed languages is:

> They must all support open evaluation out of the box.

That's because type checking requires conversion checking, and conversion
checking requires evaluating terms containing free variables.

So why not just reuse open evaluation in code generation? The idea is not
new. Open evaluation is just a tiny step away from *normalization*, so usually
we get normalization as part of the package. If we use open evaluation at
runtime, we can also normalize values at runtime. However:

- We can't just normalize code and expect it to get faster. Normal forms tend to
  blow up in size and contain massive duplication of computations. We need a lot
  of fine control over what gets computed at code generation time.
- Using open evaluation at runtime *all the time* has significant overhead.
  Closed evaluation is the standard compilation model even for dependent
  languages.

Idris 1 [reused open evaluation for code
generation](https://dl.acm.org/doi/10.1145/1863543.1863587), providing extra
control in the form of [static
arguments](https://docs.idris-lang.org/en/latest/reference/partial-evaluation.html).
This was better than naive normalization but still kinda crude and ad-hoc. I do
not know of more attempts since then. The idea is important though, and we shall
use it here.

Although closed evaluation is the standard thing at runtime, the necessity of
open evaluation is a *big* influence on the design of dependent languages. It's
a massive effort to add open evaluation to a language which wasn't originally
designed to support it. Imagine coming up with a specification for evaluating
OCaml or Haskell programs that contain free variables!

Getting to the **basic setup** of `dtt-rtcg`:

1. Code can be run both in **open** and **closed** evaluation mode. This can be
   implemented by compiling programs *twice* for the two modes (as I do here in
   the JS backend), but we can choose between interpretation and compilation for
   either mode. For instance, we could decide to natively compile closed semantics
   but leave open semantics as interpreted.
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
- We have `<~t> ≡ t` and `~<t> ≡ t` as definitional equalities.

That's it. This means that the code generation API is, type-theoretically
speaking, **trivial**! As far as type checking is concerned, it's exactly the
same as having `□ A` as a record type with a single field, with both beta
and eta rules.

This should not be too surprising. Up to definitional equality, staging does
nothing. This is also the case in [two-level type
theory](https://andraskovacs.github.io/pdfs/2ltt.pdf). The point of staging is
performance, and performance is *not invariant* under definitional equality.  We
use staging to exert control which is more fine-grained than definitional
equality. In other words, staging features are *transparent* to type checking
and conversion checking, but they have an impact on performance.

## 3. Non-staging features

A `dtt-rtcg` program is a single expression with type `Eff A`, where `Eff : U →
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

*Non-unicode syntax:* `->` for `→`, `\` for `λ`, `Nat` for `ℕ` in every primop,
so e.g. `readNat` for `readℕ`.

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
Non-unicode syntax: `Rec(...)` for `Σ(...)`.

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

## 4. Staging features

Recall the staging API:

```
□   : U → U
_~  : {A : U} → □ A → A
<_> : {A : U} → A → □ A
~<t> ≡ t
<~t> ≡ t
```
*Non-unicode: `□` can be written as `Code`.*

First I give an overview of the operational semantics, then give more examples.

- Every program starts in **closed effectful** mode. This is because complete programs
  have type `Eff A`.
- We enter **closed pure** evaluation whenever we evaluate a pure argument of an `Eff`
  primitive, like an argument to `return` or `printℕ`.
- We enter **open** evaluation whenever closed evaluation hits `<_>`.
  - Open evaluation is conceptually indexed by a natural number, the "stage" of
    evaluation. At stage 0, programs are evaluated normally, although
    computation might get blocked by free variables. At stage `suc n`, programs
    are instead evaluated to a runtime HOAS representation of code.
  - When we switch to open evaluation from closed evaluation, the starting stage is 1.
  - If open evaluation goes under a `<_>`, the stage is incremented.
  - If open evaluation goes under a `~_`, the stage is decremented.
- If closed evaluation hits `~t`, we perform **closed code generation**.
  - We evaluate `t : □ A` to a runtime HOAS representation of a piece of code,
    then reify it to a first-order syntactic representation.  Then, in the
    Haskell interpreter, we just re-evaluate it. In the JS backend, we build a
    string from it and `eval` it.
  - The newly generated code is guaranteed to not contain free variables. It can
    refer to previously constructed closed values in scope, but those are simply
    embedded "by reference" into the code. This is called "cross-stage persistence".
- If open evaluation at stage 0 hits `~t`, we perform **open code generation**.
  - It's similar to closed code generation, except that the new code can contain
    free variables.
  - Closed values are still embedded "by reference" into the code (cross-stage persisted).

**Example**. We create a simple code value at runtime.

First, we define the Church-coded Bool for demo purposes.

```
Bool = (B : U) → B → B → B;
true  : Bool = λ _ t f. t;
false : Bool = λ _ t f. f;

and (b1 b2 : Bool) : Bool =
  λ _ t f. b1 _ (b2 _ t f) f;
```
Then:
```
code : □ Bool = <and true true>;
```
When closed evaluation hits `<and true true>`, it switches over to open
evaluation at stage 1. In stage 1, everything just produces a HOAS
representation of expressions. So `and true true` is a representation of a
binary application.  But `and`, `true` and `true` are all references to existing
closed values, so they are represented as embedded pointers to runtime objects.

**Example**. We trigger *closed code generation*.

```
b : Bool = ~code;
```

When closed evaluation hits `~code`, we create code which simply corresponds to
`and true true`. We can put everything into a file called `tutorial.rtcg`:
```
Bool = (B : U) → B → B → B;
true  : Bool = λ _ t f. t;
false : Bool = λ _ t f. f;

and (b1 b2 : Bool) : Bool =
  λ _ t f. b1 _ (b2 _ t f) f;

code = <and true true>;
b = ~code;

return ()
```
When we run this with `dtt-rtcg tutorial.rtcg interp`, we get
the following output:
```
CODE GENERATED AT:
tutorial.rtcg:9:5:
  |
  | code = <and true true>;
9 | b = ~code;
  |     ^

CODE:

*and* *true* *true*

RESULT:
()
```
Note the `*and*`, `*true*` and `*false*`. The Haskell interpreter uses `*` wrapping
to display the closed values that are cross-stage persisted, i.e. embedded into
code by runtime reference.

Let's check out the JS execution, by `dtt-rtcg tutorial.rtcg run`. We get
```
CODE GENERATED AT:
tutorial.rtcg:9:5:
  |
  | code = <and true true>;
9 | b = ~code;
  |     ^
CODE:
() => {
return ((csp_[0]/*$and*/)._1(csp_[1]/*$true*/))._1(csp_[2]/*$true*/);
}
```
As you can see, this one is a lot uglier.

- Here `(csp_[0]/*$and*/)`
  corresponds to the `*and*` output in the Haskell interpreter. `csp_` is
  an array that's created before code generation, which stores all references
  to closed values that occur in the code. We create this array because `eval` in
  JS can only capture the concrete lexical scope at the point of invocation. So,
  we cannot dynamically capture `and` and `true`, instead we store them in
  an array called `csp_` which does get captured by `eval`, and index into that array
  in the generated code.
- The `/*$and*/` is simply an inline comment which makes the code a bit more readable.
- Note that function application uses an extra `._1` field projection. That's
  because functions are actually *pairs*, containing the closed evaluation
  implementation in field `_1` and the open evaluation implementation in field
  `_2`. So here we project out the closed version.

*Remark*. It would be possible to pretty-print the same nice representation in
the JS backend as in the Haskell, but it would be tedious (for me) to write the
same pretty-printer again in JS, and in any case we can just use the Haskell
version if we want to look at it.

**Example** for having a binder inside code.

```
code : □ (Bool → Bool) = <λ x. and x x>
fun : Bool → Bool = ~code;
```
Running this with `interp`, we get
```
CODE GENERATED AT:
tutorial.rtcg:12:7:
   |
   | code = <λ x. and x x>;
12 | fun = ~code;
   |       ^

CODE:

λ x. *and* x x
```
Now, `*and*` is the only cross-stage reference in the code and the `x`-es
are plain bound variables.

With `run`, we get:
```
CODE GENERATED AT:
tutorial.rtcg:12:7:
   |
   | code = <λ x. and x x>;
12 | fun = ~code;
   |       ^
CODE:
() => {
const $cl0_c = ($x) => {return ((csp_[0]/*$and*/)._1($x))._1($x)};
const $cl0_o = ($x) => {return app_(app_(CSP_(csp_[0], `$and`), $x), $x)};
return {_1 : $cl0_c, _2 : $cl0_o};
}
```
Remember that functions are *pairs of functions* in the JS backend. Hence,
the `λ x. and x x` is compiled to `{_1 : $cl0_c, _2 : $cl0_o}`, where
`_1` implements closed evaluation and `_2` implements open evaluation.
This is indicated in the names of functions: `$cl0_c` uses `_c` and
`$cl0_o` uses `_o` suffix.

- The closed code calls the cross-stage `and` function like before.
- `CSP_(t, s)` wraps up a closed runtime value, embedding it to open values. The
  `s` is a string that's only used to help debugging and make code more
  readable; it's similar to the inline comments next to `csp_[i]` in closed
  code.
- The open code uses `app_` instead of the closed function application. `app_`
  is defined in the RTS. It has a bit more logic, because it needs to check if its
  function input is a neutral value or a CSP-d value.
  1. Applying a neutral function yields a neutral value.
  2. Applying a CSP-d function projects out the *open* implementation and applies it.
  3. Applying a CSP-d function to a CSP-d argument performs an ordinary *closed* application
     and boxes up the result as a new CSP-d value.

This is the general pattern for all open computation: computation always gets
stuck on neutrals, but it always progresses on CSP-d values, since CSP-d values
are closed and hence must be canonical. Here's the full code of `app_`:

```
function app_(t, u) {
    if (t.tag === _CSP) {
        // t must be a closed closure
        const v1 = (t._1)                // unwrap the CSP constructor
        if (u.tag === _CSP) {
            return CSP_(v1._1(u._1), '') // closed application, re-pack result
        } else {
            return v1._2(u)              // open application
        }
    } else if (t.tag === _Lam) {         // function is an open function value
        return t._2(u)
    } else {
        return App_(t, u)                // function is neutral, return new neutral
    }
}
```

**Example**. Closure conversion in the JS backend.
```
fun : Bool → Bool → Bool = ~<λ x y. and y x>;
```
`interp` simply prints `λ x y. *and* y x` for the generated code. `run` prints
instead:
```
() => {
const $cl0_c = ($x) => ($y) => {return ((csp_[0]/*$and*/)._1($y))._1($x)};
const $cl0_o = ($x) => ($y) => {return app_(app_(CSP_(csp_[0], `$and`), $y), $x)};
const $cl1_c = ($x) => {return {_1 : $cl0_c($x), _2 : $cl0_o(CSP_($x, `$x`))}};
const $cl1_o = ($x) => {return Lam_(`$y`, $cl0_o($x))};
return {_1 : $cl1_c, _2 : $cl1_o};
}
```
What's this clutter? We're doing *closure conversion*: lambda expressions in
the code are converted to "top-level" functions that take as arguments the original
argument plus all the free variables in the function body.

- `λ x. (λ y. and y x)` is a function with no free variables (`and` is CSP-d instead).
- `λ y. (and y x)` is the other function, with one free variable `x`.
- Both of them have a closed and an open implementation.

Why is this needed? Since we need to generate closed and open code for each
function body, if we try to generate code in a naive structurally recursive way,
we get exponential-sized output. In this naive fashion, the body of `λ x y. t`
gets compiled twice, and in each case, `t` also gets compiled
twice. Closure-conversion is necessary to avoid blowup.

**Example** for combining expressions by having splices inside quotations.

```
compose {A B C}(f : B → C)(g : A → B) (a : A) : C =
  f (g a);

plus2code : □ ℕ → □ ℕ = λ x. <suc (suc ~x)>;
plus4code : □ ℕ → □ ℕ = compose plus2code plus2code;
plus4 : ℕ → ℕ = ~<λ n. ~(plus4code <n>)>;
```
This generates `λ n. suc (suc (suc (suc n)))` for `plus4`.

It's good to keep in mind that code generation for splices *never happens at
compile time*. In many cases, it would be clearly better to splice code at
compile time, but `dtt-rtcg` is not intended to demonstrate that.

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

In this example, we're using runtime code generation for something that would be
better served by *compile-time code generation*, and for that we should use a
[two-level or N-level type
theory](https://andraskovacs.github.io/pdfs/2ltt.pdf). The key point of
`dtt-rtcg` is that it can generate code that depends on information that's only
available at runtime, like data resulting from IO actions.

Note though that nothing prevents us from having both RTCG and 2LTT in the same
system. We can do that by simply having both sets of staging operations.

**Example** for open evaluation.

So how does open evaluation enter the picture? For demonstration, let's have the
following function:
```
f : ℕ → ℕ = λ x. id (id (suc (suc x)));
```
We can obtain a normalized source code of `f`.
We can do this for any function, in fact.
```
normalize {A}{B : A → U} (f : (a : A) → B a) : □ ((a : A) → B a) =
  <λ x. ~(let res = f x; <res>)>;

fun = ~(normalize f);

return ()
```
Running `interp`, we get `λ x. suc (suc x)` for `fun'`.
How does this work? Let's look at `normalize` again:
 ```
<λ x. ~(let res = f x; <res>)>;
```
The program under `~` runs in open mode at stage 0. That means, in short,
that it behaves similarly to open evaluation in other dependent languages.
In this context, `x` is a variable, as a neutral value. When it gets passed
to `f`, we make an open call to `f`, which can return another neutral value
that's stuck on `x`. Then, we quote that `res` as `<res>`.

Any neutral value that gets produced by open evaluation, can be converted into
code, by the same "readback" process that's available in dependent languages.
The bound variables occurring in neutrals get converted to "ordinary" code
variables. In this concrete example, we evaluate `id (id (suc (suc x))` with
neutral `x` and we get `suc (suc x)`.

The following *does not* work for function normalization:
```
normalize {A}{B : A → U} (f : (a : A) → B a) : □ ((a : A) → B a) =
  <λ x. ~<f x>>;
```
With this, for `~(normalize f)` we instead get `λ x. *f* x` in the Haskell runtime. That's because
we mention `f` at stage 1, under more quotes than splices, where no computation
may happen. There, `f` just get CSP-d and we get `λ x. *f* x`. The extra `let` inside
the splice forces `f` to be actually called in open mode.

**Note:** `normalize` is not a full beta-normalizer, because open evaluation
preserves CSP-d values whenever possible. For example:
```
add : ℕ → ℕ → ℕ = λ x y. ℕElim suc y x;
fun = ~(normalize (λ (_ : ℕ). add));
```
This generates `λ x. *add*`. On the other hand,
```
fun = ~(normalize (λ x. add x));
```
generates `λ x y. ℕElim suc y x`. Another example:
```
fun = ~(normalize (λ (_ : ℕ). add 10));
```
This generates, according to the Haskell interpreter, `λ x. *CSP*`. Huh? Here,
`*CSP*` denotes some closed value which doesn't have any *variable name*
corresponding to it, so we don't try to print anything informative. The value is
the closure that we get from applying `add` to `10`.  Since both `add` and `10`
are closed values, the application returns yet another CSP value.

You might find this "open evaluation modulo CSP" weird. Can we do something
different? There is a bit of wiggle room, but we can't deviate a lot from the
current design.

First, in any design, CSP *must* be preserved by open evaluation. The main
reason is *mutable references*. Mutable refs can be only created during
effectful closed execution, and generated code can refer to them. If we do not
have mutable refs, it is actually possible to generate a syntactic
representation of *any* runtime value, by the usual readback procedure of
normalization-by-evaluation.  If we have mutable refs, this is not possible.
Example:
```
do ref ← new 100;

f (x : ℕ) : Eff Σ() =
  do y ← read ref;
  do write ref (add x y);
  return ();

f' = ~(normalize f);
```
The generated code is:
```
λ x.
  do y ← read *ref*;
  do write *ref* (ℕElim suc y x);
  return ()
```
The `*ref*` occurrences only make sense as CSP. It is not possible to "unfold"
them any further.

Second, how much computation should happen in open evaluation? Consider:
```
foo = ~<λ (x : ℕ). ~(id <x>)>;
```
It's clear here that `id <x>` should beta-reduce during codegen. What about
```
foo = ~<λ (x : ℕ). ~((λ x. x) <x>)>;
```
This should beta-reduce the same way. What about
```
foo = ~<λ (x : ℕ). ~(let id = λ y. y; id <x>)>;
```
This should reduce as well.

Now, the type system allows the outer `x` to occur under the splice as a "naked"
bound variable. We can do, for instance:
```
foo = ~<λ (x : ℕ). ~(let y = x; <y>)>;
```
which generates `λ x. x`. In this case, `let y = x` must be handled by the
*same* evaluation strategy as in the previous examples, because a) the type
system doesn't care about naked bound variables b) in more complicated programs,
we cannot even statically decide if some program bumps into bound variables at
runtime. And since the evaluation strategy computes "normally" for values that
don't contain bound variables, in the general case we should also compute
"normally", except for:

1. Getting stuck on bound vars.
2. Preserving CSP-d values.

### 4.2 Bigger examples

See [`examples.rtcg`](examples.rtcg) for:

- Generating monadic code like in "Closure-Free Functional Programming in a
  Two-Level Type Theory".
- Examples with length indexed-vectors, also reusing the `Gen` monad from the
  previous example.
- Fold fusion for lists.

This already includes some natural examples for generating code generators. For
example, we want to define:
```
down : (n : ℕ) → {A} → Vec n (□ A) → □ (Vec n A)
```
This function converts a vector of expressions to a vector expression, where we
individually let-bind all elements and then rebuild a vector of variables. When
defining this function, we want to reuse a `foldr` function which inlines the
function argument.

## Conclusions

Let's wrap up.

1. The biggest advantage is the extremely simple staging API, in terms of
   typing. It's so simple that it's close to trivial to add to the kernel and
   the surface of any dependently typed language.
2. However, the simplicity of typing is somewhat countered by the complexity of
   the runtime semantics. Programmers have to know about closed and open
   evaluation, and their interaction. I don't think it's *too* complex though,
   and I believe that this is much better as a design point than e.g. using
   substructural typing to prohibit open evaluation.
3. In terms of practical benefit in "ordinary" programming, runtime code
   generation is not as nearly as useful as control over compile time code
   generation. But there are some unique use cases, and it's perfectly fine to
   support both compile-time and runtime code generation in the same system.
