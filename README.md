# dtt-rtcg

Runtime code generation with dependent types. This is a small demo which is only
mildly practical. In principle, we can extend any practical dependently typed
language (like Idris, Lean) with the features here.

*[Installation](#installation)
*[Tutorial](#tutorial)
*[Technical background](#technical-background)

## Installation

1. Install the [Haskell Stack](https://docs.haskellstack.org/en/stable/).
2. Download or clone this repository.
2. Hit `stack install` in the project folder.
3. Make sure that you have the `stack` install directory on your PATH; if you
   don't, you will be reminded when `stack install` finishes.
5. Run `dtt-rtcg` to get an overview of command line usage.

## Tutorial

`dtt-rtcg` is a small dependently typed language which supports runtime code
generation. Its basic features are:

- Dependent functions, records, type-in-type, builtin natural numbers.
- Convenience: Agda-style implicit arguments, overloaded record fields,
  opening records to bring their fields into scope.

Code generation features:

- Full type safety: generated code is always well-typed.
- Any number of stages: generated code can also generate code.
- Cross-stage persistence:

WIP

## Technical background

WIP


<!-- Runtime code generation with dependent types. Documentation WIP. -->

<!-- Features right now: -->

<!-- - Dependent functions, anonymous records, type-in-type, builtin natural numbers with induction. -->
<!-- - Builtin `Eff` monad supporting minimal IO and mutable references. -->
<!-- - Runtime code generation: -->
<!--   - Full type safety. -->
<!--   - No restriction on generated code. The whole language is supported; generated code can itself arbitrarily generate code. -->
<!--   - Full cross-stage persistence for all types. Definitions can be reused in any code. No structural or scope-based restrictions, only typing rules.  -->
<!-- - Reference interpreter in Haskell. -->
<!-- - Javascript backend that uses `eval` to implement runtime code generation "for real". -->
<!-- - Efficient native compilation model with no runtime overheads compared to Lean or Idris. -->
<!--   - (That's in principle; we do have overheads in the JS backend, where we don't have full control over runtime object representation). -->
<!-- - Convenience features: Agda-style implicit arguments with decent higher-order unification, `open`-ing records to bring their contents to scope. -->

<!-- Installation: -->

<!-- 1. Install `dtt-rtcg` with `stack install` in the project directory. -->
<!-- 2. Install nodejs: https://nodejs.org/en/learn/getting-started/how-to-install-nodejs -->
