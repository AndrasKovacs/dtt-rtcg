# dtt-rtcg

Runtime code generation with dependent types. Documentation WIP.

Features right now:

- Dependent functions, anonymous records, type-in-type, builtin natural numbers with induction.
- Builtin `Eff` monad supporting minimal IO and mutable references.
- Runtime code generation:
  - Full type safety.
  - No restriction on generated code. The whole language is supported; generated code can itself arbitrarily generate code.
  - Full cross-stage persistence for all types. Definitions can be reused in any code.
- Reference interpreter in Haskell.
- Javascript backend that uses `eval` to implement runtime code generation "for real".
- Efficient native compilation model with no runtime overheads compared to Lean or Idris.
  - (But we do have overheads in the JS backend, where we don't have full control over runtime object representation).
- Convenience features: Agda-style implicit arguments with decent higher-order unification, `open`-ing records to bring their contents to scope.

Installation:

1. Install with `stack install`
2. Install nodejs and npm: https://docs.npmjs.com/downloading-and-installing-node-js-and-npm
3. Do `npm install readline-sync`.  
