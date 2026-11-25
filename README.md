# The `exception-monads-tf` Package  [![Hackage](https://img.shields.io/hackage/v/exception-monads-tf.svg)](https://hackage.haskell.org/package/exception-monads-tf) [![Actions Status: haskell-ci](https://github.com/mainland/exception-monads-tf/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/mainland/exception-monads-tf/actions?query=workflow%3Ahaskell-ci)

Provides exception monad transformer instances for the classes defined by [`monads-tf`](https://hackage.haskell.org/package/monads-tf).

Import `Control.Monad.Exception.Instances` for its instances:

```haskell
import Control.Monad.Exception.Instances ()
```

The [`exceptions`](https://hackage.haskell.org/package/exceptions) package
supersedes
[`exception-transformers`](https://hackage.haskell.org/package/exception-transformers),
and new code should use it.

On GHC 9.14, the only `monads-tf` release that accepts the compiler's `base` is
0.1.0.3, and it requires `transformers` 0.5. Cabal then reinstalls
`transformers`, so a build plan that also needs the `transformers` installed
with GHC 9.14, such as one that depends on the `ghc` package, has no solution.

## AI assistance

All contributions included in releases through version `0.4.0.1` were made
without AI assistance. AI tools were used in development, review, and release
preparation after that version, through version `0.4.0.2`. The maintainer
remains responsible for this release.
