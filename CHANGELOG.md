# Changelog

## 0.4.0.2

- Allow `monads-tf` 0.2 and 0.3 and `transformers` 0.6. On GHC 9.14,
  `monads-tf` 0.1.0.3 is the only usable release, and it requires
  `transformers` 0.5, so a build plan that needs the `transformers` installed
  with GHC has no solution there.
- Add a test suite. It checks the instances through `ExceptionT`, including
  exception propagation.
- Document the module and how to import its instances, and note that the
  `exceptions` package supersedes `exception-transformers`.
- Drop GHC 7.x, and test with the latest patch release of each GHC series from
  8.0 through 9.14.
- Require `base >= 4.9`, matching the supported GHC 8.0+ range. Raise the lower
  bounds on `exception-transformers`, `monads-tf`, and `transformers` to
  0.4.0.4, 0.1.0.3, and 0.5, the oldest versions installable with that `base`.
- Correct the issue tracker URL, use HTTPS for the source repository, and
  include the README and changelog in source distributions.
- Update the maintainer address in the module header and link the package
  homepage.
