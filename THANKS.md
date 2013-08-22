I thank Don Stewart, Henning Thielemann, Bulat Ziganshin, Heinrich Apfelmus,
and all the people in the Haskell mailing lists for their help.

I am particularly grateful to Vivian McPhail for his excellent
contributions: improved configure.hs, Binary instances for
Vector and Matrix, support for Float and Complex Float elements,
module reorganization, monadic mapVectorM, and many other improvements.

- Nico Mahlo discovered a bug in the eigendecomposition wrapper.

- Frederik Eaton discovered a bug in the design of the wrappers.

- Eric Kidd has created a wiki page explaining the installation on MacOS X:
  http://www.haskell.org/haskellwiki/GSLHaskell_on_MacOS_X

- Fawzi Mohamed discovered a portability bug in the lapack wrappers.

- Pedro E. López de Teruel fixed the interface to lapack.

- Antti Siira discovered a bug in the plotting functions.

- Paulo Tanimoto helped to fix the configuration of the required libraries.
  He also discovered the segfault of minimize.hs in ghci.

- Xiao-Yong Jin reported a bug on x86_64 caused by the assumptions in f2c.h,
  which are wrong for this architecture.

- Jason Schroeder reported an error in the documentation.

- Bulat Ziganshin gave invaluable help for the ST monad interface to
  in-place modifications.

- Don Stewart fixed the implementation of the internal data structures
  to achieve excellent, C-like performance in Haskell functions which
  explicitly work with the elements of vectors and matrices.

- Dylan Alex Simon improved the numeric instances to allow optimized
  implementations of signum and abs on Vectors.

- Pedro E. López de Teruel discovered the need of asm("finit") to
  avoid the wrong NaNs produced by foreign functions.

- Reiner Pope added support for luSolve, based on (d|z)getrs.
  Made Matrix a product type and added changes to improve the code generated
  by hmatrix-syntax.

- Simon Beaumont reported the need of QuickCheck<2 and the invalid
  asm("finit") on ppc. He also contributed the configuration options
  for the accelerate framework on OS X.

- Daniel Schüssler added compatibility with QuickCheck 2 as well
  as QuickCheck 1 using the C preprocessor. He also added some
  implementations for the new "shrink" method of class Arbitrary.

- Tracy Wadleigh improved the definitions of (|>) and (><), which now
  apply an appropriate 'take' to the given lists so that they may be
  safely used on lists that are too long (or infinite).

- Chris Waterson improved the configure.hs program for OS/X.

- Erik de Castro Lopo added buildVector and buildMatrix, which take a
  size parameter(s) and a function that maps vector/matrix indices
  to the values at that position.

- Jean-Francois Tremblay discovered an error in the tutorial.

- Gilberto Camara contributed improved blas and lapack dlls for Windows.

- Heinrich Apfelmus fixed hmatrix.cabal for OS/X. He also tested the package
  on PPC discovering a problem in zgesdd.

- Felipe Lessa tested the performance of GSL special function bindings
  and contributed the cabal flag "safe-cheap".

- Ozgur Akgun suggested better symbols for the Bound constructors in the
  Linear Programming package.

- Tim Sears reported the zgesdd problem also in intel mac.

- Max Suica simplified the installation on Windows and improved the instructions.

- John Billings first reported an incompatibility with QuickCheck>=2.1.1

- Alexey Khudyakov cleaned up PRAGMAS and fixed some hlint suggestions.

- Torsten Kemps-Benedix reported an installation problem in OS/X.

- Stefan Kersten fixed hmatrix.cabal for 64-bit ghc-7 in OS/X

- Sacha Sokoloski reported an installation problem on Arch Linux and
  helped with the configuration.

- Carter Schonwald helped with the configuration for Homebrew OS X and
  found a tolerance problem in test "1E5 rots". He also discovered
  a bug in the signature of cmap.

- Duncan Coutts reported a problem with configure.hs and contributed
  a solution and a simplified Setup.lhs.

- Mark Wright fixed the import of vector >= 0.8.

- Bas van Dijk fixed the import of vector >= 0.8, got rid of some
  deprecation warnings, used more explicit imports, and updated to ghc-7.4.

- Tom Nielsen discovered a problem in Config.hs, exposed by link problems
  in Ubuntu 11.10 beta.

- Daniel Fischer reported some Haddock markup errors.

- Danny Chan added support for integration over infinite intervals.

- Clark Gaebel removed superfluous thread safety.

- Jeffrey Burdges reported a glpk link problem on OS/X

- Jian Zhang reported the Windows installation problem due to new ODE interface.

