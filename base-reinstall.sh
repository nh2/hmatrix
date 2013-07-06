cd packages/base
cabal install --enable-documentation --enable-library-profiling
cd ../gsl
cabal install --enable-documentation --enable-library-profiling
cd ../special
cabal install --enable-documentation --enable-library-profiling
cd ../glpk
cabal install --enable-documentation --enable-library-profiling
cd ../tests 
cabal install --enable-tests --enable-documentation --enable-library-profiling
cd ../hmatrix
cabal install
