Binomial-Tree
=============

A binomial-tree model (Cox, Ross and Rubinstein) for pricing European call and put options.

## To run:
    ghci
    :l Binomial.hs
    main

This runs the model with a trinomial tree height of 125.

## To run tests:
    cabal install HSpec
    runhaskell Tests.hs
