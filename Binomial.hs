module Binomial
    (
    ) where

import           Option
import           Data.Matrix

main =  do
        let option  = Option { optionType="call"
                             , stock=31.25
                             , strike=22.75
                             , riskFree=0.07
                             , time=0.01
                             , vol=0.5
                             , divYield=0.0
                             }
            steps       = 16
            priceTree   = fromList steps steps $ map (genPrice steps option) (pairs [0..(steps - 1)])
            finals      = fromList steps steps $ map (finalVals option steps priceTree) (pairs [0..(steps - 1)])
        print $ optionVal option steps (0,0) finals

optionVal
    :: Option
    -> Int
    -> (Int, Int)
    -> Matrix Double
    -> Double
optionVal
    option
    steps
    (downs, ups)
    finalOpts
    | (downs, ups) `elem` (finalNodes steps) = (!) finalOpts (downs+1, ups+1)
    | otherwise = exp (-(riskFree option) * (deltaT option steps)) * (((prob option steps) * (optionVal option steps (downs, ups+1) finalOpts))
                                                             +  ((1 - (prob option steps)) * (optionVal option steps (downs+1, ups) finalOpts)))

finalVals
    :: Option
    -> Int
    -> Matrix Double
    -> (Int, Int)
    -> Double
finalVals
    option
    steps
    prices
    (i, j)
    | (i, j) `elem` (finalNodes steps) = if (optionType option) == "call"
                                         then (!) prices (i+1, j+1) - strike option
                                         else strike option - (!) prices (i+1, j+1)
    | otherwise                        = 0

finalNodes
    :: (Num a, Enum a, Eq a)
    => a
    -> [(a, a)]
finalNodes
    steps = let end = steps - 1
            in [ (x, y) | x <- [0..end], y <- [0..end], (x, y) == (x, end - x) ]

genPrice
    :: Int
    -> Option
    -> (Int, Int)
    -> Double
genPrice
    steps
    option
    (i, j) = stock o * (up o steps)^^(j-i)
             where o = option

pairs
    :: (Eq a)
    => [a]
    -> [(a, a)]
pairs
    xs = [ (x, y) | x <- xs, y <- xs ]
