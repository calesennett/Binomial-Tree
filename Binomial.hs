module Binomial
    ( pairs
    , finalNodes
    , finalVals
    , optionVal
    , genPrice
    , Option (..)
    ) where

import           Option
import           Data.Matrix
import           Data.MemoUgly

main =  do
        let option  = Option { optionType="call"
                             , stock=31.25
                             , strike=30.00
                             , riskFree=0.07
                             , time=0.1
                             , vol=0.5
                             , divYield=0.0
                             }
            steps       = 22
            rf          = riskFree option
            dt          = deltaT option steps
            p           = prob option steps
            priceTree   = fromList steps steps $ map (genPrice steps option) (pairs [0..(steps - 1)])
            finals      = toLists $ fromList steps steps $ map (finalVals option steps priceTree) (pairs [0..(steps - 1)])
        print $ optionVal rf dt p steps (0,0) finals (finalNodes steps)

optionVal
    :: Double
    -> Double
    -> Double
    -> Int
    -> (Int, Int)
    -> [[Double]]
    -> [(Int, Int)]
    -> Double
optionVal
    rf
    dt
    p
    steps
    (downs, ups)
    finalOpts
    leaves =  memo optionVal' rf dt p steps (downs, ups) finalOpts leaves
                 where
                 optionVal'
                    rf
                    dt
                    p
                    steps
                    (downs, ups)
                    finalOpts
                    leaves
                    | (downs, ups) `elem` leaves = finalOpts !! downs !! ups
                    | otherwise = exp (-rf * dt) * ((p * (optionVal rf dt p steps (ups+1, downs) finalOpts leaves))
                                                                 +  ((1.0 - p) * (optionVal rf dt p steps (ups, downs+1) finalOpts leaves)))

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
                                         then maximum [0.0, (!) prices (i+1, j+1) - strike option]
                                         else maximum [0.0, strike option - (!) prices (i+1, j+1)]
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
