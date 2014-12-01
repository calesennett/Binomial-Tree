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
import qualified Data.Map as Map

main =  do
        let option  = Option { optionType="call"
                             , stock=31.25
                             , strike=22.75
                             , riskFree=0.07
                             , time=0.08333333333
                             , vol=0.5
                             , divYield=0.0
                             }
            steps       = 16
            priceTree   = fromList steps steps $ map (genPrice steps option) (pairs [0..(steps - 1)])
            finals      = fromList steps steps $ map (finalVals option steps priceTree) (pairs [0..(steps - 1)])
        print $ optionVal option steps (0,0) finals (finalNodes steps)

optionVal
    :: Option
    -> Int
    -> (Int, Int)
    -> Matrix Double
    -> [(Int, Int)]
    -> Double
optionVal
    option
    steps
    (downs, ups)
    finalOpts
    leaves
    | (downs, ups) `elem` leaves = (!) finalOpts (downs+1, ups+1)
    | otherwise = exp (-(riskFree option) * (deltaT option steps)) * (((prob option steps) * (optionVal option steps (downs, ups+1) finalOpts leaves))
                                                             +  ((1 - (prob option steps)) * (optionVal option steps (downs+1, ups) finalOpts leaves)))
        --where optionVal' o s (d, u) finalOpts leaves vs = let val = Map.lookup (show d ++ "," ++ show u) vs
        --                                           in if (fromMaybe 0.0 val) == 0.0
        --                                              then let memos = Map.insert (show d ++ "," ++ show u) (optionVal o s (d, u) finalOpts leaves vs) vs
        --                                                   in (optionVal o s (d, u) finalOpts leaves memos)
        --                                              else (fromMaybe 0.0 val)

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
