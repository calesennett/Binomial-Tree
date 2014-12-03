module Binomial
    ( pairs
    , finalNodes
    , finalVals
    , optionVal
    , genPrice
    , Option (..)
    , optionMemo
    ) where

import           Option
import           Data.Matrix
import           Data.Maybe
import           Data.IORef
import           System.IO.Unsafe
import qualified Data.Map as Map

main =  do
        let option  = Option { optionType="call"
                             , stock=31.25
                             , strike=30.00
                             , riskFree=0.07
                             , time=0.01
                             , vol=0.5
                             , divYield=0.0
                             }
            steps       = 125
            rf          = riskFree option
            dt          = deltaT option steps
            p           = prob option steps
            priceTree   = fromList steps steps $ map (genPrice steps option) (pairs [0..(steps - 1)])
            finals      = toLists $ fromList steps steps $ map (finalVals option steps priceTree) (pairs [0..(steps - 1)])
            val         = optionMemo (rf, dt, p, steps, (0,0), finals, (finalNodes steps))
        print val

optionVal
    ::  ( Double
        , Double
        , Double
        , Int
        , (Int, Int)
        , [[Double]]
        , [(Int, Int)])
    -> Double
optionVal
    ( rf
    , dt
    , p
    , steps
    , (downs, ups)
    , finalOpts
    , leaves)
    | (downs, ups) `elem` leaves = finalOpts !! downs !! ups
    | otherwise =   let upVal   = optionMemo (rf, dt, p, steps, (downs, ups+1), finalOpts, leaves)
                        downVal = optionMemo (rf, dt, p, steps, (downs+1, ups), finalOpts, leaves)
                    in  (exp (-rf * dt) * ((p * upVal) + ((1.0 - p) * downVal)))

optionMemo
    ::  ( Double
        , Double
        , Double
        , Int
        , (Int, Int)
        , [[Double]]
        , [(Int, Int)])
    -> Double
optionMemo = memoize optionVal

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
                                         then maximum [0, (!) prices (i+1, j+1) - strike option] -- call option
                                         else maximum [0, strike option - (!) prices (i+1, j+1)] -- put  option
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

{- Memoize function credit to Martin Lutke,
 - URL: http://stackoverflow.com/questions/141650/how-do-you-make-a-generic-memoize-function-in-haskell
 - Desc: Memoizes a function using a Map structure
 --}

memoize :: Ord a => (a -> b) -> (a -> b)
memoize f = unsafePerformIO $ do
    r <- newIORef Map.empty
    return $ \ x -> unsafePerformIO $ do
        m <- readIORef r
        case Map.lookup x m of
            Just y  -> return y
            Nothing -> do
                    let y = f x
                    writeIORef r (Map.insert x y m)
                    return y

pairs
    :: (Eq a)
    => [a]
    -> [(a, a)]
pairs
    xs = [ (x, y) | x <- xs, y <- xs ]
