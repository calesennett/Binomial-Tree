module Option
    ( Option (..)
    , deltaT
    , up
    , down
    , prob
    ) where

data Option = Option { optionType :: String
                     , stock      :: Double
                     , strike     :: Double
                     , riskFree   :: Double
                     , time       :: Double
                     , vol        :: Double
                     , divYield   :: Double
         }
deltaT
    :: Option
    -> Int
    -> Double
deltaT
    option
    steps = (time option) / (fromIntegral steps)

up
    :: Option
    -> Int
    -> Double
up
    option
    steps = exp $ (vol o) * sqrt (deltaT o steps)
            where o = option

down
    :: Option
    -> Int
    -> Double
down
    option
    steps = 1 / (up option steps)

prob
    :: Option
    -> Int
    -> Double
prob
    option
    steps = (exp ((riskFree o - divYield o) * deltaT o steps) - down o steps) / (up o steps - down o steps)
             where o = option

