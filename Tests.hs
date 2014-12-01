import           Test.Hspec
import           Binomial
import           Option
import qualified Data.Matrix as M

main =  hspec $ do
        describe "pairs" $ do
            it "can return all permutations of a list" $ do
                pairs [0..2] `shouldMatchList` [(0,0), (0,1), (0,2),
                                                (1,0), (1,1), (1,2),
                                                (2,0), (2,1), (2,2)]

        describe "finalNodes" $ do
            it "can return all correct final price nodes" $ do
                finalNodes 4 `shouldMatchList` [(0,3), (1,2), (2,1), (3,0)]

        describe "finalVals" $ do
            it "can return correct payoff at expiry nodes" $ do
                let matrix = M.fromList 3 3 $ [0, 0, 30,
                                               0, 25, 0,
                                               20, 0, 0]
                    option  = Option { optionType="call"
                                     , stock=31.25
                                     , strike=10.00
                                     , riskFree=0.07
                                     , time=0.08333333333
                                     , vol=0.5
                                     , divYield=0.0
                                     }

                map (finalVals option 3 matrix) (pairs [0..2]) `shouldMatchList` [0,0,20,0,15,0,10,0,0]

        describe "optionVal" $ do
            it "can return correct option value at time 0" $ do
                let option  = Option { optionType="call"
                                     , stock=31.25
                                     , strike=22.75
                                     , riskFree=0.07
                                     , time=0.08333333333
                                     , vol=0.5
                                     , divYield=0.0
                                     }
                    steps   = 16
                    leaves  = finalNodes steps
                    prices  = M.fromList steps steps $ map (genPrice steps option) (pairs [0..(steps - 1)])
                    finals  = M.fromList steps steps $ map (finalVals option steps prices) (pairs [0..(steps - 1)])
                optionVal option steps (0,0) finals leaves `shouldSatisfy` within 0.005 8.649020611573942

within
    :: Double
    -> Double
    -> Double
    -> Bool
within
    tolerance
    expected
    actual = if lower <= actual && actual <= upper
             then True
             else False
             where lower = (expected - expected * tolerance)
                   upper = (expected + expected * tolerance)
