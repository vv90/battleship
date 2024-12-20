
module LibSpec where 

import Relude
import Test.Hspec
import Lib 

spec :: Spec
spec = do
  describe "sinkAllShips" $ do
    it "handles 2x2 grid and 1x1 ship at (1, 0)" $ do
      case placeTargets [Ship 1 Horizontal (1, 0)] of
        Left e -> expectationFailure $ show e
        Right shoot -> do
          sinkAllShips 2 2 [1] shoot
      
    it "handles 2x2 grid and 2x1 ship at (0, 0)" $ do
      case placeTargets [Ship 2 Horizontal (0, 0)] of
        Left e -> expectationFailure $ show e
        Right shoot -> do
          sinkAllShips 2 2 [2] shoot

    it "handles no-win scenario" $ do
      case placeTargets [] of
        Left e -> expectationFailure $ show e
        Right shoot -> do
          sinkAllShips 2 2 [1] shoot

  describe "placeTargets" $ do
    it "handles valid ship placement" $ do
      case placeTargets [Ship 2 Horizontal (0, 0), Ship 2 Vertical (0, 2)] :: Either Text (ShootFn IO) of
        Left e -> expectationFailure $ show e
        Right _ -> pure ()

    it "returns error for invalid ship placement" $ do
      case placeTargets [Ship 2 Horizontal (0, 0), Ship 2 Vertical (0, 1)] :: Either Text (ShootFn IO) of
        Left _ -> pure ()
        Right _ -> expectationFailure "Expected error"