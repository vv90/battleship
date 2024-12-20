module Lib
  ( sinkAllShips
  , ShootResult(..)
  , Ship(..)
  , ShipOrientation(..)
  , ShootFn
  , placeTargets
  , Board
  ) where

import Relude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Control.Monad (foldM)

data ShootResult = Hit | Miss | HitSunk
  deriving (Show, Eq)

-- public state
type Board = HashMap (Int, Int) ShootResult

-- private state
type Battlefield = HashMap (Int, Int) Ship

-- minor change from the original problem definition
-- replaced State with StateT for easier composition with IO
type ShootFn m = Int -> Int -> StateT Board m ShootResult

data ShipOrientation = Horizontal | Vertical
  deriving (Show, Eq)

data Ship = Ship
  { shipLength :: Int
  , shipOrientation :: ShipOrientation
  , shipHeadCoords :: (Int, Int)
  } deriving (Show, Eq)

-- | Places the provided list of ships on the private battlefield
-- and returns a function that can be used to shoot at the battlefield
-- with a possible error message if ships overlap
placeTargets :: Monad m => [Ship] -> Either Text (ShootFn m)
placeTargets targets = 
  shoot <$> foldM placeOnTheBattlefield HM.empty targets 
  where 
    shoot :: Monad m => Battlefield -> ShootFn m
    shoot battlefield impactX impactY = do
      repeatedShotResult <- gets (HM.lookup (impactX, impactY))

      case (repeatedShotResult, HM.lookup (impactX, impactY) battlefield) of
        -- already shot at those coordinates
        (Just _, _) -> 
          pure Miss

        (Nothing, Just ship) -> do 
          modify $ HM.insert (impactX, impactY) Hit

          sunk <- gets (isSunk ship)

          if sunk 
            then do
              forM_ (shipSectionsCoords ship) 
                $ (\(x, y) -> modify $ HM.insert (x, y) HitSunk)
              pure HitSunk
            else
              pure Hit
          
        (Nothing, Nothing) -> do 
          modify $ HM.insert (impactX, impactY) Miss
          pure Miss

    isSunk :: Ship -> Board -> Bool
    isSunk ship board =
      all (\(x, y) -> HM.lookup (x, y) board == Just Hit) $ shipSectionsCoords ship


    placeOnTheBattlefield :: Battlefield -> Ship ->  Either Text Battlefield
    placeOnTheBattlefield currentBattlefield ship  =
      let 
        withNewShip :: Battlefield
        withNewShip = HM.fromList $ unfoldShip ship

        withNewShipMask :: HashSet (Int, Int)
        withNewShipMask = HS.fromList $ shipMask ship
      in 

      if not $ HS.null $ HS.intersection (HM.keysSet currentBattlefield) withNewShipMask
        then Left "Ship overlaps with or touches another ship"
        else Right $ HM.union currentBattlefield withNewShip

    shipSectionsCoords :: Ship -> [(Int, Int)]
    shipSectionsCoords (Ship len orientation (x, y)) = 
      case orientation of
        Horizontal -> [(x + i, y) | i <- [0..(len - 1)]]
        Vertical -> [(x, y + i) | i <- [0..(len - 1)]] 

    unfoldShip :: Ship -> [((Int, Int), Ship)]
    unfoldShip ship = (\xy -> (xy, ship)) <$> shipSectionsCoords ship

    shipMask :: Ship -> [(Int, Int)]
    shipMask (Ship len orientation (headX, headY)) = 
      case orientation of
        Horizontal -> [(x, y) | x <- [(headX - 1)..(headX + len)], y <- [(headY - 1)..(headY + 1)]]
        Vertical -> [(x, y) | x <- [(headX - 1)..(headX + 1)], y <- [(headY - 1)..(headY + len)]]


sinkAllShips :: Int -> Int -> [Int] -> ShootFn IO -> IO ()
sinkAllShips n m shipLengths shoot = do
  _ <- runStateT (exterminate plan (length shipLengths)) HM.empty
  pure ()

  where
    plan :: [(Int, Int)]
    plan = [(x, y) | x <- [0..n - 1], y <- [0..m - 1]]

    announcedShot :: (Int, Int) -> StateT Board IO ShootResult
    announcedShot (x, y) = do
      r <- shoot x y
      print $ show (x, y) <> (" "::Text) <> show r
      pure r

    -- sprayAndPray :: StateT Board IO ()
    -- sprayAndPray = traverse_ announcedShot [(x, y) | x <- [0..n - 1], y <- [0..m - 1]]
    
    exterminate :: [(Int, Int)] -> Int -> StateT Board IO ()
    exterminate [] _ =
      pure ()
    
    exterminate _ 0 =
      pure ()

    exterminate (target:rest) survivors = do
      r <- announcedShot target
      case r of
        Hit -> exterminate rest survivors
        Miss -> exterminate rest survivors
        HitSunk -> exterminate rest (survivors - 1)
