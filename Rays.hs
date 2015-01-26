module Rays where

import Data.Array
import System.Random
import Control.Applicative

data Player = Player {
    x :: Double,
    y :: Double,
    dir :: Double
}

data Map = Map {
    size :: Int,
    wallGrid :: Array Int Bool
} deriving Show

data Input = Input {
    left     :: Bool,
    right    :: Bool,
    forward  :: Bool,
    backward :: Bool
}

randomMap :: Int -> IO (Map)
randomMap size = do
    gen <- getStdGen
    return $ Map size $ listArray (0, size*size) (randomRs (False, True) gen)
    
get :: Double -> Double -> Map -> Maybe Bool
get x y m = if ix < 0 || ix > size m || iy < 0 || iy > size m 
            then Nothing
            else Just $ wallGrid m ! (size m * iy + ix) 
  where
    ix = floor x
    iy = floor y

data Wall = Wall {
    wallX :: Double,
    wallY :: Double,
    sqLength :: Double
}

data Origin = Origin {
    originX  :: Double,
    originY  :: Double,
    height   :: Double,
    distance :: Double
}

noWall :: Wall
noWall = Wall { wallX = 0, wallY = 0, sqLength = 1e309 {-infinity-}}

cast :: (Double, Double) -> Double -> Double -> [Origin]
cast (pointX, pointY) angle range = undefined
  where
    step :: Double -> Double -> Double -> Double -> Bool -> Wall
    step rise run x y inverted = 
      let 
        dx = if run > 0 then fl (x+1) - x else ce (x-1) - x
        dy = dx * (rise/run)
      in 
        Wall { wallX = if inverted then y+dx else x+dy 
             , wallY = if inverted then x+dx else y+dy
             , sqLength = dx*dx + dy*dy
             }

    ray :: Origin -> [Origin]
    ray origin = 
      let
        stepX = step (sin angle) (cos angle) (originX origin) (originY origin) False
        stepY = step (cos angle) (sin angle) (originY origin) (originX origin) True
        nextStep = if sqLength stepX < sqLength stepY
                   then 
    inspect :: Origin -> Double -> Double -> Double -> Dpib;e

fl = fromIntegral . floor
ce = fromIntegral . ceiling