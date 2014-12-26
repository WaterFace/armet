module Main where

import qualified Vector as V

import FRP.Helm
--import Control.Applicative
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Time as Time
import qualified Control.Arrow as A

data Player = Player {
    pos          :: (Double, Double),
    vel          :: (Double, Double),
    frictionMult :: Double,
    accel        :: Double
}

deltaTime :: Signal Double
deltaTime = (/ Time.second) <~ Time.fps 120.0

step :: (Double, Double) -> Player -> Player
step dv player@Player{pos=p,vel=v,frictionMult=f,accel=a} = player {
    pos = p `V.add` v,
    vel = V.scale f $ v `V.add` (a `V.scale` dv)
}

render :: (Int, Int) -> Player -> Element
render (w, h) (Player { pos = (mx, my) }) =
  centeredCollage w h [move (mx, my) $ filled white $ square 100]

main :: IO ()
main = run config $ render <~ Window.dimensions ~~ stepper
  where
    config = defaultConfig { windowTitle = "Armet"}
    acceleration = 10.0 :: Double
    player = Player { pos = (0, 0), vel = (0, 0), frictionMult = 0.95, accel = acceleration }
    stepper = foldp step player (V.scale <~ deltaTime ~~ doubleKeyboard)
    both f = f A.*** f
    doubleKeyboard = both fromIntegral <~ Keyboard.wasd