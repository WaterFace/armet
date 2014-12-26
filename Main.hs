module Main where

import qualified Vector as V

import FRP.Helm
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Time as Time
import qualified Control.Arrow as A

data GameState = GameState {
    _player :: Actor,
    _enemies :: [Actor]
}

data Actor = Actor {
    pos          :: (Double, Double),
    vel          :: (Double, Double),
    frictionMult :: Double,
    accel        :: Double
}

deltaTime :: Signal Double
deltaTime = (/ Time.second) <~ Time.fps 120.0

stepPlayer :: (Double, Double) -> Actor -> Actor
stepPlayer dv actor@Actor{pos=p,vel=v,frictionMult=f,accel=a} = actor {
    pos = p `V.add` v,
    vel = V.scale f $ v `V.add` (a `V.scale` dv)
}

stepGame :: (Double, Double) -> GameState -> GameState
stepGame input GameState { _player = p, _enemies = es} = GameState { _player=stepPlayer input p, _enemies=map id es }

render :: (Int, Int) -> GameState -> Element
render (w, h) GameState { _player = Actor { pos = (mx, my) } } =
  centeredCollage w h [move (mx, my) $ filled green $ square 100]

main :: IO ()
main = run config $ render <~ Window.dimensions ~~ stepper
  where
    config = defaultConfig { windowTitle = "Armet"}
    acceleration = 10.0 :: Double
    player = Actor { pos = (0, 0), vel = (0, 0), frictionMult = 0.95, accel = acceleration }
    state = GameState { _player = player, _enemies = []}
    stepper = foldp stepGame state (V.scale <~ deltaTime ~~ doubleKeyboard)
    both f = f A.*** f
    doubleKeyboard = both fromIntegral <~ Keyboard.wasd