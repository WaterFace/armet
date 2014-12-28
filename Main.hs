module Main where

import qualified Vector as V

import FRP.Helm
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Time as Time
import qualified FRP.Helm.Text as Text
import qualified Control.Arrow as A

data GameState = GameState {
    _player :: Actor,
    _enemies :: [Actor]
}

data Actor = Actor {
    pos          :: (Double, Double),
    vel          :: (Double, Double),
    frictionMult :: Double,
    accel        :: Double,
    gravity      :: (Double, Double),
    box          :: Bbox
}

data Bbox = Bbox {
    topLeft :: (Double, Double),
    width   :: Double,
    height  :: Double
}

boxToForm :: Bbox -> Form
boxToForm Bbox {width = w, height = h} = move (w / 2, h / 2) $ outlined (dashed white) $ rect w h

winDim :: Num a => (a, a)
winDim = (640, 480)

playerSprite :: Form
playerSprite = sprite 32 64 (0, 0) "player.png"

deltaTime :: Signal Double
deltaTime = (/ Time.second) <~ Time.fps 120.0

stepPlayer :: (Double, Double) -> Actor -> Actor
stepPlayer dv actor@Actor{pos=p,vel=v,frictionMult=f,accel=a,gravity=g,box=b@Bbox{topLeft=(x,y),width=w,height=h}} = actor {
    pos = clampedPos,
    vel = (V.scale f $ v `V.add` (a `V.scale` dv)) `V.add` (if y+h < 480 then g else V.zero),
    box = b { topLeft = clampedPos }
}
  where
    newPos = p `V.add` v
    clampedPos = if snd newPos > 480-h then (fst newPos, 480-h) else newPos
    
stepGame :: (Double, Double) -> GameState -> GameState
stepGame input GameState { _player = p, _enemies = es} = GameState { _player=stepPlayer input p, _enemies=map id es }

render :: (Int, Int) -> GameState -> Element
render (w, h) GameState { _player = Actor { pos = p@(mx, my), box = b } } =
  collage w h [move (mx, my) $ playerSprite,
               move (200, 50) $ toForm $ Text.text (Text.color white (Text.toText $ show p)),
               move (mx, my) $ boxToForm b]
  --filled green $ square 100

main :: IO ()
main = run config $ render <~ Window.dimensions ~~ stepper
  where
    config = defaultConfig { windowDimensions = (640, 480), windowTitle = "Armet" }
    player = Actor {
        pos = (0, 0),
        vel = (0, 0),
        frictionMult = 0.95,
        accel = 10.0,
        gravity = (0, 0.3),
        box = Bbox { 
            topLeft = (0, 0),
            width = 32.0,
            height = 64.0
        }
    }
    state = GameState { _player = player, _enemies = []}
    stepper = foldp stepGame state (V.scale <~ deltaTime ~~ doubleKeyboard)
    both f = f A.*** f
    doubleKeyboard = both fromIntegral <~ Keyboard.wasd