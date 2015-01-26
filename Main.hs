module Main where

import qualified Vector as V

import FRP.Helm
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Mouse    as Mouse
import qualified FRP.Helm.Window   as Window
import qualified FRP.Helm.Time     as Time
import qualified FRP.Helm.Text     as Text
import qualified FRP.Helm.Graphics as Graphics
import qualified FRP.Helm.Color    as Color

import qualified Control.Arrow as A
import Control.Applicative

debug :: Bool
debug = False

{-
    Player Input
-}

data PlayerInput = PlayerInput {
    movement  :: (Double, Double),
    mousePos  :: (Int, Int),
    mouseDown :: Bool 
} deriving Show

type Input = (Time, PlayerInput)

playerInput :: Signal PlayerInput
playerInput = PlayerInput <$> doubleWasd
                          <*> Mouse.position
                          <*> Mouse.isDown
  where doubleWasd = both fromIntegral <$> Keyboard.wasd
        both f = f A.*** f

{-
    Game State
-}

data Player = Player {
    pos :: (Double, Double),
    dir :: Double,
    speed :: Double,
    spr :: Form
}

data GameState = GameState {
    player :: Player
}

defaultGame :: GameState
defaultGame = GameState {
    player = Player {
        pos = (100, 100),
        dir = 0,
        speed = 200,
        spr = Graphics.sprite 32 32 (0, 0) "sheet.png"
    }
}


{-
    Stepping
-}

stepPlayer :: Input -> Player -> Player
stepPlayer (dt, uin) p = p {
    pos =  pos p `V.add` (V.scale (dt * speed p) $ (normalize . movement) uin),
    dir = atan2 dy dx
}
  where
    dy = snd (pos p) - (fromIntegral $ snd (mousePos uin))
    dx = (fromIntegral $ fst (mousePos uin)) - fst (pos p)
    normalize (0, 0) = (0, 0)
    normalize a@(x, y) = (x/pythag a, y/pythag a)
    pythag (a, b) = sqrt $ a*a + b*b

stepGame :: Input -> GameState -> GameState
stepGame i state = state {
    player = stepPlayer i $ player state
} 

{-
    Display
-}

debugForm :: Form -> Form
debugForm f = if debug then f else Graphics.blank

playerToForm :: Player -> Form
playerToForm p = group [ move (pos p) $ rotateWithOrigin origin theta $ spr p
                       , debugForm $ (move (pos p) . filled red) (circle 1)]
  where
    theta = (2*pi) - (dir p)
    origin = (w/2, h/2)
    (w, h) = (32, 32)

-- given origin is relative to the form's origin
rotateWithOrigin :: (Double, Double) -> Double -> Form -> Form
rotateWithOrigin (ox, oy) theta = move ((-ox) * (cos theta - sin theta), (-oy) * (cos theta + sin theta)) . rotate theta

display :: (Int, Int) -> GameState -> Element
display (w, h) state = collage w h [p]
  where 
    p = playerToForm $ player state

{-
    Main
-}

delta :: Signal Time
delta = (/Time.second) <$> Time.fps 60.0

gameState :: Signal GameState
gameState = foldp stepGame defaultGame input

input :: Signal Input
input = (,) <$> delta <*> playerInput

main :: IO ()
main = do
    run config $ display <$> Window.dimensions <*> gameState
  where 
    config = defaultConfig { windowTitle = "Armet" }