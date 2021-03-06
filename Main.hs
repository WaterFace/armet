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

import Control.Arrow
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
        both f = f *** f

{-
    Game State
-}

data Player = Player {
    pos :: (Double, Double),
    dir :: Double,
    speed :: Double,
    spr :: Sprite
}

data GameState = GameState {
    player :: Player
}

defaultGame :: GameState
defaultGame = GameState {
    player = Player {
        pos = (100, 100),
        dir = 0,
        speed = 300,
        spr = Sprite {
            width = w,
            height = h,
            _sprite = Graphics.sprite w h (0, 0) "playerShip1_red.png"
        }
    }
}
  where
    (w, h) = (75, 99)


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

data Sprite = Sprite {
    width :: Int,
    height :: Int,
    _sprite :: Form
}

drawSprite :: Sprite -> Form
drawSprite = _sprite

debugForm :: Form -> Form
debugForm f = if debug then f else Graphics.blank

playerToForm :: Player -> Form
playerToForm p = group [ rotateAboutCenter origin size theta . move (pos p) . drawSprite $ (spr p)
                       , debugForm $ move (pos p) (filled red $ circle 1)
                       ]
  where
    theta = (2*pi) - (dir p)
    origin = (w/2, h/2)
    size@(w, h) = (fromIntegral *** fromIntegral) . (width &&& height) $ spr p

rotateAboutCenter :: (Double, Double) -> (Double, Double) -> Double -> Form -> Form
rotateAboutCenter (x, y) (w, h) theta = move (dx + (w/2), dy + (h/2)) . rotate theta
  where
    (ul_x1, ul_y1) = (x-(w/2), y-(h/2))
    ul_x2 = x + ((w/2)*cos theta) - ((h/2)*sin theta)
    ul_y2 = y + ((h/2)*cos theta) + ((w/2)*sin theta)
    (dx, dy) = (ul_x1 - ul_x2, ul_y1 - ul_y2)
    
background :: Form
background = group [move (fromIntegral (x * w), fromIntegral (y * h)) img | x <- [0..nwide], y <- [0..nhigh]]
  where
    nwide = (w_window `div` w) + 1
    nhigh = (h_window `div` h) + 1
    img = Graphics.sprite w h (0, 0) "darkPurple.png"
    (w, h) = (256, 256)
    (w_window, h_window) = windowDimensions config

display :: (Int, Int) -> GameState -> Element
display (w, h) state = collage w h [background, p]
  where 
    p = playerToForm $ player state

{-
    Main
-}

config :: EngineConfig
config = defaultConfig { windowTitle = "Armet"
                       , windowDimensions = (800, 600)
                       }

delta :: Signal Time
delta = (/Time.second) <$> Time.fps 60.0

gameState :: Signal GameState
gameState = foldp stepGame defaultGame input

input :: Signal Input
input = (,) <$> delta <*> playerInput

main :: IO ()
main = do
    run config $ display <$> Window.dimensions <*> gameState