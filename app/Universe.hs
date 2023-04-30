module Universe where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector

data Sandbox = Sandbox {
  bodies :: [Body]
                       }

data Body = Body {
  bodyName       :: String,
  bodyMass       :: Float,
  bodyVelocity   :: Vector,
  bodyPos        :: Vector,
  bodyPosHistory :: [Vector]
                 }

gravitationalConstant :: Float
gravitationalConstant = 100

gravitationalLaw :: Float -> (Float, Float) -> Float -> Float
gravitationalLaw g m r = (g * (fst m) * (snd m)) / (r**2)



attractionCycle :: [Body] -> [Body]
attractionCycle b = b

window :: Display
window = InWindow "Universe" (500, 500) (250, 250)

background :: Color
background = black

drawing :: Picture
drawing = pictures [ color red $ circle 80 ]

universeEngine :: IO()
universeEngine = display window background drawing
