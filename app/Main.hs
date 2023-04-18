module Main where

import Graphics.Gloss

window :: Display
window = InWindow "Test Window" (500, 500) (0, 0)

background :: Color
background = white

drawing :: Picture
drawing = circle 80

main :: IO ()
main = display window background drawing
