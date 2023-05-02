module Main where

import Raycasting
import Universe

project :: Int -> IO()
project code 
  | code == 1 = raycastEngine
  | code == 2 = universeEngine
  | otherwise = project 1

main :: IO()
main = do
  putStrLn "[1] Raycast Project\n[2] Solar System Project"
  x <- readLn

  project x  
