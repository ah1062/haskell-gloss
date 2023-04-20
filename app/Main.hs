module Main where

import Graphics.Gloss
-- https://lodev.org/cgtutor/raycasting.html#Untextured_Raycaster_

screenWidth  = 500
screenHeight = 500

mapWidth  = 10
mapHeight = 10

data World = World {
  player_F   :: Player,
  worldMap_F :: [[Int]]
                   }

data Player = Player {
  position_F  :: Vector,
  direction_F :: Vector,
  plane_F     :: Vector
                     }

data Ray = Ray {
  rayOrigin_F    :: Vector,
  rayDirection_F :: Vector,
  rayDeltaDist_F :: Vector,
  raySideDist_F  :: Vector,
  rayMapSector_F :: Vector,
  raySteps_F     :: (Float, Float),
  rayVertHoriz_F :: Int,
  rayHit_F       :: Bool
               }

truncateV :: Vector -> Vector
truncateV vector = (fromIntegral (floor $ fst vector),
                    fromIntegral (floor $ snd vector))

rayDirection :: Vector -> Vector -> Float -> Vector
rayDirection dir plane cameraX = (fst dir + fst plane * cameraX,
                                  snd dir + snd plane * cameraX)

deltaDistance :: Vector -> Vector
deltaDistance rayDir = (abs (1 / fst rayDir), abs (1 / snd rayDir))
                       
sideDistance :: Float -> Float -> Float -> Float
sideDistance step pos delta
  | step == (-1) = (pos - fromIntegral (floor pos)) * delta
  | otherwise    = (fromIntegral (floor pos) + 1 - pos) * delta

determineStep :: Float -> Float
determineStep rayDirVal
  | rayDirVal < 0 = (-1)
  | otherwise     = 1

raycast :: World -> Player -> Int -> Bool
raycast world player column = do
  let cameraX   = 2 * column `div` screenWidth - 1
      ray       = Ray (position_F player) (rayDir) (deltaDist) (sideDist) (truncateV $ position_F player) steps 0 False
                    where rayDir    = rayDirection (direction_F player) (plane_F player) (fromIntegral cameraX)
                          deltaDist = deltaDistance rayDir
                          steps     = (determineStep $ fst rayDir, determineStep $ snd rayDir)
                          sideDist  = ( sideDistance (fst steps) (fst $ position_F player) (fst deltaDist), sideDistance (snd steps) (snd $ position_F player) (snd deltaDist) )
  True

raycast' :: World -> Ray -> Vector -> Ray
raycast' world ray delta
  | rayHit_F ray == True = ray
  | otherwise            = raycast' world (determine $ increment ray) delta
                             where increment ray
                                     | (fst $ raySideDist_F ray) < (snd $ raySideDist_F ray) = do
                                         let newSideDist  = ((fst $ raySideDist_F ray) + fst delta, snd $ raySideDist_F ray)
                                             newMapSector = ( (fst $ rayMapSector_F ray) + (fst $ raySteps_F ray), snd $ raySteps_F ray )
                                         Ray (rayOrigin_F ray) (rayDirection_F ray) (rayDeltaDist_F ray) (newSideDist) (newMapSector) (raySteps_F ray) 0 (rayHit_F ray)
                                     | otherwise                                         = do
                                         let newSideDist  = (fst $ raySideDist_F ray, (snd $ raySideDist_F ray) + snd delta)
                                             newMapSector = ( fst $ rayMapSector_F ray, (snd $ rayMapSector_F ray) + (snd $ raySteps_F ray) )
                                         Ray (rayOrigin_F ray) (rayDirection_F ray) (rayDeltaDist_F ray) (newSideDist) (newMapSector) (raySteps_F ray) 1 (rayHit_F ray)
                                   determine ray
                                     | worldMap_F world !! (round $ snd $ rayMapSector_F ray) !! (round $ fst $ rayMapSector_F ray) /= 0 = Ray {
                                                                                                                                             rayOrigin_F    = rayOrigin_F ray,
                                                                                                                                             rayDirection_F = rayDirection_F ray,
                                                                                                                                             rayDeltaDist_F = rayDeltaDist_F ray,
                                                                                                                                             raySideDist_F  = raySideDist_F ray,
                                                                                                                                             rayMapSector_F = rayMapSector_F ray,
                                                                                                                                             raySteps_F     = raySteps_F ray,
                                                                                                                                             rayVertHoriz_F = rayVertHoriz_F ray,
                                                                                                                                             rayHit_F       = True
                                                                                                                                               }
                                     | otherwise                                                                                         = Ray {
                                                                                                                                             rayOrigin_F    = rayOrigin_F ray,
                                                                                                                                             rayDirection_F = rayDirection_F ray,
                                                                                                                                             rayDeltaDist_F = rayDeltaDist_F ray,
                                                                                                                                             raySideDist_F  = raySideDist_F ray,
                                                                                                                                             rayMapSector_F = rayMapSector_F ray,
                                                                                                                                             raySteps_F     = raySteps_F ray,
                                                                                                                                             rayVertHoriz_F = rayVertHoriz_F ray,
                                                                                                                                             rayHit_F       = False
                                                                                                                                               }
  
window :: Display
window = InWindow "Universe" (screenWidth, screenHeight) (100, 100)

background :: Color
background = black

drawing :: World -> Picture
drawing world = pictures ((build $ worldMap_F world) ++ [translate (fst playPos) (snd playPos) $ color green $ circleSolid 2])
                  where playPos = position_F $ player_F world
                        
build :: [[Int]] -> [Picture]
build codes = [translate (fromIntegral x * 20) (fromIntegral y * (-20)) $ build' z | x <- [0..length (codes !! 0) - 1],
                                                                                  y <- [0..length codes - 1],
                                                                                  let z = codes !! y !! x ]
              where build' code
                      | code == 0 = color red $ rectangleSolid 20 20
                      | code == 1 = color blue $ rectangleSolid 20 20
                      | otherwise = color white $ rectangleSolid 20 20

main :: IO ()
main = do
  let player  = Player (20, (-20)) ((1), 0) (0, 0.66)
      world   = World player [ [1,1,1,1,1,1,1,1,1,1],[1,0,0,0,0,1,0,0,1,1],[1,1,1,1,0,0,0,0,0,1],[1,0,0,0,0,0,1,1,1,1],[1,0,0,0,0,1,0,0,0,1],
                               [1,0,0,0,0,0,0,0,0,1],[1,0,0,0,1,1,1,1,0,1],[1,1,1,1,0,0,0,0,0,1],[1,1,0,0,0,0,1,1,0,1],[1,1,1,1,1,1,1,1,1,1] ]

  display window background (drawing world)
