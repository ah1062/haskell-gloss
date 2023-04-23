module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
-- https://lodev.org/cgtutor/raycasting.html#Untextured_Raycaster_
-- https://github.com/vinibiavatti1/RayCastingTutorial/wiki/RayCasting

data World = World {
               worldScreen :: Screen,
               worldPlayer :: Player,
               worldMap    :: [[Int]]
                   }

data Screen = Screen {
                screenDimensions :: (Float, Float)
                     }

data Ray = Ray {
             rayOrigin    :: Vector,
             rayDirection :: Vector,
             rayMapSector :: (Int, Int),
             rayDistance  :: Float,
             rayColor     :: Color
               } deriving (Show)

data Player = Player {
                playerFOV       :: Float,
                playerPosition  :: Vector,
                playerDirection :: Float
                     }

precision :: Float
precision = 64

colorChoice :: Int -> Color
colorChoice code
  | code == 1 = red
  | code == 2 = yellow
  | code == 3 = cyan
  | code == 4 = magenta
  | otherwise = black

degreesToRadians :: Float -> Float
degreesToRadians d = d * pi / 180

truncateV :: Vector -> Vector
truncateV vector = (fromIntegral (floor $ fst vector),
                    fromIntegral (floor $ snd vector))

roundV :: Vector -> (Int, Int)
roundV vec = (round $ fst vec, round $ snd vec)

getMapValV :: [[Int]] -> Vector -> Int
getMapValV mapW vec = mapW !! snd vecR !! fst vecR
                       where vecR = roundV vec

raycastIncrementRad :: World -> Float
raycastIncrementRad world = degreesToRadians ( (playerFOV $ worldPlayer world) / (fst $ screenDimensions $ worldScreen world) )

raycast :: World -> [Ray]
raycast world = do
  let player   = worldPlayer world
      rayAngle = degreesToRadians (playerDirection player - playerFOV player / 2)
 
  [ raycast' world rayDir rayOrigin |
      x <- [0..(fst $ roundV $ screenDimensions $ worldScreen world) - 1],
      let rayDir    = rayAngle + (raycastIncrementRad world) * fromIntegral x,
      let rayOrigin = playerPosition player
                                                                                                      ]

raycast' :: World -> Float -> Vector -> Ray
raycast' world rayAngle rayPos
  | getMapValV (worldMap world) rayPos /= 0 = Ray {
                                               rayOrigin    = playerPosition $ worldPlayer world,
                                               rayDirection = unitVectorAtAngle rayAngle,
                                               rayMapSector = roundV rayPos,
                                               rayDistance  = magV ((fst rayPos) - (fst $ playerPosition $ worldPlayer world),
                                                                    (snd rayPos) - (snd $ playerPosition $ worldPlayer world)) *
                                                              cos (rayAngle - (playerDirection $ worldPlayer world)),
                                               rayColor     = colorChoice (getMapValV (worldMap world) (rayPos))
                                                                              }
  | otherwise                                                           = do
      let rayCos = (cos rayAngle) / precision
          raySin = (sin rayAngle) / precision
      raycast' world rayAngle (fst rayPos + rayCos, snd rayPos + raySin)


drawRay :: World -> Ray -> Float -> Picture
drawRay world ray x = do
  let halfHt = ((snd $ screenDimensions $ worldScreen world) / 2)
      wallHt = (halfHt / rayDistance ray)
  pictures [ color (dark $ dark $ dark blue)                                 $ line [(x, 0), (x, halfHt - wallHt)],
             color (iterate dim (rayColor ray) !! (round $ rayDistance ray)) $ line [(x, halfHt - wallHt), (x, halfHt + wallHt)],
             color (dark $ dark $ dark white)                                $ line [(x, halfHt + wallHt), (x, halfHt * 2)]
           ]
    
window :: World -> Display
window world = InWindow "Universe" dimensions offset
                 where dimensions = roundV $ truncateV $ screenDimensions $ worldScreen world
                       offset     = (100, 100)

background :: Color
background = black

drawing :: World -> Picture
drawing world = pictures rays 
                  where rays = [ drawRay world (raycast world !! x) (fromIntegral x) | x <- [ 0..(length $ raycast world) - 1 ] ] 

main :: IO ()
main = do
  let screen = Screen (500, 500)
      player = Player 60 (1,3) 0
      mapW   = [
                 [1,1,1,1,1,1,1,1],
                 [1,0,0,0,0,0,0,1],
                 [1,0,0,2,0,0,3,1],
                 [1,0,0,0,0,0,0,1],
                 [1,0,0,0,0,4,0,1],
                 [1,0,0,0,0,0,0,1],
                 [1,0,0,0,0,0,0,1],
                 [1,1,1,1,1,1,1,1]
               ]
      world  = World screen player mapW

  putStrLn $ show $ playerDirection player
  mapM_ putStrLn (map show (raycast world))
  
  display (window world) background (drawing world)
