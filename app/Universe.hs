module Universe where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector

data World = World {
    screen :: (Int, Int),
    bodies :: [Planet]
}

data Planet = Planet {
  planetColor    :: Color,
  planetMass     :: Float,
  planetRadius   :: Float,
  planetPosition :: Vector,
  planetVelocity :: Vector,
  planetAccelera :: Vector
                     } deriving (Eq)

earthRadius, earthMass, earthDistance :: Float
earthRadius   = 10
earthMass     = 1
earthDistance = 300

gravitationalConstant :: Float
gravitationalConstant = 100

gravitationalLaw :: (Float, Float) -> Float -> Float
gravitationalLaw m r = (gravitationalConstant * fst m * snd m) / (r**2)

distanceV :: Vector -> Vector -> Vector
distanceV (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

euclidean :: Vector -> Vector -> Float
euclidean (x1, y1) (x2, y2) = sqrt (x ** 2 + y ** 2)
                                where x = x1 - x2
                                      y = y1 - y2

planetGen :: Float -> Planet
planetGen code
  | code == 0 = Planet (makeColorI 225 225 035 255) (330000 * earthMass) (5 * earthRadius)     (0, 0)                     (0, 0) (0, 0)
  | code == 1 = Planet (makeColorI 201 201 201 255) (0.0553 * earthMass) (0.383 * earthRadius) (0.39 * earthDistance, 0)  (0, 0) (0, 0)
  | code == 2 = Planet (makeColorI 255 223 128 255) (0.815 * earthMass)  (0.949 * earthRadius) (0.72 * earthDistance, 0)  (0, 0) (0, 0)
  | code == 3 = Planet (makeColorI 000 255 105 255) earthMass            earthRadius           (earthDistance, 0)         (0, 0) (0, 0)
  | code == 4 = Planet (makeColorI 255 102 000 255) (0.107 * earthMass)  (0.532 * earthRadius) (1.52 * earthDistance, 0)  (0, 0) (0, 0)
  | code == 5 = Planet (makeColorI 255 179 102 255) (318 * earthMass)    (11.2 * earthRadius)  (5.20 * earthDistance, 0)  (0, 0) (0, 0)
  | code == 6 = Planet (makeColorI 255 204 153 255) (95.2 * earthMass)   (9.45 * earthRadius)  (9.58 * earthDistance, 0)  (0, 0) (0, 0)
  | code == 7 = Planet (makeColorI 153 255 255 255) (14.5 * earthMass)   (4.01 * earthRadius)  (19.18 * earthDistance, 0) (0, 0) (0, 0)
  | code == 8 = Planet (makeColorI 000 051 204 255) (17.1 * earthMass)   (3.88 * earthRadius)  (30.07 * earthDistance, 0) (0, 0) (0, 0)
  | otherwise = planetGen 1

netForce :: Planet -> [Planet] -> Float
netForce body [] = 0
netForce body (p:ps)
  | body == p    = 0 + netForce body ps
  | otherwise    = gravitationalLaw (planetMass body, planetMass p) (euclidean (planetPosition body) (planetPosition p)) + netForce body ps

timestep :: World -> World
timestep world = world

window :: World -> Display
window w = InWindow "Universe" (screen w) (250, 250)

background :: Color
background = black

drawing :: World -> Picture
drawing w = pictures [ uncurry translate (planetPosition p) $ color (planetColor p) $ circleSolid (planetRadius p) | x <- [0..length (bodies w) - 1],
                                                                                                                     let p = bodies w !! x ]

universeEngine :: IO()
universeEngine = do
    let world = World (500, 500) [planetGen x | x <- [0..8]]
    
    print $ netForce (bodies world !! 3) (bodies world)

    display (window world) background (drawing world)
