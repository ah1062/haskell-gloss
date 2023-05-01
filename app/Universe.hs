module Universe where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.IO.Simulate

data World = World {
    screen :: (Int, Int),
    bodies :: [Planet]
}

data Planet = Planet {
  planetName     :: String,
  planetStation  :: Bool,
  planetColor    :: Color,
  planetMass     :: Float,
  planetRadius   :: Float,
  planetPosition :: Vector,
  planetVelocity :: Vector,
  planetHistory  :: [Point]
                     } deriving (Eq)

earthRadius, earthMass, earthDistance :: Float
earthRadius   = 10
earthMass     = 1
earthDistance = 300

gravitationalConstant :: Float
gravitationalConstant = 100

gravitationalLaw :: (Float, Float) -> Float -> Float
gravitationalLaw m r = (gravitationalConstant * fst m * snd m) / (r**2)

sumV :: [Vector] -> Vector
sumV vs = (sum (fst <$> vs), sum (snd <$> vs))

differenceV :: Vector -> Vector -> Vector
differenceV (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

euclidean :: Vector -> Vector -> Float
euclidean (x1, y1) (x2, y2) = sqrt (x ** 2 + y ** 2)
                                where x = x1 - x2
                                      y = y1 - y2

planetGen :: Float -> Planet
planetGen code
  | code == 0 = Planet "Sun"     True  (makeColorI 225 225 035 255) (330000 * earthMass) (0.5 * earthRadius)   (0, 0)                     (0, 0e-3) []
  | code == 1 = Planet "Mercury" False (makeColorI 201 201 201 255) (0.0553 * earthMass) (0.383 * earthRadius) (0.39 * earthDistance, 0)  (0, 4e-1) []
  | code == 2 = Planet "Venus"   False (makeColorI 255 223 128 255) (0.815 * earthMass)  (0.949 * earthRadius) (0.72 * earthDistance, 0)  (0, 3e-1) []
  | code == 3 = Planet "Earth"   False (makeColorI 000 255 105 255) earthMass            earthRadius           (earthDistance, 0)         (0, 2.5e-1) []
  | code == 4 = Planet "Mars"    False (makeColorI 255 102 000 255) (0.107 * earthMass)  (0.532 * earthRadius) (1.52 * earthDistance, 0)  (0, 2e-1) []
  | code == 5 = Planet "Jupiter" False (makeColorI 255 179 102 255) (318 * earthMass)    (11.2 * earthRadius)  (5.20 * earthDistance, 0)  (0, 4e-2) []
  | code == 6 = Planet "Saturn"  False (makeColorI 255 204 153 255) (95.2 * earthMass)   (9.45 * earthRadius)  (9.58 * earthDistance, 0)  (0, 4e-3) []
  | code == 7 = Planet "Uranus"  False (makeColorI 153 255 255 255) (14.5 * earthMass)   (4.01 * earthRadius)  (19.18 * earthDistance, 0) (0, 4e-3) []
  | code == 8 = Planet "Neptune" False (makeColorI 000 051 204 255) (17.1 * earthMass)   (3.88 * earthRadius)  (30.07 * earthDistance, 0) (0, 4e-3) []
  | otherwise = planetGen 1

netForce :: Planet -> [Planet] -> [Vector]
netForce body [] = []
netForce body (p:ps)
  | body == p    = (0, 0) : netForce body ps
  | otherwise    = mulSV accel (normalizeV $ differenceV (planetPosition body) (planetPosition p)) : netForce body ps
                     where accel = gravitationalLaw (planetMass body, planetMass p) (euclidean (planetPosition body) (planetPosition p)) / planetMass body

timestep :: World -> World
timestep world = do
  let dt = 1e-8 * sqrt (maximum (map planetMass (bodies world)) / gravitationalConstant)
  let nfs =  [ mulSV dt (sumV $ netForce p ps) | x <- [0..length (bodies world) - 1],
                                                 let p  = bodies world !! x,
                                                 let ps = bodies world ]
  World {
    screen = screen world,
    bodies = [ alter p f | x <- [0..length (bodies world) - 1], let p = bodies world !! x, let f = nfs !! x ]
    }
    where alter p f
            | planetStation p         = p
            | otherwise               = Planet {
                    planetName     = planetName p,
                    planetStation  = planetStation p,
                    planetColor    = planetColor p,
                    planetMass     = planetMass p,
                    planetRadius   = planetRadius p,
                    planetPosition = sumV [planetPosition p, planetVelocity p],
                    planetVelocity = sumV [planetVelocity p, f],
                    planetHistory  = planetHistory p ++ [planetPosition p]
                                                    }

timeCycle :: ViewPort -> Float -> World -> World
timeCycle v f = timestep

window :: World -> Display
window w = InWindow "Universe" (screen w) (50, 50)

background :: Color
background = black

drawing :: World -> Picture
drawing w = pictures [ shapes, lines, strings ]
              where shapes  = pictures [ uncurry translate (planetPosition p) $ color (planetColor p) $ circleSolid (planetRadius p) | 
                                x <- [0..length (bodies w) - 1], let p = bodies w !! x ]
                    lines   = pictures [ color (planetColor p) $ line (take 5000 $ reverse $ planetHistory p) |
                                x <- [0..length (bodies w) - 1], let p = bodies w !! x ]
                    strings = pictures [ uncurry translate (sumV [planetPosition p, (0, -planetRadius p * 4)]) $ color (planetColor p) $ scale 0.2 0.2 (text (planetName p)) | 
                                x <- [0..length (bodies w) - 1], let p = bodies w !! x ] 

universeEngine :: IO()
universeEngine = do
    let world = World (500, 500) [planetGen x | x <- [0..4]]

    simulate (window world) background 100 world drawing timeCycle 
