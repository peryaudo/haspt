-- Haspt: A Path Tracing Renderer Written in Haskell

import Debug.Trace
import Data.List
import Data.Bits
import Numeric

-- you MUST use -O2 or -O3 optimization option, otherwise you will take extremely long time.
-- $ ghc -O3 haspt.hs && ./haspt > image.ppm

main = putStrLn (imageToPpm (render 640 480 4 4))
--main = putStrLn (imageToPpm (render 320 240 8 4))

-- constants

inf = 1e128
eps = 1e-6

depthLowerBound = 5
depthUpperBound = 64

-- vectors

type Vec = (Double, Double, Double)
type Color = Vec
type Image = [[Color]]

-- addition
infixl 6 %+
(ax, ay, az) %+ (bx, by, bz) = (ax + bx, ay + by, az + bz)
-- subtraction
infixl 6 %-
(ax, ay, az) %- (bx, by, bz) = (ax - bx, ay - by, az - bz)
-- scalar product
infixl 7 %%*
k %%* (x, y, z) = (k * x, k * y, k * z)
-- scalar division
infixl 7 %%/
(x, y, z) %%/ k = (x / k, y / k, z / k)
-- interior product
infixl 7 %.
(ax, ay, az) %. (bx, by, bz) = ax * bx + ay * by + az * bz
-- exterior product 
infixl 7 %*
(ax, ay, az) %* (bx, by, bz) =
  (ay * bz - by * az,
   az * bx - bz * ax,
   ax * by - bx * ay)
-- element product
infixl 7 %<*>
(ax, ay, az) %<*> (bx, by, bz) = (ax * bx, ay * by, az * bz)

-- generate orthonormal basis whose one component is the argument
genBasis :: Vec -> (Vec, Vec, Vec)
genBasis w@(wx, _, _) = (w, u, w %* u)
  where u = normalize $ (if abs wx > eps then (0.0, 1.0, 0.0) else (1.0, 0.0, 0.0)) %* w

-- make the normal vector oriented
orient :: Vec -> Vec -> Vec
orient normal dir = (if normal %. dir < 0.0 then 1.0 else (-1.0)) %%* normal

-- normalize the vector
normalize :: Vec -> Vec
normalize (x, y, z) = (x / k, y / k, z / k)
  where k = sqrt (x * x + y * y + z * z)

-- take the average of the vectors
average :: [Vec] -> Vec
average xs = foldl1' (\prev cur -> prev %+ cur) xs %%/ fromIntegral (length xs)

-- objects 

data Ray = Ray { rayOrg :: Vec, dir :: Vec }

data HitPoint = HitPoint { dist :: Double, normal :: Vec, hpPos :: Vec }

data Intersection = Intersection HitPoint Sphere

data ReflType = ReflTypeDiffuse | ReflTypeSpecular | ReflTypeRefraction

data Sphere = Sphere { rad :: Double,
                       spPos :: Vec,
                       emission :: Color,
                       color :: Color,
                       reflType :: ReflType }

-- check whether the ray intersects with the sphere
intersectSphere :: Ray -> Sphere -> Maybe HitPoint
intersectSphere (Ray org dir) s@(Sphere rad pos _ _ _)
  | d4 < 0.0             = Nothing
  | t1 < eps && t2 < eps = Nothing
  | otherwise = Just $ HitPoint { dist = dist',
                                  hpPos = hpPos',
                                  normal = normalize $ hpPos' %- pos }

  where hpPos' = org %+ (dist' %%* dir)
        po = pos %- org
        b  = po %. dir
        d4 = b * b - po %. po + rad * rad

        t1 = b - sqrt d4
        t2 = b + sqrt d4
        dist' = if t1 > eps then t1 else t2

-- scene data

bgColor = (0.0, 0.0, 0.0)

defaultScene =
  [Sphere 1e5  (1e5 + 1  , 40.8       , 81.6      ) (0.0 , 0.0 , 0.0 ) (0.75, 0.25, 0.25) ReflTypeDiffuse,
   Sphere 1e5  (-1e5 + 99, 40.8       , 81.6      ) (0.0 , 0.0 , 0.0 ) (0.25, 0.25, 0.75) ReflTypeDiffuse,
   Sphere 1e5  (50.0     , 40.8       , 1e5       ) (0.0 , 0.0 , 0.0 ) (0.75, 0.75, 0.75) ReflTypeDiffuse,
   Sphere 1e5  (50.0     , 40.8       , -1e5 + 250) (0.0 , 0.0 , 0.0 ) (0.0 , 0.0 , 0.0 ) ReflTypeDiffuse,
   Sphere 1e5  (50.0     , 1e5        , 81.6      ) (0.0 , 0.0 , 0.0 ) (0.75, 0.75, 0.75) ReflTypeDiffuse,
   Sphere 1e5  (50.0     , -1e5 + 81.6, 81.6      ) (0.0 , 0.0 , 0.0 ) (0.75, 0.75, 0.75) ReflTypeDiffuse,
   --Sphere 20   (65.0     , 20.0       , 20.0      ) (0.0 , 0.0 , 0.0 ) (0.25, 0.75, 0.25) ReflTypeDiffuse,
   Sphere 16.5 (65.0     , 20.0       , 20.0      ) (0.0 , 0.0 , 0.0 ) (0.99, 0.99, 0.99) ReflTypeRefraction,
   Sphere 16.5 (27.0     , 16.5       , 47.0      ) (0.0 , 0.0 , 0.0 ) (0.99, 0.99, 0.99) ReflTypeSpecular,
   Sphere 16.5 (77.0     , 16.5       , 78.0      ) (0.0 , 0.0 , 0.0 ) (0.99, 0.99, 0.99) ReflTypeRefraction,
   Sphere 15.0 (50.0     , 90.0       , 81.6      ) (36.0, 36.0, 36.0) (0.0 , 0.0 , 0.0 ) ReflTypeDiffuse]

defaultRefractiveIndex = 1.5

-- check whether there's a sphere which intersects with the ray
intersectScene :: Ray -> Maybe Intersection
intersectScene ray = fst (foldl' check (Nothing, inf) defaultScene)
  where check prev@(res, dist) curSphere =
          case ray `intersectSphere` curSphere of
            Just (curHP@(HitPoint curDist _ _))
              | curDist < dist -> (Just (Intersection curHP curSphere), curDist)
            _ -> prev

-- radiance function
radiance :: Ray -> [Double] -> Int -> (Color, [Double])
radiance ray (rnd:rnds) depth =
  case intersectScene ray of
    Nothing -> (bgColor, rnds)
    Just (inter@(Intersection _ object)) ->
      if rnd >= termProb then
        (emission object, rnds)
      else
        radiance' (reflType object) inter rnds termProb

      where termProb = getTermProb (color object)

            getTermProb :: Color -> Double
            getTermProb (x, y, z)
              | depth < depthLowerBound = 1.0
              | depth > depthUpperBound = xyzmax * (0.5 ** fromIntegral (depth - depthUpperBound))
              | otherwise = xyzmax
                where xyzmax = max x $ max y z

  where
    radiance' :: ReflType -> Intersection -> [Double] -> Double -> (Color, [Double])

    radiance' ReflTypeDiffuse (Intersection point object) (rnd:rnd':rnds) termProb = (reflected, rnds')
      where (w, u, v) = genBasis $ orient (normal point) (dir ray)

            r1 = 2 * pi * rnd
            r2 = rnd'

            dir' = normalize ((cos r1 * sqrt r2) %%* u %+
                              (sin r1 * sqrt r2) %%* v %+
                              (sqrt (1.0 - r2)) %%* w)

            (inRadiance, rnds') = radiance (Ray (hpPos point) dir') rnds (depth + 1)

            reflected = emission object %+ weight %<*> inRadiance

            weight = color object %%/ termProb

    radiance' ReflTypeSpecular (Intersection point object) rnds termProb = (reflected, rnds')
      where (inRadiance, rnds') = radiance (Ray org' dir') rnds (depth + 1)

            reflected = emission object %+ weight %<*> inRadiance

            org' = hpPos point

            -- This normalization is not written in original edupt and it should be a normalized vector.
            -- Is it a bug of original edupt? or I have a bug in another part of the code?
            dir' = normalize (dir ray %- 2.0 * (normal point %. dir ray) %%* normal point)

            weight = color object %%/ termProb

    radiance' ReflTypeRefraction (Intersection point object) (rnd:rnds) termProb
      | cos2t < 0.0 = let (inRadiance, rnds') = radiance (Ray (hpPos point) reflDir) rnds (depth + 1)
                          weight = color object %%/ termProb
                      in (emission object %+ weight %<*> inRadiance, rnds')

      | depth > 2 = if rnd < refrTermProb then
                      let (inRadiance, rnds') = radiance (Ray (hpPos point) reflDir) rnds (depth + 1)
                          weight = color object %%/ (termProb * refrTermProb)
                      in (emission object %+ weight %<*> (re %%* inRadiance), rnds')
                    else
                      let (inRadiance, rnds') = radiance (Ray (hpPos point) refrDir) rnds (depth + 1)
                          weight = color object %%/ (termProb * (1.0 - refrTermProb))
                      in (emission object %+ weight %<*> (tr %%* inRadiance), rnds')

      | otherwise = let (reflInRadiance, rnds') = radiance (Ray (hpPos point) reflDir) rnds (depth + 1)
                        (refrInRadiance, rnds'') = radiance (Ray (hpPos point) refrDir) rnds' (depth + 1)
                        inRadiance = re %%* reflInRadiance %+ tr %%* refrInRadiance
                        weight = color object %%/ termProb
                    in (emission object %+ weight %<*> inRadiance, rnds'')

      where isRayThrownIn = (normal point) %. orient (normal point) (dir ray) > 0.0
            reflDir = normalize (dir ray %- 2.0 * (normal point %. dir ray) %%* normal point)
            refrDir = normalize (rel %%* dir ray %- (if isRayThrownIn then 1.0 else -1.0)
                                    * (dirDotNormal * rel + sqrt cos2t) %%* normal point)

            refrTermProb = 0.25 + 0.5 * re

            -- snell's law
            nc = 1.0
            nt = defaultRefractiveIndex
            rel = if isRayThrownIn then nc / nt else nt / nc
            dirDotNormal = (dir ray) %. orient (normal point) (dir ray)
            cos2t = 1.0 - rel * rel * (1.0 - dirDotNormal * dirDotNormal)

            -- Schlick's approximation of Fresnel equations
            a = nt - nc
            b = nt + nc
            r0 = (a * a) / (b * b)
            c = 1.0 - (if isRayThrownIn then
                         -dirDotNormal
                       else
                         (-1.0) * (refrDir %. orient (normal point) (dir ray)))
            re = r0 + (1.0 - r0) * (c ** 5.0)
            tr = (1.0 - re) * rel * rel

-- renderers

render :: Int -> Int -> Int -> Int -> Image
render width height sample supersample =
  do y <- reverse [0 .. height - 1]
     return $ trace (showGFloat (Just 3) (fromIntegral (height - y - 1) / fromIntegral height * 100.0) ""
                     ++ "% completed (rendering y = " ++ show (height - y - 1) ++ ")") $ fst $ 
                foldr
                  (\x (prev, rnds) ->
                        let (cur, rnds') = renderSubpixels x y rnds in (cur : prev, rnds'))
                  ([], xorShiftRands y)
                  [0 .. width - 1]
  where
    subpixels = [(sx, sy) | sx <- [0 .. supersample - 1],
                            sy <- [0 .. supersample - 1],
                            _  <- [0 .. sample - 1]]

    renderSubpixels x y rnds =
      let (res, resRnds) = foldl' renderSubpixel ((0.0, 0.0, 0.0), rnds) subpixels
      in (res %%/ fromIntegral (length subpixels), resRnds)

      where

        renderSubpixel (prev, rnds) (sx, sy) =
          let (cur, rnds') = radiance (Ray camPos dir) rnds 0 in (prev %+ cur, rnds')

          where 
            rate = 1.0 / fromIntegral supersample
            r1 = fromIntegral sx * rate + rate / 2.0
            r2 = fromIntegral sy * rate + rate / 2.0

            scrPos = scrCenter %+
                     ((r1 + fromIntegral x) / fromIntegral width - 0.5) %%* scrX %+
                     ((r2 + fromIntegral y) / fromIntegral height - 0.5) %%* scrY

            dir = normalize $ scrPos %- camPos

    camPos = (50.0, 52.0, 220.0)
    camDir = normalize (0.0, -0.04, -1.0)
    camUp  = (0.0, 1.0, 0.0)

    scrWidth  = 30.0 * fromIntegral width / fromIntegral height
    scrHeight = 30.0
    scrDist   = 40.0

    scrX = scrWidth %%* (normalize (camDir %* camUp))
    scrY = scrHeight %%* (normalize (scrX %* camDir))
    scrCenter = camPos %+ scrDist %%* camDir

-- randoms

xorShiftRands :: Int -> [Double]
xorShiftRands seed = xorShiftRands' 123456789 362436069 521288629 seed

-- to use the standard random number generator, comment out this and import System.Random
-- (it may have quality and speed problems)

-- xorShiftRands a b c d = randomRs (0.0, 1.0) (mkStdGen ((((a * p) + b) * p + c) * p + d))

xorShiftRands' :: Int -> Int -> Int -> Int -> [Double]
xorShiftRands' x y z w = 
  let t = x `xor` (x `shiftL` 11)
      x' = y
      y' = z
      z' = w
      w' = (w `xor` (w `shiftR` 19)) `xor` (t `xor` (t `shiftR` 8))
  in (fromIntegral w' / fromIntegral (maxBound :: Int)) : xorShiftRands' x' y' z' w'

-- PPM exporters 

imageToPpm :: Image -> String
imageToPpm image = header ++ body
  where header = "P3\n" ++ show (length (head image)) ++ " " ++ show (length image) ++ "\n255\n"
        body = concat (map (\xs -> (concat (map pixel xs))) image)

        pixel (x, y, z) = show (hdrToLdr x) ++ " " ++ show (hdrToLdr y) ++ " " ++ show (hdrToLdr z) ++ " "

        hdrToLdr x = round (clamp x ** (1.0 / 2.2) * 255)
        clamp = max 0.0 . min 1.0


