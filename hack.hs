module Main where

import qualified Data.Map as M
import Control.Monad (liftM, forM_, forM, forever, mapM)
import System.Random (randomRIO)
import Data.Maybe (catMaybes)
import Data.List ((!!), minimum, elemIndex)
import Shuffle
import Util (getData, getOptTour)

computeDistanceMap cities = M.fromList [((fst3 x, fst3 y), x `dist` y) | x <- cities, y <- cities]
    where dist (c1, x1, y1) (c2, x2, y2) = round $ sqrt ((x1-x2)^2 + (y1-y2)^2) 
          fst3 (x,y,z) =  x

tourDistance distMap tour = sum . catMaybes . map gtr $ selfZip tour
    where gtr k = M.lookup k distMap
          selfZip lst = zip lst (tail lst)

randomOf xs = (xs!!) `liftM` randomRIO (0, length xs-1)

tournament distMap tours = do
    a <- randomOf tours
    b <- randomOf tours
    let xdist = tourDistance distMap a
    let ydist = tourDistance distMap b
    case xdist <= ydist of
        True -> return a
        False -> return b

makeTours ncities ntours = do
    shuffd <- mapM shuffle $ replicate ntours [2..ncities]
    let ret = map (\x -> 1:x ++ [1]) shuffd
    return ret

getDM = computeDistanceMap `liftM` getData

main = do
    cities <- getData
    optTour <- getOptTour
    dm <- getDM
    pop <- makeTours 51 1000
    let dz = map (tourDistance dm) pop
    print (minimum dz)
    ng <- mapM (\x -> tournament dm pop) [1..1000]
    let ngz = map (tourDistance dm) ng
    let a = ngz!!0
    let b = ngz!!10
    kid <- greedyCross dm a b []
    print kid
    print (tourDistance dm kid)

