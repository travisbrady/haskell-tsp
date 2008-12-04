{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.Map as M
import Data.Map ((!))
import Control.Monad (liftM, forM_, forM, mapM)
import System.Random (randomRIO, randomR, mkStdGen)
import Data.Maybe (catMaybes, fromJust)
import Data.List ((!!), minimum, maximum, elemIndex, elem, (\\), nub, delete, sort, minimumBy, sortBy)
import Shuffle
import Util (getData, getOptTour, dumpTour)

generations = 50
npergen = 1000

mutProb :: Float
mutProb = 0.10
crossProb :: Float
crossProb = 0.75

computeDistanceMap :: [(Int, Float, Float)] -> M.Map (Int, Int) Int
computeDistanceMap cities = M.fromList [((fst3 x, fst3 y), x `dist` y) | x <- cities, y <- cities]
    where dist (c1, x1, y1) (c2, x2, y2) = round $ sqrt ((x1-x2)^2 + (y1-y2)^2) 
          fst3 (x,y,z) =  x

tourDistance :: (Ord a1, Num a) => M.Map (a1, a1) a -> [a1] -> a
tourDistance distMap tour = sum . catMaybes . map gtr $ selfZip tour
    where gtr k = M.lookup k distMap
          selfZip lst = zip lst (tail lst)

tdo distMap [] = 0
tdo distMap tour = td tour 0
    where td [one] !total = total
          td (x:y:rest) !total = td (y:rest) (total + (distMap ! (x, y)))
          
randomOf :: [a] -> IO a
randomOf xs = (xs!!) `liftM` randomRIO (0, length xs-1)

tournament :: (Ord a1, Ord t, Num a1, Num a, Enum a) =>
              M.Map (t, t) a1 -> a -> [[t]] -> IO [t]
tournament distMap n tours = do
    players <- mapM (\x -> randomOf tours) [1..n]
    let dists = map (tdo distMap) players
    let winner = head . sort $ zip dists players
    return (snd winner)

makeTours :: (Num a, Enum a) => a -> Int -> IO [[a]]
makeTours ncities ntours = do
    shuffd <- mapM shuffle $ replicate ntours [2..ncities]
    let ret = map (\x -> 1:x ++ [1]) shuffd
    return ret

getNextCity :: (Eq a) => [a] -> a -> a
getNextCity tour city = tour!!((fromJust $ elemIndex city tour)+1)

getDM = computeDistanceMap `liftM` getData

greedyCross :: (Num a) =>
               (a -> t -> t1 -> [a] -> a) -> t -> t1 -> [a] -> [a] -> [a]
greedyCross chooser mom dad notPicked [] = greedyCross chooser mom dad notPicked [1] 
greedyCross chooser mom dad [last] !acc = (1:last:acc)
greedyCross chooser mom dad notPicked !acc = (greedyCross chooser mom dad (delete theChosen notPicked) newAcc)
    where city = head acc
          theChosen = chooser city mom dad notPicked
          newAcc = theChosen:acc

chooseOne distMap city mom dad notPicked = choose
    where mnext = getNextCity mom city
          dnext = getNextCity dad city
          mdist = distMap ! (city, mnext)
          ddist = distMap ! (city, dnext)
          choose = case mdist <= ddist && mnext `elem` notPicked  of
            True -> mnext
            False -> case ddist <= mdist && dnext `elem` notPicked of
                True -> dnext
                False -> notPicked!!0

oneOne :: [Int]
oneOne = [1]

noOnes lst = (nub lst)\\oneOne

maybeCross chooser mtx tour dm pop = do
    crossRoll <- randomRIO (0.0, 1.0) :: IO Float
    case crossRoll < crossProb of 
        True -> do
            dad <- mtx pop
            let kid = greedyCross chooser tour dad (noOnes tour) []
            return kid
        False -> return tour

mutate dm tour = do
    mutRoll <- randomRIO (0.0, 1.0) :: IO Float
    case mutRoll < mutProb of
        True -> twoOpt dm tour
        False -> return tour

genGen chooser mtx pop dm i end acc = do
    mom <- mtx pop
    thisTour <- maybeCross chooser mtx mom dm pop
    muted <- mutate dm thisTour
    case i == end of
        True -> return acc
        False -> genGen chooser mtx pop dm (i+1) end (thisTour:acc)

twoOpt :: (Ord a, Num a1, Ord a1) => M.Map (a, a) a1 -> [a] -> IO [a]
twoOpt dm tour = do
    ai <- randomRIO (1, length tour - 2)
    bi <- randomRIO (1, length tour - 2)
    let [si, ei] = sort [ai, bi]
    let start = take si tour
    let mid = take (ei - si) $ drop si tour
    let end = drop ei tour
    let done = start ++ (reverse mid) ++ end
    case (tourDistance dm done) `compare` (tourDistance dm tour) of
        LT -> twoOpt dm done
        _ -> return tour

echo dists i = do
    if i `mod` 10 == 0
        then print (minimum dists)
        else putStr "."

keepOn chooser mtx pop dm i end = do
    let best = take 5 $ sortBy (\x y -> compare (tdo dm x) (tdo dm y)) pop
    ng <- genGen chooser mtx pop dm 0 npergen best
    let dists = map (tdo dm) ng
    echo dists i
    case i == end of
        True -> return (head best)
        False -> keepOn chooser mtx ng dm (i+1) end

main = do
    dm <- getDM
    pop <- makeTours 51 15000
    let chooser = chooseOne dm
    let mtx = tournament dm 2
    best <- keepOn chooser mtx pop dm 0 generations
    let bestDist = tourDistance dm best
    dumpTour best ("best_" ++ (show bestDist) ++ ".tour")
    return ()
