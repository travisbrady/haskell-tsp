module Util where

import qualified Data.ByteString.Lazy.Char8 as S
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import Data.Map ((!))

makeCity :: [Maybe (Int, S.ByteString)] -> (Int, Float, Float)
makeCity = toTuple . map fst . catMaybes
    where toTuple (x:y:z:rest) = (x, fromIntegral y, fromIntegral z)

getData = do
    contents <- S.readFile "eil51.tsp"
    let fields = map S.words $ takeWhile (\x -> x /= S.pack "EOF") $ drop 6 $ S.lines contents
    let intFields = map (map S.readInt) fields
    let cities = map makeCity intFields
    return cities

getOptTour = do
    contents <- S.readFile "eil51.opt.tour"
    let theData = takeWhile (\x -> x /= S.pack "EOF") $ drop 5 $ S.lines contents
    let tour = map (abs . fst) $ catMaybes $ map S.readInt theData
    return tour

tobs = S.pack . show
yex (x,y,z) = (tobs x, [tobs $ truncate y, tobs $ truncate z])

dumpTour tour fn = do
    cities <- getData
    let tt = map tobs tour
    let cmap = M.fromList $ map yex cities
    let outlist = [x:(cmap!x) | x <- tt]
    let outStr = S.unlines $ map S.unwords outlist
    S.writeFile fn outStr
    return ()
