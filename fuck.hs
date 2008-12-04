module Main where

a = round $ sqrt 16

distance (c1, x1, y1) (c2, x2, y2) = ((c1, c2), round $ sqrt ((x1-x2)^2 + (y1-y2)^2))

main = do
    print a
    print (distance (1, 3, 5) (2, 5, 12))
--computeDistanceMap :: [(Int, Int, Int)] -> M.Map (Int, Int) Int
--computeDistanceMap cities = M.fromList $ map (uncurry distance) [(x, y) | x <- cities, y <- cities]

--getTourDistance :: [[Int]] -> [Int] -> Int
--getTourDistance distMat tour = foldl distr 0 zpd
--    where zpd = zip (take (length tour -1) tour) (drop 1 tour)
--          distr curlen (x, y) = curlen + (distMat!!x-1)!!y-1

computeDistanceMatrix :: [(Int, Float, Float)] -> [[Int]]
computeDistanceMatrix cities = [[(x `dist` y) | x <- cities] | y <- cities]
    where dist (c1, x1, y1) (c2, x2, y2) = round $ sqrt ((x1-x2)^2 + (y1-y2)^2) 

distance :: (Int, Float, Float) -> (Int, Float, Float) -> ((Int, Int), Float)
distance (c1, x1, y1) (c2, x2, y2) = ((c1, c2), sqrt ((x1-x2)^2 + (y1-y2)^2))

greedyCross distMap mom dad acc = do
    let acclen = length acc
    case acclen == (length mom) of
        True -> return acc
        False -> do
            let cx = head acc
            let mnext = getNextCity mom cx
            let dnext = getNextCity dad cx
            guard (mnext==dnext && mnext `notElem` acc) return (greedyCross distMap mom dad mnext:acc)
            let mdist = M.lookup (cx, mnext) distMap
            let ddist = M.lookup (cx, dnext) distMap
            case mdist <= ddist && mnext `notElem` acc of
                True -> return (greedyCross distMap mom dad mnext:acc)
                False -> do
                    case ddist <= mdist && dnext `notElem` acc of
                        True -> return (greedyCross distMap mom dad mnext:acc)
                        False -> return (greedyCross distMap mom dad 99:acc)
