module Shuffle where

import System.Random

shuffle :: [a] -> IO [a]
shuffle l = shuffle' l []
  where
    shuffle' [] acc = return acc

    shuffle' l acc =
      do k <- randomRIO (0, length l - 1)
         let (lead, x:xs) = splitAt k l
         shuffle' (lead ++ xs) (x:acc)

