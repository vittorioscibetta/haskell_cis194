module Golf where

-- Hopscotch


skips :: [a] -> [[a]]
skips list = let indexedList = list `zip` [1..] in
                  let skips' (list, nth) = [fst x | x <- indexedList, (snd x) `mod` nth == 0] in map skips' . map (\(x,y) -> (list, y)) $ indexedList
