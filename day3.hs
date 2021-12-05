toInts :: String -> [Integer]
toInts "" = []
toInts (c:rest) = (if c == '1' then 1 else -1):(toInts rest)

accumulate :: [[Integer]] -> [Integer]
accumulate l = foldl1 (zipWith (+)) l

power :: Integer -> Int -> Integer
power _ 0 = 1
power x y = x * (power x (y - 1))

asDec :: (Integer -> Integer) -> [Integer] -> Integer
asDec _ []  = 0
asDec f (x:xs) = (f x) * (power 2 $ length xs) + asDec f xs

asDecEasy = asDec (\x -> if x > 0 then 1 else 0)

type Scorer = [Integer] -> [Integer] -> Int -> Bool
scoreIt :: (Integer -> Integer -> Bool) -> Scorer
scoreIt f x y index = f (x!!index) (y!!index)

scoreItOnes = scoreIt  (\x y -> (x >= 0 && y > 0) || (x < 0 && y < 0))
scoreItZeros = scoreIt (\x y -> (x >= 0 && y < 0) || (x < 0 && y > 0))

filterRun :: Int -> Scorer -> [[Integer]] -> [Integer]
filterRun _ _ [item] = item
filterRun iteration scorer items =
  let key = accumulate items in
  filterRun (iteration + 1) scorer $ filter (\item -> scorer key item iteration) items

main = do
  content <- readFile "/home/max/projects/haskell/advent-of-code-2021/data/day3.txt"
  print $
    foldl1 (*) $
    fmap (\f -> f $ accumulate $ map toInts $ lines content) $
    fmap asDec [(\x -> if x > 0 then 1 else 0), (\x -> if x > 0 then 0 else 1)]
  print $
    let codes = map toInts $ lines content in
    (asDecEasy $ filterRun 0 scoreItZeros codes) * (asDecEasy $ filterRun 0 scoreItOnes codes)
--    findMax (accumulate $ codes) codes
--    (asDecEasy (findMin (accumulate $ map toInts $ lines content) (map toInts $ lines content)))