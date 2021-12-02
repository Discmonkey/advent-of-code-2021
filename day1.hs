toInts :: [Char] -> [Integer]
toInts text = map read $ lines text

countIncreasing :: [Integer] -> Integer
countIncreasing [] = 0
countIncreasing [x] = 0
countIncreasing (x:xs) = (if x < (head xs) then 1 else 0) + (countIncreasing xs)

windowBatch :: [Integer] -> [[Integer]]
windowBatch (x0:(x1:(x2:xs))) = [x0, x1, x2] : (windowBatch $ [x1, x2] ++ xs)
windowBatch _ = []

countIncreasingWindow :: [Integer] -> Integer
countIncreasingWindow [] = 0
countIncreasingWindow x = countIncreasing $ map sum $ windowBatch x
main = do
  content <- readFile "/home/max/projects/haskell/advent-of-code-2021/data/day1.txt"
  print $ countIncreasing $ toInts content
  print $ countIncreasingWindow $ toInts content