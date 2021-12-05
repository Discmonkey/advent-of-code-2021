data Instruction = Forward Integer | Down Integer | Up Integer deriving (Show)
type Depth = Integer
type Position = Integer
type Aim = Integer
data Location = Location Depth Position Aim deriving (Show)

newInstruction :: [String] -> Instruction
newInstruction ["forward", x] = Forward $ read x
newInstruction ["down", x] = Down $ read x
newInstruction ["up", x] = Up $ read x

updateOne :: Instruction -> Location -> Location
updateOne (Forward x) (Location d p a) = Location d (p + x) a
updateOne (Down x) (Location d p a) = Location (d + x) p a
updateOne (Up x) (Location d p a) = Location (d - x) p a

updateTwo :: Instruction -> Location -> Location
updateTwo (Forward x) (Location d p a) = Location (d + (a * x)) (p + x) a
updateTwo (Down x) (Location d p a) = Location d p (a + x)
updateTwo (Up x) (Location d p a) = Location d p (a - x)

travel :: (Instruction -> Location -> Location) -> [Instruction] -> Location
travel _ [] = Location 0 0 0
travel update (i:xs) = update i $ travel update xs

answer :: Location -> Integer
answer (Location d p _) = d * p

main = do
  content <- readFile "/home/max/projects/haskell/advent-of-code-2021/data/day2.txt"
  print $ answer . (travel updateOne) $ reverse $ map (newInstruction . words) $ lines content
  print $ answer . (travel updateTwo) $ reverse $ map (newInstruction . words) $ lines content