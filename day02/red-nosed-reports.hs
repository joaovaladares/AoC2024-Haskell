main :: IO ()
main = do
    contents <- readFile "input1.txt"
    let linesOfNumbers = parseInput contents
        safeLineCount = part2 linesOfNumbers
    print safeLineCount

parseInput :: String -> [[Int]]
parseInput contents = map parseLine $ lines contents

parseLine :: String -> [Int]
parseLine line = map read $ words line

part1 :: [[Int]] -> Int
part1 xs = length $ filter isSafe xs

isSafe :: [Int] -> Bool
isSafe xs = isStrictlyMonotonic xs && all validDifferences (pairs xs)

isStrictlyMonotonic :: [Int] -> Bool
isStrictlyMonotonic xs = isIncreasing xs || isDecreasing xs
    where 
        isIncreasing xs = all (uncurry (<)) (pairs xs)
        isDecreasing xs = all (uncurry (>)) (pairs xs)

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

validDifferences :: (Int, Int) -> Bool
validDifferences (a, b) = let diff = abs (a - b) in diff >= 1 && diff <= 3 

part2 :: [[Int]] -> Int
part2 xs = length $ filter isSafeOrAlmost xs

isSafeOrAlmost :: [Int] -> Bool
isSafeOrAlmost xs = isSafe xs || any isSafe (removeOne xs)

removeOne :: [Int] -> [[Int]]
removeOne xs = [take i xs ++ drop (i+1) xs | i <- [0..length xs - 1]]
