main :: IO ()
main = do
    input <- readFile "input.txt"
    let parsedInput = parseInput input
        result = part2 parsedInput
    print result

parseInput :: String -> [(Int, Int)]
parseInput input = map parseLine (lines input)

parseLine :: String -> (Int, Int)
parseLine line =
    let [a, b] = words line
        a' = read a :: Int
        b' = read b :: Int
    in (a', b')

part1 :: [(Int, Int)] -> Int
part1 listOfPairs = totalDifference
    where
        list1 = sort $ map fst listOfPairs
        list2 = sort $ map snd listOfPairs
        totalDifference = sum (zipWith (\x y -> abs (x - y)) list1 list2)

part1' :: [(Int, Int)] -> Int
part1' =
    sum
    . map (abs . uncurry (-))
    . uncurry zip
    . (sort *** sort)
    . unzip

part2 :: [(Int, Int)] -> Int
part2 listOfPairs = sum $ zipWith (*) list1 countsInList2
    where
        list1 = map fst listOfPairs
        list2 = map snd listOfPairs
        countsInList2 = map (`countOccurrences` list2) list1

countOccurrences :: (Eq a) => a -> [a] -> Int
countOccurrences x xs = length $ filter (== x) xs
