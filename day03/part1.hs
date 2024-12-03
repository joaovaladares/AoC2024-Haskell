module Main (main) where

import Text.Regex.TDFA
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let products = getProducts contents
    let totalSum = sum products
    putStrLn $ "Total sum: " ++ show totalSum

getProducts :: String -> [Integer]
getProducts str =
    let regex = "mul\\(([0-9]+),([0-9]+)\\)"
        matches = str =~ regex :: [[String]]
    in mapMaybe parseMul matches

parseMul :: [String] -> Maybe Integer
parseMul [_, xStr, yStr] = do
    x <- readMaybe xStr :: Maybe Integer
    y <- readMaybe yStr :: Maybe Integer
    return (x * y)
parseMul _ = Nothing
