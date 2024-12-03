module Main (main) where

import System.IO
import Text.Regex.TDFA
import Data.List (stripPrefix)

data Instruction = Do | Dont | Mul Integer Integer

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let totalSum = processInput contents True 0
    putStrLn $ "Total sum: " ++ show totalSum

processInput :: String -> Bool -> Integer -> Integer
processInput [] _ sumSoFar = sumSoFar
processInput str enabled sumSoFar =
    case matchInstruction str of
        Just (instr, rest) ->
            case instr of
                Do      -> processInput rest True sumSoFar
                Dont    -> processInput rest False sumSoFar
                Mul x y ->
                    if enabled
                        then processInput rest enabled (sumSoFar + x * y)
                        else processInput rest enabled sumSoFar
        Nothing ->
            processInput (tail str) enabled sumSoFar
matchInstruction :: String -> Maybe (Instruction, String)
matchInstruction str
    | Just rest <- stripPrefix "do()" str    = Just (Do, rest)
    | Just rest <- stripPrefix "don't()" str = Just (Dont, rest)
    | otherwise =
        let pattern = "^mul\\(([0-9]+),([0-9]+)\\)"
            matchResult = str =~ pattern :: (String, String, String, [String])
        in case matchResult of
            (_, matched, rest, [xStr, yStr]) | not (null matched) ->
                let x = read xStr :: Integer
                    y = read yStr :: Integer
                in Just (Mul x y, rest)
            _ ->
                Nothing
