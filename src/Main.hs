module Main where

import qualified Data.Set as Set
import Data.Char
import qualified Data.Map as Map
import qualified Debug.Trace as Debug
import qualified Data.List as List
import System.IO
import Data.List.Split (wordsBy)

letters = ['a'..'z']

known word =
  word `elem` [ "something"
              , "soothing"
              ]

split n word
  | n <= length word = (take n word, drop n word):split (n+1) word
  | otherwise = []


filterNullR =
  filter ((/=) "" . snd)


deletes =
  map (\(l, _:r) -> l ++ r) . filterNullR

transposes =
  map (\(l, a:b:rest) -> l ++ b:a:rest) . filter ((<) 1 . length . snd)


replaces =
  concatMap (\(l, _:rest) -> map (\c -> l ++ c:rest) letters)
    . filterNullR

inserts :: [(String, String)] -> [String]
inserts =
  concatMap (\(l, r) -> map (\c -> l ++ c:r) letters)

edits word =
  let splits = split 0 word
  in
  Set.fromList $
    deletes splits
    ++ transposes splits
    ++ replaces splits
    ++ inserts splits

toLowerString :: String -> String
toLowerString = map toLower

count :: Map.Map String Int -> String -> Map.Map String Int
count counts word =
  let loweredWord = toLowerString word
  in
  case Map.lookup loweredWord counts of
    Just c -> Map.insert loweredWord (c + 1) counts
    Nothing -> Map.insert loweredWord 1 counts

wordCount =
  foldl count Map.empty

mostCommon =
  take 10
    . List.sortOn (negate . snd)
    . Map.toList

main :: IO ()
main = do
  handle <- openFile "words.txt" ReadMode
  contents <- hGetContents handle
  putStr $ show $ mostCommon $ wordCount $ wordsBy (not . isLetter) contents
  hClose handle
