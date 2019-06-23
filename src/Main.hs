module Main where

import qualified Data.Set                      as Set
import           Data.Char
import           Data.Maybe
import qualified Data.Map                      as Map
import qualified Debug.Trace                   as Debug
import qualified Data.List                     as List
import           System.IO
import           Data.List.Split                ( wordsBy )
import Control.Monad

letters = ['a' .. 'z']

known counts = filter (`Map.member` counts)

split n word
  | n <= length word = (take n word, drop n word) : split (n + 1) word
  | otherwise        = []


filterNullR = filter ((/=) "" . snd)

deletes = map (\(l, _ : r) -> l ++ r) . filterNullR

transposes =
  map (\(l, a : b : rest) -> l ++ b : a : rest) . filter ((<) 1 . length . snd)

replaces =
  concatMap (\(l, _ : rest) -> map (\c -> l ++ c : rest) letters) . filterNullR

inserts :: [(String, String)] -> [String]
inserts = concatMap (\(l, r) -> map (\c -> l ++ c : r) letters)

edits word =
  let splits = split 0 word
  in  Set.fromList
        $  deletes splits
        ++ transposes splits
        ++ replaces splits
        ++ inserts splits

toLowerString :: String -> String
toLowerString = map toLower

count :: Map.Map String Int -> String -> Map.Map String Int
count counts word =
  let loweredWord = toLowerString word
  in  case Map.lookup loweredWord counts of
        Just c  -> Map.insert loweredWord (c + 1) counts
        Nothing -> Map.insert loweredWord 1 counts

wordCount = foldl count Map.empty

mostCommon = take 10 . List.sortOn (negate . snd) . Map.toList

freq :: String -> Map.Map String Int -> Int
freq word counts =
  fromMaybe 0 $ Map.lookup word counts

probability counts word =
  let n = fromIntegral $ sum $ Map.elems counts
      f = fromIntegral $ freq word counts
  in f / n

candidates counts word =
  known counts [word] ++ known counts (Set.toList $ edits word)

correction counts =
  head . List.sortOn (negate . probability counts) . candidates counts

main :: IO ()
main =
  withFile "words.txt" ReadMode (\handle -> do
  contents <- hGetContents handle
  input <- getLine
  let counts = wordCount $ wordsBy (not . isLetter) contents
  putStr $ show $ correction counts input)
