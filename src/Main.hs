module Main where

import qualified Data.Set                      as Set
import           Data.Char
import           Data.Maybe
import qualified Data.Map                      as Map
import qualified Debug.Trace                   as Debug
import qualified Data.List                     as List
import           System.IO
import           Data.List.Split                ( wordsBy )
import           Control.Monad

type WordCount = Map.Map String Int

split :: Int -> String -> [(String, String)]
split n word
  | n <= length word = (take n word, drop n word) : split (n + 1) word
  | otherwise        = []

rightNotNull = filter ((/=) "" . snd)

withLetters f = concatMap (\split' -> map (f split') ['a' .. 'z'])

deletes :: [(String, String)] -> [String]
deletes = map (\(l, _ : r) -> l ++ r) . rightNotNull

transposes :: [(String, String)] -> [String]
transposes =
  map (\(l, a : b : rest) -> l ++ b : a : rest) . filter ((<) 1 . length . snd)

replaces :: [(String, String)] -> [String]
replaces = withLetters (\(l, _ : rest) c -> l ++ c : rest) . rightNotNull

inserts :: [(String, String)] -> [String]
inserts = withLetters (\(l, r) c -> l ++ c : r)

edits :: String -> Set.Set String
edits word =
  let splits = split 0 word
  in  Set.fromList
        $  deletes splits
        ++ transposes splits
        ++ replaces splits
        ++ inserts splits

lower :: String -> String
lower = map toLower

count :: Map.Map String Int -> String -> Map.Map String Int
count counts word =
  let word' = lower word
  in  case Map.lookup word' counts of
        Just c  -> Map.insert word' (c + 1) counts
        Nothing -> Map.insert word' 1 counts

wordCount :: String -> WordCount
wordCount = foldl count Map.empty . wordsBy (not . isLetter)

freq :: String -> Map.Map String Int -> Int
freq word counts = fromMaybe 0 $ Map.lookup word counts

probability :: Fractional a => WordCount -> String -> a
probability counts word =
  let n = fromIntegral $ sum $ Map.elems counts
      f = fromIntegral $ freq word counts
  in  f / n

known :: WordCount -> [String] -> [String]
known counts = filter (`Map.member` counts)

candidates :: WordCount -> String -> [String]
candidates counts word =
  known counts [word] ++ known counts (Set.toList $ edits word)

correction :: WordCount -> String -> String
correction counts =
  head . List.sortOn (negate . probability counts) . candidates counts

correct c =
  putStr . show . correction c

main :: IO ()
main = do
  handle <- openFile "words.txt" ReadMode
  contents <- hGetContents handle
  let counts = wordCount contents
  input <- getLine
  correct counts input
  hClose handle
