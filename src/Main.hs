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
import           Control.Monad.Reader

type WordCount = Map.Map String Int

rightNotNull = filter ((/=) "" . snd)

withLetters f = concatMap (\split' -> map (f split') ['a' .. 'z'])

words' = wordsBy (not . isLetter)

split :: Int -> String -> [(String, String)]
split n word
  | n <= length word = (take n word, drop n word) : split (n + 1) word
  | otherwise        = []

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

countOne :: WordCount -> String -> WordCount
countOne counts word =
  let word' = lower word
  in  case Map.lookup word' counts of
        Just c  -> Map.insert word' (c + 1) counts
        Nothing -> Map.insert word' 1 counts

wordCount :: String -> WordCount
wordCount = foldl countOne Map.empty . words'

freq :: WordCount -> String -> Int
freq count word = fromMaybe 0 $ Map.lookup word count

probability :: Fractional a => WordCount -> String -> a
probability count word =
  let n = fromIntegral $ sum $ Map.elems count
      f = fromIntegral $ freq count word
  in  f / n

known :: [String] -> Reader WordCount [String]
known words = do
  count <- ask
  return $ filter (`Map.member` count) words

candidates :: String -> Reader WordCount [String]
candidates word = do
  a <- known [word]
  b <- known (Set.toList $ edits word)
  case a of
    [] -> return b
    _  -> return a

correction :: String -> Reader WordCount String
correction word = do
  count <- ask
  c     <- candidates word
  return $ head $ List.sortOn (negate . probability count) c

main :: IO ()
main = do
  handle   <- openFile "words.txt" ReadMode
  contents <- hGetContents handle
  input    <- getLine
  putStr $ show $ map (\w -> runReader (correction w) (wordCount contents))
                      (words' input)
  hClose handle
