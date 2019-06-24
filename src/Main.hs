module Main where

import           WordCount                      ( WordCount
                                                , fromFile
                                                , probability
                                                )
import           System.IO
import           Data.List.Split                ( wordsBy )
import           Control.Monad
import           Control.Monad.Reader

import qualified Data.Set                      as Set
import qualified Data.Map                      as Map
import qualified Debug.Trace                   as Debug
import qualified Data.List                     as List

split :: Int -> String -> [(String, String)]
split n word
  | n <= length word = (take n word, drop n word) : split (n + 1) word
  | otherwise        = []

notNull = (/=) ""

edits1 :: String -> Set.Set String
edits1 word = Set.fromList $ deletes ++ transposes ++ replaces ++ inserts
 where
  letters = ['a' .. 'z']
  withLetters f = concatMap (\split' -> map (f split') letters)
  splits     = split 0 word
  deletes    = [ l ++ r | (l, r) <- splits, notNull r ]
  transposes = [ l ++ b : a : rs | (l, a : b : rs@r) <- splits, length r > 1 ]
  replaces   = withLetters (\(l, _ : rs) letter -> l ++ letter : rs)
    $ filter (notNull . snd) splits
  inserts = withLetters (\(l, r) letter -> l ++ letter : r) splits

edits2 :: String -> Set.Set String
edits2 = Set.fromList . concatMap (Set.toList . edits1) . edits1

known :: [String] -> Reader WordCount [String]
known words = do
  count <- ask
  pure $ filter (`Map.member` count) words

pickOne :: [[a]] -> [a]
pickOne []           = []
pickOne ([]  : tail) = pickOne tail
pickOne (one : _   ) = one

candidates :: String -> Reader WordCount [String]
candidates word = do
  set0 <- known [word]
  set1 <- known (Set.toList $ edits1 word)
  set2 <- known (Set.toList $ edits2 word)
  pure $ pickOne [set0, set1, set2, [word]]

correction :: String -> Reader WordCount String
correction word = do
  count <- ask
  c     <- candidates word
  pure $ head $ List.sortOn (negate . WordCount.probability count) c

main :: IO ()
main = do
  count <- WordCount.fromFile "words.txt"
  input <- getLine
  putStr $ show $ map (\w -> runReader (correction w) count) (words input)
