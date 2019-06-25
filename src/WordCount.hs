module WordCount
  ( WordCount
  , fromFile
  , probability
  )
where

import qualified Data.Map                      as Map
import           Data.List.Split                ( wordsBy )
import           System.IO
import           Data.Maybe
import           Data.Char

type WordCount = Map.Map String Int

words' :: String -> [String]
words' = wordsBy (not . isLetter)

lower :: String -> String
lower = map toLower

countWords :: String -> WordCount
countWords = foldl go Map.empty . words'
 where
  go count word =
    let loweredWord = lower word
    in  Map.insert loweredWord (freq count loweredWord + 1) count

freq :: WordCount -> String -> Int
freq c word = fromMaybe 0 $ Map.lookup word c

probability :: Fractional a => WordCount -> String -> a
probability c word = f / n
 where
  n = fromIntegral $ sum $ Map.elems c
  f = fromIntegral $ freq c word

fromFile :: FilePath -> IO WordCount
fromFile path = countWords <$> readFile path
