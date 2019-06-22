module Main where

import qualified Data.Set as Set

words =
    [ "something"
    , "soothing"
    ]


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
  let
      splits = split 0 word
  in
  Set.fromList $
    deletes splits
    ++ transposes splits
    ++ replaces splits
    ++ inserts splits


main :: IO ()
main = do
  putStrLn $ show (Set.filter known $ edits "somthing")
