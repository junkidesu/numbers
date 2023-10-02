module Main (main) where

import MyNatural (fromInt)

main :: IO ()
main = do
  x <- readLn :: IO Int
  case fromInt x of
    Nothing ->
      do
        print "Not a natural number!"
    Just n  ->
      do
        print $ "The natural number is "++show n
