import Control.Applicative
import Data.List as List
import Data.Text


main :: IO ()
main = do
  pairs <- splitEntries . List.map pack  . List.lines <$> readFile "lang/de-en.txt"
  mapM_ print pairs 

splitEntries = List.map (splitOn (pack "|"))
