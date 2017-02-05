import Control.Applicative
import Data.List as List
import Data.Text


main :: IO ()
main = do
  pairs <- splitEntries . packLines <$> readFile "lang/de-en.txt"
  mapM_ quiz pairs


quiz :: [Text] -> IO ()
quiz (foreignTerm:englishTerm:_) =
  askEnToForeign englishTerm >> pack <$> getLine >>= confirmAnswer foreignTerm


confirmAnswer :: Text -> Text -> IO ()
confirmAnswer foreignTerm userAnswer
  | anwerIsCorrect == True = putStrLn "\tCorrect!"
  | anwerIsCorrect == False = putStrLn $ "\tWrong!  The answer is: " ++ (unpack foreignTerm)
  where anwerIsCorrect = userAnswer == foreignTerm


askEnToForeign :: Text -> IO ()
askEnToForeign term = putStrLn $ "\nHow do you say: \"" ++ (unpack term) ++ "\"?"


printSecond :: [Text] -> IO ()
printSecond (x1:x2:_) = putStrLn . unpack $ x2


packLines :: String -> [Text]
packLines = List.map pack . List.lines


splitEntries :: [Text] -> [[Text]]
splitEntries = List.map (splitOn (pack "|"))
