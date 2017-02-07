import Control.Applicative
import Data.List as List
import Data.Text
import System.Random

main :: IO ()
main = do
  pairs <- splitEntries . packLines <$> readFile "lang/de-en.txt"
  randomGen <- getStdGen
  let listLength = List.length pairs
  let num = randomRs (0 :: Int, listLength - 1) randomGen 
  let quizIndecies =  List.take quizLength $ List.nub $ List.take (listLength * 3)  num
  mapM_ quiz $ List.map (pairs !!) quizIndecies

quizLength :: Int
quizLength = 10 

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
packLines = List.map pack . removeEmpty . List.lines


removeEmpty :: [String] -> [String]
removeEmpty = List.filter (not . List.null)

splitEntries :: [Text] -> [[Text]]
splitEntries = List.map (splitOn (pack "|"))
