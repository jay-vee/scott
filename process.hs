import Text.CSV
import Data.Either
import qualified Data.Text as T
import System.Environment
import System.IO

cleanCSV :: [String] -> [String]
cleanCSV thelist = filter (\x -> not $ null x && x /= "0" && x /= "N") $ map (T.unpack . T.strip . T.pack) thelist

{-

main :: IO()
main = do
  args <- getArgs
  csv <- parseCSVFromFile $ head args
  let actualcsv = head $ rights [csv]
  let flatcsv = concat actualcsv
  let cleancsv = 

-}
