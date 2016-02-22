import Data.List
import Data.List.Split
import Data.String.Utils
import Data.Either
import Text.CSV
import Text.Regex.Posix
import System.Environment
import System.IO
import qualified Data.Text as Text

stripRow :: [String] -> [String]
stripRow = map (Text.unpack . Text.strip . Text.pack)

removeEmptyCells = filter (\x -> not $ null x) 

fixColons :: [String] -> [String]
fixColons = map (replace ":" "_")

multiplyRow :: [String] -> [[String]]
multiplyRow [] = [[]]
multiplyRow xs
	    | isMultiplier = replicate (read n :: Int) (init xs) 
	    | otherwise = [xs] 
            where
	    x = last xs
	    pattern = "x([0-9])+"
	    isMultiplier = x =~ pattern :: Bool
	    (_,_,_,[n]) = x =~ pattern :: (String, String, String, [String])

replaceDots :: String -> String
replaceDots xs = join " " (snd $ mapAccumL copyIntoDot "" (splitOn " " xs))
            where
	    copyIntoDot :: String -> String -> (String,String)
	    copyIntoDot prev curr = if curr == "." then (prev,prev) else (curr,curr)

replaceDotsRow :: [String] -> [String]
replaceDotsRow = map replaceDots

splitMeasureRow :: [String] -> [String]
splitMeasureRow xs = concat (map (\x -> if isTitle x then [x] else splitOn " " x) xs)

isTitle :: String -> Bool
isTitle xs = xs =~ "^[A-Za-z]+"

isValid :: String -> Bool
isValid xs = not (or [xs =~ "[(][0-9]", xs `elem` ["*","->","->, fadeout","0","N"]])

removeInvalid :: [String] -> [String]
removeInvalid = filter isValid

clean :: [[String]] -> [[String]]
clean = map (removeInvalid . splitMeasureRow . replaceDotsRow . fixColons . removeEmptyCells . stripRow) 

cleanAndMultiply = removeEmptyCells . concat . (map multiplyRow) . clean

main = do
         args <- getArgs
         csv <- parseCSVFromFile (args !! 0)
         let csv' = cleanAndMultiply $ head $ rights [csv]
         writeFile (args !! 1) (printCSV csv')
