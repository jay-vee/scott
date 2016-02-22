import Text.Regex.Posix

compress :: [String] -> [String]
compress xs = compressR [] xs
              where
	      compressR :: [Char] -> [String] -> [String]
              compressR [] [] = [];
              compressR temp [] = [temp];
              compressR [] (x:xs)
	                              | x =~ "[A-Za-z]+" = compressR x xs
				      | otherwise = x : (compressR [] xs)
	      compressR temp (x:xs)
	                              | x =~ "[A-Za-z]+" = compressR (temp ++ x) xs
				      | otherwise = x : temp : (compressR [] xs)
	
