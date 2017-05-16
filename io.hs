import Control.Monad

import System.IO
import Data.Char

capitalize :: String -> String
capitalize = map (\c -> toUpper c)

main = do
	handle <- openFile "example.txt" ReadMode
	content <- hGetContents handle
	putStr . capitalize $ content
	hClose handle