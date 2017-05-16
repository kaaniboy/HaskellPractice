import Control.Monad

import System.IO
import Data.Char
import System.Random

capitalize :: String -> String
capitalize = map (\c -> toUpper c)

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' g = val:(randoms' gen)
	where (val, gen) = random g


main = do
 	handle <- openFile "example.txt" ReadMode
	content <- hGetContents handle
	putStr . capitalize $ content
	hClose handle