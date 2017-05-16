calculate :: String -> [Float]
calculate str = foldl . func [] $ (words str)
	where func (x:y:xs) "+" = (x + y):xs
	      func (x:y:xs) "-" = (x - y):xs
	      func (x:y:xs) "*" = (x * y):xs
	      func (x:y:xs) "/" = (x / y):xs
	      func xs num = (read num):xs