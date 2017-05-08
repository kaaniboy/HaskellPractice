-- Question #1
last' :: [a] -> a
last' [x] = x
last' (_:xs) = last xs
last' [] = error "Empty lists have no last element."

-- Question #2
lastButOne' :: [a] -> a
lastButOne' [x] = error "Lists with one element do not have a last but one."
lastButOne' [] = error "Empty lists do not have a last but one."
lastButOne' [x, _] = x
lastButOne' (_:xs) = lastButOne' xs

-- Question #3
elementAt' :: [a] -> Int -> a
elementAt' xs k
  | (k <= length xs) = xs !! (k - 1)
  | otherwise = error "Index out of bounds."
  
-- Question #4
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

-- Question #5

reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

-- Question #6
-- $ can be thought of as 'of,' as in f(g(x)) .

palindrome' xs
  | (length xs == 0) = True
  | (length xs == 1) = True
  | otherwise = head xs == last xs && palindrome' $ tail $ init xs)
