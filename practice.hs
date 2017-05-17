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

palindrome' :: (Eq a) => [a] -> Bool
palindrome' xs
  | (length xs == 0) = True
  | (length xs == 1) = True
  | otherwise = head xs == last xs && (palindrome' $ tail $ init xs)
  
-- Question #7

compress' :: String -> String -> String
compress' [] prev = []
compress' (x:xs) prev
  | (null prev) = x:(compress' xs [x])
  | (prev == [x]) = compress' xs [x]
  | otherwise = x:(compress' xs [x])

-- Using dropWhile to simplify the function.
compress2' :: String -> String
compress2' "" = ""
compress2' (x:xs) = [x] ++ compress2' (dropWhile (== x) xs)

-- Just wanted to try writing dropWhile on my own.
dropWhile' :: (Char -> Bool) -> String -> String
dropWhile' f "" = ""
dropWhile' f list@(x:xs)
  | (f x) = dropWhile' f xs
  | otherwise = list


runLength' :: String -> [(Char, Int)]
runLength' "" = []
runLength' list@(x:_) = [(x, length (takeWhile (== x) list))] ++ runLength' (dropWhile (== x) list)

take' :: Int -> [b] -> [b]
take' 0 xs = []
take' n (x:xs) = x:(take' (n - 1) xs) 

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (a:as) (b:bs) = (a, b):zip' as bs

elem' :: (Eq a) => a -> [a] -> Bool
elem' n [] = False
elem' n (x:xs)
  | (n == x) = True
  | otherwise = elem' n xs

quicksort :: (Ord a) => [a] => [a]
quicksort [] = []
quicksort (x:xs) =
  let smaller = quicksort [a | a <- xs, a <= x]
      larger = quicksort [a | a <- xs, a > x]
  in smaller ++ [x] ++ larger


foldMap :: (a -> b) -> [a] -> [b]
foldMap f = foldr (\x acc -> f x:acc) []

dupli :: [a] -> [a]
dupli = foldr (\x acc -> [x, x] ++ acc) []

dupli' :: [a] -> [a]
dupli' xs = concat [[x, x] | x <- xs]

repli :: [a] -> Int -> [a]
repli xs n = concat [take n $ repeat x | x <- xs]

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate xs n = rotate (tail xs ++ [head xs]) (n - 1)

remove_at :: [a] -> Int -> [a]
remove_at [] k = []
remove_at (x:xs) k
  | (k == 0) = xs
  | otherwise = x:remove_at xs (k - 1)

insert_at :: a -> [a] -> Int -> [a]
insert_at e (x:xs) n
  | (n == 1) = x:(e:xs)
  | otherwise = x:insert_at e xs (n - 1)

insert_at' :: a -> [a] -> Int -> [a]
insert_at' e xs n
  | (n >= length xs) = error "Provided index is too large."
  | otherwise = let start = fst $ splitAt n xs
                    end   = snd $ splitAt n xs
                    in start ++ [e] ++ end