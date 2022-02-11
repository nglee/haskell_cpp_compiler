-- https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems

-- Problem 1
myLast :: [a] -> a
myLast [] = error "list is empty"
myLast [x] = x
myLast (_:xs) = myLast xs

-- Problem 2
myButLast :: [a] -> a
myButLast [] = error "list is empty"
myButLast [_] = error "list has only one element"
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

-- Problem 3
elementAt :: [a] -> Int-> a
elementAt [] count = error "list has fewer elements than requested"
elementAt (x:_) 1 = x
elementAt (_:xs) count
    | count < 1 = error "index out of bound"
    | otherwise = elementAt xs (count - 1)

-- Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength [x] = 1
myLength (_:xs) = 1 + myLength xs

-- Problem 5
myReverse :: [a] -> [a]
myReverse = myReverseImpl []
    where
        myReverseImpl reversed []     = reversed
        myReverseImpl reversed (x:xs) = myReverseImpl (x:reversed) xs

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs) = x == last xs && isPalindrome (init xs)

-- Problem 7

-- Problem 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = x : compressImpl x xs
    where
        compressImpl :: (Eq a) => a -> [a] -> [a] -- the last puhsed value, remaining input list
        compressImpl x [] = []
        compressImpl x xs
            | x == head xs = compressImpl x (tail xs)
            | otherwise = head xs : compressImpl (head xs) (tail xs)

-- above solution added an element whenever a new element is met
-- the following solution delays adding elements until the next element is a different element

compress' :: (Eq a) => [a] -> [a]
compress' [] = []
compress' [x] = [x]
compress' (x:xs)
    | x == head xs = compress' xs
    | otherwise = x : compress' xs

-- Problem 9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = packImpl [[x]] xs
    where
        packImpl :: (Eq a) => [[a]] -> [a] -> [[a]]
        packImpl packed [] = packed
        packImpl packed (x:xs)
            | last (last packed) == x = packImpl (init packed ++ [last packed ++ [x]]) xs
            | otherwise = packed ++ packImpl [[x]] xs

pack' :: (Eq a) => [a] -> [[a]]
pack' [] = []
pack' [x] = [[x]]
pack' (x:xs) = packImpl [x] xs
    where
        packImpl :: (Eq a) => [a] -> [a] -> [[a]]
        packImpl packed [] = [packed]
        packImpl packed (x:xs)
            | head packed == x = packImpl (packed ++ [x]) xs
            | otherwise = packed : packImpl [x] xs

-- Problem 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) (pack xs)