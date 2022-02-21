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
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

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

-- Problem 11
data EncodedStr a = Multiple Int a | Single a
    deriving (Show)
encodeModified :: (Eq a) => [a] -> [EncodedStr a]
encodeModified [] = []
encodeModified (x:xs) = impl x xs 1
    where
        impl :: (Eq a) => a -> [a] -> Int -> [EncodedStr a]
        impl x [] n
            | n == 1 = [Single x]
            | otherwise = [Multiple n x]
        impl x xs n
            | x == head xs = impl x (tail xs) (n+1)
            | otherwise = impl x [] n ++ impl (head xs) (tail xs) 1

-- use result of Problem 10
encodeModified' :: Eq a => [a] -> [EncodedStr a]
encodeModified' xs = map impl (encode xs)
    where
        impl :: (Int, a) -> EncodedStr a
        impl (1, x) = Single x
        impl (n, x) = Multiple n x

-- Problem 12
decodeModified :: [EncodedStr a] -> [a]
decodeModified = foldr impl []
    where
        impl :: EncodedStr a -> [a] -> [a]
        impl (Multiple n x) xs = replicate n x ++ xs
        impl (Single x) xs = x : xs

-- Problem 13
encodeDirect :: Eq a => [a] -> [EncodedStr a]
encodeDirect [] = []
encodeDirect (x:xs) = impl x 1 xs
    where
        impl :: Eq a => a -> Int -> [a] -> [EncodedStr a]
        impl x n []
            | n == 1 = [Single x]
            | otherwise = [Multiple n x]
        impl x n xs
            | x == head xs = impl x (n+1) (tail xs)
            | otherwise = impl x n [] ++ impl (head xs) 1 (tail xs)

-- Problem 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

dupli' :: [a] -> [a]
dupli' = foldr (\x xs -> x : x : xs) []

dupli'' :: [a] -> [a]
dupli'' = foldl (\acc x -> acc ++ [x, x]) []

-- Problem 15
repli :: [a] -> Int -> [a]
repli xs n
    | n > 0 = repliImpl xs n
    | otherwise = []
        where
            repliImpl [] _ = []
            repliImpl (x:xs) 1 = x : repliImpl xs n
            repliImpl (x:xs) k = x : repliImpl (x:xs) (k-1)

repli' :: [a] -> Int -> [a]
repli' list n = foldr (\x xs -> replicate n x ++ xs) [] list

repli'' :: [a] -> Int -> [a]
repli'' list n = foldl (\acc x -> acc ++ replicate n x) [] list

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n
    | n > 0 = dropImpl xs n
    | otherwise = []
        where
            dropImpl [] _ = []
            dropImpl (x:xs) 1 = dropImpl xs n
            dropImpl (x:xs) k = x : dropImpl xs (k-1)