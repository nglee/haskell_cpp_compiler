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
