-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl f acc [] = acc
-- foldl f acc (x:xs) =  foldl f (f acc x) xs

myFoldr :: (a-> b-> b) -> b -> [a] -> b
myFoldr f z [] = z
myFoldr f z (x:xs) = f x (foldr f z xs)

 -- 1 : 2 : 3 : []
 -- right associativity (f 1 (f 2 (f 3 z)))
 -- left associativity (f (f (f z 1) 2) 3)


--  1 ^ (2 ^ (3 ^ 2))
--  (((2 ^ 1)^2)^3)

-- myReverse = foldl (flip (:)) []

-- can you calculate fibonacci as a fold?


-- fib n = take n $ foldr (\x y -> x + y) 


-- foldl (flip (*)) 1 [1..3]
-- 1 + 2 + 3 ((1 + 2) + 3)
-- (((1 * 1) * 2) * 3)
-- (3 * ( 2 * (1 * 1)))


-- all of the successive steps of a fold exist as the first argument

-- a left fold has to go through the entire spine unconditionally before it evaluates.
-- the same isn't true for a right spine which can evaluate as it recurses.


-- fibs = takeWhile (<100) $ 1 : scanl (+) 1 fibs
-- fact 0 = []
-- fact n = n : scanl (*) fact (n - 1)


-- scanl :: (a -> b -> a) -> a -> [b] -> [a]
-- scanl f q ls = q : (case ls of  
--     [] -> []
--     x:xs -> scanl f (f q x) xs)

-- f applied to the base case and the front of the list 
-- becomes the base case for the next scanl call


-- fibs = 1 : scanl (+) 1 fibs

-- factorial =  scanl 


stops  = "pbtdkg"
vowels = "aeiou"

combos s v = [x:y:z:[] | x <- s, y <- v, z <- s]
combosOnLetter s v c = filter ( \x -> (==c) $ head x) $ combos s v

verbs = ["fucks", "eats", "hates"]
subject = ["Rechine", "Robert", "Weird Software Guy"]
senCombos s v = map (unwords) $ combos v s

-- Avg letter length of each word
seekritFunc x = (fromIntegral . sum . (map length) . words) x / (fromIntegral . length . words) x



myOr :: [Bool] -> Bool
myOr = foldr (||) False


myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x y -> (f x) || y) False


myElem :: Eq a => a -> [a] -> Bool
myElem a = foldr (\x y -> x == a || y) False 

myElem2 a = myAny (==a)


myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []


myMap :: (a -> b) -> [a] -> [b]
myMap = \f -> foldr (\x y -> f x : y) []

myFilter :: (a-> Bool) -> [a] -> [a]
myFilter = \f -> foldr (\x y -> if f x then x : y else y) []


squish :: [[a]] -> [a]
squish = foldr (\x y -> x ++ y) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x y -> (f x) ++ y) [] 

squishAgain :: [[a]] -> [a]
squishAgain = squishMap (\x -> x)

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldr (\x y -> if (f x y) == GT then x else y) x xs


myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = myMaximumBy (flip f)

