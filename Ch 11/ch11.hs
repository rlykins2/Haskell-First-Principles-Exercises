{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
import Data.Int
import Data.Char
-- data Bool = False | True sum type, in the sense that there's a choice?
-- data [] a = [] | a : [a] This a recursive type?


-- Product type is a type with more than one parameter
-- constants don't take parameters, constructors do
-- constructors must be applied to values, in order to become 
-- a concrete type or value
-- concrete type constructor only exists when all of its parameters are applied


data Trivial = Trivial'
data UnaryTypeCon a = UnaryValueCon a
-- POSSIBLE QUESTIONS PLEASE LOOK
    -- constant = constructor w/ no params = nullary constructor
    -- To get a concrete type from a type constructor we apply TYPES until its kind equals *
    -- To get a concrete value from a data constructor we apply VALUES until no longer accepts values.
    -- type is an enumeration of constructors with 0+ arguments
    -- data constructors with >1 arguments = products

data Doggies a = 
    Husky a | Mastiff a deriving (Eq, Show)
-- 1. A type constructor
-- 2. * -> *
-- 3. Doggies String :: *
-- 4. Husky 10  :: Num a => Doggies a FUCK the type always refers to the type constructor
-- 5. Husky (10 :: Integer)   Doggies Integer
-- 6. Mastiff "Scooby Doo" Doggies String
-- 7. Both
-- 8. DogueDeBordeaux's type is a -> DogueDeBordeaux a
-- 9. DogueDeBordeaux "doggie!" DogueDeBordeaux String


data Price = Price Integer deriving (Eq, Show)
data Size = Size Integer deriving (Eq, Show)
-- Num is a type constructor that can become other type constructors??


data Manufacturer = Mini | Mazda | Tata deriving (Eq,Show)
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq,Show)
data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Eq, Show)


myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 5500)

isCar :: Vehicle -> Bool
isCar x = 
    case x of  
        (Car _ _) -> True
        _ -> False
isPlane :: Vehicle -> Bool
isPlane x =
    case x of 
        (Plane _ _) -> True
        _ -> False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu x = 
    case x of
        (Car x _) -> x
        _ -> undefined
-- when the argument structures of a type can be described as sums and products, we have 
-- an algebraic datatype
-- A recursive datatype has what kind of cardinality? infinite
-- any type with a type parameter is it infinite?? 
-- What is the cardinality of Integer? it's infinite
-- How do we figure out how unary constructor contributes to type's cardinality? 
   -- It adds the cardinality of the type it contains.
-- What kind of constructor do we use newtype for?
    -- single unary data constructor
-- A typeclass probably isn't a concrete type 
    -- The cardinality of a data type affects how hard it is to reason about something


class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42
-- Exercises : Cardinality
-- 1. data PugType = PugData 1
-- 2. 3
-- 3. 

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

-- instance TooMany Goats where
--     tooMany (Goats n) = tooMany n
-- What do we do if we want to utilize typeclasses defined on a type
    -- that our newtype contains? (This isn't automatically given)
-- What does GeneralizedNewTypeDeriving do?
    -- newtypes can derive typeclass definitions from the types they are
newtype Fluh = F (Int, String)

instance TooMany Fluh where
    tooMany (F (f,g)) = tooMany f || (length g) > 42


newtype Gluh = G (Int, Int)

instance TooMany Gluh where
    tooMany (G (f,g)) = tooMany (f + g) 



-- instance TooMany ((Num a, TooMany a) => (a, a)) where
--     tooMany (f,g) = tooMany (f :: Int) || tooMany g

-- data BigSmall = Big Bool | Small Bool deriving (Eq, Show)
-- 4 


-- 258 because 256 from Int8 and 2 from Bool 


data NumberOrBool = Numba Int8 | BoolyBool Bool deriving (Eq, Show)
-- Is the data constructor a product type or is the type the data constructor that's a product a product type??
-- data Person = MkPerson String Int deriving (Eq, Show)


data Person = Person { name :: String, age :: Int} deriving (Eq, Show)


-- data Fiction = Fiction deriving Show
-- data Nonfiction = Nonfiction deriving Show


-- data BookType = FictionBook Fiction | NonfictionBook Nonfiction deriving Show

type AuthorName = String

data Author = Fiction AuthorName | Nonfiction AuthorName

-- We know we're in normal form when we have a sum of products.
-- data Expr = Number Int | Add Expr Expr | Minus Expr | Mult Expr Expr | Divide Expr Expr

-- type Number = Int
-- type Add = (Expr, Expr)
-- type Minus = Expr
-- type Mult = (Expr, Expr)
-- type Divide = (Expr, Expr)


-- type Expr = Either Number (Either Add (Either Minus (Either Mult Divide)))


type Gardener = String
data Garden =  Gardenia Gardener | Daisy Gardener | Rose Gardener | Lilac Gardener deriving Show

-- value can be generated/constructed or matched/consumed

data GuessWhat = Chickenbutt deriving (Eq, Show)
data Id a = MkId a deriving (Eq, Show)
data Product a b = Product a b deriving (Eq, Show)
data Sum a b = First a | Second b deriving (Eq, Show)
data RecordProduct a b = RecordProduct { pfirst :: a , psecond :: b} deriving (Eq, Show)


newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)
newtype NumSheep = NumSheep Int deriving (Eq, Show)
-- data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)

-- type Farmhouse = Product NumCow NumPig
-- type BigFarmhouse =  Product Farmhouse NumSheep

type Awesome = Bool
type Name = String

person :: Product Name Awesome
person = Product "Simon" True

-- Does Either = Sum?
-- type Twitter = String
-- type AskFm = String



data OperatingSystem = 
    GnuPlusLinux
    | OpenBSD
    | Mac
    | Windows
    deriving (Eq, Show)

data ProgLang = 
    Haskell 
    | Agda
    | Idris
    | PureScript
    deriving (Eq, Show)

data Programmer = Programmer { os :: OperatingSystem, lang :: ProgLang}
    deriving (Eq, Show)


allOperatingSystems = 
    [
        GnuPlusLinux,
        OpenBSD,
        Mac,
        Windows
    ]
allLanguages  =
    [
        Haskell,
        Agda,
        Idris,
        PureScript
    ]

allProgrammers :: [Programmer]
allProgrammers =  [Programmer {lang=x, os=y} | x <- allLanguages, y <- allOperatingSystems]


data ThereYet = 
    There Float Int Bool
    deriving (Eq, Show)

notYet = There 25.5
notQuite = notYet 10
yussss = notQuite False

-- 1. 8 sumtype means 4 + 4
-- 2. 16 prod type means 4 * 4
-- 3. 256 func quad, 4 elements in domain can each map to 4 elements in range 4^4 = 256
-- 4. 2*2*2 ^ 2^3
-- 5. 2^2^2 = 2^(2*2) = 16 (Bool -> Bool) -> Bool 
-- 6. 2^4^4 = 2^(256)



data BinaryTree a =
    Leaf | Node (BinaryTree a) a (BinaryTree a)
        deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right 
    | b < a = Node (insert' b left) a right 
    | b > a = Node left a (insert' b right)

tMap :: (a -> b) -> BinaryTree a -> BinaryTree b

tMap f Leaf = Leaf
tMap f (Node l a r) = Node (tMap f l) (f a) (tMap f r)



preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node l a r) = concat [[a], preorder l, preorder r]

postorder Leaf = []
postorder (Node l a r) = concat [postorder l , postorder r, [a]]

inorder Leaf = []
inorder (Node l a r) = concat [inorder l, [a], inorder r]

testTree :: BinaryTree Integer
testTree = 
    Node (Node Leaf 1 Leaf)
        2
        (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder = 
    if preorder testTree == [2,1,3]
        then putStrLn "Preorder fine!"
        else putStrLn "Bad news billy bears."

testInorder :: IO ()
testInorder = 
    if inorder testTree == [1,2,3]
        then putStrLn "inorder fine!"
        else putStrLn "Bad news billy bears."

testPostOrder :: IO ()
testPostOrder = 
    if postorder testTree == [1,3,2]
        then putStrLn "postorder fine!"
        else putStrLn "Bad news billy bears."

main :: IO()

main = 
    do 
        testPreorder
        testInorder
        testPostOrder
        return ()

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f init= ((foldr f init) . inorder) 

f :: Show a => (a,b) -> IO (a,b)
f t@(a, _ ) = 
    do
        print a
        return t


isSubSeq :: (Eq a) => [a] -> [a] -> Bool
isSubSeq [] _ = True
isSubSeq (x:_) [] = False
isSubSeq super@(x:xs) (y:ys) =  if x == y  then (isSubSeq xs ys || isSubSeq super ys) else isSubSeq super ys

split ::(a->Bool) -> [a] -> [[a]]
split f [] = []
split f s = takeWhile (f) s : ((split f) . (drop 1) . dropWhile(f)) s
-- myWords :: String -> [String]

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\x -> let (y:ys) = x in (x,(toUpper y):ys)) . words


capitalizeWord (x:xs) = toUpper x : xs
join c = foldr (\x y -> x ++ [c] ++ y) [] 

capitalizeFirstLetter [] = []
capitalizeFirstLetter t@(x:xs) = if elem x (['A'..'Z'] ++ ['a'..'z']) then (capitalizeWord t) else x : capitalizeFirstLetter xs

capitalizeParagraph = ((join '.') . (map capitalizeFirstLetter) . (split (/='.')))


data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add l r) = (eval l) + (eval r)


printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add l r) = printExpr l ++ " + " ++ printExpr r 


