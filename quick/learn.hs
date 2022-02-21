data Vector a = Vector a a a deriving (Show)

data Person = Person { firstName :: String,
                       lastName :: String,
                       age :: Int
                     } deriving (Show, Eq, Read)

data Car a b c = Car { company :: a
               , model :: b
               , year :: c
               } deriving (Show)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Show, Read, Bounded, Enum )

compareWithHundred :: (Ord a, Num a) => a -> Ordering
compareWithHundred  = compare 100

divideByTen :: Double -> Double
divideByTen = (/10)


isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


flip' f y x = f x y

map' _ [] = []
map' f (x:xs) = f x : map' f xs


filter' _ [] = []
filter' p (x:xs)
    | p x        = x:filter' p xs
    | otherwise  = filter p xs


chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n = n:chain (n*3 + 1)

numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

--Data Types Srypes

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n


tellCar ::(Show a) =>  Car String String a -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++c++ " " ++m++ " was made in "++ show y
