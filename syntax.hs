-- values, assignments
x :: Int
x = 1

-- functions are values
plusOne :: Int -> Int
plusOne = (+1)

plusOne' :: Int -> Int
plusOne' x = x + 1

-- functions take arguments
f :: Int -> Int -> [Int]
f x y = [x, y]

-- functions can be declared using lambda expressions
f' :: Int -> Int -> [Int]
f' = \x -> \y -> [x, y]

-- operators: infix and prefix
(|+|) :: Int -> Int -> Int
(|+|) x y = x + y

a = 1 |+| 2   -- infix
b = (|+|) 1 2 -- prefix

-- prefix notation
zum :: Int -> Int -> Int
zum = (+)

c = zum 1 2
d = 1 `zum` 2

-- polymorphism
g :: a -> Maybe b
g x = Nothing

-- data types
data Boolean = Yes | No deriving (Show, Eq)

data Option a = Some a | None deriving (Show, Eq)

-- pattern matching (on the left side)
mapOption :: Option a -> (a -> b) -> Option b
mapOption (Some x) f = Some (f x) -- constructor on the right side
mapOption None _     = None

-- pattern matching using 'case'
flatMapOption :: Option a -> (a -> Option b) -> Option b
flatMapOption opt f = case opt of
  (Some x) -> f x
  None     -> None

-- guards
binary :: Int -> String
binary x
  | x == 0 = "Zero"
  | x == 1 = "One"
  | otherwise = "Not binary!"
