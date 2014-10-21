lucky :: (Integral a) => a->String
lucky 7 = "Lucky Number Seven!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a->String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe 6 = "Six!"
sayMe 7 = "Seven!"
sayMe 8 = "Eight!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1+x2 , y1+y2)

first :: (a, b, c) ->  a
first (x, _, _) = x

second :: (a, b, c) ->  b
second (_, y, _) = y

third :: (a, b, c) ->  c
third (_, _, z) = z

listPattern = [(1,3), (4,3), (2,4), (5,3), (5, 6), (3,1)]

head' :: (Show a) => [a] -> String
head' [] = "The list is empty!"
head' (x:[]) = "The list has one element: " ++ show x
head' (x:y:[]) = "The  list has two elements: " ++ show x ++ " and "++ show y
--head' (x:y:z:[]) = "The list has three elements: " : (show x)
--head' (x:y:z:m:[]) = "The list has four elements: " : (show x)
--

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- Lo equivalente al cond
-- guards
max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

-- Where para no repetirme
bmiTell :: (RealFloat a) => a -> a-> String
bmiTell weight height
  | bmi <= skinny =  "You are underweight, you emo, you!"
  | bmi <= normal =  "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= fat =  "You're fat! Lose some weight, fatty!"
  | otherwise =  "You're a whale, congratulations!"
  where bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)

