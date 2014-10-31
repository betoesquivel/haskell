-- Haskell homework for our Programming Languages Class

--Implementar la función recursiva multiplica que obtenga una lista de 1’s que 
--represente el resultado en unario de multiplicar dos enteros no negativos en decimal. 
multiplica :: Integer -> Integer -> [Integer] 
multiplica a b = repite (a * b)

repite :: Integer -> [Integer]
repite 0 = []
repite n = [1] ++ repite (n - 1)
