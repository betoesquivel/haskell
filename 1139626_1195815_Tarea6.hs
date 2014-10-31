-- Haskell homework for our Programming Languages Class

--2. Implementar la función recursiva multiplica que obtenga una lista de 1’s que 
--represente el resultado en unario de multiplicar dos enteros no negativos en decimal. 
multiplica :: Integer -> Integer -> [Integer] 
multiplica a b = repite (a * b)

repite :: Integer -> [Integer]
repite 0 = []
repite n = [1] ++ repite (n - 1)

--4. Implementar la función obten-mayores en Scheme que dados un árbol binario y
--un valor como argumentos, cree una lista con los valores de los nodos que contengan
--valores mayores que el valor dado como argumento. Los valores en la lista resultante
--pueden, o no, estar ordenados
data AB t = A (AB t) t (AB t) | V deriving Show
ab = A (A (A V 2 V) 
          5 
          (A V 7 V)) 
            8 
          (A V 
            9 
            (A (A V 11 V) 
            15 
            V))


obtenMayores :: AB Integer -> Integer -> [Integer] 
obtenMayores V valor = []
obtenMayores (A l v r)  valor   
    | v > valor = [v] ++ (obtenMayores l valor) ++ (obtenMayores r valor)
    | otherwise = (obtenMayores r valor)
