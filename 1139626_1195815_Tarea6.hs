-- Haskell homework for our Programming Languages Class

--1. Implementar la función recursiva distintos que liste los elementos distintos
--que pertenecen a dos listas. Asumir que los elementos no se repiten dentro de la
--misma lista.
contiene :: (Eq a) => [a] -> a -> Bool
contiene [] el = False
contiene (x : xs) el
        | x == el = True
        | otherwise = contiene xs el

distintos :: (Eq a) => [a] -> [a] -> [a]
distintos list1 list2 = distintos_aux list1 list2 [] []
 
distintos_aux :: (Eq a) => [a] -> [a] -> [a] -> [a] -> [a]
distintos_aux [] [] rep res = res
distintos_aux [] list2 rep res = distintos_aux list2 [] rep res
distintos_aux (x : xs) list2 rep res
        | contiene xs x || contiene list2 x || contiene res x || contiene rep x = distintos_aux xs list2 (rep ++ [x]) res
        | otherwise = distintos_aux xs list2 rep (res ++ [x])
 
--2. Implementar la función recursiva multiplica que obtenga una lista de 1’s que 
--represente el resultado en unario de multiplicar dos enteros no negativos en decimal. 
multiplica :: Integer -> Integer -> [Integer] 
multiplica a b = repite (a * b)

repite :: Integer -> [Integer]
repite 0 = []
repite n = [1] ++ repite (n - 1)

--3. Implementar la función recursiva bolos que genere el patrón de acomodo
--común para N bolos. La última línea de bolos puede quedar incompleta!
bolos :: Integer -> [[Integer]]
bolos 0 = [[]]
bolos 1 = [[1]]
bolos 3 = bolos 1 ++ [[2, 3]]
bolos 6 = bolos 3 ++ [[4, 5, 6]]
bolos 10 = bolos 6 ++ [[7, 8, 9, 10]]
bolos n
    | n > 6 = bolos 6 ++ [ bolosaux 7 n ]
    | n > 3 = bolos 3 ++ [ bolosaux 4 n ]
    | n > 1 = bolos 1 ++ [ bolosaux 2 n ]
    | otherwise = [[]]

bolosaux :: Integer -> Integer -> [Integer] 
bolosaux x n = if x <= n then x : ( bolosaux (x+1) n ) else []

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


--5. Implementar la función internos en Haskell que dado un árbol binario regrese una
--lista con los valores que se encuentran en los nodos internos del árbol 
internos :: AB Int -> [Int]
internos V = []
internos (A V t V) = []
internos (A i t e) = [t] ++ internos i ++ internos e

--6. Implementar la función recursiva g_distintos que utilizando guardias liste
--los elementos que pertenecen a dos listas. 
g_distintos :: (Eq a) => [a] -> [a] -> [a]
g_distintos l1 l2 = g_distintos_aux l1 l2 ++ g_distintos_aux l2 l1

g_distintos_aux :: (Eq a) => [a] -> [a] -> [a]
g_distintos_aux [] _ = []
g_distintos_aux l1@(el:ls) l2
    | l2 == [] = l1
    | contiene l2 el = g_distintos_aux ls l2 
    | otherwise = el : g_distintos_aux ls l2



--7. Implementar la función no-recursiva c_tabla en Haskell que utilizando 
--"comprensión de listas" obtenga la tabla de multiplicar especificada. Los 
--elementos de la tabla deben aparecer en tuplas. 
c_tablas :: Int -> [((Int, Int), Int)]
c_tablas n = [((n, x), n * x) | x <- [1..10]]

--8. Implementar la función no-recursiva f_prodpar en Haskell que utilizando la FOS
--(funciones de orden superior) cree una lista con los productos de los elementos de las
--listas de tamaño impar. 
f_prodpar = (\lista  -> [ product l | l <- lista, odd(length l) ] )


