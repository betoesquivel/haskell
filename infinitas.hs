fibo =  1:1:[a+b | (a,b) <- zip fibo (tail fibo)]
