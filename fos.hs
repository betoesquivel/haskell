aplica_fns = (\x n m-> map (\i -> (i n m)) x )

reduce = (\f p l -> foldl1 f (filter p l) )

secuencia = (\n-> until (\x-> last x == n) (\x-> x++[(last x + 1)]) [1])
