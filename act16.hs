cuenta_signo:: [Int] -> (Int, Int)
cuenta_signo l
  | l == [] = (0,0)
  | otherwise = (length [x | x<-l, x>0], length [x | x<-l, x<0] )
