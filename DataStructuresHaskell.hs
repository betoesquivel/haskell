data LA e = L [LA e] | E e deriving Show
{- if even elem then (E 0):(binariza resto)
                else (E 1):(binariza resto)
-}
binariza :: LA Int -> LA Int
binariza (L ((L lista):resto)) = (L (binariza (L lista)):(binariza (L resto))
binariza (L ((E elem):resto)) = (E elem):(binariza (L resto))
