import Data.String
import Data.List
import Data.Char (isDigit)

{- Somar e multiplicar polinómios representados
  como listas de coeficientes.

  Exemplo
   1+2X-X² ~ [1, 2, -1]
-}

-- somar polinómios
addPoly :: [Int] -> [Int] -> [Int]
addPoly (p:ps) (q:qs) = (p+q):addPoly ps qs
addPoly []      qs    = qs
addPoly ps      []    = ps

{-
 Multiplicar polinómios
 Ideia do algoritmo recursivo:
 Se
  P = a0 + X*P
  Q = b0 + X*Q
 então
  P*Q = a0*b0 + a0*X*Q + b0*X*P + X²*P*Q
      = a0*b0 + X*(a0*Q + b0*P + X*P*Q)
 -}
multPoly :: [Int] -> [Int] -> [Int]
multPoly []      qs      = []
multPoly ps      []      = []
multPoly (a0:ps) (b0:qs) = (a0*b0) : rest
  where
    rest = addPoly (addPoly rest1 rest2) rest3
    rest1 = [a0*q | q<-qs]
    rest2 = [b0*p | p<-ps]
    rest3 = 0 : multPoly ps qs

parseString :: String -> [[String]]
parseString [] = [[]]
parseString s = map split (filter (\n->n/="+") (words s))

split :: String -> [String]
split "" = []
split s = x : (split rest)
    where x = takeWhile (/='*') s
          rest = drop (length x + 1) s

{-add1 [[String]] -> [[String]]
add1 [[]] = [[]]
add1 x | x <-xs, | lenght x == 1 "1" : x
-}

{-aux1 :: [String] -> [String]
aux1 [] = []
aux1 xs | (length xs == 1 && checkNum el 
where el = x |x<-xs) then "1" : x-}

checkNum :: String -> Bool
checkNum = all isDigit

checkNum2 :: [String] -> Bool
checkNum2 [] = False
checkNum2 (x:xs) = checkNum2 x ++ checkNum2 xs