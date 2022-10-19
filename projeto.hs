import Data.List.Split
import Data.Text.Encoding.Error (ignore)
type Mono = (Int, [(Char, Int)])
type Poly = [Mono]


--------------------------parsing-------------------------------

parseString :: String -> [[String]] {-, [(Char, Int)])]-}
parseString [] = []{-("", [])-}
parseString s = splitCoefficient (splitOneOf "+" (removeSpace (addPlus s)))

addPlus :: String -> String
addPlus [] = ""
addPlus (x:xs) | x == '-' = "+-" ++ addPlus xs
               | otherwise =  x : addPlus xs

removeSpace :: String -> String
removeSpace s = [x | x <- s, x/=' ']

splitCoefficient :: [String] -> [[String]]
splitCoefficient [] = []
splitCoefficient (x:xs) =  removePower (splitOn "*" x) : splitCoefficient xs

removePower :: [String] -> [String]
removePower  [] = []
removePower (x:xs) = splitOn "^" x ++ removePower xs

{-
multCoefficient :: String -> String
multCoefficient "" = []
multCoefficient (x:xs) 
  | '^' : x = ignore
  |otherwise head = x * multCoefficient xs
-}

--------------------------------------------------------------------

----------------------------Normalise poly--------------------------

add1Poly :: Poly -> Poly
add1Poly [] = []
add1Poly [x] = [x]
add1Poly (Mono x1 [(y1, z1)]) (Mono x2 [(y2, z2)]) | (y1 == y2) && (z1 == z2) = (Mono (x1 + x2) [(y1,z1)])

--------------------------------------------------------------------

-----------------------------Sum Polys---------------------------------

mergePoly :: Poly -> Poly -> Poly
mergePoly [] [] = []
mergePoly [] x = []
mergePoly x [] = []
mergePoly (x:xs) (y:ys) = x : y : mergePoly xs ys

addPolys :: Poly -> Poly -> Poly
addPolys [] [] = []
addPolys [] a = a
addPolys a [] = a
addPolys a b = add1Poly (mergePoly a b)

----------------------------Mult Polys-----------------------------


multPoly :: Poly -> Poly -> Poly
multPoly []      qs      = []
multPoly ps      []      = []
multPoly (x:xs) (y:ys) = (Mono x1 [(y1, z1)]) (Mono x2 [(y2, z2)]) if y1 == y2 then (Mono (x1 * x2) [(y1, z1+z2)]) else (Mono (x1*x2) [(y1, z2), (y2, z2)])


---------------------------------Some values-----------------------------
a = Mono 3 [('x', 3)]
b = Mono 6 [('y', 2)]
c = Mono 2 [('z', 5)]
d = Mono 9 [('x', 5)]

f = Poly [a]
h = Poly [d]