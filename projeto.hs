import Data.List.Split
import Data.Text.Encoding.Error (ignore)
--type Mono = (Int, [(Char, Int)])
data Mono = Mono { 
    coef :: Integer,
    vars :: [(Char, Int)]
} deriving (Eq,Ord, Show)
type Poly = [Mono]


--------------------------parsing-------------------------------
{-
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
-}
--------------------------------------------------------------------

----------------------------Normalise poly--------------------------

add1Poly :: Poly -> Poly
add1Poly [] = []
add1Poly [x] = [x]
add1Poly (x:xs) = [Mono newCoef (vars x)] ++ add1Poly [y | y <- (x:xs), (vars x /= vars y)]
  where newCoef = sum [coef y | y <- (x:xs), (vars x == vars y)]

--------------------------------------------------------------------

-----------------------------Sum Polys---------------------------------

mergePoly :: Poly -> Poly -> Poly
mergePoly [] [] = []
mergePoly [] x = x
mergePoly x [] = x
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
multPoly (x:xs) (y:ys) 
  | fst (vars x !! 0) == fst (vars y !! 0) = 
    [Mono (coef x * coef y) [(fst (vars x !! 0), snd (vars x !! 0) + snd (vars y !! 0))]] ++ multPoly xs ys
  |otherwise = [Mono (coef x * coef y) ((vars x) ++ (vars y))] ++ multPoly xs ys

-- vars x -> [(Char, Int)] 
---------------------------------Some values-----------------------------
a = Mono (-3) [('x', 3)]
b = Mono 6 [('y', 2)]
c = Mono 2 [('z', 5)]
d = Mono 9 [('x', 3)]
n = Mono 5 [('y', 2)]

f = [a]
h = [a, d]
k = [b, n]
l = [d]
o = [b]
v = vars a !! 0