import Data.List.Split
import Data.List (sortBy)
import Data.Ord
import Data.Function (on)
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

rmvExpZero :: Poly -> Poly
rmvExpZero [] = []
rmvExpZero (x:xs) | snd (vars x !! 0) == 0 = Mono 1 [(' ', 1)] : rmvExpZero xs
                  |otherwise = x : rmvExpZero xs

rmvZero :: Poly -> Poly
rmvZero [] = []
rmvZero (x:xs) | coef x == 0 = rmvZero xs
               | otherwise = x : rmvZero xs

sortMono :: Ord a => [(a, b)] -> [(a, b)]
sortMono = sortBy (compare `on` fst)

sortPoly :: Poly -> Poly
sortPoly (x:xs) = a ++ sortPoly xs
  where a = [Mono (coef x) (sortMono(vars x))]

normalise :: Poly -> Poly
normalise [] = []
normalise a = add1Poly (rmvExpZero (rmvZero a))

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
addPolys a b = normalise (mergePoly a b)

----------------------------Mult Polys-----------------------------

multPolyAux :: Poly -> Poly -> Poly
multPolyAux []      qs      = []
multPolyAux ps      []      = []
multPolyAux (x:xs) qs = [multMono x y | y <- qs] ++ multPolyAux xs qs


multMono :: Mono -> Mono -> Mono
multMono a b 
  |fst (vars a !! 0) == fst (vars b !! 0) = Mono (coef a * coef b) [(fst (vars a !! 0), snd (vars a !! 0) + snd (vars b !! 0))]
  |otherwise = Mono (coef a * coef b) ((vars a) ++ (vars b))

multPoly :: Poly -> Poly -> Poly
multPoly a b = normalise (multPolyAux a b)


----------------------------------Derivation--------------------------------




---------------------------------Some values-----------------------------

a = Mono (-3) [('x', 3), ('z', 2)]
b = Mono 6 [('y', 2)]
c = Mono 2 [('z', 5), ('x', 2)]
d = Mono 9 [('x', 3)]
n = Mono 5 [('y', 2)]
zero = Mono 0 [('z', 3)]
expZero = Mono 4 [('x', 0)]
t = Mono 6 [('y', 0), ('x', 7)]

f = [a]
h = [a, d]
k = [b, n]
l = [d]
o = [b]
i = [c, a, t]
wzero = [a, c, zero, n]
wExpZero = [a, d, expZero, zero, n]
v = vars a !! 0