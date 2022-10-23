import Data.List.Split
import Data.List (sortBy)
import Data.Ord
import Data.Function (on)
import Data.List (find)
import Data.Text.Encoding.Error (ignore)


-- Data types
data Mono = Mono { 
    coef :: Int,
    vars :: [(Char, Int)]
} deriving (Eq,Ord, Show)
type Poly = [Mono]


--------------------------Parsing-------------------------------
--Main parsing function
parseString :: String -> [[String]] {-, [(Char, Int)])]-}
parseString [] = []{-("", [])-}
parseString s = splitCoefficient (splitOneOf "+" (removeSpace (addPlus s)))

-- Adds a plus sign before -, as we split on + sign
addPlus :: String -> String
addPlus [] = ""
addPlus (x:xs) | x == '-' = "+-" ++ addPlus xs
               | otherwise =  x : addPlus xs

-- Removes space
removeSpace :: String -> String
removeSpace s = [x | x <- s, x/=' ']

-- Splits coefficient from variable
splitCoefficient :: [String] -> [[String]]
splitCoefficient [] = []
splitCoefficient (x:xs) =  removePower (splitOn "*" x) : splitCoefficient xs

-- Splits coefficient from power
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

-- Adds monomials with same variables
add1Poly :: Poly -> Poly
add1Poly [] = []
add1Poly [x] = [x]
add1Poly (x:xs) = [Mono newCoef (vars x)] ++ add1Poly [y | y <- (x:xs), (vars x /= vars y)]
  where newCoef = sum [coef y | y <- (x:xs), (vars x == vars y)]

-- Removes variables with power 0
rmvExpZero :: Poly -> Poly
rmvExpZero [] = []
rmvExpZero (x:xs) = [Mono (coef x) (rmvExpZeroAux (vars x))] ++ rmvExpZero xs

--Checks if power is 0
rmvExpZeroAux :: [(Char, Int)] -> [(Char, Int)]
rmvExpZeroAux [] = []
rmvExpZeroAux (x:xs)
  |snd(x) == 0 = rmvExpZeroAux xs
  |otherwise = [x] ++ rmvExpZeroAux xs

-- Removes 0 coefficients 
rmvZero :: Poly -> Poly
rmvZero [] = []
rmvZero (x:xs) | coef x == 0 = rmvZero xs
               | otherwise = x : rmvZero xs

--Sorts by coefficient
sortMono :: Ord a => [(a, b)] -> [(a, b)]
sortMono = sortBy (compare `on` fst)

-- Sorts plynomial
sortPoly :: Poly -> Poly
sortPoly [] = []
sortPoly (x:xs) = a ++ sortPoly xs
  where a = [Mono (coef x) (sortMono(vars x))]

-- Main normalise function
normalise :: Poly -> Poly
normalise [] = []
normalise a = add1Poly (rmvExpZero (rmvZero (sortPoly a)))

--------------------------------------------------------------------

-----------------------------Sum Polys---------------------------------

-- Merges the 2 polynomials into one
mergePoly :: Poly -> Poly -> Poly
mergePoly [] [] = []
mergePoly [] x = x
mergePoly x [] = x
mergePoly (x:xs) (y:ys) = x : y : mergePoly xs ys

-- Adds the 2 polynomials
addPolys :: Poly -> Poly -> Poly
addPolys [] [] = []
addPolys [] a = a
addPolys a [] = a
addPolys a b = normalise (mergePoly a b)

----------------------------Multi Polys-------------------------------

-- Joins all  multiplied items
multPolyAux :: Poly -> Poly -> Poly
multPolyAux []      qs      = []
multPolyAux ps      []      = []
multPolyAux (x:xs) qs = [multMono x y | y <- qs] ++ multPolyAux xs qs

--  Multiplies all items in polynomials
multMono :: Mono -> Mono -> Mono
multMono a b 
  |fst (vars a !! 0) == fst (vars b !! 0) = Mono (coef a * coef b) [(fst (vars a !! 0), snd (vars a !! 0) + snd (vars b !! 0))] 
  |otherwise = Mono (coef a * coef b) ((vars a) ++ (vars b))

-- Main multiplication function
multPoly :: Poly -> Poly -> Poly
multPoly a b = normalise (multPolyAux a b)


----------------------------------Derivation--------------------------------

-- Check if variableis the one to be derivable 
findVar :: [(Char, Int)] -> Char -> Bool
findVar [] a = False
findVar a ' ' = False
findVar (x:xs) b
  |fst x == b = True
  |otherwise = findVar xs b

-- Returns the power of the variable
findExp :: [(Char, Int)] -> Char -> Int
findExp (x:xs) b
  |fst x == b = snd x
  |otherwise = findExp xs b

-- Main derivation function
derivatePoly :: Poly -> Char -> Poly
derivatePoly [] b = []
derivatePoly (x:xs) b 
  |(findVar (vars x) b) && ((findExp (vars x) b) > 1) = [Mono ((coef x) * (findExp (vars x) b)) (updateVar (vars x) b)] ++ normalise (derivatePoly xs b)
  |(findVar (vars x) b) && ((findExp (vars x) b) == 1) = [Mono (coef x) (updateVar2 (vars x) b)] ++ normalise (derivatePoly xs b)
  |otherwise = [x] ++ normalise (derivatePoly xs b)

-- Derives the power of the  variable (previous value minus 1)
updateVar :: [(Char, Int)] -> Char -> [(Char, Int)]
updateVar [] b = []
updateVar (x:xs) b 
  |fst(x) == b = [(b, snd(x) - 1)] ++ updateVar xs b
  |otherwise = [x] ++ updateVar xs b

-- Removes variable if power 0
updateVar2 :: [(Char, Int)] -> Char -> [(Char, Int)]
updateVar2 [] b = []
updateVar2 (x:xs) b 
  |fst(x) == b = updateVar2 xs b
  |otherwise = [x] ++ updateVar2 xs b 
 
---------------------------------Output----------------------------------

--polyToString :: Poly -> String
--toString [] = []

{-
varsToString :: [(Char, Int)] -> String
varsToString [] = []
varsToString (x:xs) = fst(x) ++ show(snd (x))
-}

---------------------------------Some values-----------------------------

m1 = Mono (-3) [('x', 3), ('z', 2)]
m2 = Mono 6 [('y', 2)]
m3 = Mono 2 [('z', 5), ('x', 2)]
m4 = Mono 9 [('x', 3)]
m5 = Mono 5 [('y', 2)]
m6 = Mono 6 [('y', 0), ('x', 7)]
m7 = Mono 7 [('z', 1)]
m8 = Mono 8 [('x', 1)]
m9 = Mono 4 [('x', 0)]
m10 = Mono 0 [('z', 3)]


p1 = [m1]
p2 = [m1, m4]
p3 = [m2, m3]
p4 = [m4]
p5 = [m2, m5]
p6 = [m3]
p7 = [m5, m1, m6]
p8 = [m7, m8]
wzero = [m1, m3, m10, m5]
wExpZero = [m1, m4, m9, m10, m5]
v = vars m1 !! 0