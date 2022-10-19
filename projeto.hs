Type Poly = [Int, [Char, Int]]

parseString :: String -> [[[String]]]
parseString [] = [[[]]]
parseString s = splitCoefficient (split '+' (removeSpace (addPlus s)))

split :: Char -> String -> [String]
split _ "" = []
split a b = x : (split a s)
    where x = takeWhile (/=a) b
          s = drop (length x + 1) b


addPlus :: String -> String
addPlus [] = ""
addPlus (x:xs) | x == '-' = "+-" ++ addPlus xs
               | otherwise =  x : addPlus xs

removeSpace :: String -> String
removeSpace s = [x | x <- s, x/=' ']

splitCoefficient :: [String] -> [[[String]]]
splitCoefficient [] = []
splitCoefficient (x:xs) =  [removePower (split '*' x)] ++ splitCoefficient xs

removePower :: [String] -> [[String]]
removePower  [] = []
removePower (x:xs) = [split '^' x] ++ removePower xs

addPoly :: [Int] -> [Int] -> [Int]
addPoly (p:ps) (q:qs) = (p+q):addPoly ps qs
addPoly []      qs    = qs
addPoly ps      []    = ps

multPoly :: [Int] -> [Int] -> [Int]
multPoly []      qs      = []
multPoly ps      []      = []
multPoly (a0:ps) (b0:qs) = (a0*b0) : rest
  where
    rest = addPoly (addPoly rest1 rest2) rest3
    rest1 = [a0*q | q<-qs]
    rest2 = [b0*p | p<-ps]
    rest3 = 0 : multPoly ps qs

  