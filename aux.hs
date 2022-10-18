-- somar polinómios; versão usando listas em compreensão
addPoly' :: [Int] -> [Int] -> [Int]
addPoly' ps qs
  | n<=m =  -- qs é maior ou igual que ps
     [p+q | (p,q) <- zip (ps ++ replicate (m-n) 0) qs]
  | otherwise = -- ps é maior que qs
      [p+q | (p,q) <- zip ps (qs ++ replicate (n-m) 0) ]
  where
    n = length ps
    m = length qs

{-
  Solução sem recursão usando listas em compreensão:
  P = sum_0<=i<=n p_i * X^i
  Q = sum_0<=j<=m q_j * X^j
  Logo P*Q = sum_0<=k<=n+m c_k * X^k
  onde c_k = sum {p_i*q_j | i+j = k}
-}

multPoly' :: [Int] -> [Int] -> [Int]
multPoly' ps qs = [sum [pi*qj | (i,pi)<-zip [0..] ps, (j,qj)<-zip [0..] qs,
                        i+j == k]
                  | k<-[0..kmax]]
                         
  where
    kmax = length ps + length qs - 2 