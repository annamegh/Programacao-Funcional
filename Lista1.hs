module Lista1 where 

-- Anna Paula Meneghelli de Oliveira - Lista 1 PFN.

-- questao 1
-- verifica se 3 números formam um triângulo

ehTriangulo a b c = if a + b > c && a + c > b && b + c > a 
                    then True else False

-- questao 2
-- verifica o tipo de um triângulo

tipoTriangulo a b c = if a == b && b == c then "equilatero" 
                      else ( if a /= c && c /= b && a /= b then "escaleno" 
                      else "isosceles")

-- questão 3

{-triangulo a b c = if a + b > c && a + c > b && b + c > a 
                   then ( if a == b && b == c then "equilatero" 
                          else ( if a /= c && c /= b && a /= b then "escaleno" 
                                 else "isosceles" ) ) 
                  else "nao eh um triangulo" -}
  
triangulo a b c = if ehTriangulo a b c == True 
                   then ( if tipoTriangulo a b c == "equilatero" then "equilatero" 
                          else ( if tipoTriangulo a b c == "escaleno" then "escaleno" 
                                 else "isosceles" ) ) 
                  else "nao eh um triangulo"

-- questão 4 
-- recebe como parâmetro um inteiro n e retorna a soma dos números pares entre 0 e n.
somaPares 0 = 0
somaPares 1 = 0
somaPares n = if (rem n 2 == 0) 
               then (n + somaPares (n - 2)) else ( somaPares (n - 1))

--questão 5
-- 2^0m + 2^1m + 2^2m+ ... + 2^nm

somaPot2m m 0 = m
somaPot2m m 1 = 2 * m + m
somaPot2m m n = 2 * somaPot2m m (n - 1) +  m 
  
--questão 6
-- verifica se é primo

teste1 0 0 = 1
teste1 1 1 = 1
teste1 a 0 = 1
teste1 a 1 = 0
teste1 a b = if rem a b /= 0 
              then teste1 a (b-1) else 1

primo n = if (teste1 n (n-1)) == 0 
           then True else False

-- questão 7
-- (4/1 – 4/3) + 4/5 – 4/7 + 4/9 – 4/11 + ...

teste2 1 0 = 0
teste2 a b = if (a <= b) 
              then (if (rem b 2 /= 0) 
                     then  ((4/fromIntegral a) - teste2 (a+2) b) 
                     else (teste2 a (b-1))) 
             else 0

seriePI n = teste2 1 n 

