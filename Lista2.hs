module Lista2 where

--Questão 1
--diz se o número pertence à lista

pertence :: Eq t => t -> [t] -> Bool

pertence n [] = False
pertence n (x:xs) = if n == x then True else pertence n xs

--Questão 2
--retorna uma lista que é a interseção de duas listas

intercessao :: Eq a => [a] -> [a] -> [a]

intercessao [] ys = []
intercessao (x:xs) ys = if pertence x ys 
                        then x:intercessao xs ys 
                        else intercessao xs ys

--Questão 3
--inverte uma lista

concateno [] ys = ys
concateno (x:xs) ys = x:concateno xs ys  

inversoLista :: [a] -> [a]

inversoLista [] = []
inversoLista (x:xs) = concateno (inversoLista xs) [x]

--inverso (1:2,3) = concateno (inverso (2:3)) [1]
--				  = concateno (concateno (inverso [3]) [2]) [1]
--				  = concateno (concateno ( concateno [] [3]) [2]) [1]
--				  = concateno ( concateno [3] [2]) [1] )
--				  = concateno [3,2] [1]
--				  =[3,2,1]

--Questão 4
--retorna os n ultimos elementos de uma lista

tamanho [] = 0
tamanho (y:ys) = 1 + tamanho ys

nUltimos :: (Ord t, Num t) => t -> [a] -> [a]

nUltimos n [] = []
nUltimos n (x:xs) = if n >= tamanho (x:xs) 
                    then x:xs 
                    else nUltimos n xs

--Questão 5
--retornar o elemento n de uma lista

enesimo :: (Eq t, Num a, Num t) => t -> [a] -> a

enesimo n [] = -1
enesimo 1 (x:xs) = x
enesimo n (x:xs) = enesimo (n-1) xs

--Questao 6
--repetir n vezes o número m

repetir :: (Eq t, Num t) => t -> a -> [a]

repetir 0 m = []
repetir n m = m:repetir (n-1) m 

--Questão 7
--intercalar duas listas

intercalacao [] (y:ys) = y:ys

intercalacao (x:xs) [] = x:xs
intercalacao (x:xs) (y:ys) = if x > y 
                              then y:intercalacao (x:xs) ys 
                              else x:intercalacao xs (y:ys)

--Questão 8
-- retorna o menor de uma lista

menor :: Ord a => [a] -> a

menor [x] = x
menor (x:xs) = if x < menor xs then x else menor xs

--Questão 9
--remove o primeiro elemento n de uma lista

removerElem :: Eq a => a -> [a] -> [a]

removerElem n [] = []
removerElem n (x:xs) = if pertence n (x:xs) == True 
                        then (if n == x then xs else x:removerElem n xs) 
                        else (x:xs)

--Questão 10
--ordena os elementos de uma lista na ordem crescente

ordenarLista :: Ord a => [a] -> [a]

ordenarLista [] = []
ordenarLista (x:xs) = menor (x:xs) : ordenarLista (removerElem (menor (x:xs)) (x:xs))

--Questão 11
--insere um elemento novo na lista, se a lista não tiver o elemento
--a lista deve estar ordenada

insereElem :: Ord a => a -> [a] -> [a]

insereElem n [] = [n]
insereElem n (x:xs) = if pertence n (x:xs) == False 
                       then (if n < x then n:x:xs else x:insereElem n xs) 
                       else (x:xs)

--Questão 12
--recebe uma lista de duplas e retorna uma lista com o primeiro elemento de cada dupla

primeirosDuplas :: [(a,b)] -> [a]

primeirosDuplas [] = []
primeirosDuplas ((x,y):xys) = x: primeirosDuplas xys

--Questão 13
--recebe uma lista de duplas e retorna uma lista com a soma dos elementos das duplas

somaDuplas :: Num a => [(a,a)] -> [a]

somaDuplas [] = []
somaDuplas ((x,y):xys) = (x + y):somaDuplas xys

--Questão 14
-- recebe uma lista de duplas e retorna as duplas em que a < b

menoresDuplas :: Ord a => [(a,a)] -> [(a,a)]

menoresDuplas [] = []
menoresDuplas ((x,y):xys) = if x < y 
                            then (x,y): menoresDuplas xys 
                            else menoresDuplas xys

--Questão 15
--recebe um número e uma lista 
--retorna uma dupla de duas listas, a primeira com x <= v e a segunda com x > v

separarDuplas :: Ord a => a -> [a] -> ([a],[a])

separarDuplas v [] = ([],[])
separarDuplas v (x:xs) = ([ x | x <- x:xs, x <= v], [ x | x <- x:xs, x > v])

--Questão 16
--máximo divisor comum

mdc :: Integral a => (a, a) -> a

mdc (a, 0) = a
mdc (a, b) = mdc (b, (mod a b))

--Questão 17
--interve as duplas de uma lista (a,b) -> (b,a)

inversoDupla :: [(a,b)] -> [(b,a)]

inversoDupla [] = []
inversoDupla ((x,y):xys) = (y,x): inversoDupla xys

--Questão 18
--diz se os elementos de uma dupla são iguais

simetrico :: Ord a => [(a,a)] -> [Bool]

simetrico [] = []
simetrico ((x,y):xys) = if x == y then True : simetrico xys else False : simetrico xys

--Questão 19
-- recebe um número e retorna uma lista de duplas tal que 1 <= x <= i, 1 <= y <= i e x /= y

pares :: (Num b, Enum b, Ord b) => b -> [(b, b)]

pares i = ([(x,y)| x <- [0..100], y <- [0..100], 1 <= x, x <= i, 1 <= y, y <= i, x /= y ])

--Questão 20
--troca as letras da sequencia de um DNA e inverte a nova lista

inverteDNA :: [Char] -> [Char]

invC 'A' = 'T'
invC 'T' = 'A'
invC 'C' = 'G'
invC 'G' = 'C'

invL [] = []
invL (x:xs) = (invC x):invL xs

inverteDNA (x:xs) = inversoLista (invL (x:xs))

--Questão 21
--calcula a quantidade de moedas do troco de um café, a partir do valor do café e do valor pago pelo cliente

trocoCafe :: Integral a => a -> a -> [(a,a)]
calculoTroco :: Integral a => a -> [a] -> [(a, a)]

calculoTroco t [] = []
calculoTroco t (x:xs) = if div t x > 0 
                 then (x, div t x): calculoTroco (rem t x) xs  
                 else calculoTroco t xs

trocoCafe v p = calculoTroco (p-v) [50,20,10,5]

--Questão 22

magica :: Eq a => [a] -> [a]

magica' [] = []
magica' (x:xs) = repetir (tamanho (x:xs)) x ++ magica' xs

magica'' (x:xs) = removerElem (last (magica' (x:xs))) (magica' (x:xs))

--magica''' (x:xs) = inversoLista (magica'' (x:xs))
--magica (x:xs) = magica' (x:xs) ++ magica''' (x:xs)

magica (x:xs) = magica'' (x:xs) ++ inversoLista (magica' (x:xs))