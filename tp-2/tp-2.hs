-- Práctica de ejercicios # 2 - Listas y Recursión

{- 1. Recursión sobre listas
De las siguientes funciones utilizando recursión estructural sobre listas, salvo que se indique
lo contrario: -}


sumatoria :: [Int] -> Int
--Prop- Dada una lista de enteros devuelve la suma de todos sus elementos
sumatoria [] = 0 
sumatoria (n : ns) = n + sumatoria ns 

longitud :: [a] -> Int
{-Prop: Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad
de elementos que posee. -}
longitud [] = 0
longitud (n : ns) = 1 + longitud ns 


sucesores :: [Int] -> [Int]
--Prop: Dada una lista de enteros, devuelve la lista de los sucesores de cada entero
sucesores [] = [0]
sucesores (n : ns) = (n + 1) : sucesores ns

--CORREGIR SIN PREC 
conjuncion :: [Bool] -> Bool
--Prop: Dada una lista de booleanos devuelve True si todos sus elementos son True
--Prec: Debe haber al menos un booleano en la lista dada
conjuncion [b] = b
conjuncion (b : bs) = b && conjuncion bs 

disyuncion :: [Bool] -> Bool
--Prop: Dada una lista de booleanos devuelve True si alguno de sus elementos es True
--Prec: Debe haber al menos un booleano en la lista dada
disyuncion [b] = b
disyuncion (b : bs) = b  || disyuncion bs 

{-aplanar :: [[a]] -> [a]
--Prop: Dada una lista de listas, devuelve una única lista con todos sus elementos.
aplanar = [[]] = []
aplanar ([a] : [ns]) =  [a] : aplanar [ns] -}


pertenece :: Eq a => a -> [a] -> Bool
--Prop: Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sean iguales

pertenece  x [] = False
pertenece x (y : ys) = x == y || pertenece x ys 

apariciones :: Eq a => a -> [a] -> Int
--Prop: Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs
apariciones x [] = 0 
apariciones x (n : ns) = if  (x == n)
                            then 1 + apariciones x ns
                            else 0 + apariciones x ns

losMenoresA :: Int -> [Int] -> [Int]
--Prop: Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n

losMenoresA n  [_] =  []  
losMenoresA n (p : xs) =  agregar (cuandoEsMayorConLista n p)  (losMenoresA n xs) 

--subtarea
cuandoEsMayorConLista :: Int -> Int -> [Int]
--Prop: Dados dos numeros, si el primero es mayor al otro numero devuelve xs en una lista, sino devuelve una lista vacía
cuandoEsMayorConLista x y = if (x > y)
                        then [y]
                        else []

--subtarea
agregar ::  [a] ->  [a] ->  [a]
--Propósito combinar las dos listas dadas en una lista 
agregar  []  ys = ys 
agregar  (x : xs) ys = x : agregar xs ys  

{-
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
{- Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más
de n elementos -}
lasDeLongitudMayorA x [[]] = []
lasDeLongitudMayorA x ((n : ns) : nss) = -}

agregarAlFinal :: [a] -> a -> [a]
{- Prop: Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la
lista. -} 
agregarAlFinal [] x = agregar [] [x]
agregarAlFinal ys x =  agregar ys [x]
