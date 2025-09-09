--Práctica de ejercicios #3 - Tipos recursivos

--1.  Tipos recursivos simples

--1.1. Celdas con bolitas
data Color = Azul | Rojo
    deriving Show
instance Eq Color where
    Azul == Azul = True
    Rojo == Rojo = True
    _  == _ = False
data Celda = Bolita Color Celda | CeldaVacia
    deriving Show

--casos de prueba
celda1 = CeldaVacia
celda2 = Bolita Rojo (Bolita Rojo CeldaVacia)
celda3 = Bolita Azul celda2

nroBolitas :: Color -> Celda -> Int
--Prop: Dados un color y una celda, indica la cantidad de bolitas de ese color. 
nroBolitas _ CeldaVacia = 0
nroBolitas c (Bolita co cs) = unoSi (c == co) + nroBolitas c cs


--subtarea
unoSi :: Bool -> Int
--Prop: dada una condición devuelve 1 si se cumple o 0 si no se cumple.
unoSi c = if c then 1 else 0

poner :: Color -> Celda -> Celda
--Prop: Dado un color y una celda, agrega una bolita de dicho color a la celda.
poner c ce = Bolita c ce

sacar :: Color -> Celda -> Celda
--Prop: Dado un color y una celda, quita una bolita de dicho color de la celda. 
sacar c CeldaVacia = CeldaVacia
sacar c (Bolita co ce) = if c == co 
                            then ce
                            else Bolita co (sacar c ce)

ponerN :: Int -> Color -> Celda -> Celda
--Prop: Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda
ponerN 0 c ce = ce
ponerN n c ce = ponerN (n-1) c (poner c ce)

--1.2. Camino hacia el tesoro
data Objeto = Cacharro | Tesoro
    deriving Show
instance Eq Objeto where
    Cacharro == Cacharro = True
    Tesoro == Tesoro = True 
    _ == _ = False
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
    deriving Show

--Casos de prueba 
camino1 = Fin
camino2 = Nada Fin
camino3 = Cofre [] camino2
camino4 = Cofre [Cacharro] camino3
camino5 = Cofre [Cacharro, Cacharro, Tesoro] camino4
camino6 = Cofre [] (camino5)

hayTesoro :: Camino -> Bool
--Prop: Indica si hay un cofre con un tesoro en el camino
hayTesoro Fin = False
hayTesoro (Cofre o c) = vieneConTesoro o || hayTesoro c 
hayTesoro (Nada c) = hayTesoro c

--subtarea
vieneConTesoro :: [Objeto] -> Bool 
--Prop: Indica si en la lista de objetos dada hay un tesoro
vieneConTesoro [] = False
vieneConTesoro (x : xs) = if x == Tesoro
                            then True
                            else vieneConTesoro xs

pasosHastaTesoro :: Camino -> Int
{-Prop: Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro.
Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.
Precondición: tiene que haber al menos un tesoro -}
pasosHastaTesoro Fin = error "Debe haber al menos un cofre"
pasosHastaTesoro (Nada c) = 1 + pasosHastaTesoro c
pasosHastaTesoro (Cofre o c) = if vieneConTesoro o
                                    then 0
                                    else 1 + pasosHastaTesoro c

hayTesoroEn :: Int -> Camino -> Bool
{- Prop: Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
pasos es 5, indica si hay un tesoro en 5 pasos. -}
hayTesoroEn n c = if hayTesoro c 
                    then pasosHastaTesoro c == n
                    else hayTesoro c


alMenosNTesoros :: Int -> Camino -> Bool
--Prop: Indica si hay al menos n tesoros en el camino.
alMenosNTesoros n c = cantidadDeTesorosEn c >= n 

--subtarea
cantidadDeTesorosEn :: Camino -> Int
--Prop: Describe la cantidad de tesoros que hay en el camino dado
cantidadDeTesorosEn Fin = 0
cantidadDeTesorosEn (Nada c) = 0 + cantidadDeTesorosEn c 
cantidadDeTesorosEn (Cofre o c) = (cantDeTesorosEn o) + cantidadDeTesorosEn c


cantTesorosEntre :: Int -> Int -> Camino -> Int
{-Prop: Dado un rango de pasos, Describe la cantidad de tesoros que hay en ese rango. Por ejemplo, si
el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
incluidos tanto 3 como 5 en el resultado. -}
cantTesorosEntre pr sr c = (cantTesorosEnNPasos sr c) - cantTesorosEnNPasos (pr-1) c  

--subtarea
cantTesorosEnNPasos :: Int -> Camino -> Int
--Prop: Dado un camino y una cantidad de pasos, describe la cant de tesoros que hay tras dar esos pasos o al acabarse el camino
cantTesorosEnNPasos _ Fin = 0
cantTesorosEnNPasos 0 _ = 0
cantTesorosEnNPasos n (Nada c) = cantTesorosEnNPasos (n-1) c
cantTesorosEnNPasos n (Cofre o c) = cantDeTesorosEn o + cantTesorosEnNPasos (n-1) c  

--subtarea
cantDeTesorosEn :: [Objeto] -> Int
--Prop: Describe la cantidad de tesoros que hay en una lista de Objeto
cantDeTesorosEn [] = 0
cantDeTesorosEn (x : xs) = unoSi (x == Tesoro) + cantDeTesorosEn xs


--2. Tipos arbóreos

--2.1. Árboles binarios
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

--casos de prueba
a1 = EmptyT
a2 = NodeT (1 :: Int) (EmptyT) (EmptyT)
a3 = NodeT (3 :: Int) EmptyT a2
a4 = NodeT (2 :: Int) a3 a1

aa = EmptyT
ab = NodeT "b" aa aa
ac = NodeT "c" EmptyT ab

sumarT :: Tree Int -> Int
--Prop: Dado un árbol binario de enteros devuelve la suma entre sus elementos.
sumarT EmptyT = 0
sumarT (NodeT n tx ty) = n + sumarT tx + sumarT ty

sizeT :: Tree a -> Int
--Prop: Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size en inglés).
sizeT     EmptyT      = 0
sizeT (NodeT _ tx ty) = 1 + sizeT tx + sizeT ty 

mapDobleT :: Tree Int -> Tree Int
--Prop: Dado un árbol de enteros devuelve un árbol con el doble de cada número.
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT n tx ty) = (NodeT (n*2) (mapDobleT tx) (mapDobleT ty))

perteneceT :: Eq a => a -> Tree a -> Bool
--Prop:Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el árbol.
perteneceT _ EmptyT = False
perteneceT e (NodeT y tx ty) = ((e==y) || (perteneceT e tx) || (perteneceT e ty))

aparicionesT :: Eq a => a -> Tree a -> Int
--Prop: Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son iguales a e.
aparicionesT _ EmptyT = 0
aparicionesT x (NodeT y tx ty) = unoSi(x == y) + (aparicionesT x tx) + (aparicionesT x ty)

leaves :: Tree a -> [a]
--Prop: Dado un árbol devuelve los elementos que se encuentran en sus hojas.
--NOTA: en este tipo se define como hoja a un nodo con dos hijos vacíos.

leaves EmptyT = []
leaves (NodeT x tx ty) = singularSi x (esHoja(NodeT x tx ty)) ++ leaves tx ++ leaves ty
--subtarea
esHoja :: Tree a -> Bool
--Prop: Indica si el tree dado es una hoja.
esHoja (NodeT _ EmptyT EmptyT) = True
esHoja _ = False
--subtarea
singularSi :: a -> Bool -> [a]
--Prop: dado un elemento y un booleano devuelve ese elemento en una lista si se cumple la condición dada.
singularSi x c = if c then [x] else [] 

heightT :: Tree a -> Int
--Prop: Dado un árbol devuelve su altura.
{-Nota: la altura de un árbol (height en inglés), también llamada profundidad, es
la cantidad de niveles del árbol1. La altura para EmptyT es 0, y para una hoja es 1. -}
heightT EmptyT = 0
heightT (NodeT x tx ty) = unoSi(esHoja(NodeT x tx ty)) + heightT tx + heightT ty

mirrorT :: Tree a -> Tree a
--Prop: Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el derecho, en cada nodo del árbol.
mirrorT EmptyT = EmptyT
mirrorT (NodeT x tx ty) = NodeT x (mirrorT ty) (mirrorT tx)

toList :: Tree a -> [a]
--Prop: Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo in-order.
--Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo, luego la raiz y luego los elementos del hijo derecho.
toList EmptyT = []
toList (NodeT x tx ty) = elementosDe tx ++ [x] ++ elementosDe ty
--subtarea
elementosDe :: Tree a -> [a]
--Prop: dado un arbol binario devuelve los elementos de ese arbol
elementosDe EmptyT = []
elementosDe (NodeT x tx ty) = [x] ++ elementosDe tx ++ elementosDe ty

levelN :: Int -> Tree a -> [a]
{-Prop: Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El nivel de un nodo es la distancia que hay de la raíz hasta él. La distancia de la raiz a sí misma es 0, y la distancia de la raiz a uno de sus hijos es 1.
Nota: El primer nivel de un árbol (su raíz) es 0. -}
levelN _ EmptyT = []
levelN 0 (NodeT x _ _ ) = [x]
levelN n (NodeT _ tx ty) = levelN (n-1) tx ++ levelN (n-1) ty

listPerLevel :: Tree a -> [[a]]
--Prop: Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de dicho árbol.
listPerLevel EmptyT = []
listPerLevel (NodeT x tx ty) = [[x]] ++ combinarNiveles (listPerLevel tx) (listPerLevel ty)
--subtarea
combinarNiveles :: [[a]] -> [[a]] -> [[a]]
--Prop: combinar las dos primeras listas que se encuentran en otras listas.
combinarNiveles [] yss = yss
combinarNiveles xss [] = xss
combinarNiveles (xs : xss) (ys : yss) = (xs ++ ys) : combinarNiveles xss yss

ramaMasLarga :: Tree a -> [a]
--Prop: Devuelve los elementos de la rama más larga del árbol
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT x tx ty) = x : elegirLaMasLarga (ramaMasLarga tx) (ramaMasLarga ty)
--subtarea
elegirLaMasLarga :: [a] -> [a] -> [a]
--Prop: Dada dos listas de elementos devuelve la listas con mas elementos
elegirLaMasLarga x y = if length x > length y then x else y

todosLosCaminos :: Tree a -> [[a]]
--Prop: Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raíz hasta cualquiera de los nodos
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT x tx ty) = [[x]] ++  (agregarElemento x (todosLosCaminos tx ++ todosLosCaminos ty))
--subtarea
agregarElemento :: a -> [[a]] -> [[a]]
--Prop: dado un elemento y una listas de lista agrega ese elemento a cada lista
agregarElemento x [] = []
agregarElemento x (xs : xss) = (x : xs) : agregarElemento x xss 

--2.2. Expresiones Aritméticas
data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA
    deriving Show

--CasosDePrueba
uno = Valor 1
menosUno = Neg uno
dos = Valor 2
sumar2y1 = Sum uno dos
sumar2ym1 = Sum dos menosUno
mul2y2 = Prod dos dos

eval :: ExpA -> Int
--Prop: Dada una expresión aritmética devuelve el resultado evaluarla.
eval  (Valor x) = x
eval  (Sum x y) = (eval x) + (eval y)
eval (Prod x y) = (eval x) * (eval y)
eval   (Neg x)  = - (eval x)

simplificar :: ExpA -> ExpA
{-Dada una expresión aritmética, la simplifica según los siguientes criterios (descritos utilizando notación matemática convencional):
a) 0 + x = x + 0 = x
b) 0 * x = x * 0 = 0
c) 1 * x = x * 1 = x
d) - (- x) = x -}
simplificar (Valor x) = (Valor x)
simplificar (Sum x y) = simplificarSumaCero (Sum (simplificar x) (simplificar y))
simplificar (Prod x y) = simplificarProdUno (simplificarProdCero (Prod (simplificar x) (simplificar y)))
simplificar (Neg x) = simplificarNeg (Neg (simplificar x))
simplificar (Neg x) = simplificarNeg (Neg (simplificar x))


--Subtareas
esCero :: ExpA -> Bool
--Prop: indica si la expresion aritmetica dada es cero.
esCero x = (eval x) == 0

esUno :: ExpA -> Bool
--Prop: Indica si la expresion aritmetica dada es uno
esUno x = (eval x) == 1
{-No Usada
esNegativo :: ExpA -> Bool
--Prop: Indica si la expresion aritmetica dada es negativa
esNegativo x = (eval x) < 0 -} 

simplificarSumaCero :: ExpA -> ExpA 
--Prop: Dada una expresiones de suma devuelve el otro numero que no es cero, si los dos no lo son no hace nada 
--Prec: la ExpA dada debe ser un Sum
simplificarSumaCero (Sum x y) = if esCero x
                        then y
                        else if esCero y 
                            then x
                            else (Sum x y)

simplificarProdCero :: ExpA -> ExpA
--Prop: dada una expresion de Prod devuelve 0 si uno de las dos expresiones son cero, sino no hace nada
--Prec: la ExpA dada debe ser un Prod
simplificarProdCero (Prod x y) = if (esCero x || esCero y )
                                    then (Valor 0)
                                    else (Prod x y)

simplificarProdUno :: ExpA -> ExpA
--Prop: dada una expresion de producto devuelve el otro numero que no  sea uno, si ninguno es uno no hace nada
--Prec: la ExpA dada debe ser un Prod
simplificarProdUno (Prod x y) = if esUno x 
                                    then y 
                                    else if esUno y
                                        then x
                                        else (Prod x y)

simplificarNeg :: ExpA -> ExpA 
--Prop: Dado un Neg x simplifica si x ya es un negativo, sino no hace nada
--Prec: La ExpA dada debe ser Neg
simplificarNeg (Neg (Neg x)) = x 
simplificarNeg (Neg x) = (Neg x)