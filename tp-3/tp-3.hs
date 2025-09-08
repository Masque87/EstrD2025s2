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
                            else sacar c ce

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
