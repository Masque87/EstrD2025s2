--Práctica de ejercicios #4 - Ejercicios Integradores

--1. Pizzas
data Pizza = Prepizza | Capa Ingrediente Pizza
    deriving Show
data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int
    deriving Show

--Casos de prueba
p1 = Prepizza
p2 = Capa Salsa Prepizza
p3 = Capa Queso p2
p4 = Capa Jamon p3
p5 = Capa Queso (Capa Queso p4)
p6 = Capa Jamon (Capa Queso (Capa Jamon Prepizza))
p7 = Capa Salsa (Capa Jamon (Capa (Aceitunas 10) (Capa (Aceitunas 4) Prepizza)))

cantidadDeCapas :: Pizza -> Int
--Prop: Dada una pizza devuelve la cantidad de ingredientes
cantidadDeCapas Prepizza = 0 
cantidadDeCapas (Capa t p) = 1 + cantidadDeCapas p

armarPizza :: [Ingrediente] -> Pizza
--Prop: Dada una lista de ingredientes construye una pizza
armarPizza [] = Prepizza
armarPizza (x : xs) = (Capa x (armarPizza xs))

sacarJamon :: Pizza -> Pizza
--Prop: Le saca los ingredientes que sean jamón a la pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa i r) = if esJamon i
                            then r
                            else (Capa i  (sacarJamon r))
--subtarea
esJamon :: Ingrediente -> Bool
--Prop: indica si el ingrediente dado es Jamon
esJamon Jamon = True
esJamon _ = False 

tieneSoloSalsaYQueso :: Pizza -> Bool
--Prop: Dice si una pizza tiene solamente salsa y queso (o sea, no tiene de otros ingredientes. En particular, la prepizza, al no tener ningún ingrediente, debería dar verdadero.)
tieneSoloSalsaYQueso Prepizza = True
tieneSoloSalsaYQueso (Capa i r) = if esJamonOQueso i 
                                    then tieneSoloSalsaYQueso r
                                    else False
--subtarea
esJamonOQueso :: Ingrediente -> Bool
--Prop: Indica si el ingrediente dado es Jamon o Queso
esJamonOQueso i = esJamon i || esQueso i
--subtarea

esQueso :: Ingrediente -> Bool
--Prop: Indica si el ingrediente dado es Queso
esQueso Queso = True
esQueso _     = False

duplicarAceitunas :: Pizza -> Pizza
--Prop: Recorre cada ingrediente y si es aceitunas duplica su cantidad
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa i r) = if esAceituna i 
                                then (Capa (duplicarAceituna i) (duplicarAceitunas r))
                                else (Capa i (duplicarAceitunas r))

--subtarea 
esAceituna :: Ingrediente -> Bool
--Prop: indica si el ingrediente dado es una aceituna
esAceituna (Aceitunas _) = True
esAceituna _            = False

--subtarea
duplicarAceituna :: Ingrediente -> Ingrediente
--Prop: Dado una Aceituna duplica su cantidad. Prec: el ingrediente dado debe ser una Aceituna
duplicarAceituna (Aceitunas n) = (Aceitunas (n*2))
duplicarAceituna _ = error "El ingrediente dado no es una aceituna"

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
{-Prop: Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de ingredientes de la pizza, y la respectiva pizza como segunda componente. -}
cantCapasPorPizza [] = []
cantCapasPorPizza (p : ps) = cantCapasDePizza p : cantCapasPorPizza ps 

--subtarea
cantCapasDePizza :: Pizza -> (Int, Pizza)
--Prop: Dada una pizza devuelve un par donde la primera componente es la cantidad de ingredientes de la pizza, la respectiva pizza como segundo componente 
cantCapasDePizza p = (cantidadDeCapas p, p)

--2. Mapa de tesoros (con bifurcaciones)

data Dir = Izq | Der
data Objeto = Tesoro | Chatarra
data Cofre = Cofre [Objeto]
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa

--casos de prueba
m0 = Fin (Cofre [])
m1 = Bifurcacion (Cofre [Chatarra]) (Fin (Cofre [])) (Fin (Cofre [Chatarra, Tesoro]))
hayTesoro :: Mapa -> Bool
--Indica si hay un tesoro en alguna parte del mapa.
hayTesoro (Fin c) = esCofreConTesoro c
hayTesoro (Bifurcacion c i d) = esCofreConTesoro c || hayTesoro i || hayTesoro d 

--subtarea
esCofreConTesoro :: Cofre -> Bool
esCofreConTesoro (Cofre os) = esListaConTesoros os

--subtarea
esListaConTesoros :: [Objeto] -> Bool 
esListaConTesoros [] = False
esListaConTesoros (o : os) = esTesoro o || esListaConTesoros os 

--subtarea
esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False


hayTesoroEn :: [Dir] -> Mapa -> Bool
--Prop: Indica si al final del camino hay un tesoro 
hayTesoroEn (d : ds) (Fin c) = error "Las direcciones dadas son Mayores al mapa dado"  
hayTesoroEn [] (Fin c) = esCofreConTesoro c
hayTesoroEn [] (Bifurcacion c iz de) = esCofreConTesoro c  
hayTesoroEn (d : ds) (Bifurcacion c iz de) = if esIzq d 
                                                then hayTesoroEn ds iz 
                                                else hayTesoroEn ds de 

--subtarea
esIzq :: Dir -> Bool 
--Prop: indica si la direccion dada es izquierda
esIzq Izq = True 
esIzq _   = False

caminoAlTesoro :: Mapa -> [Dir]
--Prop: Indica el camino al tesoro. Precondición: existe un tesoro y es único
caminoAlTesoro (Fin c) = if esCofreConTesoro c 
                            then []
                            else error "Debe existir un tesoro"
caminoAlTesoro (Bifurcacion c iz de) = if esCofreConTesoro c
                                            then []
                                            else if hayTesoro iz 
                                                    then Izq : caminoAlTesoro iz 
                                                    else Der : caminoAlTesoro de 

{-caminoAlTesoro (Bifurcacion c iz de) = caminoAlTesoroSegun (esCofreConTesoro c) (hayTesoro iz) iz de
caminoAlTesoroSegun :: Bool -> Bool -> Mapa -> Mapa -> [Dir]
caminoAlTesoroSegun True  _     _  _  = []
caminoAlTesoroSegun False True  iz _  = Izq : caminoAlTesoro iz
caminoAlTesoroSegun False False _  de = Der : caminoAlTesoro de
-}

caminoDeLaRamaMasLarga :: Mapa -> [Dir]
--Prop: Indica el camino de la rama más larga.
caminoDeLaRamaMasLarga (Fin c) = []
caminoDeLaRamaMasLarga (Bifurcacion c iz de) = elegirLaMasLarga (Izq : caminoDeLaRamaMasLarga iz) (Der : caminoDeLaRamaMasLarga de)

--subtarea
elegirLaMasLarga :: [a] -> [a] -> [a]
--Prop: dada dos listas devuelve la lista con más elementos
elegirLaMasLarga l1 l2 = if length l1 > length l2 
                            then l1 
                            else l2 

tesorosPorNivel :: Mapa -> [[Objeto]]
--Prop: Devuelve los tesoros separados por nivel en el árbol.
tesorosPorNivel (Fin c) = [tesoroSiHayEn c]
tesorosPorNivel (Bifurcacion c iz de) = [tesoroSiHayEn c ] ++ combinarNiveles (tesorosPorNivel iz) (tesorosPorNivel de)

--subtarea
tesoroSiHayEn :: Cofre -> [Objeto]
tesoroSiHayEn (Cofre []) = []
tesoroSiHayEn (Cofre (o : os)) = if esTesoro o 
                                    then o : tesoroSiHayEn (Cofre os) 
                                    else tesoroSiHayEn (Cofre os) 

--subtarea
combinarNiveles :: [[a]] -> [[a]] -> [[a]]
--Prop: combinar las dos primeras listas que se encuentran en otras listas.
combinarNiveles [] yss = yss
combinarNiveles xss [] = xss
combinarNiveles (xs : xss) (ys : yss) = (xs ++ ys) : combinarNiveles xss yss

{-
listPerLevel :: Tree a -> [[a]]
--Prop: Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de dicho árbol.
listPerLevel EmptyT = []
listPerLevel (NodeT x tx ty) = [[x]] ++ combinarNiveles (listPerLevel tx) (listPerLevel ty)
--subtarea
combinarNiveles :: [[a]] -> [[a]] -> [[a]]
--Prop: combinar las dos primeras listas que se encuentran en otras listas.
combinarNiveles [] yss = yss
combinarNiveles xss [] = xss
combinarNiveles (xs : xss) (ys : yss) = (xs ++ ys) : combinarNiveles xss yss-}

todosLosCaminos :: Mapa -> [[Dir]]
--Prop: Devuelve todos lo caminos en el mapa.
todosLosCaminos (Fin c) = [[]]
todosLosCaminos (Bifurcacion c iz de) = (agregarElemento Izq (todosLosCaminos iz) ++ agregarElemento Der (todosLosCaminos de))

--subtarea
agregarElemento :: a -> [[a]] -> [[a]]
--Prop: dado un elemento y una listas de lista agrega ese elemento a cada lista
agregarElemento x [] = []
agregarElemento x (xs : xss) = (x : xs) : agregarElemento x xss


{- todosLosCaminos :: Tree a -> [[a]]
--Prop: Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raíz hasta cualquiera de los nodos
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT x tx ty) = [[x]] ++  (agregarElemento x (todosLosCaminos tx ++ todosLosCaminos ty)) 
--subtarea
agregarElemento :: a -> [[a]] -> [[a]]
--Prop: dado un elemento y una listas de lista agrega ese elemento a cada lista
agregarElemento x [] = []
agregarElemento x (xs : xss) = (x : xs) : agregarElemento x xss -}
