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

