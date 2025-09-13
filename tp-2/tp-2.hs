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
sucesores [] = []
sucesores (n : ns) = (n + 1) : sucesores ns


conjuncion :: [Bool] -> Bool
--Prop: Dada una lista de booleanos devuelve True si todos sus elementos son True

conjuncion [] = True
conjuncion [b] = b
conjuncion (b : bs) = b && conjuncion bs 

disyuncion :: [Bool] -> Bool
--Prop: Dada una lista de booleanos devuelve True si alguno de sus elementos es True

disyuncion [] = False
disyuncion [b] = b
disyuncion (b : bs) = b  || disyuncion bs 

aplanar :: [[a]] -> [a]
--Prop: Dada una lista de listas, devuelve una única lista con todos sus elementos.
aplanar [] = []
aplanar (xs : xss) =  xs ++ aplanar xss  


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

losMenoresA n  [] =  []  
losMenoresA n (p : xs) =  agregar (cuandoEsMayorConLista n p)  (losMenoresA n xs) 

--subtarea
cuandoEsMayorConLista :: Int -> Int -> [Int]
--Prop: Dados dos numeros, si el primero es mayor al otro numero devuelve xs en una lista, sino devuelve una lista vacía
cuandoEsMayorConLista x y = if (x > y)
                        then [y]
                        else []


lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
{- Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más
de n elementos -}
lasDeLongitudMayorA x []= []
lasDeLongitudMayorA x (ns : nss) = if (longitud ns > x) 
                                        then [ns] ++ lasDeLongitudMayorA x nss
                                        else lasDeLongitudMayorA x nss

agregarAlFinal :: [a] -> a -> [a]
{- Prop: Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la
lista. -} 
agregarAlFinal [] x = agregar [] [x]
agregarAlFinal ys x =  agregar ys [x]


agregar ::  [a] ->  [a] ->  [a]
--Propósito combinar las dos listas dadas en una lista 
agregar  []  ys = ys 
agregar  (x : xs) ys = x : agregar xs ys  


reversa :: [a] -> [a]
{- Prop: Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Definida en Haskell como reverse-} 
reversa [] = [] 
reversa (x : xs) = (reversa xs) ++ [x] 


zipMaximos :: [Int] -> [Int] -> [Int]
{-Prop: Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que las listas no necesariamente tienen la misma longitud -}
zipMaximos [] []  = [] 
zipMaximos [] ys = ys
zipMaximos xs [] = xs
zipMaximos (x : xs) (y : ys) =  maxDelPar (x, y) : zipMaximos xs ys


--subtarea
maxDelPar :: (Int, Int) -> Int 
maxDelPar (x, y) = if (x > y) 
                then x
                else y

elMinimo :: Ord a => [a] -> a
{- Prop: Dada una lista devuelve el mínimo
Prec: La lista no puede ser vacia -}
elMinimo [] = error "La lista es vacia"
elMinimo [x] = x
elMinimo (x : xs) =  min x (elMinimo xs)



-- 2. Recursión sobre números
factorial :: Int -> Int
{- Prop: Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta
llegar a 0. Si n es 0 devuelve 1. 
Prec: No puede ser un numero negativo -}
factorial 0 = 1
factorial x = x * factorial (x-1)


cuentaRegresiva :: Int -> [Int]
{- Prop: Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre
n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía. 
Prec: No puede ser un numero negativo-}
cuentaRegresiva  x  = if x < 1
                        then []
                        else  x : cuentaRegresiva (x-1)

repetir :: Int -> a -> [a]
--Prop: Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces
--Prec : El numero n debe ser mayor a 0
repetir  0 _ = []
repetir  x t = t : repetir (x-1) t 

losPrimeros :: Int -> [a] -> [a]
{- Prop: Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs.
Si la lista es vacía, devuelve una lista vacía. 
Prec: No puede ser el numero n un numero negativo-}
losPrimeros 0 (n : ns) = []
losPrimeros x (n : ns) = n : losPrimeros (x-1) ns
losPrimeros _ [] = []

sinLosPrimeros :: Int -> [a] -> [a]
{- Prop: Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista
recibida. Si n es cero, devuelve la lista completa
Prec: el numero n no puede ser un numero negativo -}
sinLosPrimeros 0 ns = ns
sinLosPrimeros x [] = []
sinLosPrimeros x (n : ns) = sinLosPrimeros (x-1) ns


--3. Registros
--1
data Persona = P String Int 
--               Nombre Edad
    deriving Show

--SujetosDePrueba
lucas = P "LUCAS" 14
pablo = P "PABLO" 21
maria = P "MARIA" 31 

mayoresA :: Int -> [Persona] -> [Persona]
--Prop: Dados una edad y una lista de personas devuelve a las personas mayores a esa edad
mayoresA _ [] = []
mayoresA x ((P n e) : ps) = if (x < e) 
                                then (P n e) : mayoresA x ps   
                                else mayoresA x ps

promedioEdad :: [Persona] -> Int
--Prop: Dada una lista de personas devuelve el promedio de edad entre esas personas. Precondición: la lista al menos posee una persona.
promedioEdad [] = error "La lista no puede ser vacia"
promedioEdad ((P n e) : []) = e
promedioEdad xs = div (sumaDeEdades xs)  (longitud xs) 


--Subtarea
sumaDeEdades :: [Persona] -> Int
{- Prop: Dada una lista de personas devuelve la edad de esas personas sumadas, si la lista es vacía devuelve 0 -}
sumaDeEdades [] = 0
sumaDeEdades ((P n e) : xs) = e + sumaDeEdades xs 


elMasViejo :: [Persona] -> Persona
{- Prop: Dada una lista de personas devuelve la persona más vieja de la lista. Precondición: la
lista al menos posee una persona -}
elMasViejo [] = error "la lista debe poseer al menos una persona"
elMasViejo (x : []) = x
elMasViejo (x : xs) = if edadDeUnaPersona x >= edadDeUnaPersona (elMasViejo xs) 
                                then x
                                else elMasViejo xs

--subtarea 
edadDeUnaPersona :: Persona -> Int 
--Prop: Dada una persona devuelve su edad
edadDeUnaPersona (P _ e) = e
--2
data TipoDePokemon = Agua | Fuego | Planta
instance Eq TipoDePokemon where
    Agua == Agua = True
    Fuego == Fuego = True 
    Planta == Planta = True
    _ == _  = False
data Pokemon = ConsPokemon TipoDePokemon Int
data Entrenador = ConsEntrenador String [Pokemon]

--Sujeto de Prueba
piplup= ConsPokemon Agua 3
litleo = ConsPokemon Fuego 4
rowlet = ConsPokemon Planta 2
green = ConsEntrenador "GREEN" [piplup, litleo, rowlet]
lilly = ConsEntrenador "LILLY" []

cantPokemon :: Entrenador -> Int
{- Prop: Devuelve la cantidad de Pokémon que posee el entrenador -}
cantPokemon (ConsEntrenador _ ps) = longitud ps

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
-- Prop: Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantPokemonDe t (ConsEntrenador n []) = 0
cantPokemonDe t (ConsEntrenador n ((ConsPokemon x _) : ps)) = if (t == x)
                                                                then 1 + cantPokemonDe t (ConsEntrenador n ps)
                                                                else 0 + cantPokemonDe t (ConsEntrenador n ps)
{-
cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
{-Prop: Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo pertenecientes al
primer entrenador, que le ganarían a todos los Pokemon del segundo entrenador. -}
cuantosDeTipo_De_LeGananATodosLosDe_ t (ConsEntrenador _ []) _ = 0
cuantosDeTipo_De_LeGananATodosLosDe_ t (ConsEntrenador n (p : ps)) e2 = if (tipoDe p == t && leGanaATodosLosDe p (pokemonDe e2))
                                                                            then 1 + cuantosDeTipo_De_LeGananATodosLosDe_ t (ConsEntrenador n ps) e2
                                                                            else cuantosDeTipo_De_LeGananATodosLosDe_ t (ConsEntrenador n ps) e2 -}

cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
{-Prop: Dados dos entrenadores, Describe la cantidad de Pokemon de cierto tipo pertenecientes al primer entrenador, que le ganarían a todos los Pokemon del segundo entrenador. -}
cuantosDeTipo_De_LeGananATodosLosDe_ t (ConsEntrenador _ ps) e2 = cuantosDeTipoEnLista t ps (pokemonDe e2)

--subtarea
cuantosDeTipoEnLista :: TipoDePokemon -> [Pokemon] -> [Pokemon] -> Int
--Prop: Describe dado los pokemon de la primera lista que son del tipo dado, que le ganan a todos los pokemons de la segunda lista
cuantosDeTipoEnLista _ [] _ = 0
cuantosDeTipoEnLista t (p:ps) p2 = if tipoDe p == t && leGanaATodosLosDe p p2
                                    then 1 + cuantosDeTipoEnLista t ps p2
                                    else cuantosDeTipoEnLista t ps p2

--subtarea
tipo_LeGanaA_ :: TipoDePokemon -> TipoDePokemon -> Bool
-- Prop: indica si un tipo le gana a otro, en caso de ser mismo tipo devuelve False
tipo_LeGanaA_ Agua Fuego = True
tipo_LeGanaA_ Fuego Planta = True
tipo_LeGanaA_ Planta Agua = True
tipo_LeGanaA_    _     _ = False 

--subtarea
tipoDe :: Pokemon -> TipoDePokemon
--Prop: Dado un pokemon indica cual es su tipo 
tipoDe (ConsPokemon t _) = t

--subtarea 
leGanaA :: Pokemon -> Pokemon -> Bool
{-Prop: indica si un pokemon le gana a otro en base al tipo. -}
leGanaA x y = tipo_LeGanaA_ (tipoDe x) (tipoDe y)

--subtarea 
pokemonDe :: Entrenador -> [Pokemon]
--Prop: dado un entrenador devuelve la lista de sus pokemons
pokemonDe (ConsEntrenador p n) = n

--subtarea 
leGanaATodosLosDe :: Pokemon -> [Pokemon] -> Bool
--Prop: Indica si el pokemon dado le gana a todos los demás pokemon de la lista dada segun el tipo
leGanaATodosLosDe x [] = True
leGanaATodosLosDe x (y : ys) = leGanaA  x y && leGanaATodosLosDe x ys

esMaestroPokemon :: Entrenador -> Bool
--Prop: Dado un entrenador, devuelve True si posee al menos un Pokémon de cada tipo posible.
esMaestroPokemon (ConsEntrenador  _ []) = False 
esMaestroPokemon x = if (cantPokemonDe Agua x  >= 1 && cantPokemonDe Fuego x >= 1 && cantPokemonDe Planta x >= 1)
                        then True
                        else False

--3
data Seniority = Junior | SemiSenior | Senior
    deriving Show
instance Eq Seniority where
    Junior == Junior = True
    SemiSenior == SemiSenior = True
    Senior == Senior = True 
    _      == _      = False

data Proyecto = ConsProyecto String
    deriving (Show, Eq)

data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
    deriving Show

data Empresa = ConsEmpresa [Rol]
    deriving Show

--Sujetos de prueba
counter = ConsProyecto "Counter"
fortnite = ConsProyecto "Fortnite"
lol = ConsProyecto "lol"

primerDev = Developer Senior counter
primerMan = Management Senior fortnite 
segundoDev = Developer SemiSenior counter
segundoMan = Management Junior lol

ubisoft = ConsEmpresa [primerDev, primerMan, segundoDev, segundoMan, primerDev]
nintendo = ConsEmpresa []

--1
proyectos :: Empresa -> [Proyecto]
--Prop: Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos.
proyectos x = sinRepetidos (todosLosProyectos x)



--subtarea
proyectoDeUnRol :: Rol -> Proyecto
--Prop: Dado un rol devuelve el proyecto en el que trabaja 
proyectoDeUnRol (Developer _ p) = p
proyectoDeUnRol (Management _ p) = p

--subtarea
sinRepetidos :: Eq a => [a] -> [a]
--Prop: Dada una lista de a devuelve una lista con los elementos de tal lista sin repetirse
sinRepetidos [] = []
sinRepetidos (x : xs) = if not (estaEnLaLista x xs) 
                            then x : sinRepetidos xs 
                            else sinRepetidos xs

--subtarea
estaEnLaLista :: Eq a => a -> [a] -> Bool
--Prop: Dada una lista y un elemento del mismo tipo que los elementos de la lista, indica con un True si ese elemento está en la lista, sino False
estaEnLaLista _ [] = False
estaEnLaLista y (x : xs) = if y == x 
                                then True 
                                else  estaEnLaLista y xs  

--subtarea
todosLosProyectos :: Empresa -> [Proyecto]
--Prop: Dada una Empresa denota la lista de proyectos en los que trabaja, con elementos repetidos
todosLosProyectos (ConsEmpresa []) = []
todosLosProyectos (ConsEmpresa (x : xs))  = proyectoDeUnRol x : todosLosProyectos (ConsEmpresa xs)  


--2
{-
losDevSenior :: Empresa -> [Proyecto] -> Int
{- Prop: Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertecen
además a los proyectos dados por parámetro. -}
losDevSenior _ [] = 0
losDevSenior e (x : xs) = longitud (losDevsSeniorQueTrabajanEnElProyecto e x) + (losDevSenior e xs)


losDevsSeniorQueTrabajanEnElProyecto :: Empresa -> Proyecto ->  [Rol]
--Dado una Empresa y un proyecto devuelve la lista de Developer Senior que trabajan en el proyecto dado
losDevsSeniorQueTrabajanEnElProyecto (ConsEmpresa []) _ = []
losDevsSeniorQueTrabajanEnElProyecto (ConsEmpresa (x : xs)) p = if ((esDevSenior x) && (devTrabajaEnProyecto x p)) 
                                                                    then x : losDevsSeniorQueTrabajanEnElProyecto (ConsEmpresa xs) p
                                                                    else losDevsSeniorQueTrabajanEnElProyecto (ConsEmpresa xs) p  -}

losDevSenior :: Empresa -> [Proyecto] -> Int
{- Prop: Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertecen
además a los proyectos dados por parámetro. -}
losDevSenior (ConsEmpresa _) [] = 0
losDevSenior (ConsEmpresa rs) (p:ps) = longitud (losDevsSeniorQueTrabajanEnElProyecto rs p) + losDevSenior (ConsEmpresa rs) ps

losDevsSeniorQueTrabajanEnElProyecto :: [Rol] -> Proyecto -> [Rol]
--Prop: Dado una lista de Rol y un Proyecto, devuelve los developer Senior que trabajan en el proyecto dado
losDevsSeniorQueTrabajanEnElProyecto [] _ = []
losDevsSeniorQueTrabajanEnElProyecto (x:xs) p = if esDevSenior x && devTrabajaEnProyecto x p
                                                    then x : losDevsSeniorQueTrabajanEnElProyecto xs p
                                                    else losDevsSeniorQueTrabajanEnElProyecto xs p


--subtarea
esDevSenior :: Rol -> Bool
--Prop: Indica si el Rol dado es un developer Senior, si es asi devuelve True, caso contrario False
esDevSenior (Developer Senior _ ) = True
esDevSenior _ = False 

--subtarea
devTrabajaEnProyecto :: Rol -> Proyecto -> Bool 
--Prop: indica si un Developer  trabaja en el proyecto dado, si lo hace True, caso contrario False
devTrabajaEnProyecto (Developer _ p) e =  p == e 
devTrabajaEnProyecto _ _ = False


--3

cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
--Prop: Indica la cantidad de empleados que trabajan en alguno de los proyectos dados
cantQueTrabajanEn [] _ = 0
cantQueTrabajanEn _ (ConsEmpresa []) = 0
cantQueTrabajanEn ps (ConsEmpresa (x : xs)) = if trabajaEnAlguno x ps 
                                                then 1 + cantQueTrabajanEn ps (ConsEmpresa xs)
                                                else cantQueTrabajanEn ps (ConsEmpresa xs)



--subtarea
trabajaEn :: Rol -> Proyecto -> Bool
--Prop: indica si el empleado dado trabaja en el proyecto dado, si es afirmativo con True, caso contrario False
trabajaEn e p = ((proyectoDeUnRol e) == p) 

--subtarea 
trabajaEnAlguno :: Rol -> [Proyecto] -> Bool
--Prop: indica si el empleado dado trabaja en alguno de los proyectos dados, si es afirmativo con True, caso contrario False
trabajaEnAlguno _ [] = False
trabajaEnAlguno e (x : xs) = if ((proyectoDeUnRol e) == x)
                                        then True
                                        else trabajaEnAlguno e xs

--4

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
{- Prop: Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su
cantidad de personas involucradas. -}
asignadosPorProyecto (ConsEmpresa []) = []
asignadosPorProyecto (ConsEmpresa (x:xs)) = sumarAlProyecto (proyectoDeUnRol x) (asignadosPorProyecto (ConsEmpresa xs))

-- subtarea
sumarAlProyecto :: Proyecto -> [(Proyecto, Int)] -> [(Proyecto, Int)]
--Prop: dado un proyecto y una lista de pares de proyectos con su cantidad de involucrados, incrementa la cantidad de involucrados en ese proyecto.
sumarAlProyecto p [] = [(p,1)]
sumarAlProyecto p ((p2, n): xs) = if p == p2
                                        then (p, n+1) : xs
                                        else (p2, n) : sumarAlProyecto p xs


{-
asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
Prop: Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su
cantidad de personas involucradas. 
asignadosPorProyecto e =  asignadosPorProyectoConCantidad (proyectos e) e 

--subtarea
asignadoAlProyecto :: Proyecto -> Empresa -> [(Proyecto, Int)]
--Prop: Dado un proyecto indica la cantidad de empleados de la empresa dada que trabajan en el proyecto 
asignadoAlProyecto p e = [(p, (cantQueTrabajanEn [p] e))]
-- subtarea
asignadosPorProyectoConCantidad :: [Proyecto] -> Empresa -> [(Proyecto, Int)]
asignadosPorProyectoConCantidad [] _ = []
asignadosPorProyectoConCantidad (x:xs) e = asignadoAlProyecto x e ++ asignadosPorProyectoConCantidad xs e

Estás recorriendo muchísimas veces todos los proyectos de la empresa, lo que no es muy bueno. Primero le sacás los repetidos, luego por cada uno volvés a recorrer para contar cuántos hay. 
-}