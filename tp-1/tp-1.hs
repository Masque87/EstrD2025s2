-- 2) Numeros Enteros
sucesor :: Int -> Int
sucesor a = a + 1

sumar :: Int -> Int -> Int
sumar x y = x + y

divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto x y = ((div x y), (mod x y))

maxDelPar :: (Int, Int) -> Int 
maxDelPar (x, y) = if (x > y) 
                then x
                else y 

{- maxDelPar (divisionYResto (sumar 5 5) (sucesor 0))
maxDelPar (divisionYResto (sumar 8 2) (sucesor 0))
maxDelPar (divisionYResto (sumar 6 4) (sucesor 0))
maxDelPar (divisionYResto (sumar 9 1) (sucesor 0)) -}

-- 3 Tipos Enumerativos
-- 1)
data Dir = Norte | Este | Sur | Oeste
    deriving Show



opuesto :: Dir -> Dir
opuesto  Este = Oeste
opuesto  Norte = Sur
opuesto  Sur = Norte
opuesto  Oeste = Este

iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Este Este = True
iguales Sur Sur = True
iguales Oeste Oeste = True
iguales _ _ = False


siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = Norte

-- 2)

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo 
                deriving Show
primerDiaDeSemana :: DiaDeSemana
primerDiaDeSemana = Lunes

{-
esPrimerDiaDeSemana :: DiaDeSemana -> Bool
esPrimerDiaDeSemana x = if (x == Lunes)
                        then True
                        else False 
-}

ultimoDiaDeSemana :: DiaDeSemana
ultimoDiaDeSemana = Domingo

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (primerDiaDeSemana, ultimoDiaDeSemana)

empiezaConM :: DiaDeSemana -> Bool
empiezaConM Lunes = False
empiezaConM Jueves = False
empiezaConM Viernes = False
empiezaConM Sabado = False
empiezaConM Domingo = False
empiezaConM _ = True


disyuncionPM :: Bool -> Bool -> Bool
disyuncionPM False False = False
disyuncionPM _ _ = True


vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
-- Prec: Poner Los Argumentos en orden semanal, ej Miercoles y Lunes poner primero Lunes.
-- Nota: Preguntar por disyuncion y Conjuncion en Haskell
vieneDespues Lunes Martes = True
vieneDespues Martes Miercoles = True
vieneDespues Miercoles Jueves = True
vieneDespues Jueves Viernes = True
vieneDespues Viernes Sabado = True
vieneDespues Sabado Domingo = True 
vieneDespues Domingo Lunes = True
vieneDespues _ _ = False 

{-
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio x = (if disyuncionPM x = primerDiaDeSemana x =ultimoDiaDeSemana)
                then False
                else True 
FALLA-}

negar :: Bool -> Bool
negar x = if (x == True)
        then False
        else True

implica :: Bool -> Bool -> Bool
implica x y = if (x)
            then y
            else True

yTambien :: Bool -> Bool -> Bool
yTambien x y = if (x)
            then y 
            else False

oBien :: Bool -> Bool -> Bool
oBien x y = if (x)
    then x
    else y 


-- 4 Registros
data Persona = P String Int String 
                --Nombre Edad DNI
                deriving Show

nombre :: Persona -> String
nombre (P n e d) = n

edad :: Persona -> Int
edad (P n e d) = e

crecer :: Persona -> Persona
--Prop: Aumenta en uno la edad de la persona
crecer (P n e d) = (P n (e + 1) d)


pablo = P  "HOLA" 17 "324980481" 
-- Sujeto de Prueba
roberto = P "Roberto" 18 "29420823"

cambioDeNombre :: String -> Persona -> Persona
--PROP: Dados un nombre y una persona, devuelve una persona con la edad de la persona y el nuevo nombre.
cambioDeNombre x (P n e d) = (P x e d)

esMayorQueLaOtra :: Persona -> Persona -> Bool
--PROP:  Dadas dos personas indica si la primera es mayor que la segunda.
esMayorQueLaOtra (P n e d) (P m r f) = e > r

laQueEsMayor :: Persona -> Persona -> Persona
--Prop: Dadas dos personas devuelve a la persona que sea mayor.

laQueEsMayor x y = if (esMayorQueLaOtra x y)
                then x
                else y

--2)
data Pokemon = Z  TipoDePokemon Int
                --    _         porcentaje de Energía
data TipoDePokemon = Agua | Fuego | Planta deriving (Eq, Show)

charmander = Z Fuego 8
squirtle = Z Agua 10

data Entrenador = E String Pokemon Pokemon
--                  nombre  _        _

carlos = E "Carlos" charmander squirtle

superaA :: Pokemon -> Pokemon -> Bool
--Prop: Dados dos Pokémon indica si el primero, en base al tipo, es superior al segundo. Agua
-- supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
superaA (Z t p) (Z y a) = leGanaPorTipo t y 


leGanaPorTipo :: TipoDePokemon -> TipoDePokemon -> Bool
leGanaPorTipo x y = if ((x == Planta && y == Agua)  || (x == Fuego && y == Planta)  || (x == Agua && y == Fuego))
    then True
    else False


cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
--Prop: Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantidadDePokemonDe t (E n p q) = if (tieneDeTipo t (E n p q)) 
                                then cuantosPokeDeTipoTiene t (E n p q)
                                else 0

tieneDeTipo :: TipoDePokemon -> Entrenador -> Bool
tieneDeTipo t (E n (Z tipo1 por1) (Z tipo2 por2)) = if ((t == tipo1) || (tipo2 == t)) 
                        then True
                        else False

cuantosPokeDeTipoTiene :: TipoDePokemon -> Entrenador -> Int
--Prec: Debe el entrenador tener al menos un pokemon del tipo dado
cuantosPokeDeTipoTiene t (E _ (Z tipo1 por1) (Z tipo2 por2)) = if ((tipo1 == t) && (tipo2 == t))
                                    then 2
                                    else 1




juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
--Prop: Dado un par de entrenadores, devuelve a sus Pokémon en una lista.
juntarPokemon ((E n1 p1 p2), (E n2 p3 p4)) = [p1, p2, p3, p4]



-- 5. Funciones polimorficas
loMismo :: a -> a
-- Dado un elemento de algún tipo devuelve ese mismo elemento.
loMismo a = a

siempreSiete :: a -> Int
--Prop: Dado un elemento de algún tipo devuelve el número 7.
siempreSiete a = 7

swap :: (a, b) -> (b, a)
--Prop: Dadas una tupla, invierte sus componentes
swap (a, b) = (b, a)

--2) Rta: son funciones polimorficas porque no restringen el tipo de dato solicitado como argumento



-- 6. Pattern matching sobre listas

estaVacia :: [a] -> Bool
--Prop: Dada una lista de elementos, si es vacía devuelve True, sino devuelve False.
estaVacia [] = True
estaVacia _ = False

elPrimero :: [a] -> a
--Prop: Dada una lista devuelve su primer elemento.
elPrimero (x : _)  = x

sinElPrimero :: [a] -> [a]
--Prop: Dada una lista devuelve esa lista menos el primer elemento.
sinElPrimero (_ : x) = x

splitHead :: [a] -> (a, [a])
{-Prop: Dada una lista devuelve un par, donde la primera componente es el primer elemento de la
lista, y la segunda componente es esa lista pero sin el primero. -}
splitHead x = (elPrimero x, sinElPrimero x)




















