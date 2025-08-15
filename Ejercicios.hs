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




-- 4 Registros
data Persona = P String Int String 
                --Nombre Edad DNI
                deriving Show

nombre :: Persona -> String
nombre (P n e d) = n

edad :: Persona -> Int
edad (P n e d) = e
















