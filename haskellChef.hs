import Text.Show.Functions()

type Ingrediente = (String, Int)
type Truco = Plato -> Plato

data Plato = UnPlato {
    lvlDificultad :: Int,
    componentes :: [Ingrediente]
} deriving (Show)

data Participante = UnParticipante {
    nombre :: String,
    trucosDeCocina :: [Truco],
    especialidad :: Plato
} deriving (Show)

pepeRonccino :: Participante
pepeRonccino = UnParticipante "Pepe Ronccino" [darSabor 5 2, simplificar, duplicarPorcion] (UnPlato 8 [("Salsa de Tomate", 25), ("Sal", 4), ("Pasta", 50), ("Queso", 10), ("Carne", 50), ("Azucar", 2)])

nombreIngrediente :: Ingrediente -> String
nombreIngrediente (nombreIngrediente, _) = nombreIngrediente

cantidadIngrediente :: Ingrediente -> Int
cantidadIngrediente (_, cantidadIngrediente) = cantidadIngrediente

agregarComponente :: Ingrediente -> Plato -> Plato
agregarComponente unIngrediente unPlato = unPlato { componentes = unIngrediente : componentes unPlato }
-- agrega unIngrediente al inicio de la lst de ingredientes de unPlato (componentes)

endulzar :: Int -> Truco
endulzar grAzucar unPlato = agregarComponente ("Azucar", grAzucar) unPlato 

salar :: Int -> Truco
salar grSal unPlato = agregarComponente ("Sal", grSal) unPlato

darSabor :: Int -> Int -> Truco
darSabor grAzucar grSal unPlato = (salar grSal . endulzar grAzucar) unPlato

dobleIngrediente :: Ingrediente -> Ingrediente
dobleIngrediente unIngrediente = (nombreIngrediente unIngrediente, cantidadIngrediente unIngrediente *2)

duplicarPorcion :: Truco
duplicarPorcion unPlato = unPlato { componentes = map dobleIngrediente (componentes unPlato) }

cantidadComponentes :: Plato -> Int
cantidadComponentes unPlato = length . componentes $ unPlato

esComplejo :: Plato -> Bool
esComplejo unPlato = lvlDificultad unPlato > 7 && cantidadComponentes unPlato > 5

ingredienteMayor10gr :: Ingrediente -> Bool
ingredienteMayor10gr unIngrediente = cantidadIngrediente unIngrediente >= 10

simplificar :: Truco
simplificar unPlato
    | esComplejo unPlato = unPlato { 
        lvlDificultad = 5, 
        componentes = filter ingredienteMayor10gr (componentes unPlato) 
        }
    | otherwise = unPlato

derivadoDeAnimal :: Ingrediente -> Bool
derivadoDeAnimal unIngrediente = elem (nombreIngrediente unIngrediente) ["Leche", "Carne", "Huevo", "Manteca"]
-- elem puede comparar un string con una lista de strings

harina :: Ingrediente -> Bool
harina unIngrediente = nombreIngrediente unIngrediente == "Harina"

sal :: Ingrediente -> Bool
sal unIngrediente = nombreIngrediente unIngrediente == "Sal" && cantidadIngrediente unIngrediente > 2

contiene :: (Ingrediente -> Bool) -> Plato -> Bool
contiene unIngrediente unPlato = any unIngrediente (componentes unPlato)

esVegano :: Plato -> Bool
esVegano unPlato = not (contiene derivadoDeAnimal unPlato)

esSinTacc :: Plato -> Bool
esSinTacc unPlato = not (contiene harina unPlato)

noAptoHipertenso :: Plato -> Bool
noAptoHipertenso unPlato = contiene sal unPlato

aplicarTrucos :: [Truco] -> Plato -> Plato
aplicarTrucos listaTrucos unPlato = foldr ($) unPlato listaTrucos

cocinar :: Participante -> Participante
cocinar unParticipante = unParticipante { especialidad = aplicarTrucos (trucosDeCocina unParticipante) (especialidad unParticipante) }

pesoComponentes :: Plato -> Int
pesoComponentes unPlato = sum . map cantidadIngrediente $ componentes unPlato

esMejorQue :: Plato -> Plato -> Bool
esMejorQue unPlato otroPlato = 
    (lvlDificultad otroPlato > lvlDificultad unPlato) && 
    (pesoComponentes otroPlato < pesoComponentes unPlato)
-- el plato de otroParticipante es mejor que el de unParticipante si 
-- su dificultad es mayor y su peso menor

mejorPlato :: Participante -> Participante -> Participante
mejorPlato unParticipante otroParticipante
    | esMejorQue (especialidad unParticipante) (especialidad otroParticipante) = otroParticipante
    | otherwise = unParticipante

participanteConMejorPlato :: [Participante] -> Participante
participanteConMejorPlato participantes = foldl1 mejorPlato participantes

-- foldl1 toma el primer elemento de la lista y lo compara con el resto usando mejorPlato
-- si usara foldl seria (p:ps) = foldl mejorPlato p ps
-- foldl toma un valor inicial (p) y lo compara con el resto de la lista (ps) usando mejorPlato
-- pero foldl1 no necesita un valor inicial, toma el primer elemento de la lista como valor inicial
-- ya que no estoy usando una lista vacia puedo usar foldl1
-- en caso de empate, se queda con el primer participante que aparece en la lista (unParticipante)

participanteEstrella :: [Participante] -> Participante
participanteEstrella participantes = (participanteConMejorPlato . map cocinar) participantes

incrementarIngrediente :: Int -> Ingrediente
incrementarIngrediente unNumero = ("Ingrediente " ++ show unNumero, unNumero)
-- show convierte en un string el numero
-- "Ingrediente " ++ "1" = "Ingrediente 1"

componentesInfinitosIncrementales :: [Ingrediente]
componentesInfinitosIncrementales = map incrementarIngrediente [1 ..]

platinum :: Plato
platinum = UnPlato 10 componentesInfinitosIncrementales

-- ¿Qué sucede si aplicamos cada uno de los trucos modelados en la Parte A al platinum?
-- si aplicamos trucos a platinum:
-- endulzar: agregaria azucar al inicio de la lista de platinum, pero al retornar se trabaria
-- x los inifinitos ingredientes
-- sal: agregaria sal al inicio de la lista de platinum, pero al retornar se trabaria
-- x los inifinitos ingredientes
-- darSabor: agregaria azucar y sal al inicio de la lista de platinum, pero al retornar se trabaria
-- x los inifinitos ingredientes
-- duplicarPorcion: al retornar se trabaria xq duplicaria cada ingrediente infinitamente 
-- simplificar: platinum es complejo, por lo que se le asignaria una dificultad de 5 
-- y se quedaria con los ingredientes que tengan mas de 10gr, pero se trabaria al retornar
-- xq hay infinitos ingredientes con mas de 10gr

-- ¿Cuáles de las preguntas de la Parte A (esVegano, esSinTacc, etc.) se pueden responder sobre el platinum? 
-- esVegano: no responderia nada, ya q se quedaria analizando infinitamente si contiene 
-- derivados de animales
-- esSinTacc: no responderia nada, ya q se quedaria analizando infinitamente si contiene harina
-- esComplejo: retornaria true
-- noAptoHipertenso: no responderia nada, ya q se quedaria analizando infinitamente si contiene sal

-- ¿Se puede saber si el platinum es mejor que otro plato?
-- no, ya que no se puede calcular el peso de los ingredientes de platinum 
-- no terminaria de aplicar el map para la cantidadIngrediente)