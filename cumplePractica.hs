type Fecha = (Int, Int)
type Festejo = (String, Fecha, Int)

listaFestejos :: [Festejo]
listaFestejos = [("Gus", (8, 3), 314), ("Vicky", (13, 4), 1000), ("Dia", (14, 4), 250), ("Ro", (14, 4), 31), ("Orne", (26, 4), 50), ("Lu", (2, 9), 150), ("Nacho", (2, 9), 22), ("Petru", (28, 5), 24)]

nombreFestejo :: Festejo -> String
nombreFestejo (nombre, _, _) = nombre

cantidadRegalos :: Festejo -> Int
cantidadRegalos (_, _, regalos) = regalos

fechaFestejo :: Festejo -> Fecha
fechaFestejo (_, fecha, _) = fecha

diaFestejo :: Festejo -> Int
diaFestejo unFestejo = (fst . fechaFestejo) unFestejo
-- fst devuelve el 1er elem de la tupla q devuelve fechaFestejo

mesFestejo :: Festejo -> Int
mesFestejo unFestejo = (snd . fechaFestejo) unFestejo
-- snd devuelve el 2do elem de la tupla q devuelve fechaFestejo

botinDeRegalos :: [Festejo] -> Int
botinDeRegalos listaFestejos = (sum . map cantidadRegalos) listaFestejos
-- mapeo la lst de festejos app parcialmente map para conseguir los regalos de c/ festejo con cR
-- uso sum para sumar c/ elem de la lista de cR s conseguida con map

cumpleEn :: Int -> Festejo -> Bool
cumpleEn unMes unFestejo = mesFestejo unFestejo == unMes

esCumpleInolvidable :: Festejo -> Bool
esCumpleInolvidable unFestejo = cumpleEn 4 unFestejo || cantidadRegalos unFestejo > 400 || nombreFestejo unFestejo == "Gus" 

hayCumplesNavidenios :: [Festejo] -> Bool
hayCumplesNavidenios listaFestejos = any (cumpleEn 12) listaFestejos
-- any se usa para ver si al menos 1 elem de la lst cumple la condicion 

-- any verifica si al menos un elemento de la lista cumple con una condición, 
-- en cambio map transforma cada elemento de la lista aplicando una función, devolviendo una nueva lista.

tematicaFestejo :: Festejo -> String
tematicaFestejo unFestejo
    | cumpleEn 12 unFestejo = "Cumple navidenio"
    | otherwise = "Sin tematica"