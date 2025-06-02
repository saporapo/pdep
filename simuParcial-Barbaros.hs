import Text.Show.Functions()
import Data.Char (toUpper)

type Objeto = Barbaro -> Barbaro
type Evento = Barbaro -> Bool
type Aventura = [Evento]

data Barbaro = UnBarbaro {
    nombre :: String,
    fuerza :: Float,
    habilidades :: [String],
    objetos :: [Objeto]
} deriving (Show)

dave :: Barbaro
dave = UnBarbaro "Dave" 100 ["tejer","escribirPoesia"] [ardilla, varitasDefectuosas]

aumentarFuerza :: Float -> Barbaro -> Barbaro
aumentarFuerza unPeso unBarbaro = unBarbaro {
    fuerza = fuerza unBarbaro + 2 * unPeso
}

espadas :: Float -> Objeto
espadas unPeso unBarbaro = aumentarFuerza unPeso unBarbaro

otorgarHabilidad :: String -> Barbaro -> Barbaro
otorgarHabilidad unElemento unBarbaro = unBarbaro {
    habilidades = unElemento : habilidades unBarbaro
}

amuletosMisticos :: String -> Objeto
amuletosMisticos unElemento unBarbaro = otorgarHabilidad unElemento unBarbaro

desaparecerObjetos :: Barbaro -> Barbaro
desaparecerObjetos unBarbaro = unBarbaro {
    objetos = []
}

varitasDefectuosas :: Objeto
varitasDefectuosas unBarbaro = desaparecerObjetos . otorgarHabilidad "hacerMagia" $ unBarbaro

ardilla :: Objeto
ardilla unBarbaro = id unBarbaro

cuerda :: Objeto -> Objeto -> Objeto
cuerda unObjeto otroObjeto unBarbaro = unObjeto . otroObjeto $ unBarbaro


juntarHabilidades :: Barbaro -> String
juntarHabilidades unBarbaro = (map toUpper . concat) (habilidades unBarbaro)
-- concat junta todos los elems de una lista en una sola
-- map toUpper transforma esa lista de un elem a una lista
-- con el elem en mayus
-- toUpper no transforma una lista, sino un char

potenciarBarbaro :: Barbaro -> Barbaro
potenciarBarbaro unBarbaro = desaparecerObjetos . otorgarHabilidad (juntarHabilidades unBarbaro) $ unBarbaro
-- concat une la lst de habilidades en una sola
-- map toUpper convierte cada letra de la habilidad en mayuscula

megafono :: Objeto
megafono unBarbaro = potenciarBarbaro unBarbaro

megafonoBarbarico :: Objeto
megafonoBarbarico unBarbaro = cuerda megafono ardilla unBarbaro


ejemploAventura :: Aventura
ejemploAventura = [invasionDeSuciosDuendes, cremalleraDelTiempo, ritualDeFechorias]

poseeHabilidad :: String -> Barbaro -> Bool
poseeHabilidad unElemento unBarbaro = elem unElemento (habilidades unBarbaro)

invasionDeSuciosDuendes :: Evento
invasionDeSuciosDuendes unBarbaro = poseeHabilidad "escribirPoesiaAtroz" unBarbaro

tienePulgares :: String -> Bool
tienePulgares nombreBarbaro = elem nombreBarbaro ["Faffy", "Astro"]

cremalleraDelTiempo :: Evento
cremalleraDelTiempo unBarbaro = not . tienePulgares $ (nombre unBarbaro)

fuerzaMayor :: Float -> Barbaro -> Bool
fuerzaMayor unNumero unBarbaro = fuerza unBarbaro > unNumero

saqueo :: Barbaro -> Bool
saqueo unBarbaro = poseeHabilidad "robar" unBarbaro && fuerzaMayor 80 unBarbaro

poderDeGrito :: Barbaro -> Int
poderDeGrito unBarbaro = length . juntarHabilidades $ unBarbaro
-- juntarHabilidades me devuelve un string
-- con length cuento la cantidad de letras q tiene ese string

cantidadObjetos :: Barbaro -> Int
cantidadObjetos unBarbaro = length . objetos $ unBarbaro
-- cantidad de objetos q tiene el barbaro
-- lenght cuenta la cantidad de elementos de una lista

gritoDeGuerra :: Barbaro -> Bool
gritoDeGuerra unBarbaro = poderDeGrito unBarbaro > 4 * cantidadObjetos unBarbaro

esVocal :: Char -> Bool
esVocal unaLetra = elem unaLetra "aeiouAEIOU"
-- elem devuelve True o False si el char pertenece a la lista de vocales

vocalesMayor3 :: String -> Bool
vocalesMayor3 unElemento = (length . filter esVocal) unElemento > 3
-- filter devuelve una lista con los elementos que son vocales

comienzaConMayus :: String -> Bool
comienzaConMayus unElemento = head unElemento == (toUpper.head) unElemento

cumplenCondicion :: String -> Bool
cumplenCondicion unElemento = vocalesMayor3 unElemento && comienzaConMayus unElemento

caligrafiaPerfecta :: Barbaro -> Bool
caligrafiaPerfecta unBarbaro = all cumplenCondicion (habilidades unBarbaro)

caligrafia :: Barbaro -> Bool
caligrafia unBarbaro = caligrafiaPerfecta unBarbaro

ritualDeFechorias :: Evento
ritualDeFechorias unBarbaro = saqueo unBarbaro || gritoDeGuerra unBarbaro || caligrafia unBarbaro
-- si el barbaro pasa al menos 1 de las pruebas sobrevive

pasaEvento :: Barbaro -> Evento -> Bool
pasaEvento unBarbaro unEvento = unEvento unBarbaro
--barbaro pasa un evento

pasaAventura :: Aventura -> Barbaro -> Bool
pasaAventura unaAventura unBarbaro = all (pasaEvento unBarbaro) unaAventura
-- barbaro pasa la aventura si pasa todos los eventos de la misma

sobrevivientes :: Aventura -> [Barbaro] -> [Barbaro]
sobrevivientes unaAventura grupoDeBarbaros = filter (pasaAventura unaAventura) grupoDeBarbaros
-- devuelvo los barbaros que pasaron la aventura


repetido :: [String] -> String -> Bool
repetido listaBarbara unElemento = (length . filter (== unElemento)) listaBarbara > 1
-- filter devuelve una lista con los elems de listaBarbara que son iguales a unElemento
-- length cuenta la cantidad de elems de la lista de filter
-- si la cantidad de elems es mayor a 1, significa que se repitió

sinRepetidos :: [String] -> [String]
sinRepetidos listaBarbara = filter (not . repetido listaBarbara) listaBarbara
-- devuelvo una lista con los elems de listaBarbara que no se repiten

compartirNombre :: Barbaro -> Barbaro
compartirNombre unBarbaro = unBarbaro {
    nombre = nombre unBarbaro ++ "*"
}

heredarHabilidades :: Barbaro -> Barbaro
heredarHabilidades unBarbaro = unBarbaro {
    habilidades = sinRepetidos (habilidades unBarbaro)
}

heredarObjetos :: Barbaro -> Barbaro
heredarObjetos unBarbaro = unBarbaro {
    objetos = objetos unBarbaro
}

usarObjetos :: Barbaro -> Barbaro
usarObjetos unBarbaro = foldr ($) unBarbaro (objetos unBarbaro)
-- foldr aplica cada objeto de la lista a unBarbaro
-- $ es para componer funciones

descendientes :: Barbaro -> [Barbaro]
descendientes unBarbaro = iterate (usarObjetos . heredarObjetos . heredarHabilidades . compartirNombre)  unBarbaro
-- sinRepetidos solo funciona sobre listas de Strings, asi que no se puede usar sobre la lista de objetos
-- sobre el nom de unBarbaro tampoco puedo usar sinRepetidos xq no es una lista