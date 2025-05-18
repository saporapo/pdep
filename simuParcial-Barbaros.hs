import Data.Char (toUpper)

type Objeto = Barbaro -> Barbaro
type Evento = Barbaro -> Bool
type Aventura = [Evento]

data Barbaro = UnBarbaro {
    nombre :: String,
    fuerza :: Float,
    habilidades :: [String],
    objetos :: [Objeto]
}

dave :: Barbaro
dave = UnBarbaro "Dave" 100 ["tejer","escribirPoesia"] [ardilla, varitasDefectuosas]

aumentarFuerza :: Float -> Barbaro -> Barbaro
aumentarFuerza unPeso unBarbaro = unBarbaro {
    fuerza = fuerza unBarbaro + 2 * unPeso
}

espadas :: Float -> Objeto
espadas unPeso unBarbaro = aumentarFuerza unPeso unBarbaro

otorgarHabilidad :: String -> Barbaro -> Barbaro
otorgarHabilidad unaHabilidad unBarbaro = unBarbaro {
    habilidades = unaHabilidad : habilidades unBarbaro
}

amuletosMisticos :: String -> Objeto
amuletosMisticos unaHabilidad unBarbaro = otorgarHabilidad unaHabilidad unBarbaro

desaparecerObjetos :: Barbaro -> Barbaro
desaparecerObjetos unBarbaro = unBarbaro {
    objetos = []
}

varitasDefectuosas :: Objeto
varitasDefectuosas unBarbaro = desaparecerObjetos . otorgarHabilidad "hacerMagia" $ unBarbaro

ardilla :: Objeto
ardilla unBarbaro = unBarbaro

cuerda :: Objeto -> Objeto -> Objeto
cuerda unObjeto otroObjeto unBarbaro = unObjeto . otroObjeto $ unBarbaro


juntarHabilidades :: Barbaro -> String
juntarHabilidades unBarbaro = (map toUpper . concat) (habilidades unBarbaro)
-- concat junta todos los elems de una lista en una sola
-- map toUpper transforma esa lista de un elem a una lista
-- con el elem en mayus

potenciarBarbaro :: Barbaro -> Barbaro
potenciarBarbaro unBarbaro = unBarbaro {
    habilidades = [juntarHabilidades unBarbaro]
}
-- concat une la lst de habilidades en una sola
-- map toUpper convierte cada letra de la habilidad en mayuscula

megafono :: Objeto
megafono unBarbaro = potenciarBarbaro unBarbaro

megafonoBarbarico :: Objeto
megafonoBarbarico unBarbaro = cuerda megafono ardilla unBarbaro


ejemploAventura :: Aventura
ejemploAventura = [invasionDeSuciosDuendes, cremalleraDelTiempo, ritualDeFechorias]

poseeHabilidad :: String -> Barbaro -> Bool
poseeHabilidad unaHabilidad unBarbaro = any (== unaHabilidad) (habilidades unBarbaro)

invasionDeSuciosDuendes :: Evento
invasionDeSuciosDuendes unBarbaro = poseeHabilidad "escribirPoesiaAtroz" unBarbaro

tienePulgares :: String -> Bool
tienePulgares nombreBarbaro = nombreBarbaro == "Faffy" || nombreBarbaro == "Astro"

cremalleraDelTiempo :: Evento
cremalleraDelTiempo unBarbaro = not(tienePulgares (nombre unBarbaro))

fuerzaMayor :: Float -> Barbaro -> Bool
fuerzaMayor unNumero unBarbaro = fuerza unBarbaro > unNumero

saqueo :: Barbaro -> Bool
saqueo unBarbaro = poseeHabilidad "robar" unBarbaro && fuerzaMayor 80 unBarbaro

poderDeGrito :: Barbaro -> Int
poderDeGrito unBarbaro = length . juntarHabilidades $ unBarbaro
-- juntarHabilidades me devuelve un string
-- con length cuento la cantidad de letras q tiene ese string

cantidadObjetos :: Barbaro -> Int
cantidadObjetos unBarbaro = length (objetos unBarbaro)
-- cantidad de objetos q tiene el barbaro
-- lenght cuenta la cantidad de elementos de una lista

gritoDeGuerra :: Barbaro -> Bool
gritoDeGuerra unBarbaro = poderDeGrito unBarbaro > 4 * cantidadObjetos unBarbaro

esVocal :: Char -> Bool
esVocal unaLetra = elem unaLetra "aeiouAEIOU"
-- elem devuelve True o False si el char pertenece a la lista de vocales

vocalesMayor3 :: String -> Bool
vocalesMayor3 unaHabilidad = (length . filter esVocal) unaHabilidad > 3
-- filter devuelve una lista con los elementos que son vocales

comienzaConMayus :: String -> Bool
comienzaConMayus unaHabilidad = head unaHabilidad == toUpper (head unaHabilidad)

cumplenCondicion :: String -> Bool
cumplenCondicion unaHabilidad = vocalesMayor3 unaHabilidad && comienzaConMayus unaHabilidad

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
repetido habilidadesBarbaras unaHabilidad = (length . filter (== unaHabilidad)) habilidadesBarbaras > 1
-- filter devuelve una lista con los elems de habilidadesBarbaras que son iguales a unaHabilidad
-- length cuenta la cantidad de elems de la lista de filter
-- si la cantidad de elems es mayor a 1, significa que se repitio

sinRepetidos :: [String] -> [String]
sinRepetidos habilidadesBarbaras = filter (not . repetido habilidadesBarbaras) habilidadesBarbaras
-- devuelvo una lista con los elems de habilidadesBarbaras que no se repiten

descendientes :: Barbaro -> Barbaro
descendientes unBarbaro =  foldr 