module Ejemplo where
import Text.Show.Functions()

type Ruedas = Float
type Chasis = Float
type Tramo = Auto -> Auto
type ListaAutos = [Auto]

data Auto = UnAuto {
    marca :: String,
    modelo :: String,
    desgaste :: (Ruedas, Chasis),
    velocidadMaxima :: Float,
    tiempoCarrera :: Float,
    apodos :: [String]
} deriving (Show, Eq)

data Pista = UnaPista {
    nombrePista :: String,
    pais :: String,
    precioBaseEntrada :: Float,
    tramos :: [Tramo]
} deriving (Show)


-- {Punto 1: Modelado de Autos}

ferrari :: Auto
ferrari = UnAuto "Ferrari" "F50" (0, 0) 65 0 ["La nave", "El fierro", "Ferrucho"]

lamborghini :: Auto
lamborghini = UnAuto "Lamborghini" "Diablo" (4, 7) 73 0 ["Lambo", "La bestia"]

fiat :: Auto
fiat = UnAuto "Fiat" "600" (27, 33) 44 0 ["La Bocha", "La bolita", "fitito"]

peugeot :: Auto
peugeot = UnAuto "Peugeot" "504" (0, 0) 40 0 ["El rey del desierto"]

------------------------------------------------------------------------------------------------------------------------------

-- {Punto 2: Estado del Auto}

-- Función para acceder al desgaste en las ruedas de un auto
desgasteRuedasDeUnAuto :: Auto -> Ruedas
desgasteRuedasDeUnAuto unAuto = (fst.desgaste) unAuto

-- Función para acceder al desgaste en el chasis de un auto
desgasteChasisDeUnAuto :: Auto -> Chasis
desgasteChasisDeUnAuto unAuto = (snd.desgaste) unAuto

marcaAuto :: String -> Auto -> Bool
marcaAuto unaMarca unAuto = marca unAuto == unaMarca
--esPeugeout
verificarTiempoAuto :: (Float -> Bool) -> Auto -> Bool
verificarTiempoAuto condicion unAuto = condicion.tiempoCarrera $ unAuto

verificarDesgasteRuedasAuto :: (Float -> Bool) -> Auto -> Bool
verificarDesgasteRuedasAuto condicion unAuto = verificarDesgasteAuto condicion (const True) unAuto

verificarDesgasteChasisAuto :: (Float -> Bool) -> Auto -> Bool
verificarDesgasteChasisAuto condicion unAuto = verificarDesgasteAuto (const True) condicion unAuto

verificarDesgasteAuto :: (Float -> Bool) -> (Float -> Bool) -> Auto -> Bool
verificarDesgasteAuto condicionRuedas condicionChasis unAuto = 
    (condicionRuedas.desgasteRuedasDeUnAuto $ unAuto) && (condicionChasis.desgasteChasisDeUnAuto $ unAuto)

autoEnBuenEstado :: Auto -> Bool
autoEnBuenEstado unAuto = not (marcaAuto "Peugeot" unAuto) && 
    ( (verificarTiempoAuto (<100) unAuto && verificarDesgasteChasisAuto (<20) unAuto) || 
    verificarDesgasteAuto (<40) (<60) unAuto)

-- Casos de prueba

-- *Ejemplo> autoEnBuenEstado peugeot
-- False

-- *Ejemplo> lamborghini2 = lamborghini { tiempoCarrera = 99 }
-- *Ejemplo> autoEnBuenEstado lamborghini2
-- True

-- *Ejemplo> fiat2 = fiat { tiempoCarrera = 99 }
-- *Ejemplo> autoEnBuenEstado fiat2
-- False

-- *Ejemplo> ferrari2 = ferrari { tiempoCarrera = 130, desgaste = (50, 30) }
-- *Ejemplo> autoEnBuenEstado ferrari2
-- True

-- *Ejemplo> ferrari3 = ferrari { tiempoCarrera = 15, desgaste = (50, 45) }
-- *Ejemplo> autoEnBuenEstado ferrari3
-- False

-- *Ejemplo> ferrari4 = ferrari { tiempoCarrera = 150, desgaste = (70, 30) }
-- *Ejemplo> autoEnBuenEstado ferrari4
-- False

empiezaConLa :: [String] -> Bool
empiezaConLa (apodo:_) = take 2 apodo == "La"

noDaMas :: Auto -> Bool
noDaMas unAuto = (empiezaConLa (apodos unAuto) && verificarDesgasteChasisAuto (>80) unAuto) || verificarDesgasteRuedasAuto (>80) unAuto

cantidadDeApodos :: Auto -> Int
cantidadDeApodos = length.apodos

longitudModelo :: Auto -> Int
longitudModelo = length.modelo

-- Casos de prueba

-- *Ejemplo> ferrari5 = ferrari { desgaste = (20, 90) }
-- *Ejemplo> noDaMas ferrari5
-- True

-- *Ejemplo> ferrari6 = ferrari { desgaste = (0, 20) }
-- *Ejemplo> noDaMas ferrari6
-- False

-- *Ejemplo> lamborghini3 = lamborghini { desgaste = (90, 20) }
-- *Ejemplo> noDaMas lamborghini3
-- True

-- *Ejemplo> noDaMas lamborghini
-- False

apodosPar :: Auto -> Bool
apodosPar unAuto = even.cantidadDeApodos $ unAuto

esUnChiche :: Auto -> Bool
esUnChiche unAuto = (apodosPar unAuto && verificarDesgasteChasisAuto (<20) unAuto) || (not(apodosPar unAuto) && verificarDesgasteChasisAuto (<50) unAuto)

-- Casos de prueba

-- *Ejemplo> esUnChiche lamborghini
-- True

-- *Ejemplo> lamborghini4 = lamborghini { desgaste = (90, 20) }
-- *Ejemplo> esUnChiche lamborghini4
-- False

-- *Ejemplo> ferrari7 = ferrari { desgaste = (20, 90) }
-- *Ejemplo> esUnChiche ferrari7
-- False

-- *Ejemplo> esUnChiche ferrari
-- True

esUnaJoya :: Auto -> Bool
esUnaJoya unAuto = desgaste unAuto == (0,0) && cantidadDeApodos unAuto <= 1

-- Casos de prueba

-- *Ejemplo> esUnaJoya peugeot
-- True

-- *Ejemplo> esUnaJoya ferrari
-- False 

-- product multiplica los elementos de una lista
nivelDeChetez :: Auto -> Float
nivelDeChetez unAuto = product [20, fromIntegral (cantidadDeApodos unAuto), fromIntegral (longitudModelo unAuto)]

-- Casos de prueba

-- *Ejemplo> nivelDeChetez ferrari
-- 180

capacidadSupercalifragilisticaespialidosa :: Auto -> Int
capacidadSupercalifragilisticaespialidosa = length.head.apodos

-- Casos de prueba

-- *Ejemplo> capacidadSupercalifragilisticaespialidosa ferrari
-- 7

calculoRiesgoAuto :: Auto -> Float
calculoRiesgoAuto unAuto = product [velocidadMaxima unAuto, desgasteRuedasDeUnAuto unAuto / 10]

-- velocidadMax unAuto * div (desgasteRuedasDeUnAuto unAuto) 10

riesgoso :: Auto -> Float
riesgoso unAuto
    | autoEnBuenEstado unAuto = calculoRiesgoAuto unAuto
    | otherwise = 2 * calculoRiesgoAuto unAuto

-- Casos de prueba

-- *Ejemplo> riesgoAuto lamborghini
-- 29.2

-- *Ejemplo> riesgoAuto fiat
-- 237.6

------------------------------------------------------------------------------------------------------------------------------

-- {Punto 3: Manos a la obra}

mapDesgasteRuedasAuto :: (Float -> Float) -> Auto -> Auto
mapDesgasteRuedasAuto modificacion unAuto = mapDesgasteAuto modificacion (id) unAuto

mapDesgasteChasisAuto :: (Float -> Float) -> Auto -> Auto
mapDesgasteChasisAuto modificacion unAuto = mapDesgasteAuto (id) modificacion unAuto

mapDesgasteAuto :: (Float -> Float) -> (Float -> Float) -> Auto -> Auto
mapDesgasteAuto modificacionRuedas modificacionChasis unAuto = unAuto {
    desgaste = (modificacionRuedas.desgasteRuedasDeUnAuto$ unAuto, modificacionChasis.desgasteChasisDeUnAuto$ unAuto)
    }

mapTiempoCarreraAuto :: (Float -> Float) -> Auto -> Auto
mapTiempoCarreraAuto modificacion unAuto = unAuto {
    tiempoCarrera = modificacion.tiempoCarrera$ unAuto
    }

mapVelocidadMaximaAuto :: (Float -> Float) -> Auto -> Auto
mapVelocidadMaximaAuto modificacion unAuto = unAuto {
    velocidadMaxima = modificacion.velocidadMaxima$ unAuto
    }

mapApodosAuto :: ([String] -> [String]) -> Auto -> Auto
mapApodosAuto modificacion unAuto = unAuto {
    apodos = modificacion.apodos$ unAuto
    }

mapMarcaAuto :: (String -> String) -> Auto -> Auto
mapMarcaAuto modificacion unAuto = unAuto {
    marca = modificacion.marca$ unAuto
    }

mapModeloAuto :: (String -> String) -> Auto -> Auto
mapModeloAuto modificacion unAuto = unAuto {
    modelo = modificacion.modelo$ unAuto
    }

repararUnAuto :: Auto -> Auto
repararUnAuto unAuto = mapDesgasteAuto (const 0) (*0.15) unAuto

-- Casos de pruebas

-- *Ejemplo> repararUnAuto fiat
-- UnAuto {marca = "Fiat", modelo = "600", desgaste = (0.0,4.9500003), velocidadMaxima = 44.0, tiempoCarrera = 0.0, apodos = ["La Bocha","La bolita","fitito"]}

-- *Ejemplo> repararUnAuto ferrari
-- UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (0.0,0.0), velocidadMaxima = 65.0, tiempoCarrera = 0.0, apodos = ["La nave","El fierro","Ferrucho"]}

aplicarUnaPenalidad :: Float -> Auto -> Auto
aplicarUnaPenalidad unTiempo unAuto = mapTiempoCarreraAuto (+ unTiempo) unAuto

-- Casos de prueba

-- *Ejemplo> aplicarUnaPenalidad 20 ferrari {tiempoCarrera = 10}
--UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (0.0,0.0), velocidadMaxima = 65.0, tiempoCarrera = 30.0, apodos = ["La nave","El fierro","Ferrucho"]}

-- *Ejemplo> aplicarUnaPenalidad 0 ferrari {tiempoCarrera = 10}
-- UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (0.0,0.0), velocidadMaxima = 65.0, tiempoCarrera = 10.0, apodos = ["La nave","El fierro","Ferrucho"]}

ponerleNitroAUnAuto :: Auto -> Auto
ponerleNitroAUnAuto unAuto = mapVelocidadMaximaAuto (*1.2) unAuto

-- Casos de pruebas

-- *Ejemplo> ponerleNitroAUnAuto fiat
-- UnAuto {marca = "Fiat", modelo = "600", desgaste = (27.0,33.0), velocidadMaxima = 52.800003, tiempoCarrera = 0.0, apodos = ["La Bocha","La bolita","fitito"]}

-- *Ejemplo> ponerleNitroAUnAuto fiat {velocidadMaxima = 0}
-- UnAuto {marca = "Fiat", modelo = "600", desgaste = (27.0,33.0), velocidadMaxima = 0.0, tiempoCarrera = 0.0, apodos = ["La Bocha","La bolita","fitito"]}

bautizarUnAuto :: String -> Auto -> Auto
bautizarUnAuto unApodo unAuto = mapApodosAuto (unApodo:) unAuto

-- Casos de prueba

-- *Ejemplo> bautizarUnAuto "El diablo" lamborghini
-- UnAuto {marca = "Lamborghini", modelo = "Diablo", desgaste = (4.0,7.0), velocidadMaxima = 73.0, tiempoCarrera = 0.0, apodos = ["El diablo","Lambo","La bestia"]}

-- *Ejemplo> bautizarUnAuto "El diablo" lamborghini{apodos = []}
-- UnAuto {marca = "Lamborghini", modelo = "Diablo", desgaste = (4.0,7.0), velocidadMaxima = 73.0, tiempoCarrera = 0.0, apodos = ["El diablo"]}

cambiarMarca :: String -> Auto -> Auto
cambiarMarca unaMarca unAuto = mapMarcaAuto (const unaMarca) unAuto

cambiarModelo :: String -> Auto -> Auto
cambiarModelo unModelo unAuto = mapModeloAuto (const unModelo) unAuto

perderApodos :: Auto -> Auto
perderApodos unAuto = mapApodosAuto (const ["Nunca Taxi"]) unAuto

llevarUnAutoAUnDesarmadero :: String -> String -> Auto -> Auto
llevarUnAutoAUnDesarmadero unaMarca unModelo unAuto = ((perderApodos . cambiarModelo unModelo) . cambiarMarca unaMarca) unAuto

-- Casos De Pruebas

-- *Ejemplo> llevarUnAutoAUnDesarmadero "Tesla" "x" fiat
-- UnAuto {marca = "Tesla", modelo = "x", desgaste = (27.0,33.0), velocidadMaxima = 44.0, tiempoCarrera = 0.0, apodos = ["Nunca Taxi"]}

------------------------------------------------------------------------------------------------------------------------------

-- {Punto 4: Pistas}

tiempoCurva :: Float -> Auto -> Float
tiempoCurva unaLongitud unAuto = unaLongitud / (velocidadMaxima unAuto / 2)
desgasteRuedasCurva :: Float -> Float -> Float
desgasteRuedasCurva unAngulo unaLongitud = 3 * unaLongitud / unAngulo

tiempoRecto :: Float -> Auto -> Float
tiempoRecto unaLongitud unAuto = unaLongitud / velocidadMaxima unAuto
desgasteChasisRecto :: Float -> Float
desgasteChasisRecto unaLongitud = unaLongitud / 100

tiempoZigZag :: Int -> Int
tiempoZigZag cantidadCambios = cantidadCambios * 3
desgasteRuedasZigZag :: Int -> Auto -> Float
desgasteRuedasZigZag cantidadCambios unAuto = fromIntegral cantidadCambios * velocidadMaxima unAuto / 10

tiempoRulo :: Float -> Auto -> Float
tiempoRulo unDiametro unAuto = 5 * unDiametro / velocidadMaxima unAuto
desgasteRuedasRulo :: Float -> Float
desgasteRuedasRulo unDiametro = 1.5 * unDiametro

curva :: Float -> Float -> Auto -> Auto
curva unAngulo unaLongitud unAuto = mapTiempoCarreraAuto (+ tiempoCurva unaLongitud unAuto) .
    mapDesgasteRuedasAuto (+ desgasteRuedasCurva unAngulo unaLongitud) $ unAuto

recto :: Float -> Auto -> Auto
recto unaLongitud unAuto = mapTiempoCarreraAuto (+ tiempoRecto unaLongitud unAuto) .
    mapDesgasteChasisAuto (+ desgasteChasisRecto unaLongitud) $ unAuto

zigZag :: Int -> Auto -> Auto
zigZag cantidadCambios unAuto = mapTiempoCarreraAuto (+ fromIntegral (tiempoZigZag cantidadCambios)) .
    mapDesgasteRuedasAuto (+ desgasteRuedasZigZag cantidadCambios unAuto) $ unAuto

ruloEnAire :: Float -> Auto -> Auto
ruloEnAire unDiametro unAuto = mapTiempoCarreraAuto (+ tiempoRulo unDiametro unAuto) .
    mapDesgasteRuedasAuto (+ desgasteRuedasRulo unDiametro) $ unAuto

curvaPeligrosa :: Tramo
curvaPeligrosa unAuto = curva 60 300 unAuto
curvaTranca :: Tramo
curvaTranca unAuto = curva 110 550 unAuto

-- Casos de Prueba

-- tramoCurva curvaPeligrosa ferrari
-- UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (15.0,0.0), velocidadMaxima = 65.0, tiempoCarrera = 9.230769, apodos = ["La nave","El fierro","Ferrucho"]}

-- tramoCurva curvaTranca ferrari
-- UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (15.0,0.0), velocidadMaxima = 65.0, tiempoCarrera = 16.923077, apodos = ["La nave","El fierro","Ferrucho"]}

-- tramoCurva curvaPeligrosa peugeot
-- UnAuto {marca = "Peugeot", modelo = "504", desgaste = (15.0,0.0), velocidadMaxima = 40.0, tiempoCarrera = 15.0, apodos = ["El rey del desierto"]}

-- tramoCurva curvaTranca peugeot
-- UnAuto {marca = "Peugeot", modelo = "504", desgaste = (15.0,0.0), velocidadMaxima = 40.0, tiempoCarrera = 27.5, apodos = ["El rey del desierto"]}

tramoRectoClassic :: Tramo
tramoRectoClassic unAuto = recto 715 unAuto
tramito :: Tramo
tramito unAuto = recto 260 unAuto

-- Casos de Prueba

-- tramoRecto tramoRectoClassic ferrari
-- UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (0.0,7.15), velocidadMaxima = 65.0, tiempoCarrera = 11.0, apodos = ["La nave","El fierro","Ferrucho"]}

-- tramoRecto tramito ferrari
-- UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (0.0,2.6), velocidadMaxima = 65.0, tiempoCarrera = 4.0, apodos = ["La nave","El fierro","Ferrucho"]}

zigZagLoco :: Tramo
zigZagLoco unAuto = zigZag 5 unAuto
casiCurva :: Tramo
casiCurva unAuto = zigZag 1 unAuto

-- Casos de Prueba

-- zigZagLoco ferrari
-- UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (32.5,5.0), velocidadMaxima = 65.0, tiempoCarrera = 15.0, apodos = ["La nave","El fierro","Ferrucho"]}

-- casiCurva ferrari
-- UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (6.5,5.0), velocidadMaxima = 65.0, tiempoCarrera = 3.0, apodos = ["La nave","El fierro","Ferrucho"

ruloClasico :: Tramo
ruloClasico unAuto = ruloEnAire 13 unAuto
deseoDeMuerte :: Tramo
deseoDeMuerte unAuto = ruloEnAire 26 unAuto

-- Caso de Prueba

-- ruloClasico ferrari
-- UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (19.5,0.0), velocidadMaxima = 65.0, tiempoCarrera = 1.0, apodos = ["La nave","El fierro","Ferrucho"]}

-- deseoDeMuerte ferrari
-- UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (39.0,0.0), velocidadMaxima = 65.0, tiempoCarrera = 2.0, apodos = ["La nave","El fierro","Ferrucho"]}

------------------------------------------------------------------------------------------------------------------------------

-- {Punto 5: Nivel de Joyez}

nivelDeJoyez :: ListaAutos -> Int
nivelDeJoyez listaAutos = (sum . map verificarTiempo) listaAutos

verificarTiempo :: Auto -> Int
verificarTiempo unAuto
    | esUnaJoya unAuto && tiempoCarrera unAuto < 50 = 1
    | esUnaJoya unAuto && tiempoCarrera unAuto >= 50 = 2
    | otherwise = 0

listaAutosPrueba :: ListaAutos
listaAutosPrueba = [ferrari, peugeot {tiempoCarrera = 49}, peugeot {tiempoCarrera = 50}]

-- Casos de Prueba

-- nivelDeJoyez listaAutosPrueba
-- 3

paraEntendidos :: ListaAutos -> Bool
paraEntendidos grupoDeAutos = all cumplenCondicion grupoDeAutos

cumplenCondicion :: Auto -> Bool
cumplenCondicion unAuto = autoEnBuenEstado unAuto && verificarTiempoAuto (<= 200) unAuto

-- Casos de Prueba

-- paraEntendidos [ferrari {tiempoCarrera = 201}, ferrari {tiempoCarrera = 200}]
-- no es para entendidos

-- paraEntendidos [ferrari {tiempoCarrera = 200}, peugeot]
-- no es para entendidos

-- paraEntendidos [ferrari {tiempoCarrera = 200}, lamborghini {tiempoCarrera = 200}]
-- es para entendidos





-- {Inicio Parte 2 TP}

type Cobertura = (Float, ListaAutos)

data Equipo = UnEquipo {
    nombreEquipo :: String,
    presupuesto :: Float,
    listaAutos :: ListaAutos
} deriving (Show, Eq)

-- {Punto 1: Modelado de Equipos}

equipo1 :: Equipo
equipo1 = UnEquipo "Equipo1" 20000 [ferrari {desgaste = (0, 10)}, lamborghini {desgaste = (0, 20)}]

equipo2 :: Equipo
equipo2 = UnEquipo "Equipo2" 4000 [peugeot {desgaste = (0, 50)}]

mapNombreEquipo :: (String -> String) -> Equipo -> Equipo
mapNombreEquipo modificacion unEquipo = unEquipo { nombreEquipo = modificacion.nombreEquipo$ unEquipo }

mapListaAutos :: (ListaAutos -> ListaAutos) -> Equipo -> Equipo
mapListaAutos modificacion unEquipo = unEquipo { listaAutos = modificacion.listaAutos$ unEquipo }

mapPresupuesto :: (Float -> Float) -> Equipo -> Equipo
mapPresupuesto modificacion unEquipo = unEquipo { presupuesto = modificacion.presupuesto$ unEquipo }

costoInscripcion :: Auto -> Float
costoInscripcion unAuto = velocidadMaxima unAuto * 1000

costoReparacion :: Auto -> Float
costoReparacion unAuto = (desgasteChasisDeUnAuto unAuto - desgasteChasisDeUnAuto (repararUnAuto unAuto)) * 500

costoOptimizar :: Auto -> Float
costoOptimizar unAuto = velocidadMaxima unAuto * 100

costoFerrarizar :: Auto -> Float
costoFerrarizar _ = 3500

presupuestoSuficiente :: Float -> Float -> Bool
presupuestoSuficiente unGasto presupuestoEquipo = unGasto <= presupuestoEquipo

actualizarEquipo :: (ListaAutos -> ListaAutos) -> (Float -> Float) -> Equipo -> Equipo
actualizarEquipo modificarLista modificarPresupuesto unEquipo = mapListaAutos (modificarLista) . mapPresupuesto (modificarPresupuesto)$ unEquipo

autosCubiertos :: (Auto -> Float) -> (Auto -> Auto) -> (Auto -> Bool) -> Float -> ListaAutos -> Cobertura
autosCubiertos unGasto modificacion condicionModificacion presupuesto [] = (presupuesto, [])
autosCubiertos unGasto modificacion condicionModificacion presupuesto (unAuto:colaAutos)
    | presupuestoSuficiente (unGasto unAuto) presupuesto && condicionModificacion unAuto = 
        (presupuestoRestante unGasto modificacion condicionModificacion (presupuesto - unGasto unAuto) colaAutos,
        modificacion unAuto : listaAutosCubiertos unGasto modificacion condicionModificacion (presupuesto - unGasto unAuto) colaAutos)
    | otherwise = (presupuestoRestante unGasto modificacion condicionModificacion presupuesto colaAutos, unAuto:listaAutosCubiertos unGasto modificacion condicionModificacion presupuesto colaAutos)

presupuestoRestante :: (Auto -> Float) -> (Auto -> Auto) -> (Auto -> Bool) -> Float -> ListaAutos -> Float
presupuestoRestante unGasto modificacion condicionModificacion presupuesto listaAutos = fst (autosCubiertos unGasto modificacion condicionModificacion presupuesto listaAutos)

listaAutosCubiertos :: (Auto -> Float) -> (Auto -> Auto) -> (Auto -> Bool) -> Float -> ListaAutos -> ListaAutos
listaAutosCubiertos unGasto modificacion condicionModificacion presupuesto listaAutos = snd (autosCubiertos unGasto modificacion condicionModificacion presupuesto listaAutos)

agregarAutoEnEquipo :: Auto -> Equipo -> Equipo
agregarAutoEnEquipo unAuto unEquipo 
    | presupuestoSuficiente (costoInscripcion unAuto) (presupuesto unEquipo) = actualizarEquipo (unAuto:) (subtract (costoInscripcion unAuto)) unEquipo
    | otherwise = id unEquipo

autosParaReparar :: Equipo -> ListaAutos
autosParaReparar unEquipo = listaAutosCubiertos costoReparacion repararUnAuto (const True) (presupuesto unEquipo) (listaAutos unEquipo)
presupuestoReparar :: Equipo -> Float
presupuestoReparar unEquipo = presupuestoRestante costoReparacion repararUnAuto (const True) (presupuesto unEquipo) (listaAutos unEquipo)

autosParaOptimizar :: Equipo -> ListaAutos
autosParaOptimizar unEquipo = listaAutosCubiertos costoOptimizar ponerleNitroAUnAuto (const True) (presupuesto unEquipo) (listaAutos unEquipo)
presupuestoOptimizar :: Equipo -> Float
presupuestoOptimizar unEquipo = presupuestoRestante costoOptimizar ponerleNitroAUnAuto (const True) (presupuesto unEquipo) (listaAutos unEquipo)

autosParaFerrarizar :: Equipo -> ListaAutos
autosParaFerrarizar unEquipo = listaAutosCubiertos costoFerrarizar (llevarUnAutoAUnDesarmadero "Ferrari" "F50") (not . marcaAuto "Ferrari") (presupuesto unEquipo) (listaAutos unEquipo)
presupuestoFerrarizar :: Equipo -> Float
presupuestoFerrarizar unEquipo = presupuestoRestante costoFerrarizar (llevarUnAutoAUnDesarmadero "Ferrari" "F50") (not . marcaAuto "Ferrari") (presupuesto unEquipo) (listaAutos unEquipo)

repararEquipo :: Equipo -> Equipo
repararEquipo unEquipo = actualizarEquipo (const (autosParaReparar unEquipo)) (const (presupuestoReparar unEquipo)) unEquipo

optimizarEquipo :: Equipo -> Equipo
optimizarEquipo unEquipo = actualizarEquipo (const (autosParaOptimizar unEquipo)) (const (presupuestoOptimizar unEquipo)) unEquipo

ferrarizarEquipo :: Equipo -> Equipo
ferrarizarEquipo unEquipo = actualizarEquipo (const (autosParaFerrarizar unEquipo)) (const (presupuestoFerrarizar unEquipo)) unEquipo


{- CASOS DE PRUEBA

1A) Casos de prueba para agregarAutoEnEquipo:

Aclaracion: para poder hacer los casos de prueba se cambiaron los valores de los equipos dependiendo de la situacion requerida y el valor del presupuesto

agregarAutoEnEquipo ferrari equipo1     
UnEquipo {nombreEquipo = "Equipo1", presupuesto = 5000.0, listaAutos = [UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (0.0,0.0), velocidadMaxima = 65.0, tiempoCarrera = 0.0, apodos = ["La nave","El fierro","Ferrucho"]}]}

agregarAutoEnEquipo fiat equipo1        
UnEquipo {nombreEquipo = "Equipo1", presupuesto = 6000.0, listaAutos = [UnAuto {marca = "Fiat", modelo = "600", desgaste = (27.0,33.0), velocidadMaxima = 44.0, tiempoCarrera = 0.0, apodos = ["La Bocha","La bolita","fitito"]},UnAuto {marca = "Peugeot", modelo = "504", desgaste = (0.0,0.0), velocidadMaxima = 40.0, tiempoCarrera = 0.0, apodos = ["El rey del desierto"]}]}

agregarAutoEnEquipo lamborghini  equipo1
UnEquipo {nombreEquipo = "Equipo1", presupuesto = 70000.0, listaAutos = []}

1B) Casos de prueba para repararEquipo:

Aclaracion: para algunos casos de prueba se modifico el presupuesto ddel equipo, segun lo que pedia el ejercicio (equipo1 presupuesto 20000, equipo2 presupuesto 10000)

repararEquipo equipo1
UnEquipo {nombreEquipo = "Equipo1", presupuesto = 7250.0, listaAutos = [UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (0.0,1.5), velocidadMaxima = 65.0, tiempoCarrera = 0.0, apodos = ["La nave","El fierro","Ferrucho"]},
UnAuto {marca = "Lamborghini", modelo = "Diablo", desgaste = (0.0,3.0), velocidadMaxima = 73.0, tiempoCarrera = 0.0, apodos = ["Lambo","La bestia"]}]}

repararEquipo equipo2
UnEquipo {nombreEquipo = "Equipo2", presupuesto = 10000.0, listaAutos = [UnAuto {marca = "Peugeot", modelo = "504", desgaste = (0.0,50.0), velocidadMaxima = 40.0, 
tiempoCarrera = 0.0, apodos = ["El rey del desierto"]}]}

1C) Casos de prueba para optimizarEquipo:

Aclaracion: para algunos casos de prueba se modifico el presupuesto ddel equipo, segun lo que pedia el ejercicio (equipo1 presupuesto 20000, equipo1 presupuesto 10000)

optimizarEquipo equipo1
UnEquipo {nombreEquipo = "Equipo1", presupuesto = 6200.0, listaAutos = [UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (0.0,10.0), velocidadMaxima = 78.0, tiempoCarrera = 0.0, apodos = ["La nave","El fierro","Ferrucho"]},
UnAuto {marca = "Lamborghini", modelo = "Diablo", desgaste = (0.0,20.0), velocidadMaxima = 87.600006, tiempoCarrera = 0.0, apodos = ["Lambo","La bestia"]}]}

optimizarEquipo equipo1
UnEquipo {nombreEquipo = "Equipo1", presupuesto = 3500.0, listaAutos = [UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (0.0,10.0), velocidadMaxima = 78.0, tiempoCarrera = 0.0, apodos = ["La nave","El fierro","Ferrucho"]},
UnAuto {marca = "Lamborghini", modelo = "Diablo", desgaste = (0.0,20.0), velocidadMaxima = 73.0, tiempoCarrera = 0.0, apodos = ["Lambo","La bestia"]}]}


1D) Casos de prueba para ferrarizarEquipo:

Aclaracion: para algunos casos de prueba se modificaron los datos de los equipos 1 y 2; al equipo 1 se le modifico agrego el auto peugeot

ferrarizarEquipo equipo1
UnEquipo {nombreEquipo = "Equipo1", presupuesto = 13000.0, listaAutos = [UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (0.0,10.0), velocidadMaxima = 65.0, 
tiempoCarrera = 0.0, apodos = ["La nave","El fierro","Ferrucho"]},UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (0.0,20.0), velocidadMaxima = 73.0, tiempoCarrera = 0.0, apodos = ["Nunca Taxi"]}]}

ferrarizarEquipo equipo2
UnEquipo {nombreEquipo = "Equipo2", presupuesto = 500.0, listaAutos = [UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (0.0,50.0), velocidadMaxima = 40.0, tiempoCarrera = 0.0, apodos = ["Nunca Taxi"]},
UnAuto {marca = "Lamborghini", modelo = "Diablo", desgaste = (0.0,20.0), velocidadMaxima = 73.0, tiempoCarrera = 0.0, apodos = ["Lambo","La bestia"]}]}

ferrarizarEquipo equipo1
UnEquipo {nombreEquipo = "Equipo1", presupuesto = 13000.0, listaAutos = [UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (4.0,7.0), velocidadMaxima = 73.0, tiempoCarrera = 0.0, apodos = ["Nunca Taxi"]},
UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (0.0,0.0), velocidadMaxima = 40.0, tiempoCarrera = 0.0, apodos = ["Nunca Taxi"]},
UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (0.0,0.0), velocidadMaxima = 65.0, tiempoCarrera = 0.0, apodos = ["La nave","El fierro","Ferrucho"]}]}

-}

-- {Punto 2: Costo Total de Reparacion}

costoTotalReparacion :: Equipo -> Float
costoTotalReparacion unEquipo = sum . map costoReparacion $ (listaAutos unEquipo)

{-Casos de Pruebas

Aclaracion: para el segundo caso de prueba se modifico el equipo2 agragando el fiat con desgasta de chasis de 50 y peugeot se modifico su desgaste de chasis a 0

costoTotalReparacion equipo1
12750.0

costoTotalReparacion equipo2
21250.0

-}

-- {Punto 3: Infinia}

incrementarAuto :: Int -> Auto
incrementarAuto unNumero = (ferrari { 
    desgaste = (0, 1), 
    velocidadMaxima = 65 * fromIntegral unNumero 
    })

listaFerraris :: ListaAutos
listaFerraris = map incrementarAuto [1 ..]

infinia :: Equipo
infinia = UnEquipo "Infinia" 5000 listaFerraris

-- 3b: Respuestas
-- i. Reparar equipo Infinia: repararia solo 10 ferraris y se acabaria el presupuesto, sin embargo
-- al retornar un equipo con lista de autos infinitos se trabaria al devolver el resto de autos
--      (5000 - (1 * 500) = 4500) -> (5000 / 500 = 10 autos), desgaste reparado de los 10 autos = (0, 0.15)
-- ii. Optimizar equipo Infinia: optimizaria 2 autos y se acabaria el presupuesto, tambien
-- al retornar un equipo con lista de autos infinitos se trabaria al devolver el resto de autos
--      (5000 - (65 * 100) = 3500) -> (3500 - (65 * 200) = 1500) -> (1500 - (65 * 300) = -100)
-- iii. Ferrarizar equipo Infinia: se quedaria buscando un auto distinto de ferrari en la lista del equipo infinia
-- infinitamente, y no gastaria el presupuesto. No retornaria ningun valor.
-- iv. costoTotalReparacion equipo Infinia: no retornaria ningun valor, se trabaria por el map infinito


-- {Punto 4: Modificadores de Tramos}

boxes :: Tramo -> Tramo
boxes unTramo unAuto
    | not (autoEnBuenEstado unAuto) = (repararUnAuto . mapTiempoCarreraAuto (+10) . unTramo) unAuto
    | otherwise = unTramo unAuto

tiempoExtra :: Tramo -> Auto -> Float
tiempoExtra unTramo unAuto = subtract (tiempoCarrera unAuto) (tiempoCarrera . unTramo $ unAuto) * 0.5

tramoMojado :: Tramo -> Tramo
tramoMojado unTramo unAuto = mapTiempoCarreraAuto (+ tiempoExtra unTramo unAuto) . unTramo $ unAuto

tramoRipio :: Tramo -> Tramo
tramoRipio unTramo unAuto = unTramo . unTramo $ unAuto

desgasteObstruccion :: Float -> Float
desgasteObstruccion metrosObstruidos = 2 * metrosObstruidos

tramoObstruido :: Float -> Tramo -> Tramo
tramoObstruido metrosObstruidos unTramo unAuto = mapDesgasteRuedasAuto (+ desgasteObstruccion metrosObstruidos) . unTramo $ unAuto

autoConTurbo :: Auto -> Auto
autoConTurbo unAuto = mapVelocidadMaximaAuto (*2) unAuto

autoSinTurbo :: Auto -> Auto
autoSinTurbo unAuto = mapVelocidadMaximaAuto (/2) unAuto

tramoConTurbo :: Tramo -> Tramo
tramoConTurbo unTramo unAuto = autoSinTurbo . unTramo . autoConTurbo $ unAuto

-- {Punto 5: Pasar Tramo}

pasarPorTramo :: Auto -> Tramo -> Auto
pasarPorTramo unAuto unTramo
    | noDaMas unAuto = unAuto
    | otherwise = unTramo unAuto


-- {Punto 6: Dar la Vuelta}

vueltaALaManzana :: Pista
vueltaALaManzana = UnaPista "La Manzana" "Italia" 30 [recto 130, curva 90 13, recto 130, curva 90 13, recto 130, curva 90 13, recto 130, curva 90 13]

superPista :: Pista
superPista  = UnaPista "Super Pista" "Argentina" 300 [tramoRectoClassic, curvaTranca, tramoConTurbo.tramoMojado$ tramito, ruloEnAire 10, tramoObstruido 2 (curva 80 400), 
    curva 115 650, recto 970, curvaPeligrosa, tramoRipio tramito, boxes (recto 800), tramoObstruido 5 casiCurva, zigZag 2, tramoRipio.tramoMojado$ deseoDeMuerte,
    ruloClasico, zigZagLoco]

autosManzana :: ListaAutos
autosManzana = [ferrari {desgaste = (0, 0)}, peugeot {desgaste = (79, 0)}]

recorrerTramo :: [Tramo] -> Auto -> Auto
recorrerTramo tramosPista unAuto = foldl pasarPorTramo unAuto tramosPista

peganLaVuelta :: Pista -> ListaAutos -> ListaAutos
peganLaVuelta unaPista listaAutos = map (recorrerTramo (tramos unaPista)) listaAutos
-- cada auto de la lista va a recorrer la pista completa o hasta donde llegue

-- Caso de prueba

-- peganLaVuelta vueltaALaManzana autosManzana
-- [UnAuto {marca = "Ferrari", modelo = "F50", desgaste = (1.7333333,5.2), velocidadMaxima = 65.0, tiempoCarrera = 9.6, apodos = ["La nave","El fierro","Ferrucho"]},
-- UnAuto {marca = "Peugeot", modelo = "504", desgaste = (80.3,3.8999999), velocidadMaxima = 40.0, tiempoCarrera = 11.7, apodos = ["El rey del desierto"]}]


-- {Punto 7: Carreras}
type ResultadoVuelta = (Int, Auto, ListaAutos)
data Carrera = UnaCarrera {
    unaPista :: Pista,
    cantidadVueltas :: Int
} deriving (Show)

tourBuenosAires :: Carrera
tourBuenosAires = UnaCarrera vueltaALaManzana 3

autosCarrera :: ListaAutos
autosCarrera = [ferrari, fiat, peugeot, lamborghini] 

quickSortBy :: Ord b => (a -> b) -> [a] -> [a]
quickSortBy _ [] = []
quickSortBy valoracion (x:xs) = anteriores ++ [x] ++ posteriores    
    where
        anteriores  = quickSortBy valoracion $ filter ((< valoracion x).valoracion)  xs
        posteriores = quickSortBy valoracion $ filter ((>= valoracion x).valoracion) xs

autoGanadorVuelta :: ResultadoVuelta -> Auto
autoGanadorVuelta (_, auto, _) = auto

eliminarSiNoDaMas :: ListaAutos -> ListaAutos
eliminarSiNoDaMas unosAutos = filter (not . noDaMas) unosAutos

autosDespuesDeVuelta :: ListaAutos -> Pista -> ListaAutos
autosDespuesDeVuelta unosAutos unaPista = (eliminarSiNoDaMas . quickSortBy tiempoCarrera) (peganLaVuelta unaPista unosAutos)

resultadoVuelta :: ListaAutos -> Pista -> Int -> ResultadoVuelta
resultadoVuelta unosAutos unaPista numeroVuelta = (numeroVuelta, head(autosDespuesDeVuelta unosAutos unaPista), autosDespuesDeVuelta unosAutos unaPista)

resultadosPorVuelta :: ListaAutos -> Carrera -> [ResultadoVuelta]
resultadosPorVuelta unosAutos (UnaCarrera pista vueltas)
    | vueltas == 0 || null (autosDespuesDeVuelta unosAutos pista) = []
    | otherwise = resultadoVuelta unosAutos pista vueltas 
        : resultadosPorVuelta (peganLaVuelta pista unosAutos) (UnaCarrera pista (vueltas - 1))

mostrarAutosRestantes :: Auto -> String
mostrarAutosRestantes unAuto = marca unAuto ++ " " ++ modelo unAuto ++ " " ++ show (tiempoCarrera unAuto)

mostrarResultadoVuelta :: Int -> ResultadoVuelta -> String
mostrarResultadoVuelta cantVueltas (nroVuelta, autoGanador, autos) =
    "Vuelta: " ++ show (subtract nroVuelta (cantVueltas+1)) ++
    " | Ganador: " ++ marca autoGanador ++ " " ++ modelo autoGanador ++ " " ++ show (tiempoCarrera autoGanador) ++
    " | Autos: " ++ show (map (mostrarAutosRestantes) autos)

imprimirAutos :: Int -> [ResultadoVuelta] -> [String]
imprimirAutos cantVueltas listaResultados = map (mostrarResultadoVuelta cantVueltas) listaResultados
