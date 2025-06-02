
aplicarDescuento :: Fractional a => a -> a -> a
aplicarDescuento unDescuento unPrecio  = 
    unPrecio - unPrecio * unDescuento / 100
-- uso / para división de fraccionarios y agrupo con ()
-- fromIntegral convierte un num int a float para q / sea compatible con R

aplicarCostoEnvio :: Num a => a -> a -> a
aplicarCostoEnvio unCostoEnvio unPrecio = unPrecio + unCostoEnvio

precioTotal :: Fractional a => a -> a -> a -> a -> a
precioTotal unPrecioUnitario unaCantidad unDescuento unCostoEnvio = 
    (aplicarCostoEnvio unCostoEnvio . aplicarDescuento unDescuento) (unPrecioUnitario * unaCantidad)
-- si no pongo el 2do () * todo x unaCant al final
-- de esta forma al precio*cant se le appDescto y dsp se le + el costoEnvio

productoXL :: String -> String
productoXL unProducto = unProducto ++ "XL"
-- ++ concatena strings

esProductoCorriente :: String -> Bool
esProductoCorriente unProducto = elem (head unProducto) "aeiou"
-- head devuelve el primer char de un string o el 1er elem de una lista
-- elem devuelve T o F si el char pertenece a la lista de vocales

esProductoCodiciado :: String -> Bool
esProductoCodiciado unProducto = length unProducto > 10
-- length devuelve la cant de chars de un string

esProductoDeLujo :: String -> Bool
esProductoDeLujo unProducto = elem 'x' unProducto || elem 'z' unProducto

descodiciarProducto :: String -> String
descodiciarProducto unProducto 
    | esProductoCodiciado unProducto = take 10 unProducto
    | otherwise = init unProducto
-- si es codiciado, lo corto a 10 chars, sino le saco el último char con init para length <10

versionBarata :: String -> String
versionBarata unProducto = (reverse . descodiciarProducto) unProducto
-- descodicio el prod y dsp le invierto el nom con reverse

esProductoDeElite :: String -> Bool
esProductoDeElite unProducto = 
    esProductoDeLujo unProducto && esProductoCodiciado unProducto && not (esProductoCorriente unProducto)
-- es de lujo, codiciado y no corriente (usando not)

entregaSencilla :: String -> Bool
entregaSencilla unaFecha = (even . length) unaFecha 
-- length devuelve la cant de chars de unaFecha y even ve si ese length es par
-- version equivalente: 
-- entregaSencilla :: String -> Bool
-- entregaSencilla unaFecha = rem (length unaFecha) 2 == 0
-- rem devuelve el resto de la division entre 2, si es 0 es par (entrega sencilla)