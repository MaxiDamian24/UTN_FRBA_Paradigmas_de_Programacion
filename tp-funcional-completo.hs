import Text.Show.Functions
import Data.List(genericLength)
--
-- * genericLength :: Num i => [a] -> i
-- -- Esta función es exactamente igual que length,
-- -- con la única diferencia que no devuelve un Int, sino un número
-- -- fácil de operar con otro número que pueden o no ser enteros.
-- --
-- -- -- ghci> length "Luigi Mario" / 2
-- -- -- error:
-- -- --     • No instance for (Fractional Int) arising from a use of ‘/’
-- -- --     • In the expression: length "Luigi Mario" / 2
-- -- --       In an equation for ‘it’: it = length "Luigi Mario" / 2
-- -- -- ghci> genericLength "Luigi Mario" / 2
-- -- -- 5.5

type Atraccion = String

data Ciudad = Ciudad {
    nombre      :: String,
    anio        :: Float,
    atracciones :: [Atraccion],
    costoDeVida :: Float
} deriving (Show)

-------------------------
--       PUNTO 1       --
-- Valor de una ciudad --
-------------------------

-- Valor de una ciudad --
--Definir el valor de una ciudad, un número que se obtiene de la siguiente manera:
--Si fue fundada antes de 1800, su valor es 5 veces la diferencia entre 1800 y el año de fundación.
--Si no tiene atracciones, su valor es el doble del costo de vida.
--De lo contrario, será 3 veces el costo de vida de la ciudad.

valorCiudad :: Ciudad -> Float
valorCiudad unaCiudad
    | anio unaCiudad < 1800 = valorAnio unaCiudad
    | otherwise = valorAtracciones unaCiudad

valorAnio :: Ciudad -> Float
valorAnio unaCiudad
    | anio unaCiudad < 1800 = (1800 -  anio unaCiudad) * 5
    | otherwise = 0

valorAtracciones :: Ciudad -> Float
valorAtracciones unaCiudad
    | genericLength (atracciones unaCiudad) == 0 = (costoDeVida unaCiudad) * 2
    | otherwise = (costoDeVida unaCiudad) * 3


-------------------------------------
--             PUNTO 2             --
-- Características de las ciudades --
-------------------------------------

-- Alguna atracción copada --
--Queremos saber si una ciudad tiene alguna atracción copada, esto es que la atracción comience con una vocal. Por ejemplo: "Acrópolis" es una atracción copada y "Golden Gate" no es copada.

esVocal :: Char -> Bool
esVocal letra = elem letra "aeiouAEIOU"

esCopada :: Ciudad -> Bool
esCopada unaCiudad = any (comienzaVocal) (atracciones unaCiudad)

comienzaVocal :: Atraccion -> Bool
comienzaVocal unaAtraccion = esVocal (head unaAtraccion)


-- Ciudad sobria --
--Queremos saber si una ciudad es sobria, esto se da si todas las atracciones tienen más de cierta cantidad de letras.

esSobria :: Ciudad -> Int -> Bool
esSobria  unaCiudad  cantCaracteres = all (letrasSuficientes cantCaracteres ) (atracciones unaCiudad)

letrasSuficientes ::  Int -> Atraccion -> Bool
letrasSuficientes cantCaracteres unaAtraccion  = ((> cantCaracteres) . length) unaAtraccion


-- Ciudad con nombre raro --
--Queremos saber si una ciudad tiene un nombre raro, esto implica que tiene menos de 5 letras en su nombre.

nombreRaro :: Ciudad -> Bool
nombreRaro unaCiudad =  ((<5) . length . nombre) unaCiudad


-------------
-- PUNTO 3 --
-- Eventos --
-------------

-- Queremos poder registrar eventos que ocurren sobre una ciudad y que la afectan en mayor o menor medida. Dichos eventos son:
type Evento = Ciudad -> Ciudad

-- Sumar una nueva atracción --
--Queremos poder agregar una nueva atracción a la ciudad. Esto implica un esfuerzo de toda la comunidad en tiempo y dinero, lo que se traduce en un incremento del costo de vida de un 20%.

sumarAtraccion :: Atraccion -> Evento
sumarAtraccion unaAtraccion = cambiarCostoDeVidaPorcentual 20 . agregarAtraccion unaAtraccion

cambiarCostoDeVidaPorcentual :: Float -> Ciudad -> Ciudad
cambiarCostoDeVidaPorcentual porcentaje unaCiudad = flip cambiarCostoDeVida unaCiudad $ porcentaje * costoDeVida unaCiudad / 100
cambiarCostoDeVida :: Float -> Ciudad -> Ciudad
cambiarCostoDeVida modificador unaCiudad = unaCiudad { costoDeVida = modificador + costoDeVida unaCiudad }

modificarAtracciones :: ([Atraccion] -> [Atraccion]) -> Ciudad -> Ciudad
modificarAtracciones unaFuncion unaCiudad = unaCiudad { atracciones = unaFuncion $ atracciones unaCiudad }
agregarAtraccion :: Atraccion -> Ciudad -> Ciudad
agregarAtraccion unaAtraccion = modificarAtracciones (unaAtraccion :)


-- Crisis --
--Al atravesar una crisis, la ciudad baja un 10% su costo de vida y se debe cerrar la última atracción de la lista.

crisis :: Evento
crisis = cerrarUltimaAtraccion . cambiarCostoDeVidaPorcentual (-10)

cerrarUltimaAtraccion :: Ciudad -> Ciudad
cerrarUltimaAtraccion unaCiudad = modificarAtracciones (take ((genericLength $ atracciones unaCiudad) - 1)) unaCiudad


-- Remodelación --
--Al remodelar una ciudad, incrementa su costo de vida un porcentaje que se indica al hacer la remodelación y le agrega el prefijo "New " al nombre.

remodelar :: Float -> Evento
remodelar porcentaje = agregarPrefijo "New" . cambiarCostoDeVidaPorcentual porcentaje

agregarPrefijo :: String -> Ciudad -> Ciudad
agregarPrefijo unPrefijo unaCiudad = unaCiudad { nombre = unPrefijo ++ " " ++ nombre unaCiudad }


-- Reevaluación --
--Si la ciudad es sobria con atracciones de más de n letras (valor que se quiere configurar), aumenta el costo de vida un 10%, si no baja 3 puntos.

reevaluar :: Int -> Evento
reevaluar cantidadLetras unaCiudad
    | flip esSobria cantidadLetras unaCiudad = cambiarCostoDeVidaPorcentual 10 unaCiudad
    | otherwise = cambiarCostoDeVida (-3) unaCiudad


-------------------------------
--          PUNTO 4          --
-- La transformación no para --
-------------------------------
{-
Reflejar de qué manera podemos hacer que una ciudad tenga

- el agregado de una nueva atracción

    ej: azulConAtraccion = sumarAtraccion "Parque del Este" azul

- una remodelación

    ej: azulRemodelada = remodelar 12.5 azul

- una crisis

    ej: azulCunCrisis crisis azul

- y una reevaluación

    ej: azulReevaluado = reevaluar 10 azul

-}

--------------------------
--        PUNTO 5       --
-- Un año para recordar --
--------------------------

-- Los años pasan... --
--Queremos modelar un año, donde definamos
--el número que le corresponde
--una serie de eventos que se produjeron
--También queremos reflejar el paso de un año para una ciudad, es decir, que los eventos afecten el estado final en el que queda una ciudad.

data Anio = Anio {
    numero :: Int,
    eventos :: [Evento]
} deriving (Show)

pasoDeUnAnio :: Anio -> Ciudad -> Ciudad
pasoDeUnAnio unAnio unaCiudad = foldl (aplicarEvento) unaCiudad $ eventos unAnio

aplicarEvento :: Ciudad -> Evento -> Ciudad
aplicarEvento unaCiudad unEvento = unEvento unaCiudad


-- Algo mejor --
--Implementar una función que reciba una ciudad, un criterio de comparación y un evento, de manera que nos diga si la ciudad tras el evento subió respecto a ese criterio. 

compararSegunCriterio :: Ord a => Ciudad -> (Ciudad -> a) -> Evento -> Bool
compararSegunCriterio unaCiudad unCriterio unEvento = unCriterio unaCiudad < (unCriterio . flip aplicarEvento unEvento) unaCiudad


-- Costo de vida que suba --
--Para un año, queremos aplicar sobre una ciudad solo los eventos que hagan que el costo de vida suba. Debe quedar como resultado la ciudad afectada con dichos eventos.

aplicarSoloMayorCostoDeVida :: Anio -> Ciudad -> Ciudad
aplicarSoloMayorCostoDeVida unAnio unaCiudad = pasoDeUnAnio (filtrarEventos (compararSegunCriterio unaCiudad costoDeVida) unAnio) unaCiudad

filtrarEventos :: (Evento -> Bool) -> Anio -> Anio
filtrarEventos unaFuncion unAnio = unAnio { eventos = filter (unaFuncion) $ eventos unAnio }


-- Costo de vida que baje --
--Para un año, queremos aplicar solo los eventos que hagan que el costo de vida baje. Debe quedar como resultado la ciudad afectada con dichos eventos.

aplicarSoloMenorCostoDeVida :: Anio -> Ciudad -> Ciudad
aplicarSoloMenorCostoDeVida unAnio unaCiudad = pasoDeUnAnio (filtrarEventos (not . compararSegunCriterio unaCiudad costoDeVida) unAnio) unaCiudad


-- Valor que suba --
--Para un año, queremos aplicar solo los eventos que hagan que el valor suba. Debe quedar como resultado la ciudad afectada con dichos eventos.

aplicarSoloMayorValor :: Anio -> Ciudad -> Ciudad
aplicarSoloMayorValor unAnio unaCiudad = pasoDeUnAnio (filtrarEventos (compararSegunCriterio unaCiudad valorCiudad) unAnio) unaCiudad


--------------------------
--        PUNTO 6       --
-- Funciones a la orden --
--------------------------

-- 6.1 Eventos ordenados --
--Dado un año y una ciudad, queremos saber si los eventos están ordenados en forma correcta,
--esto implica que el costo de vida al aplicar cada evento a la ciudad original se va incrementando
--respecto al anterior evento. Debe haber al menos un evento para dicho año.

eventosOrdenados :: Anio -> Ciudad -> Bool
eventosOrdenados Anio{ eventos = [] } _ = False
eventosOrdenados Anio{ eventos = [_] } _ = True
eventosOrdenados anio ciudad =
    (costoDeVida . ev1 $ ciudad) <= (costoDeVida . ev2 $ ciudad) && eventosOrdenados anio{ eventos = ev2:evs } ciudad
    where ev1:ev2:evs = eventos anio





-- Ciudades ordenadas --
--Dado un evento y una lista de ciudades, queremos saber si esa lista está ordenada.
--Esto implica que el costo de vida al aplicar el evento sobre cada una de las ciudades
--queda en orden creciente. Debe haber al menos una ciudad en la lista.

ciudadesOrdenadas :: Evento -> [Ciudad] -> Bool
ciudadesOrdenadas _ [] = False
ciudadesOrdenadas _ [_] = True
ciudadesOrdenadas evento (c1:c2:ciudades) =
    costoTrasEvento c1 <= costoTrasEvento c2 && ciudadesOrdenadas evento (c2:ciudades)
    where costoTrasEvento = costoDeVida . evento

-- 6.3 Años ordenados --
--Dada una lista de años y una ciudad, queremos saber si el costo de vida al aplicar todos
--los eventos de cada año sobre esa ciudad termina generando una serie de costos de vida
--ascendente (de menor a mayor). Debe haber al menos un año en la lista.

aniosOrdenados :: [Anio] -> Ciudad -> Bool
aniosOrdenados [] _ = False
aniosOrdenados [_] _ = True
aniosOrdenados (anio1:anio2:anios) ciudad =
    costoDeVidaDeUnAnio anio1 <= costoDeVidaDeUnAnio anio2 && aniosOrdenados (anio2:anios) ciudad
    where costoDeVidaDeUnAnio = costoDeVida . flip pasoDeUnAnio ciudad

--------------------------------
--        PUNTO 7             --
-- Al infinito, y más allá... --
--------------------------------

-- Eventos ordenados --
-- Definir el año 2024 con una lista de eventos que inicia con una crisis, luego una reevaluación
-- de atracciones con 7 letras y luego tiene una sucesión infinita de remodelaciones cuyo porcentaje
-- de aumento en el costo de vida es 1 para la primera remodelación, 2 para la siguiente, y así hasta el infinito.
{--

¿Puede haber un resultado posible para la función del punto 6.1 (eventos ordenados) para el año 2024?
Justificarlo relacionándolo con conceptos vistos en la materia.

Respuesta:
    Depende, Gracias al Lazy Evaliation, si los primeros 3 eventos resultan "No estar orenados", la función eventosOrdenados
    retornar False, por ejemplo:

        eventosOrdenados anio2024 baradero

    pero sí los primeros 3 eventos resultan "Estar Ordenados", la recursividad de eventosOrdenados no podrá llegar nunca al
    final de la lista, ya que los subsiguientes eventos (remodelar) siempre estarán "Ordenados" respecto al anterior
    por haber sido construidos con un valor mayor que el anterior. Por ejemplo:

        eventosOrdenados anio2024 (Ciudad "Maipú" 1878 ["Fortín"] 115)
--}

anio2024 = Anio {
    numero = 2024,
    eventos = crisis : reevaluar 7 : map remodelar (iterate (+1) 1)
}


-- Ciudades ordenadas --
-- Definir una lista de ciudades “disco rayado” que comience con Azul y Nullish y luego cicle
-- intercalando infinitamente entre Caleta Olivia y Baradero.

{--
¿Puede haber un resultado posible para la función del punto 6.2 (ciudades ordenadas) para la lista
“disco rayado”? Justificarlo relacionándolo con conceptos vistos en la materia.

Respuesta:
    Depende bajo qué evento se esté evaluando. Como haskell tiene lazy Evaluation, los miembros de la lista no se crean sino hasta que se necesitan,
    permitiendole a la función ciudadesOrdenadas llegar a conseguir un a ciudad que no cumpla el criterio entre las primesa 4 ciudades, si no la
    conigue, como el resto de las ciudades son las mismas que en las de índice 2 y 3, la recursión de ciudadesOrdenadas se quedará evaluando
    infinitamente(hasta que se agoten los recursos y el SO "mate" al proceso o hasta que se detenga su ejecución explicitamente).
    
    ej:
    
    - sí termina: 
        ciudadesOrdenadas (cambiarCostoDeVida 22) discoRayado
        ciudadesOrdenadas crisis discoRayado
    
    - no termina:
        ciudadesOrdenadas (\ciudad -> ciudad { costoDeVida = costoDeVida ciudad ** 200 }) discoRayado
--}

discoRayado = azul : nullish : cycle [caletaOlivia, baradero]


-- Años ordenados --
-- Definir una lista de años “la historia sin fin” que comience con 2021, 2022 y continúe con infinitos 2023.
{--
¿Puede haber un resultado posible para la función del punto 6.3 (años ordenados) para “la historia sin fin”?
Justificarlo relacionándolo con conceptos vistos en la materia.

Respuesta:
    Sí, y siempre será Falso ya que la función aniosOrdenados solo llega a compara los años 2021 y 2022 los
    cuales siempre "Están desordenados" (para cualquier ciudad) cuando se los evalúe en ese orden, además de
    que Gracias al Lazy Evaluation, la lista no necesita crearse por completo, permitiendo así que aniosOrdenados
    pueda completar su ejecución.
--}

laHistoriaSinFin = anio2021 : anio2022 : repeat anio2023

-----------------------------------------------------------------------------------------------------------
-- Definición de los TDAs que se mencionan en los enunciados para testear el TP que desarrollemos --

-- Ciudades del punto 1
baradero = Ciudad "Baradero" 1615 ["Parque del Este", "Museo Alejandro Barbich"] 150
nullish = Ciudad "Nullish" 1800 [] 140
caletaOlivia = Ciudad "Caleta Olivia" 1901 ["El Gorosito", "Faro Costanera"] 120
-- Ciudades del punto 2
maipu = Ciudad "Maipú" 1878 ["Fortín Kakel"] 115
azul = Ciudad "Azul" 1832 ["Teatro Español", "Parque Municipal Sarmiento", "Costanera Cacique Catriel"] 190
-- Años del punto 5
anio2022 = Anio 2022 [crisis, remodelar 5, reevaluar 7]
anio2015 = Anio 2015 []
-- Años del punto 6
anio2023 = Anio 2023 [crisis, sumarAtraccion "Parque", remodelar 10, remodelar 20]
anio2021 = Anio 2021 [crisis, sumarAtraccion "Playa"]
-----------------------------------------------------------------------------------------------------------
