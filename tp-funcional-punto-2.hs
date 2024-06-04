import Text.Show.Functions

type Atraccion = String

data Ciudad = Ciudad {
    nombre      :: String,
    anio        :: Int,
    atracciones :: [Atraccion],
    costoDeVida :: Int
} deriving(Show, Eq)

-- Alguna atracción copada --
-- Queremos saber si una ciudad tiene alguna atracción copada, esto es que la atracción comience con una vocal. Por ejemplo: "Acrópolis" es una atracción copada y "Golden Gate" no es copada.

esVocal :: Char -> Bool
esVocal letra = elem letra "aeiouAEIOU"

esCopada :: Ciudad -> Bool 
esCopada unaCiudad = any (comienzaVocal) (atracciones unaCiudad)

comienzaVocal :: String -> Bool
comienzaVocal unaAtraccion = esVocal(head unaAtraccion)

-- Ciudad sobria --
-- Queremos saber si una ciudad es sobria, esto se da si todas las atracciones tienen más de cierta cantidad de letras.

esSobria :: Ciudad -> Int -> Bool
esSobria  unaCiudad  cantCaracteres = all (letrasSuficientes cantCaracteres ) (atracciones unaCiudad)

letrasSuficientes ::  Int -> String -> Bool
letrasSuficientes cantCaracteres unaAtraccion  = ((> cantCaracteres) . length) unaAtraccion 

-- Ciudad con nombre raro --
-- Queremos saber si una ciudad tiene un nombre raro, esto implica que tiene menos de 5 letras en su nombre.

nombreRaro :: Ciudad -> Bool
nombreRaro unaCiudad =  ((<5) . length . nombre) unaCiudad 