import Text.Show.Functions

type Atraccion = String

data Ciudad = Ciudad {
    nombre      :: String,
    anio        :: Int,
    atracciones :: [Atraccion],
    costoDeVida :: Int
} deriving(Show, Eq)

-- Valor de una ciudad --
--Definir el valor de una ciudad, un número que se obtiene de la siguiente manera:
--Si fue fundada antes de 1800, su valor es 5 veces la diferencia entre 1800 y el año de fundación.
--Si no tiene atracciones, su valor es el doble del costo de vida.
--De lo contrario, será 3 veces el costo de vida de la ciudad.

valorCiudad :: Ciudad -> Int 
valorCiudad unaCiudad | anio unaCiudad < 1800 = valorAnio unaCiudad
                      | otherwise = valorAtracciones unaCiudad

valorAnio :: Ciudad -> Int
valorAnio unaCiudad
    | anio unaCiudad < 1800 = (1800 -  anio unaCiudad) * 5
    | otherwise = 0

valorAtracciones :: Ciudad -> Int
valorAtracciones unaCiudad
    | length(atracciones unaCiudad) == 0 = (costoDeVida unaCiudad) * 2
    | otherwise = (costoDeVida unaCiudad) * 3