--1.Definir la función esMultiploDeTres/1, que devuelve True si un número es múltiplo de 3.
esMultiploDeTres numero =  mod numero 3 == 0

--2.Definir la función esMultiploDe/2, que devuelve True si el segundo es múltiplo del primero.
esMultiploDe primerNumero segundoNumero = mod primerNumero segundoNumero == 0

--3.Definir la función cubo/1, devuelve el cubo de un número.
cubo numero = numero * numero * numero 

--4.Definir la función area/2, devuelve el área de un rectángulo a partir de su base y su altura.
area base altura = base * altura

--5.Definir la función esBisiesto/1, indica si un año es bisiesto. (Un año es bisiesto si es divisible por 400 o es divisible por 4 pero no es divisible por 100)
esBisiesto anio = esMultiploDe anio 4 && not (esMultiploDe anio 100) 

--6.Definir la función celsiusToFahr/1, pasa una temperatura en grados Celsius a grados Fahrenheit.
celsiusToFahr temp = temp * 1.8 + 32

--7.Definir la función fahrToCelsius/1, la inversa de la anterior.
fahrToCelsius temp = (temp - 32) / 1.8

--8.Definir la función haceFrioF/1, indica si una temperatura expresada en grados Fahrenheit es fría. Decimos que hace frío si la temperatura es menor a 8 grados Celsius. 
haceFrioF temp = 8 > celsiusToFahr temp

--9.Definir la función mcm/2 que devuelva el mínimo común múltiplo entre dos números, de acuerdo a esta fórmula. 
mcm primerNumero segundoNumero = gcd primerNumero segundoNumero

{-10.Dispersión
     Trabajamos con tres números que imaginamos como el nivel del río Paraná a la altura de Corrientes medido en tres días consecutivos; cada medición es un entero que representa una cantidad de cm. 
     P.ej. medí los días 1, 2 y 3, las mediciones son: 322 cm, 283 cm, y 294 cm. 
     A partir de estos tres números, podemos obtener algunas conclusiones. 
     Definir estas funciones: 
     a. dispersion, que toma los tres valores y devuelve la diferencia entre el más alto y el más bajo. Ayuda: extender max y min a tres argumentos, usando las versiones de dos elementos. De esa forma se puede definir dispersión sin escribir ninguna guarda (las guardas están en max y min, que estamos usando). 
     b. diasParejos, diasLocos y diasNormales reciben los valores de los tres días. Se dice que son días parejos si la dispersión es chica, que son días locos si la dispersión es grande, y que son días normales si no son ni parejos ni locos. Una dispersión se considera chica si es de menos de 30 cm, y grande si es de más de un metro. 
     Nota: Definir diasNormales a partir de las otras dos, no volver a hacer las cuentas.-}
dispersion primerValor segundoValor tercerValor = max (max primerValor segundo Valor) tercerValor - min (min primerValor segundo Valor) tercerValor
diasParejos primerValor segundoValor tercerValor = 30 > dispersion primerValor segundoValor tercerValor
diasLocos primerValor segundoValor tercerValor = 100 > dispersion primerValor segundoValor tercerValor
diasNormales primerValor segundoValor tercerValor = not (diasParejos primerValor segundoValor tercerValor) && not (diasLocos primerValor segundoValor tercerValor)

{-11.En una plantación de pinos, de cada árbol se conoce la altura expresada en cm. El peso de un pino se puede calcular a partir de la altura así: 3 kg x cm hasta 3 metros, 2 kg x cm arriba de los 3 metros. P.ej. 2 metros ⇒  600 kg, 5 metros ⇒  1300 kg. 
Los pinos se usan para llevarlos a una fábrica de muebles, a la que le sirven árboles de entre 400 y 1000 kilos, un pino fuera de este rango no le sirve a la fábrica. Para esta situación: 
    a. Definir la función pesoPino, recibe la altura de un pino y devuelve su peso. 
    b. Definir la función esPesoUtil, recibe un peso en kg y devuelve True si un pino de ese peso le sirve a la fábrica, y False en caso contrario. 
    c. Definir la función sirvePino, recibe la altura de un pino y devuelve True si un pino de ese peso le sirve a la fábrica, y False en caso contrario. Usar composición en la definición.-}
pesoPino altura | altura < 3 = 3 * altura 
                | otherwise = 2 * altura
esPesoUtil peso = peso > 400 && peso < 1000
sirvePino altura = esPesoUtil(pesoPino altura)