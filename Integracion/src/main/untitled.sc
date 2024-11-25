def integracion(f: Double => Double, a: Double, b: Double): Double =
  val xar: Double = (a + b) / 2.0
  (b - a)* (f(a) + 4 * f(xar) + f(b))/6.0

// Función para calcular el error
def calcularError(valorEsperado: Double, valorObtenido: Double): Double = {
  math.abs(valorEsperado - valorObtenido)
}


// Integral 1: ∫ 3,5 (-x^2 + 8x - 12) dx
val funcion1: Double => Double = (x: Double) => -x * x + 8 * x - 12
val resultado1: Double = integracion(funcion1, 3.0, 5.0)
val error1 : Double= calcularError(7.33, resultado1)


// Integral 2: ∫_0^2 3x^2 dx
val funcion2 : Double => Double = (x: Double) => 3 * x * x
val resultado2: Double = integracion(funcion2, 0.0, 2.0)
val error2: Double = calcularError(8.0, resultado2)


// Integral 3: ∫ -1,1 (x + 2x^2 - x^3 + 5x^4) dx
val funcion3 : Double => Double = (x: Double) => x + 2 * x * x - Math.pow(x, 3) + 5 * Math.pow(x, 4)
val resultado3: Double = integracion(funcion3, -1.0, 1.0)
val error3: Double = calcularError(3.333, resultado3)


// Integral 4: ∫ 12 (2x + 1) / (x^2 + x) dx
val funcion4 : Double => Double = (x: Double) => (2 * x + 1) / (x * x + x)
val resultado4: Double = integracion(funcion4, 1.0, 2.0)
val error4: Double  = calcularError(1.09861, resultado4)


// Integral 5: ∫ 0,1 e^x dx
val funcion5 : Double => Double = (x: Double) => Math.exp(x)
val resultado5: Double = integracion(funcion5, 0.0, 1.0)
val error5: Double = calcularError(1.71828, resultado5)


// Integral 6: ∫ 2,3 1 / (x - 1) dx
val funcion6 : Double => Double = (x: Double) => 1 / (x - 1)
val resultado6: Double = integracion(funcion6, 2.0, 3.0)
val error6: Double = calcularError(0.828427, resultado6)


// Integral 7: ∫ 0,1 1 / (1 + x^2) dx
val funcion7 : Double => Double = (x: Double) => 1 / (1 + x * x)
val resultado7: Double = integracion(funcion7, 0.0, 1.0)
val error7: Double = calcularError(0.785398, resultado7)
