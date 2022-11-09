# Description ----------------------------------------------------
#
# Author: Guillermo Basulto-Elias
# Created: 2022-11-06
# Last modified: 2022-11-06
# Summary: Escuela AME 1


# Tipos de datos ----------------------------------------------------

# Caracteres
"Hola"
 
# Numéricos (real or decimal)
pi
4.2

# Enteros
3

# Lógicos
TRUE
 
# Complejos
3 + 4.5i
complex(real = 3, imaginary = 4.5)
complex(argument = 2, modulus = 3)
complex(argument = pi, modulus = 3)
Re(complex(argument = 2, modulus = 3) * complex(argument = -2, modulus = 2))
Im(complex(argument = 2, modulus = 3) * complex(argument = -2, modulus = 2))



# Assignar objectos a variables -------------------------------------------

## Caracteres
x1 <- "Hola"

# Numéricos (real or decimal)
pi -> x2

# Enteros
x3 = 3L

# Lógicos
x4 <- TRUE

# Complejos
assign("x5", 3 + 4.5i)

## Imprimir
x1; x2; x3; x4; x5

# Revisar tipo
mode(x1)
mode(x2)
mode(x3)
mode(x4)
mode(x5)

## Eliminar objetos
rm(x1, x2, x3, x4, x5)

# Estructuras Básicas de Datos --------------------------------------------

# Valores
y1 <- pi

# Vectores
y2 <- c(1, 2, 3)
y3 <- c("a", "b", "c")
y4 <- c(TRUE, FALSE, NA)

# Matrices
y5 <- matrix(c(7, 8, 9, 0), nrow = 2, ncol = 2)
 
# Cuadros de datos
y6 <- data.frame(
  v1 =  y2, 
  v2 = y3, 
  v3 = c("A", "B", "C"),
  v4 = y4
  )

# Listas
y7 <- list(
  y1, y2, y3, y4, y5, y6, y9 = list(aa = c(1, 2), bb = "Hola")
)

## Imprimir valores
y1; y2; y3; y4; y5; y6; y7

## Borrar valores
rm(y1, y2, y3, y4, y5, y6, y7)


# Acceder a Elementos en Estructuras ----------------------------------------

# Estructuras
y2 <- c(a = 1, b = 2, c = 3)
y5 <- matrix(c(7, 8, 9, 0), nrow = 2, ncol = 2)
y6 <- data.frame(v1 =  y2, v2 = -y2)
y7 <- list(a = y2, b = y5, c = y6)

## Por índice
y2[2]
y5[2, 2]
y6[3, 1]
y7[[3]]

## Por nombre
y2["b"]
y6$v1[3]
y7$c

## Borrar valores
rm(y2, y5, y6, y7)

# Construir vectores ------------------------------------------------------

## c()
c(1, 2, 3)

## Vector con nombres
c(a = 1, b = 2, c = 3)

## Usando dos puntos para incrementos unitarios
1:3

## Secuencia definiendo incremento
seq(1, 3, by = 0.5)

## Secuencia definiendo tamaño total
seq(1, 3, length.out = 9)

## Repitiendo valores
rep(0:2, each = 2)

## Repitiendo valores especificando número de veces
rep(0:2, times = 1:3)


# Funciones ---------------------------------------------------------------

## Evaluar en un solo elemento
cos(pi/4)

## Evaluar en un vector
cos(c(pi / 4, pi / 2))

## O en una matriz
cos(matrix(1:4, ncol = 2, nrow = 2))


# Ayuda -------------------------------------------------------------------

?cos

help("exp")

??tibble

# En RStudio:
# Seleccionar la función y presionar la tecla F1 mostrará la documentación.
lm

# También es posible buscar directamente en el panel de ayuda de RStudio.
# Search for outer

# Ejercicio ---------------------------------------------------------------

# 1.  Abre la guía de [funciones base de R
# producida](https://posit.co/wp-content/uploads/2022/10/base-r.pdf) por Posit.

# 2.  Busca como generar valores de una variable normal.

# 3.  Genera 101 valores de una normal con media 0 y desviación estándar 3 y
# guarda los valores en `e`

# 4.  Genera una sucesión de números del 0 al 50 en incrementos de 0.5 y
# almacena los valores en x.

# 5.  Define variables a y b con valores -10 y 0.3.

# 6.  Asigna a `y` el vector `a + b * y`

# 7.  Ajusta el modelo lineal `y ~ x` con la función `lm()` y asígnalo a una
# variable llamada "modelo".

# 8.  Grafica `y ~ x` usando la función `plot`.

# 9.  Extrae los valores estimados usando la función `coef`.


# ggplot2 -----------------------------------------------------------------

## Ejemplo básico ------------------------------------------------------

## Datos
head(cars)

## Carga paquete
library(ggplot2)

## Datos y gráfico base
ggplot(data = cars) +
  ## Especificar x y y
  aes(speed, dist) +
  ## Sistema de coordenadas (cartesiano con proporción fija)
  coord_fixed(ratio = 1 / 12) +
  ## Geometría
  geom_point()


## Una variable  --------------------------------------------------------------

## Datos: mtcars
## Extraidos de la revista Motor Trend de 1974
View(mtcars)

g_base <- ggplot(mtcars, aes(wt))
g_base

## Área
g_base + geom_area(stat = "bin")

## Denssidad
g_base + geom_density(kernel = "gaussian")

## Gráfico de puntos
g_base + geom_dotplot()

## Frecuencia con polinomio
g_base + geom_freqpoly()  

## Histograma
g_base + geom_histogram(binwidth = 0.5)

## Gráfico cuantil-cuantil 
ggplot(mtcars, aes(sample = mpg)) + geom_qq()

## Algunas mejoras
g_base + 
  geom_histogram(binwidth = 0.5, fill = "lightblue") +
  labs(x = "Peso (1000 lbs)", y = "Conteos") +
  theme_bw()

rm(g_base)

## Dos Variables: Continua - Discreta --------------------------------------

f <- ggplot(
  data = transform(mtcars, cyl = as.factor(cyl)), 
  aes(cyl, mpg))
f                                                    ## Base
f + geom_col()                                       ## Barras 
f + geom_boxplot()                                   ## Boxplot
f + geom_dotplot(binaxis = "y", stackdir = "center") ## Puntos
f + geom_violin(scale = "area")                      ## Violín

## Unas mejoras
f + 
  geom_boxplot(fill = "hotpink", color = "gray90") +
  labs(x = "Cilindros", y = "MPG") +
  coord_flip() +
  theme_dark()

rm(f)

## Dos Variables: Continua - Continua --------------------------------------

e <- ggplot(
  data = transform(mtcars, 
                   car = row.names(mtcars)), 
  aes(hp, mpg))

e ## Base
e + geom_point()  # Puntos
e + geom_label(aes(label = car), nudge_x = 1, nudge_y = 1) # Etiquetas
e + geom_rug(sides = "bl") ## ??
e + geom_smooth(method = lm) ## Modelo
e + geom_text(aes(label = car))

rm(e)

## Dos Variables: Discreta - Discreta --------------------------------------
g <- ggplot(
  data = transform(mtcars, cyl = as.factor(cyl), am = as.factor(am)), 
  aes(cyl, am))

g + geom_count() ## Conteos
g + geom_jitter(height = 0.15, width = 0.15)  ## Agregando ruido

rm(g)


# Ejercicio ---------------------------------------------------------------



