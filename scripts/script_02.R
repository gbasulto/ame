

# Details -----------------------------------------------------------------

## Author: Guillermo Basulto Elías
## Created: 2022-09-26
## Last modified: 2022-11-10
## Summary: Tidyverse- AME día 2

# Packages ----------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(janitor)

# tibble ------------------------------------------------------------------

## Define un cuadro de datos de la forma estándar
ugly_df <- data.frame(
  a = 1:1000,
  b = sample(x = c("a", "b"), size = 1000, replace = TRUE)
)

## Imprime los primeros elementos
ugly_df
head(ugly_df)

## Convierte el cuadro de datos estándar a una tibble
beautified_df <- as_tibble(ugly_df)
beautified_df

## ...O usa tibble desde el principio en lugar de data.frame()
pretty_df <- tibble(
  a = 1:1000,
  b = sample(x = c("a", "b"), size = 1000, replace = TRUE)
)

## tribble: crea un cuadro de datos pequeño de manera fácil de leer.
tribble(
  ~pokemon, ~is_caught, ~is_shiny,
  "Charmander", "yes", "no",
  "Bulbasaur", "no", NA_character_,
  "Squirtle", "yes", "yes"
)

## Remove objects (not necessary)
rm(ugly_df, pretty_df, beautified_df)


# readr -------------------------------------------------------------------

## Consumo de alcohol por país
url <- "https://raw.githubusercontent.com/fivethirtyeight/data/master/alcohol-consumption/drinks.csv"

## Leer con función base
alcohol_default_tbl <- read.csv(url)
head(alcohol_default_tbl)

## Leer con readr
## De momento no especificamos tipos de columnas
alcohol_readr_tbl <- read_csv(url) 

## Misma función, especificando los tipos de columnas
## No recibimos comentarios en esta ocasión
alcohol_readr2_tbl <- read_csv(
  file = url, 
  col_types = cols(
    country = col_character(),
    beer_servings = col_double(),
    spirit_servings = col_double(),
    wine_servings = col_character(),
    total_litres_of_pure_alcohol = col_double()
  ))

## NO olvides mencionar la importancia en 
##    i. cuadros de datos grandes o 
##    ii. procesos automatizados

## Limpia entorno
rm(alcohol_default_tbl, alcohol_readr_tbl, alcohol_readr2_tbl, url)

# Pipe --------------------------------------------------------------------

## Las pipas se usan para expresar una sequencia de instrucciones. El lado
## izquierdo pasa como primer parámetro al lado derecho.

## Pipa Tidyverse: %>%
## Pipa base: |>
## Son ligeramente diferentes, pero no cubriremos eso.

## Ejemplos muy simples
log(9.1)
9.1 %>% log()

max(pi, 3)
pi %>% max(3)

## Un poco más elaborado:

## La expresión
seq(-pi, pi, by = 0.1) |> sin() |> plot(type= "l")

## es equivalente a
seq(-pi, pi, by = 0.1) %>% sin() %>% plot(type= "l")

## O
plot(sin(seq(-pi, pi, by = 0.1)), type = "l")

## O
x <- seq(-pi, pi, by = 0.1)
y <- sin(x)
plot(y, type = "l")

rm(x, y)

# dplyr -------------------------------------------------------------------

## Invitados de Jon Stewart en TDS
url <- "https://raw.githubusercontent.com/fivethirtyeight/data/master/daily-show-guests/daily_show_guests.csv"
## Fuente: https://github.com/fivethirtyeight/data

## Lee datos directamente de página
guests_raw_tbl <- read_csv(url)
guests_raw_tbl

## Limpia nombres de columnas
guests_tbl <- clean_names(guests_raw_tbl)
guests_tbl

## Mutate
guests_tbl %>% mutate(year_plus_one = year + 1) ## Cambia una columna
guests_tbl %>% mutate(year = year + 1) ## O sobreescríbela

## Select 
guests_tbl %>% select(show, raw_guest_list) ## Selecciona dos
guests_tbl %>% select(ends_with("list"))    ## Opción más sofisticada
guests_tbl %>% select(matches("o"))         ## Usando expresiones regulares
guests_tbl[, c(1, 2)]                       ## O índices (trata de evitarlo)

## Filter
guests_tbl %>% filter(group == "Musician")
guests_tbl %>% 
  filter(raw_guest_list == "Bernie Sanders" | 
           raw_guest_list == "Sen. Hillary Clinton")

## Summarize
guests_tbl %>% summarize(counts = length(group)) ## Cuenta invitados
guests_tbl %>% summarize(counts = n()) ## Usa n() de dplyr
guests_tbl %>% 
  group_by(group) %>% 
  summarize(counts = n()) ## Combina with group_by()
guests_tbl %>% 
  group_by(group, year) %>% 
  summarize(counts = n()) ## Agrega otro agrupamiento

## Arrange
guests_tbl %>% arrange(group, raw_guest_list)

## Limpia entorno
rm(url, guests_raw_tbl, guests_tbl)

# tidyr -------------------------------------------------------------------

## Ejemplo de juguete
crashes_tbl <- 
  tribble(
    ~site, ~before, ~after, ~aadt, ~rurub, ~file, 
    "A", 30, 20, 13500, "Urban", "nov_08",
    "B", 10, 11, 15000, "Urban", "oct_08",
    "C", 9, 2, 5500, "Rural", "jan_09"
  )

crashes_tbl

## Pivot longer
long_crashes_tbl <- 
  crashes_tbl %>% 
  pivot_longer(cols = c(before, after), 
               names_to = "occasion", 
               values_to = "crashes")
long_crashes_tbl

# Pivot wider
long_crashes_tbl %>% 
  pivot_wider(names_from = occasion, values_from = crashes)

## Separate
long_crashes_tbl %>% 
  separate(col = file, into = c("month", "year"), sep = "_")

rm(long_crashes_tbl, crashes_tbl)

# stringr -----------------------------------------------------------------

baby_shark_song <- 
"
Baby Shark, doo-doo, doo-doo
Baby Shark, doo-doo, doo-doo
Baby Shark, doo-doo, doo-doo
Baby Shark
Mommy Shark, doo-doo, doo-doo
Mommy Shark, doo-doo, doo-doo
Mommy Shark, doo-doo, doo-doo
Mommy Shark
Daddy Shark, doo-doo, doo-doo
Daddy Shark, doo-doo, doo-doo
Daddy Shark, doo-doo, doo-doo
Daddy Shark
Grandma Shark, doo-doo, doo-doo
Grandma Shark, doo-doo, doo-doo
Grandma Shark, doo-doo, doo-doo
Grandma Shark
Grandpa Shark, doo-doo, doo-doo
Grandpa Shark, doo-doo, doo-doo
Grandpa Shark, doo-doo, doo-doo
Grandpa Shark
Let's go hunt, doo-doo, doo-doo
Let's go hunt, doo-doo, doo-doo
Let's go hunt, doo-doo, doo-doo
Let's go hunt
Run away, doo-doo, doo-doo
Run away, doo-doo, doo-doo
Run away, doo-doo, doo-doo
Run away (ah!)
Safe at last, doo-doo, doo-doo
Safe at last, doo-doo, doo-doo
Safe at last, doo-doo, doo-doo
Safe at last (phew)
It's the end, doo-doo, doo-doo
It's the end, doo-doo, doo-doo
It's the end, doo-doo, doo-doo
It's the end"

## Print
baby_shark_song
cat(baby_shark_song) ## Usamos cat para respetar líneas nuevas

## str_replace
baby_shark_song |>  ## Only the first
  str_replace("Shark", "Pirahna") |>  
  cat()
baby_shark_song %>%  ## Only the first
  str_replace_all("Shark", "Piranha") %>% 
  cat()
baby_shark_song %>%  ## Using regular expressions to include capital letters
  str_replace_all("(, |\\)|\\()", " - ") %>% 
  cat()

## str_detect
baby_shark_song %>%  ## Using regular expressions to detect several words. It's false
  str_detect("(Octopus|Whale)") 
baby_shark_song %>%  ## Must be true
  str_detect("Shark") 

## str_length
baby_shark_song %>% str_length()

## Limpia entorno
rm(baby_shark_song)

# forcats -----------------------------------------------------------------

tod <- rep(c("day", "night", "dawn", "dusk"), c(100, 30, 3, 2))

## Barplot
tod %>% qplot(geom = "bar")

## Cambia a factor (gráfico no cambia)
tod %>% 
  factor() %>% 
  qplot(geom = "bar")

## Fija "day" como el primer nivel
tod %>% 
  factor() %>% 
  fct_relevel("day") %>% 
  qplot(geom = "bar")

## Set order of all of them
tod %>% 
  factor() %>% 
  fct_relevel(c("day", "night", "dawn")) %>% 
  qplot(geom = "bar")

## Invert order
tod %>% 
  factor() %>% 
  fct_relevel(c("day", "night", "dawn")) %>% 
  fct_rev() %>% 
  qplot(geom = "bar")

## Lump values
tod %>% 
  factor() %>% 
  fct_lump(n = 2) %>% 
  qplot(geom = "bar")

## Collapse
tod %>% 
  factor() %>% 
  fct_collapse(nightish = c("night", "dawn", "dusk")) %>% 
  qplot(geom = "bar")

rm(tod)

# lubridate ---------------------------------------------------------------

## Caracteres a fechas
ymd("2022-09-28")
mdy("09/28/2022")

## Auxiliares
today()
now()

## Extraer elementos of fechas
today() %>% month()
today() %>% month(label = TRUE)
today() %>% year()
now() %>% minute()

## Operaciones
today() - years(1)
today() - days(30)

# purrr -------------------------------------------------------------------

## De la documentación de purrr:
mtcars %>% head()
mtcars %>%
  split(.$cyl) %>% # from base R
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")
