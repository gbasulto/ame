

# Details -----------------------------------------------------------------

## Author: Guillermo Basulto-Elias
## Created: 2022-09-26
## Last modified: 2022-09-28
# Summary: Script with examples for presentation. Each section is
# self-contained, except the packages who are all loaded at the beginning.
# Packages ----------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(janitor)

# tibble ------------------------------------------------------------------

## Define dataframe the regular way
ugly_df <- data.frame(
  a = 1:1000,
  b = sample(x = c("a", "b"), size = 1000, replace = TRUE)
)

## Print all and first elements
ugly_df
head(ugly_df)

## Cast it to tibble
beautified_df <- as_tibble(ugly_df)
beautified_df

## Define it with tibble since the beginning
pretty_df <- tibble(
  a = 1:1000,
  b = sample(x = c("a", "b"), size = 1000, replace = TRUE)
)

## tribble
tribble(
  ~pokemon, ~is_caught, ~is_shiny,
  "Charmander", "yes", "no",
  "Bulbasaur", "no", NA_character_,
  "Squirtle", "yes", "yes"
)

## Remove objects (not necessary)
rm(ugly_df, pretty_df, beautified_df)


# readr -------------------------------------------------------------------

url <- "https://raw.githubusercontent.com/fivethirtyeight/data/master/alcohol-consumption/drinks.csv"

## Read with base function
alcohol_default_tbl <- read.csv(url)
head(alcohol_default_tbl)

## Read with read_csv from readr
## We will receive a note about columns not being specified.
alcohol_readr_tbl <- read_csv(url) 

## Use the same function specifying column type
## No notes this time around.
alcohol_readr2_tbl <- read_csv(
  file = url, 
  col_types = cols(
    country = col_character(),
    beer_servings = col_double(),
    spirit_servings = col_double(),
    wine_servings = col_character(),
    total_litres_of_pure_alcohol = col_double()
  )) 

## Do NOT forget to mention how crucial this can be for large datasets and
## automated processes.

## Remove variables
rm(alcohol_default_tbl, alcohol_readr_tbl, alcohol_readr2_tbl, url)

# Pipe --------------------------------------------------------------------

## Pipes are used to express a sequence of instructions. Whatever is in the
## left, passes as firt argument to the right.

## Tidyverse pipe: %>%
## Base pipe: |>
## They are slightly different, but we will not cover that.

## Simplest examples
log(9.1)
9.1 %>% log()

max(pi, 3)
pi %>% max(3)

## A more elaborated example (but still simple.):

## The expression
seq(-pi, pi, by = 0.1) |> sin() |> plot(type= "l")

## is equivalent to
seq(-pi, pi, by = 0.1) %>% sin() %>% plot(type= "l")

## Or
plot(sin(seq(-pi, pi, by = 0.1)), type = "l")

## Or
x <- seq(-pi, pi, by = 0.1)
y <- sin(x)
plot(y, type = "l")

rm(x, y)

# dplyr -------------------------------------------------------------------

## Jon Stewart guests on TDS
url <- "https://raw.githubusercontent.com/fivethirtyeight/data/master/daily-show-guests/daily_show_guests.csv"
## Source: https://github.com/fivethirtyeight/data

## Read data
guests_raw_tbl <- read_csv(url)
guests_raw_tbl

## Clean names this time
guests_tbl <- clean_names(guests_raw_tbl)
guests_tbl

## Mutate
guests_tbl %>% mutate(year_plus_one = year + 1) ## Change a column
guests_tbl %>% mutate(year = year + 1) ## or overwrite it!

## Select 
guests_tbl %>% select(show, raw_guest_list) ## Select two
guests_tbl %>% select(ends_with("list")) ## Fancier selection
guests_tbl %>% select(matches("o"))
guests_tbl[, c(1, 2)]

## Filter
guests_tbl %>% filter(group == "Musician")
guests_tbl %>% 
  filter(raw_guest_list == "Bernie Sanders" | 
           raw_guest_list == "Sen. Hillary Clinton")

## Summarize
guests_tbl %>% summarize(counts = length(group)) ## Count guests
guests_tbl %>% summarize(counts = n()) ## Use n() from dplyr
guests_tbl %>% 
  group_by(group) %>% 
  summarize(counts = n()) ## Combine with group_by()
guests_tbl %>% 
  group_by(group, year) %>% 
  summarize(counts = n()) ## Add another group

## Arrange
guests_tbl %>% arrange(group, raw_guest_list)

## Remove datasets
rm(url, guests_raw_tbl, guests_tbl)

# tidyr -------------------------------------------------------------------

## Toy example
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

corn_song <- 
"
For me
I really like corn
What do you like about corn?
Its corn!
A big lump with knobs
It has the juice (it has the juice)
I can't imagine a more beautiful thing (Woo)
It's corn!
I can tell you all about it
I mean look at this thing
When I tried it with butter, everything changed!
Well, the best thing about corn is the grease
The corn break can restore your energy
You really appreciate the hard work when you love something
If you or anyone loves corn
If you come to me, I can tell you all about it
If I was surrounded by corn
I would eat all of it, Grow big and strong
Look at this, take a lump with the knob
Spread butter on
Boil it and toast it with the grease
Butter spreads out the good taste
I just saw corn as disgusting
But everything changed
It's corn!
A big lump with knobs
It has the juice (it has the juice)
I can't imagine a more beautiful thing (Woo)
It's corn!
I can tell you all about it
I mean look at this thing
When I tried it with butter, everything changed!
I hope you have a corntastic day (A corntastic day?)
What? It's just a pun about corn, okay? (Woo)
It's corn!
When it's like negative in the world
Take the corn break! (Corn break)
Take the corn break! (Corn Dance!)
(Mm. Corn.)
Ippety, bibbly, diddly-bop
Corn dance (Mm)
Anything else you wanna say about corn before we go?
Yeah
What?
It's corn!
A big lump with knobs
It has the juice (it has the juice)
I can't imagine a more beautiful thing (Woo)
It's corn!
I can tell you all about it
I mean, look at this thing
When I tried it with butter, everything changed!
It's corn!
A big lump with knobs
It has the juice (it has the juice)
I can't imagine a more beautiful thing (Woo)
It's corn!
I can tell you all about it
I mean, look at this thing
When I tried it with butter, everything changed!
It's corn!"

## Print
corn_song
cat(corn_song) ## Nicer print respecting special characters

## str_replace
corn_song %>%  ## Only the first
  str_replace("corn", "pomegranate") %>% 
  cat()
corn_song %>%  ## Only the first
  str_replace_all("corn", "pomegranate") %>% 
  cat()
corn_song %>%  ## Using regular expressions to include capital letters
  str_replace_all("(c|C)orn", "pomegranate") %>% 
  cat()

## str_detect
corn_song %>%  ## Using regular expressions to detect several words. It's false
  str_detect("(soy|wheat)") 
corn_song %>%  ## Must be true
  str_detect("corn") 

## str_length
corn_song %>% str_length()

## Delete variables
rm(corn_song)

# forcats -----------------------------------------------------------------


tod <- rep(c("day", "night", "dawn", "dusk"), c(100, 30, 3, 2))

## Barplot
tod %>% qplot(geom = "bar")

## Change to factor (plot does not change)
tod %>% 
  factor() %>% 
  qplot(geom = "bar")

## Set day as first level
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

## Characters to date
ymd("2022-09-28")
mdy("09/28/2022")

## Helpers
today()
now()

## Extract elements of date
today() %>% month()
today() %>% month(label = TRUE)
today() %>% year()
now() %>% minute()

## Operations
today() - years(1)
today() - days(30)

# purrr -------------------------------------------------------------------

## From purrr documentation
mtcars %>% head()
mtcars %>%
  split(.$cyl) %>% # from base R
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")

# ggplot2 -----------------------------------------------------------------

## Will not be covered here