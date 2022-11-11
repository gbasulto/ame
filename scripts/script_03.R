# Details -----------------------------------------------------------------

## Author: Guillermo Basulto-Elias
## Created: 2022-11-08
## Last modified: 2022-11-11
# Summary: Escuela AME - Día 3. Extensiones de ggplot2.

# Packages ----------------------------------------------------------------

library(tidyverse)
library(janitor)
library(datos)
library(ggplot2)
library(gapminder)

## Extensiones
library(ggmosaic)
library(GGally)
library(patchwork)
library(plotly)
library(esquisse)
library(gganimate)
library(gghighlight)
library(ggrepel)
library(ggpubr)
library(ggalt)
library(ggimage)
library(ggthemes)
library(ggforce)

# ggmosaic ----------------------------------------------------------------

datos_credito_tbl <- datos_credito |> clean_names() |> as_tibble()
datos_credito_tbl

## Ejemplo simple
datos_credito_tbl |> 
  ggplot() +
  geom_mosaic(aes(x = product(estado_civil), fill = estado))

## Misma idea, grafica mejorada
datos_credito_tbl |>
  mutate(
    estado_civil = 
      estado_civil |> 
      fct_relevel("casado", "viudo", "soltero", "divorciado") |> 
      fct_collapse(`sep/div` = c("separado", "divorciado"))
    ) |> 
  drop_na(estado_civil) |> 
  ggplot() +
  geom_mosaic(aes(x = product(estado_civil), fill = estado)) +
  scale_fill_brewer(palette="Set1") +
  theme_bw() +
  theme(legend.position = "none") +
  coord_flip()

## Agregar tan solo una variable adicional puede sobrecomplicar la gráfica
datos_credito_tbl |> 
  mutate(
    estado_civil = 
      estado_civil |> 
      fct_relevel("casado", "viudo", "soltero", "divorciado") |> 
      fct_collapse(`sep/div` = c("separado", "divorciado"))
  ) |> 
  drop_na(estado_civil) |> 
  ggplot() +
  geom_mosaic(aes(x = product(estado_civil, trabajo), fill = estado)) +
  coord_flip()

rm(datos_credito_tbl)


# GGally ------------------------------------------------------------------

mtcars |> 
  mutate(trans = if_else(am == 0, "Manual", "Automática"),
         cyl = as.factor(cyl)) |> 
  ggpairs(columns = c("mpg", "wt", "trans"))

## Agregando variable de para color
mtcars |> 
  mutate(trans = if_else(am == 0, "Manual", "Automática"),
         cyl = as.factor(cyl)) |> 
  ggpairs(columns = c("mpg", "wt", "cyl"), mapping = aes(color = trans))

# patchwork ---------------------------------------------------------------

## Simulamos datos
x <- seq(-pi + 1e-3, pi - 1e-3, length.out = 200)
seno <- sin(x)
coseno <- cos(x)
tangente <- tan(x)

## Generamos tres gráficas
g1 <- ggplot(data = NULL, aes(x, seno)) + geom_line() + theme_bw()
g2 <- ggplot(data = NULL, aes(x, coseno)) + geom_line() + theme_bw()
g3 <- ggplot(data = NULL, aes(x, tangente)) + geom_line() + theme_bw()

## De la manera que sabemos:
tibble(x, seno, coseno, tangente) |> 
  pivot_longer(-x, names_to = "Funcion", values_to = "Valor") |> 
  ggplot(aes(x, Valor)) + 
  geom_line() +
  facet_wrap(~Funcion, scales = "free_y") +
  theme_bw()

## ¿Y si las gráficas son de differente índole? ¡Usamos patchwork!

g1 + g2 + g3
g3 + (g1 / g2)


## ¿Y si tenemos una lista de gráficas?

g_list <- list(g1, g2, g3)

wrap_plots(g_list)

## ¿Y si tenemos una gráfica base y una de ggplot2"?
g1 +  wrap_elements(~plot(seno ~ x, type = "l") )

rm(g_list, g1, g2, g3, coseno, seno, tangente, x)


# plotly ------------------------------------------------------------------

## Not a ggplot2

# volcano is a numeric matrix that ships with R
fig <- plot_ly(z = ~volcano)
fig <- fig %>% add_surface()
fig

rm(fig)

## Un ggplot

p1 <- 
  ggplot(
  data = transform(
    mtcars, 
    cyl = as.factor(cyl),
    `Transmisión` = c("Automática", "Estándar")[am + 1],
    am = as.factor(am)), 
  aes(hp, mpg, color = cyl)) +
  geom_point() +
  facet_wrap(~ `Transmisión`, labeller = label_both) +
  labs(x = "HP", y = "MPG", color = "Cilindros:") +
  theme_light() +
  theme(legend.position = "bottom")

ggplotly(p1)

rm(p1)

# esquisse ----------------------------------------------------------------

## Cargar datos al entorno
data(mtcars)

rm(mtcars)


# gganimate ---------------------------------------------------------------

ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')


# gghighlight -------------------------------------------------------------

ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  gghighlight(year == 2002)

ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  gghighlight(year == 2007)


# ggrepel -----------------------------------------------------------------

e <- ggplot(
  data = transform(mtcars, 
                   car = row.names(mtcars)), 
  aes(hp, mpg))

e ## Base
e + geom_point()  # Puntos

## El que sabemos hacer
e + 
  geom_label(aes(label = car), nudge_x = 1, nudge_y = 1, size = 3) +
  geom_point()

## Cong ggrepel
e + 
  geom_label_repel(aes(label = car), nudge_x = 1, nudge_y = 1, size = 3) +
  geom_point() 

## Un poco mejorado
e + 
  geom_label_repel(aes(label = car), nudge_x = 1, nudge_y = 1, size = 3,
                   box.padding = 0.5,
                   segment.curvature = -0.2) +
  geom_point() +
  theme_bw()

## Con texto
e + 
  geom_text_repel(aes(label = car), nudge_x = 1, nudge_y = 1, size = 3) +
  geom_point() 

rm(e)



# ggforce -----------------------------------------------------------------

## Ejemplo obtenido de la documentación

ggplot(midwest, aes(state, area)) + geom_point()

# Boxplot and Violin plots convey information on the distribution but not the
# number of samples, while Jitter does the opposite.
ggplot(midwest, aes(state, area)) +
  geom_violin()

ggplot(midwest, aes(state, area)) +
  geom_jitter()

# Sina does both!
ggplot(midwest, aes(state, area)) +
  geom_violin() +
  geom_sina()

p <- ggplot(midwest, aes(state, popdensity)) +
  scale_y_log10()

p + geom_sina()

# Colour the points based on the data set's columns
p + geom_sina(aes(colour = inmetro))

# Or any other way
cols <- midwest$popdensity > 10000
p + geom_sina(colour = cols + 1L)

# Sina plots with continuous x:
ggplot(midwest, aes(cut_width(area, 0.02), popdensity)) +
  geom_sina() +
  scale_y_log10()

