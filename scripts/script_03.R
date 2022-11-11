# Details -----------------------------------------------------------------

## Author: Guillermo Basulto-Elias
## Created: 2022-11-08
## Last modified: 2022-11-11
# Summary: Escuela AME - Día 3. Extensiones de ggplot2.

# Packages ----------------------------------------------------------------


library(ggplot2)
library(ggmosaic)
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
library(datos)
library(janitor)

# ggmosaic ----------------------------------------------------------------


datos_credito |> 
  clean_names() |> 
  ggplot() +
  geom_mosaic(aes(x = product(estado_civil), fill = estado))

ggplot(mtcars, aes(factor(cyl), mpg)) + 
  geom_boxplot() + 
  # Here comes the gganimate code
  transition_states(
    gear,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')

# library(ggmosaic)

# library(patchwork)
# library(gghighlight)
# library(ggrepel)
# library(ggpubr)
# library(ggalt)
# library(ggimage)
# library(ggthemes)
# library(plotly)
# library(esquisse)

