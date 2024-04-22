
# Limpiar entorno --------------------------------------------------------------

rm(list = ls())

# Cargar paquetes --------------------------------------------------------------

pacman::p_load(tidyverse, sjmisc, sjPlot, ggplot2)

# Cargar datos -----------------------------------------------------------------

datos_proc = readRDS("Output/casen2006.rds")

#####
frq(datos_proc$prom_ing_educ)

sjt.xtab(datos_proc$educa, datos_proc$sexo_edad,
         show.summary = T,
         show.col.prc = T,
         show.row.prc = T)

sjt.xtab(datos_proc$educa, datos_proc$sexo,
         show.summary = T,
         show.col.prc = T,
         show.row.prc = T)

sjt.xtab(datos_proc$educa, datos_proc$edad_tr,
         show.summary = T,
         show.col.prc = T,
         show.row.prc = T)

tabla = unique(datos_proc[, c("educa", "prom_ing_educ")])
tabla

ggplot(tabla, aes(x = prom_ing_educ, y = educa)) +
  geom_col(fill = "skyblue") +
  geom_text(aes(label = prom_ing_educ), hjust = 1.5, size = 3, color = "black") +
  labs(x = "Promedio de Sueldos", y = "Nivel Educativo") +
  theme_minimal()

###### 



