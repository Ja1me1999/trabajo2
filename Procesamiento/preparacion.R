



#Variable escogida 


# Carga de paquetes ------------------------------------------------------------

pacman::p_load(haven, tidyverse, sjmisc, sjPlot, ggplot2)

# Carga de datos ---------------------------------------------------------------

# data = read_dta("input/casen2006.dta")
# saveRDS(data, "input/casen2006.rds)

rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica

saveRDS(data,"input/casen2006.rds")
#Se cambia el formato de los datos, ya que pesaban mucho al momento de subir un 
#push

# Exploración ------------------------------------------------------------------

view(data)
dim(data)
names(data)
head(data)
find_var(data, "ingreso")

frq(data$sexo)
frq(data$e8t)
frq(data$r_15)
frq(data$edad)

# Procesamiento ----------------------------------------------------------------

datos_proc = data%>%
  mutate(sexo = case_when(sexo==1~"Hombre", sexo==2~"Mujer", T~NA_character_),
         edad_tr = case_when(edad>=18 & edad<=39~"Jovenes",
                             edad>=40 & edad<=64~"Adultos",
                             edad>=65~"Adultos mayores",
                             T~NA_character_),
         sexo_edad = case_when(sexo=="Mujer" & edad_tr=="Jovenes"~"Mujer joven",
                               sexo=="Mujer" & edad_tr=="Adultos"~"Mujer adulta",
                               sexo=="Mujer" & edad_tr=="Adultos mayores"~"Mujer mayor",
                               sexo=="Hombre" & edad_tr=="Jovenes"~"Hombre joven",
                               sexo=="Hombre" & edad_tr=="Adultos"~"Hombre adulto",
                               sexo=="Hombre" & edad_tr=="Adultos mayores"~"Hombre mayor",
                               TRUE ~ NA_character_),
         educa = case_when(e8t>=1 & e8t<=4~"Educacion Basica",
                           e8t>=5 & e8t<=8~"Educacion Media",
                           e8t %in% c(9, 11, 13)~"Superior Incompleta",
                           e8t %in% c(10, 12, 14)~"Superior Completa",
                           e8t==15~"Postgrado",
                           T~NA_character_))%>%
  select(sexo, edad, edad_tr, sexo_edad, educa, region=r_15, ingreso=yoprhaj)%>%
  filter(region==15 & edad>=18)%>%
  group_by(educa)%>%
  mutate(prom_ing_educ = mean(ingreso, na.rm=T))%>%
  ungroup%>%
  mutate(prom_ing_educ = as.integer(prom_ing_educ))

# Revisión de variables --------------------------------------------------------

frq(datos_proc$sexo)
frq(datos_proc$edad)
frq(datos_proc$edad_tr)
frq(datos_proc$sexo_edad)
frq(datos_proc$educa)
frq(datos_proc$region)

# Guardar datos ----------------------------------------------------------------

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


datos_proc %>% ggplot(aes(x = educa)) + 
  geom_bar() +
  xlab("Confianza en instituciones") +
  ylab("Cantidad")+
  facet_wrap(~sexo)


