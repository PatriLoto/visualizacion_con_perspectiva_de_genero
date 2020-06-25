#--------------------------------------------------------------------------------------------------------
# Código por Patricia Loto @patriloto con ayuda de Roxana Villafañe para R-Ladies Resistencia-Corrientes.
#--------------------------------------------------------------------------------------------------------

# ATAJOS
# ctrl + shift + M para pipe o %>% 
# Ctrl + Enter para ejecutar un pedazo de código
# Ctrl + Shift + S para ejecutar todo el script
# Ctrl + Shift + N para abrir un nuevo script

#-------------------------------
# Importamos las librerías
#-------------------------------

library(here)             # Es muy importante para poder utilizar rutas relativas
library(tidyverse)        # Conjunto de paquetes que nos brindan las herramientas necesarias 
# para trabajar a lo largo de todo el proceso de ciencia de datos
library(scales)
library(knitr)


library(kableExtra)       # para presentar la información tablas de manera más ordenada
#-----------------------
# Leemos los datos
#----------------------
programadoras <- read.csv(file = "datos/programadorasnew.csv",header = TRUE)
view(programadoras)

#-------------------------------
# Ahora, Exploramos los datos
#------------------------------
## nombres de las variables
names(programadoras)

# cantidad de observaciones y tipo de variables
glimpse(programadoras)

# podemos ver las primeras filas del dataset con head()
head(programadoras)

# podemos ver las filas de acuerdo al rango deseado
slice(programadoras,1:30) 

# Procesamiento de datos
#-----------------------------------------------
# Trabajaremos con CANTIDAD DE ESTUDIANTES (estMujeres y estVarones)
#
#---------------------------------------------------------------------------------------------------
# ¿CUÁL ES LA CANTIDAD ABSOLUTA Y EL PORCENTAJE DE ESTUDIANTES POR SEXO A LO LARGO DEL PERÍODO 2010-2015?
#--------------------------------------------------------------------------------------------------

# calculamos totales, promedio y porcentaje por sexo y año utilizamos group_by () y summarise()

mujeres_anio<- programadoras%>%group_by(anio)%>% summarise(totalsexo= sum(estMujeres), total=sum(estMujeres, estVarones), prom=(round(mean(estMujeres),0)), porcen= round(totalsexo/total * 100)) %>% mutate(sexo='Mujeres') 
View(mujeres_anio)
hombres_anio<- programadoras %>% group_by(anio) %>% summarise(totalsexo= sum(estVarones), total=sum(estMujeres, estVarones), prom=(round(mean(estVarones),0)), porcen=round(totalsexo/total * 100)) %>% mutate(sexo='Hombres')
View(hombres_anio)

# unimos las tablas generadas anteriormente 
estudiantes_R_barra<-rbind(mujeres_anio, hombres_anio) %>% arrange(desc(totalsexo))
view(estudiantes_R_barra)

# si quiero mostrar la tabla generada con un formato más ordenado y visualmente más agradable utilizo el paquete kableExtra
kable(estudiantes_R_barra, align = "r") %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center")

#---------------------------------------------------------------------
# Gráfico de BARRAS APILADAS:

# librería para extender las funcionalidades de ggplot2
library(hrbrthemes)
hrbrthemes::import_titillium_web()

# Opción 1: presentación
# Gráfico de barras con PORCENTAJES con geom_text y etiquetas 
ggplot(data=estudiantes_R_barra, aes(x=anio, y=totalsexo, fill= sexo)) +
  geom_bar(stat= "identity", position = "stack", width= .7, color= "black") +
  #coord_flip()+
  scale_fill_manual(values= c("#d8d860","#a32ea2"), labels= c('Hombres', 'Mujeres'))+  
  scale_x_continuous(breaks= c(2010, 2011, 2012,2013,2014,2015), labels= c('2010', '2011', '2012','2013', '2014','2015')) +
  geom_text(aes (label = paste0(porcen, "%")), position = position_stack(vjust=0.5), size=4, color="#2c204d") +
  labs(title = 'Estudiantes de carreras relacionadas con programación \n de Universidades Argentinas\n', x='',y='',fill= ' ',
       subtitle ="Para el período 2010-2015" , 
       caption = "Fuente: Elaboración propia con datos de Chicas en tecnología") +
  theme_ft_rc() +
  theme(text = element_text(size=14, face = 'bold', color = "#2c204d"),
        plot.title = element_text(size=18,                     #cambiamos el tamaño, fuente y color del título
                                  # family ="Garamond",    
                                  hjust = 0,vjust = 1,
                                  #colour = "#2c204d", 
                                  face = 'bold', 
                                  margin = margin(b = 12 * 1.2)),
        legend.position="bottom",legend.text= element_text(color="grey", 
                                                           size= 12, hjust = 0.5,vjust = 1, family ="Garamond"))


# Guardamos el gráfico generado
ggsave(here("barras_apiladas_total_final2_a.png"), height = 8, width = 10, units = "in", type='cairo')
#ggsave(here("barras_apiladas_cant_final2.png"), height = 8, width = 10, units = "in", type='cairo')

# Opción 2:
# Gráfico de barras con CANTIDADES con geom_text y etiquetas 
ggplot(data=estudiantes_R_barra, aes(x=anio, y=totalsexo, fill= sexo)) +
  geom_bar(stat= "identity", position = "stack", width= .7, color= "black")+
  #coord_flip()+
  scale_fill_manual(values= c("#d8d860","#a32ea2"), labels= c('Hombres', 'Mujeres'))+  #amarillo claro:#f1fa8c  verde:#41b6a6 lilaoscuro:#713580 
  scale_x_continuous(breaks= c(2010, 2011, 2012,2013,2014,2015), labels= c('2010', '2011', '2012','2013', '2014','2015')) +
  geom_text(aes(label=paste0(totalsexo)), position = position_stack(vjust=0.5), size=4, color="#2c204d") +
  #geom_text (data =estudiantes_R_barra, aes(anio, medio=(totalsexo/2),label = paste0(totalsexo)), size=3, color = "#2c204d", vjust = - 0.7,  nudge_y = .1) +
  labs(title = 'Estudiantes de carreras relacionadas con programación \n de Universidades Argentinas\n', x='',y='',fill= ' ',
       subtitle ="Para el período 2010-2015" , 
       caption = "Fuente: Elaboración propia con datos de Chicas en tecnología") +
  theme_ft_rc() +
  theme(text = element_text(size=14, face = 'bold', color = "#2c204d"),
        plot.title = element_text(size=18,                     #cambiamos el tamaño, fuente y color del título
                                  hjust = 0,vjust = 1,
                                  face = 'bold', 
                                  margin = margin(b = 12 * 1.2)),
        legend.position="bottom",legend.text= element_text(color="grey", 
                                                           size= 12, hjust = 0.5,vjust = 1, family ="Garamond"))

# Guardamos el gráfico generado
ggsave(here("barras_apiladas_porcen_final2_B.png"), height = 8, width = 10, units = "in", type='cairo')


#-------------------------------------------------------------------------------------------------------------
# Gráfico de LÍNEAS:

# Gráfico de líneas con total de estudiantes por sexo
view(estudiantes_R)

# opción 1: presentación
ggplot(data=estudiantes_R, aes(x=anio, y=total, color=sexo))+
  geom_line(size=2) +
  geom_point(data=estudiantes_R, aes(anio, total), size=3) +
  scale_colour_manual(values= c("#d8d860","#a32ea2"), labels= c('Mujeres', 'Hombres'))+
  facet_wrap(~sexo, ncol=1) +
  labs(title = 'Estudiantes de carreras relacionadas con programación \n de Universidades Argentinas\n', x='',y='',fill= ' ',
       subtitle ="Para el período 2010-2015" , 
       caption = "Fuente: Elaboración propia con datos de Chicas en tecnología") +
  theme_ft_rc() +
  theme(text = element_text(size=14, face = 'bold', color = "#2c204d"),
        plot.title = element_text(size=18,                     #cambiamos el tamaño, fuente y color del título
                                  family ="Garamond",    
                                  hjust = 0,vjust = 1,
                                  #colour = "#2c204d", 
                                  face = 'bold', 
                                  margin = margin(b = 12 * 1.2)),
        legend.position="  ", legend.text= element_text(color="#2c204d", 
                                                        size= 12, hjust = 0.5,vjust = 1, family ="Garamond"))

# Guardamos el gráfico generado
ggsave(here("grafico_total_facet_final.png"), height = 8, width = 10, units = "in", type='cairo')

# -------------------------------------------------------
# UNIVERSIDADES CON MAYOR CANTIDAD DE ESTUDIANTES MUJERES
# ---------------------------------------------------------
#-------------------------------------------------  
# función para redondear a decimales cuando cálculo brecha de género o valores con porcentaje. 
# Código por @NatsuSh
formato_porce <- function(numero, dec = 1){
  format(round(numero, digits = dec), nsmall = dec, decimal.mark = ",")
}
# Gracias @Natsush
-------------------------------------------------
  
# calculamos cantidad total, promedio y porcentaje de ESTUDIANTES MUJERES agrupadas por institución para el AÑO 2015
# utilizamos filter(), group_by() , summmarise() y mutate()
  
  mujeres_est<- programadoras%>% filter(anio==2015) %>% group_by(institucion)%>% summarise(totalM= sum(estMujeres),totalV= sum(estVarones), total=sum(estMujeres, estVarones), prom=(round(mean(estMujeres),0)),
                                                                                           porcenM= round(totalM/total* 100), porcenV=round(totalV/total * 100))%>%            
  mutate(brecha = paste0(formato_porc((totalV-totalM)/total*100), "%"), medio = (totalV+totalM)/2)
View(mujeres_est)

# calculamos las 10 universidades con mayor CANTIDAD de estudiantes mujeres para el año 2015 ordenadas de manera descendente
# por cantidad total
# utilizamos top_n() y arrange()
mas_mujeres_est <- mujeres_est %>% top_n(10) %>% arrange(desc(totalM))
view(mas_mujeres_est)

# calculamos universidades con mayor PORCENTAJE de estudiantes mujeres para el año 2015 ordenadas de manera descendente
# por porcentaje
mayor_porcen_mujeres_est <- mujeres_est %>% top_n(10) %>% arrange(desc(porcenM))
view(mayor_porcen_mujeres_est)

# si quiero mostrar la tabla generada con un formato más ordenado y visualmente más agradable utilizo el paquete kable
kable(mas_mujeres_est, align = "r") %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center")


#-----------------
#cálculos extras 
#----------------
#calculamos universidades con menor cantidad de estudiantes mujeres para el año 2015 ordenadas de manera ascendente
# por cantidad total
menos_mujeres_est <- mujeres_est %>% top_n(10) %>% arrange(asc(totalM))
view(menos_mujeres_est)
# calculamos cuáles son las universidades con mayor brecha de género entre varones y mujeres en 2015
mayor_brecha <- mujeres_est %>% top_n(10) %>% arrange(brecha)
view(mayor_brecha)


#-----------------------------------------------------------
# grafico LOLLIPOP 

ggplot(mas_mujeres_est,aes(x=fct_reorder(institucion,(totalM)), y = totalM, group = institucion), label = totalM) +
  geom_segment(aes(xend=institucion, yend=0), colour="#e3e4e5") +
  geom_point(size=3.8, color="#a32ea2") +
  geom_text (data =mas_mujeres_est, aes(institucion, totalM/2,  label = totalM), size=3, vjust = - 0.5,  nudge_y = .1) +
  coord_flip() +
  labs(title = 'Universidades Argentinas con mayor número \n de estudiantes mujeres', x='',y='', color=" ",
       subtitle ="Para el año 2015" , 
       caption = "Fuente: Elaboración propia con datos de Chicas en tecnología") +
  theme_ft_rc() +
  theme(text = element_text(size=14, face = 'bold'),
        plot.title = element_text(size=18,                     #cambiamos el tamaño, fuente y color del título
                                  hjust = 0,vjust = 1,
                                  face = 'bold', 
                                  margin = margin(b = 12 * 1.2)),
        legend.position="bottom",
        legend.text= element_text(color="#2c204d", 
                                  size= 12, hjust = 0.5,vjust = 1, family ="Garamond"))

# Guardamos el gráfico generado
ggsave(here("lollipop_estudiantes2_final.png"), height = 8, width = 12, units = "in", type='cairo')

#---------------------------------------------------------------------------------------------------
# Gráfico Extra:
# Gráfico de dumbbell que evidencia las universidades que poseen mayor brecha de género para el año 2015. 
# utilizamos fct_reorder() para ordenar las instituciones según el valor de la brecha de género.

colores = c("#a32ea2","#d8d860") 
ggplot(mas_mujeres_est, aes(x = totalM, xend = totalV, y =fct_reorder(institucion,(brecha)), group = institucion, label = brecha)) +
  geom_dumbbell(color= "#808080",
                size_x = 3, size_xend = 3,
                colour_x = colores[1],
                colour_xend = colores[2]) +
  #geom_text(aes(label = brecha), nudge_y = .2)+
  scale_color_manual(values= colores) +
  geom_text(aes(medio, institucion, label = brecha), size=3, nudge_y = .2) +
  labs(title = 'Universidades Argentinas con mayor número brecha de género \n', x='',y='', color=" ",
       subtitle ="Para el año 2015" , 
       caption = "Fuente: Elaboración propia con datos de Chicas en tecnología") +
  theme_ft_rc() +
  theme(text = element_text(size=14, face = 'bold'),
        plot.title = element_text(size=18,                     #cambiamos el tamaño, fuente y color del título
                                  hjust = 0,vjust = 1,
                                  face = 'bold', 
                                  margin = margin(b = 12 * 1.2)),
        legend.position="bottom",
        legend.text= element_text(color="#2c204d", 
                                  size= 12, hjust = 0.5,vjust = 1, family ="Garamond"))

# ---------------------------------------------------------------------------------------------------------------------------
# Tipo de títulos donde existen mayor cantidad de EGRESADAS mujeres
# Agrupamos los títulos en las siguientes categorías: Licenciatura, tecnicatura, analista, programador e ingeniería.

# Pude hacerlo gracias a un ejemplo de  Violeta Roizman
# utilizamos la función str_detect(x, pattern) del paquete stringr
grupo_titulo <- programadoras %>%
  mutate(titulo_grupo = case_when(str_detect(titulo, "Licenciatura") ~ "Licenciatura",
                                  str_detect(titulo,"Tecnicatura") ~ "Tecnicatura",
                                  str_detect(titulo,"Analista") ~ "Analista",
                                  str_detect(titulo,"Programador") ~ "Programador",
                                  str_detect(titulo, "Ingeniería") ~ "Ingeniería",
                                  str_detect(titulo, "Computador") ~ "Tecnicatura"))

view(grupo_titulo)
# Gracias Violeta Roizman

#-----------------------------------------------------------------------------------------------------------
# ¿Cuáles son los títulos relacionados a programación que tienen mayor EGRESO de  MUJERES para el año 2015?
#-----------------------------------------------------------------------------------------------------------
# calculamos cantidad total y porcentaje de EGRESADOS por sexo para el año 2015

titulos_mas_mujeres<- grupo_titulo %>% filter(anio==2015) %>% group_by(titulo_grupo)%>% summarise(totalEM= sum(egMujeres),totalEV= sum(egVarones), totalE=sum(egMujeres, egVarones), prom=(round(mean(egMujeres),0)),
                                                                                                  porcenEM= (totalEM/totalE * 100), porcenEV=(totalEV/totalE * 100))%>% 
  mutate(brecha = paste0(formato_porce((totalEV-totalEM)/totalE *100), "%"), medio = (totalEV+totalEM)/2)
View(titulos_mas_mujeres)

titulos_mas_MU<- grupo_titulo %>% group_by(anio, titulo_grupo)%>% summarise(totalEM= sum(egMujeres),totalEV= sum(egVarones), totalE=sum(egMujeres, egVarones), prom=(round(mean(egMujeres),0)),
                                                                            porcenEM= round((totalEM/totalE * 100),2), porcenEV=round(totalEV/totalE * 100))%>% 
  mutate(brecha = paste0(formato_porc((totalEV-totalEM)/totalE *100), "%"), medio = (totalEV+totalEM)/2)
View(titulos_mas_MU)

#---------------------------------------------------------------------------
# Gráfico de DONA

# gráfico de dona con porcentajes de egresadas de títulos relacionados con programación para el año 2015

ggplot(data= titulos_mas_mujeres, aes(x=2, y= porcenEM, fill= titulo_grupo))+
  geom_bar(stat = "identity",color="white")+   ##2c204d
  coord_polar(theta="y") +  # porque es un gráfico circular
  xlim(0.5, 2.5) +
  geom_text(aes(label=percent(porcenEM/100)),  
            position=position_stack(vjust=0.5),color="#2c204d",size= 8) +
  scale_fill_manual(values= c("#41b6a6", "#753180","#e95c74","#f5a26b","#f6e37c")) + 
  theme_ft_rc() +
  labs(title = '¿Cuáles son las carreras relacionadas con \nprogramación con mayor porcentaje de egresadas?', x='',y='', fill=" ",
       subtitle ="Para el año 2015 en Argentina", 
       caption = "Fuente: Elaboración propia con datos de Chicas en tecnología") +
  theme(
    plot.title = element_text(hjust = 0,vjust = 1),                    #cambiamos el tamaño, fuente y color del título
    axis.title.x= element_blank(),
    axis.text.x= element_blank(),
    axis.ticks.x= element_blank(),
    axis.title.y= element_blank(),
    axis.text.y= element_blank(),
    axis.ticks.y= element_blank(),
    legend.position = "bottom") 

# Guardamos el gráfico generado
ggsave(here("grafico_dona_final.png"), height = 10, width = 12, units = "in", type='cairo')

# -------------------------------------------------------------------------------------------------
# Diagramas de AREA POLAR o de Florence Nightingale egresadas según carrera para el año 2015

ggplot(data= titulos_mas_mujeres,aes(x=titulo_grupo, y=porcenEM, fill=titulo_grupo))+
  geom_col(width = 1, color = "#2c204d") +
  coord_polar(start= 3*pi/2) +  # porque es un gráfico circular
  scale_y_sqrt() +
  scale_fill_manual(values= c("#41b6a6", "#753180","#e95c74","#f5a26b","#f6e37c")) +  
  geom_text(aes(label=percent(porcenEM/100), size= 4),
            position=position_stack(vjust=0.5),color="#2c204d",size= 8) +
  labs(title = '¿Cuáles son las carreras relacionadas con programación \n con mayor porcentaje de egresadas?', x='',y='', fill=" ",
       subtitle = "Para el año 2015 en Argentina", 
       caption = "Fuente: Elaboración propia con datos de Chicas en tecnología") +
  theme_ft_rc() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom") 

# Guardamos el gráfico generado
ggsave(here("grafico_area_polar_final.png"), height = 10, width = 12, units = "in", type='cairo') 



# Código extra:
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# calculamos totales, promedio y porcentaje de estudiantes mujeres y hombres agrupados por AÑO y GESTION
# utilizamos group_by y summarise y mutate


mujeres_est_gestion<- programadoras %>% group_by(anio, gestion) %>%
  summarise(totalsexoxG = sum(estMujeres), totalxG= sum(estMujeres, estVarones), promxG=(round(mean(estMujeres),0)),
            porcensexoxG = round(totalsexoxG/totalxG * 100)) %>% mutate(sexo='Mujeres')
View(mujeres_est_gestion)

hombres_est_gestion<- programadoras %>% group_by(anio, gestion) %>%
  summarise(totalsexoxG = sum(estVarones), totalxG= sum(estMujeres, estVarones), promxG=(round(mean(estVarones),0)),
            porcensexoxG = round(totalsexoxG/totalxG * 100)) %>% mutate(sexo='Hombres') 
View(hombres_est_gestion)


# Unimos las tablas generadas anteriormente;  estudiantes varones y mujeres por año y gestión
estudiantes_R_gestion<-rbind(mujeres_est_gestion, hombres_est_gestion)%>% arrange(anio, gestion, totalsexoxG)
view(estudiantes_R_gestion)

# si quiero mostrar la tabla generada con un formato más ordenado y visualmente más agradable utilizo el paquete kable
kable(estudiantes_R_barra, align = "r") %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center")
#-----------------------------
# GRAFICOS DE LINEAS
#-----------------------------
# gráfico con CANTIDAD de estudiantes por GÉNERO y GESTIÓN 

ggplot(data= estudiantes_R_gestion, aes(x= anio, y= totalsexoxG, color= sexo)) +
  geom_line(size=2) +
  scale_colour_manual(values= c("#d8d860","#a32ea2"), labels= c( 'Hombres', 'Mujeres'))+
  labs(title = 'Estudiantes de carreras relacionadas con programación de Universidades \nArgentinas por tipo de Gestión\n', x='',y='Cantidad',fill= ' ',color='',
       subtitle ="Para el período 2010-2015" , 
       caption = "Fuente: Elaboración propia con datos de Chicas en tecnología") +
  facet_wrap(~ gestion) +
  theme_ft_rc() +
  theme(text = element_text(size=14, face = 'bold', color = "#2c204d"),
        plot.title = element_text(size=18,                     #cambiamos el tamaño, fuente y color del título
                                  family ="Garamond",    
                                  hjust = 0,vjust = 1,
                                  face = 'bold', 
                                  margin = margin(b = 12 * 1.2)),
        legend.position="bottom", legend.text= element_text(color="grey", 
                                                            size= 12, hjust = 0.5,vjust = 1, family ="Garamond"))

# Guardamos el gráfico generado
ggsave(here("GESTION_lineas_facet_wrap_final.png"), height = 8, width = 10, units = "in", type='cairo')




