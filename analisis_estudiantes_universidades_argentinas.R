## Leemos datos
#setwd("D:/R ladies/MujeresPrograman
library("here")
library("tidyverse")
library(ggthemes)
#library(scales)
library(knitr)
library(ggalt)
library(kableExtra)
library(formattable)
#setwd("D:/Charla_visualizacion_con__perspectiva_de_genero/mujeres_en_tecnologia")
# Lectura de los datos
programadoras <- read.csv(file = "D:/CURSOS_DICTADOS_con_R_2020/visualizacion_con_perspectiva_de_genero/datos/programadorasnew.csv",header = TRUE)
view(programadoras)

# Exploración de los datos
## nombres de las variables
names(programadoras)
#cantidad de observaciones y tipo de variables
glimpse(programadoras)

## que contiene el dataset
#head(programadoras)
## tamaño del dataset
dim(programadoras)

programadoras$nivel

#-------------------------------------
# CANTIDAD DE ESTUDIANTES
#
#------------------------------------
# ESTUDIANTES por año y unidad académica
#--------------------------------
## cantidad de estudiantes mujeres por anio y por unidad academica   


cant_mujeres_anio_inst <- programadoras%>%group_by(institucion, anio)%>% summarise(totalMujeresXInstituto= sum(estMujeres), ) 
View(cant_mujeres_anio_inst)

## cantidad de estudiantes hombres por anio y por unidad academica
cant_varones_anio_inst <- programadoras%>%group_by(institucion, anio)%>% summarise(totalHombresXinstituto= sum(estVarones)) 
View(cant_varones_anio_inst)

# me permitirá observar cantidad de mujeres y hombres por universidad, para luego hacer un ranking.
-------------------------------------------
# función para redondear a decimales cuando calculo brecha de género. Código por Natshu
formato_porce <- function(numero, deci = 1){
    format(round(numero, digits = deci), nsmall = dec, decimal.mark = ",")
  }

--------------------------------------------------------------------------------------------------------------------------------------
# cantidad de ESTUDIANTES mujeres agrupadas por institución para el AÑO 2015
  
mujeres_est<- programadoras%>% filter(anio==2015) %>% group_by(institucion)%>% summarise(totalM= sum(estMujeres),totalV= sum(estVarones), total=sum(estMujeres, estVarones), prom=(round(mean(estMujeres),0)),
                                                                                           porcenM= round(totalM/total* 100), porcenV=round(totalV/total * 100))%>% 
mutate(brecha = paste0(formato_porc((totalV-totalM)/total*100), "%"), medio = (totalV+totalM)/2)
View(mujeres_est)

# **IMPORTANTE: VERIFICAR BRECHA, si está bien calculada.

# universidades con mayor brecha de género entre varones y mujeres en 2015
mayor_brecha <- mujeres_est %>% top_n(10) %>% arrange(brecha)
view(mayor_brecha)


#--------------------------------------------------------------------------------
# UNIVERSIDADES CON MAYOR CANTIDAD DE ESTUDIANTES
# universidades con mayor cantidad de estudiantes mujeres para el año 2015
mas_mujeres_est <- mujeres_est %>% top_n(10) %>% arrange(desc(totalM))
view(mas_mujeres_est)

# universidades con mayor cantidad PORCENTAJE de estudiantes mujeres para el año 2015
mayor_porcen_mujeres_est <- mujeres_est %>% top_n(10) %>% arrange(desc(porcenM))
view(mayor_porcen_mujeres_est)


menos_mujeres_est <- mujeres_est %>% top_n(10) %>% arrange(asc(totalM))
view(menos_mujeres_est)




# modificar nombre de columnas -revisar mi oresentacion o la de allison hill
kable(mas_mujeres_est, align = "r") %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center")

# grafico lollipop  fct_reorder(institucion,(brecha))
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
                                  # family ="Garamond",    
                                  hjust = 0,vjust = 1,
                                  #colour = "#2c204d", 
                                  face = 'bold', 
                                  margin = margin(b = 12 * 1.2)),
        legend.position="bottom",
        legend.text= element_text(color="#2c204d", 
                                  size= 12, hjust = 0.5,vjust = 1, family ="Garamond"))


ggsave(here("lollipop_estudiantes2.png"), height = 8, width = 12, units = "in", type='cairo')

#---------------------------------------------------------------------------------------------------
# Gráfico de dumble que evidencia las universidades que poseen mayor brecha de género en 2015. 
colores = c("#a32ea2","#d8d860") #fct_reorder(name, desc(val))
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
                                  # family ="Garamond",    
                                  hjust = 0,vjust = 1,
                                  #colour = "#2c204d", 
                                  face = 'bold', 
                                  margin = margin(b = 12 * 1.2)),
        legend.position="bottom",
        legend.text= element_text(color="#2c204d", 
                                  size= 12, hjust = 0.5,vjust = 1, family ="Garamond"))



#----------------------------------------------
# cantidad total de estudiantes hombres y mujeres
#-----------------------------------------------

# calculo totales por genero, promedio, porcentaje
mujeres_anio<- programadoras%>%group_by(anio)%>% summarise(totalgenero= sum(estMujeres), total=sum(estMujeres, estVarones), prom=(round(mean(estMujeres),0)), porcen= round(totalgenero/total*100)) %>% mutate(genero='Mujeres') 
View(mujeres_anio)
hombres_anio<- programadoras %>% group_by(anio) %>% summarise(totalgenero= sum(estVarones), total=sum(estMujeres, estVarones), prom=(round(mean(estVarones),0)), porcen=round(totalgenero/total*100)) %>% mutate(genero='Hombres')
View(hombres_anio)


#---------------------------------------------------------------------
# gráfico barras contiguas paradas
library(hrbrthemes)
hrbrthemes::import_titillium_web()

estudiantes_R_barra<-rbind(mujeres_anio, hombres_anio)%>%arrange(desc(totalgenero))
view(estudiantes_R_barra)

# FUNCIONA NO TOCAR
# OPCIÓN 1.A CLARA: grafico de barras  CONTIGUAS con cantidades FUNCIONA
ggplot(data=estudiantes_R_barra, aes(x=anio, y=totalgenero, fill=genero))+
  geom_bar(stat= "identity", position = "dodge", width= .8, color= "black")+
  #coord_flip()+
  scale_fill_manual(values= c("#d8d860","#a32ea2"), labels= c('Hombres', 'Mujeres'))+  #amarillo claro:#f1fa8c  verde:#41b6a6 lilaoscuro:#713580 
  #scale_x_discrete(breaks= c(2010, 2011, 2012,2013,2014,2015), labels= c('2010', '2011', '2012','2013', '2014','2015'))+
  #geom_text(aes(label=total), size=2)+
  geom_text (data =estudiantes_R_barra, aes(anio, medio=(totalgenero/2),label = paste0(totalgenero)), size=3, color = "#2c204d", vjust = - 0.7,  nudge_y = .1) +
  labs(title = 'Estudiantes de carreras relacionadas con Programación \n de Universidades Argentinas\n', x='',y='',fill= ' ',
       subtitle ="Para el período 2010-2015" , 
       caption = "Fuente: Elaboración propia con datos de Chicas en tecnología") +
  theme_ipsum_tw()+
  theme(text = element_text(size=14, face = 'bold', color = "#2c204d"),
        plot.title = element_text(size=18,                     #cambiamos el tamaño, fuente y color del título
                                  # family ="Garamond",    
                                  hjust = 0,vjust = 1,
                                  colour = "#2c204d", 
                                  face = 'bold', 
                                  margin = margin(b = 12 * 1.2)),
        legend.position="bottom",legend.text= element_text(color="#2c204d", 
                                                           size= 12, hjust = 0.5,vjust = 1, family ="Garamond"))

ggsave(here("barras_contigua_arriba_2.png"), height = 8, width = 10, units = "in", type='cairo')

#theme_ipsum_tw()
# para theme negro:theme_ft_rc() 

# BARRAS APILADAS:FUNCIONA NO TOCAR 
# OPCIÓN 2.A: OSCURA grafico de barras con PORCENTAJES funciona y con gext y etiquetas correctas
ggplot(data=estudiantes_R_barra, aes(x=anio, y=totalgenero, fill= genero)) +
  geom_bar(stat= "identity", position = "stack", width= .7, color= "black")+
  #coord_flip()+
  scale_fill_manual(values= c("#d8d860","#a32ea2"), labels= c('Hombres', 'Mujeres'))+  #amarillo claro:#f1fa8c  verde:#41b6a6 lilaoscuro:#713580 
  scale_x_continuous(breaks= c(2010, 2011, 2012,2013,2014,2015), labels= c('2010', '2011', '2012','2013', '2014','2015')) +
  geom_text(aes (label = paste0(porcen, "%")), position = position_stack(vjust=0.5), size=4, color="#2c204d") +
  #geom_text (data =estudiantes_R_barra, aes(anio, medio=(totalgenero/2),label = paste0(totalgenero)), size=3, color = "#2c204d", vjust = - 0.7,  nudge_y = .1) +
  labs(title = 'Estudiantes de carreras relacionadas con programación \n de Universidades Argentinas\n', x='',y='',fill= ' ',
       subtitle ="Para el período 2010-2015" , 
       caption = "Fuente: Elaboración propia con datos de Chicas en tecnología") +
  theme_ft_rc() +
  #theme_ipsum_tw()+
  theme(text = element_text(size=14, face = 'bold', color = "#2c204d"),
        plot.title = element_text(size=18,                     #cambiamos el tamaño, fuente y color del título
                                  # family ="Garamond",    
                                  hjust = 0,vjust = 1,
                                  #colour = "#2c204d", 
                                  face = 'bold', 
                                  margin = margin(b = 12 * 1.2)),
        legend.position="bottom",legend.text= element_text(color="grey", 
                                                           size= 12, hjust = 0.5,vjust = 1, family ="Garamond"))


ggsave(here("barras_apiladas_PORCEN_OSCURO_final2_a.png"), height = 8, width = 10, units = "in", type='cairo')
#ggsave(here("barras_apiladas_cant_final2.png"), height = 8, width = 10, units = "in", type='cairo')


# OPCIÓN 2.B: OSCURA grafico de barras con CANTIDADES funciona y con gext y etiquetas correctas
ggplot(data=estudiantes_R_barra, aes(x=anio, y=totalgenero, fill= genero)) +
  geom_bar(stat= "identity", position = "stack", width= .7, color= "black")+
  #coord_flip()+
  scale_fill_manual(values= c("#d8d860","#a32ea2"), labels= c('Hombres', 'Mujeres'))+  #amarillo claro:#f1fa8c  verde:#41b6a6 lilaoscuro:#713580 
  scale_x_continuous(breaks= c(2010, 2011, 2012,2013,2014,2015), labels= c('2010', '2011', '2012','2013', '2014','2015')) +
  geom_text(aes(label=paste0(totalgenero)), position = position_stack(vjust=0.5), size=4, color="#2c204d") +
  #geom_text (data =estudiantes_R_barra, aes(anio, medio=(totalgenero/2),label = paste0(totalgenero)), size=3, color = "#2c204d", vjust = - 0.7,  nudge_y = .1) +
  labs(title = 'Estudiantes de carreras relacionadas con programación \n de Universidades Argentinas\n', x='',y='',fill= ' ',
       subtitle ="Para el período 2010-2015" , 
       caption = "Fuente: Elaboración propia con datos de Chicas en tecnología") +
  theme_ft_rc() +
  #theme_ipsum_tw()+
  theme(text = element_text(size=14, face = 'bold', color = "#2c204d"),
        plot.title = element_text(size=18,                     #cambiamos el tamaño, fuente y color del título
                                  # family ="Garamond",    
                                  hjust = 0,vjust = 1,
                                  #colour = "#2c204d", 
                                  face = 'bold', 
                                  margin = margin(b = 12 * 1.2)),
        legend.position="bottom",legend.text= element_text(color="grey", 
                                                           size= 12, hjust = 0.5,vjust = 1, family ="Garamond"))


ggsave(here("barras_apiladas_cant_OSCURO_final2_B.png"), height = 8, width = 10, units = "in", type='cairo')
#ggsave(here("barras_apiladas_cant_final2.png"), height = 8, width = 10, units = "in", type='cairo')

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# cantidad de ESTUDIANTES mujeres agrupadas por GESTION para el AÑO 2015
  
mujeres_est_gestion<- programadoras %>% group_by(anio, gestion) %>%
  summarise(totalgeneroxG = sum(estMujeres), totalxG= sum(estMujeres, estVarones), promxG=(round(mean(estMujeres),0)),
           porcengeneroxG = round(totalgeneroxG/totalxG * 100)) %>% mutate(genero='Mujeres')
View(mujeres_est_gestion)
hombres_est_gestion<- programadoras %>% group_by(anio, gestion) %>%
  summarise(totalgeneroxG = sum(estVarones), totalxG= sum(estMujeres, estVarones), promxG=(round(mean(estVarones),0)),
            porcengeneroxG = round(totalgeneroxG/totalxG * 100)) %>% mutate(genero='Hombres') 
View(hombres_est_gestion)

#estudiantes varones y mujeres por gestión
estudiantes_R_gestion<-rbind(mujeres_est_gestion, hombres_est_gestion)%>% arrange(anio, gestion, totalgeneroxG)
view(estudiantes_R_gestion)

# %>% mutate(brechaxG = paste0(formato_porc((totalVxG - totalMxG)/totalVxG * 100), "%"), medio = (totalVxG + totalMxG)/2)

# totalVxG = sum(estVarones),
# porcenVxG=round(totalVxG/totalxG * 100))

#-----------------------------
# GRAFICOS DE LINEAS
#-----------------------------

# OPCION 1: A CLARA
# gráfico CANTIDAD de estudiantes por GENERO y GESTION 

ggplot(data= estudiantes_R_gestion, aes(x= anio, y= totalgeneroxG, color= genero)) +
   geom_line(size=2) +
   #scale_fill_manual(values= c("#d8d860","#a32ea2")) + #labels= c('Hombres', 'Mujeres')+
   facet_grid(gestion ~ genero,  scales = 'free_y') + 
   scale_colour_manual(values= c("#d8d860","#a32ea2"), labels= c('Mujeres', 'Hombres'))+
   labs(title = 'Estudiantes de carreras relacionadas con Programación \n de Universidades Argentinas\n', x='',y='',fill= ' ',
       subtitle ="Para el período 2010-2015" , 
       caption = "Fuente: Elaboración propia con datos de Chicas en tecnología") +
   theme_ipsum_tw() +
   theme(text = element_text(size=14, face = 'bold', color = "#2c204d"),
        plot.title = element_text(size=18,                     #cambiamos el tamaño, fuente y color del título
                                  family ="Garamond",    
                                  hjust = 0,vjust = 1,
                                  colour = "#2c204d", 
                                  face = 'bold', 
                                  margin = margin(b = 12 * 1.2)),
        legend.position="  ", legend.text= element_text(color="#2c204d", 
                                                        size= 12, hjust = 0.5,vjust = 1, family ="Garamond"))


ggsave(here("GESTION_lineas_1_A.png"), height = 8, width = 10, units = "in", type='cairo')

# GRAFICOS DE LINEAS
# OPCION 2: B OSCURA
# gráfico CANTIDAD de estudiantes por GENERO y GESTION 

ggplot(data= estudiantes_R_gestion, aes(x= anio, y= totalgeneroxG, color= genero)) +
  geom_line(size=2) +
  #scale_fill_manual(values= c("#d8d860","#a32ea2")) + #labels= c('Hombres', 'Mujeres')+
  #facet_grid(gestion ~ genero,  scales = 'free_y') + 
  facet_wrap(~ gestion) +
  scale_colour_manual(values= c("#d8d860","#a32ea2"), labels= c( 'Hombres', 'Mujeres'))+
  labs(title = 'Estudiantes de carreras relacionadas con programación de Universidades \nArgentinas por tipo de Gestión\n', x='',y='Cantidad',fill= ' ',color='',
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
        legend.position="bottom", legend.text= element_text(color="grey", 
                                                        size= 12, hjust = 0.5,vjust = 1, family ="Garamond"))


ggsave(here("GESTION_lineas_1_B_facet_wrap.png"), height = 8, width = 10, units = "in", type='cairo')

#-------------------------------------------------------------------------------------------------------------
view(estudiantes_R)
# FUNCIONA
# Gráfico con totales de estudiantes por género, escala real
# opción 1
ggplot(data=estudiantes_R, aes(x=anio, y=total, color=genero))+
  geom_line(size=2) +
  geom_point(data=estudiantes_R, aes(anio, total), size=3) +
  scale_colour_manual(values= c("#d8d860","#a32ea2"), labels= c('Mujeres', 'Hombres'))+
  facet_wrap(~genero, ncol=1) +
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

#theme_ipsum_tw()# opción clara
ggsave(here("grafico_total_facet_puntos.png"), height = 8, width = 10, units = "in", type='cairo')


#opcion2 negro: CON ESCALA FREE_Y, entonces cada gráfico tiene su propia escala
ggplot(data=estudiantes_R, aes(x=anio, y=total, color=genero))+
  geom_line(size=2) +
  geom_point() +
  scale_colour_manual(values= c("#d8d860","#a32ea2"), labels= c('Mujeres', 'Hombres'))+
  facet_wrap(~genero, ncol=1, scales="free_y") +
  labs(title = 'Estudiantes de carreras relacionadas con programación \n de Universidades Argentinas\n', x='',y='',fill= ' ',
       subtitle ="Para el período 2010-2015" , 
       caption = "Fuente: Elaboración propia con datos de Chicas en tecnología") +
  theme_ipsum_tw() +
  theme(text = element_text(size=14, face = 'bold', color = "#2c204d"),
        plot.title = element_text(size=18,                     #cambiamos el tamaño, fuente y color del título
                                  family ="Garamond",    
                                  hjust = 0,vjust = 1,
                                  colour = "#2c204d", 
                                  face = 'bold', 
                                  margin = margin(b = 12 * 1.2)),
        legend.position="  ", legend.text= element_text(color="#2c204d", 
                                                        size= 12, hjust = 0.5,vjust = 1, family ="Garamond"))


ggsave(here("grafico_total_facet2.png"), height = 8, width = 10, units = "in", type='cairo')



# OPCIÓN 1: grafico de barras con cantidades funciona y con gext y etiquetas correctas
# ggplot(data=estudiantes_R_barra, aes(x=anio, y=totalgenero, fill= genero)) +
#   geom_bar(stat= "identity", position = "stack", width= .8, color= "black")+
#   #coord_flip()+
#   scale_fill_manual(values= c("#d8d860","#a32ea2"), labels= c('Hombres', 'Mujeres'))+  #amarillo claro:#f1fa8c  verde:#41b6a6 lilaoscuro:#713580 
#   scale_x_continuous(breaks= c(2010, 2011, 2012,2013,2014,2015), labels= c('2010', '2011', '2012','2013', '2014','2015')) +
#   geom_text(aes(label=paste0(totalgenero)), position = position_stack(vjust=0.5), size=3) +
#   #geom_text (data =estudiantes_R_barra, aes(anio, medio=(totalgenero/2),label = paste0(totalgenero)), size=3, color = "#2c204d", vjust = - 0.7,  nudge_y = .1) +
#   labs(title = 'Estudiantes de carreras relacionadas con Programación \n de Universidades Argentinas\n', x='',y='',fill= ' ',
#        subtitle ="Para el período 2010-2015" , 
#        caption = "Fuente: Elaboración propia con datos de Chicas en tecnología") +
#   theme_ipsum_tw()+
#   theme(text = element_text(size=14, face = 'bold', color = "#2c204d"),
#         plot.title = element_text(size=18,                     #cambiamos el tamaño, fuente y color del título
#                                   # family ="Garamond",    
#                                   hjust = 0,vjust = 1,
#                                   colour = "#2c204d", 
#                                   face = 'bold', 
#                                   margin = margin(b = 12 * 1.2)),
#         legend.position="bottom",legend.text= element_text(color="#2c204d", 
#                                                            size= 12, hjust = 0.5,vjust = 1, family ="Garamond"))

# ---------------------------------------------------------------------------------------------------------------------------
# PARA GRAFICAR CARRERAS MÁS ELEGIDAS POR ESTUDIANTES
# agrupado por título, agregó un nuevo campo titulo_grupo

# Pude hacerlo gracias a Violeta Roizman
grupo_titulo <- programadoras %>%
  mutate(titulo_grupo = case_when(str_detect(titulo, "Licenciatura") ~ "Licenciatura",
                                str_detect(titulo,"Tecnicatura") ~ "Tecnicatura",
                                str_detect(titulo,"Analista") ~ "Analista",
                                str_detect(titulo,"Programador") ~ "Programador",
                                str_detect(titulo, "Ingeniería") ~ "Ingeniería",
                                str_detect(titulo, "Computador") ~ "Tecnicatura"))
         
view(grupo_titulo)
# Gracias Violeta Roizman

grupo_titulo%>% group_by(titulo) %>% summarise(mu=sum(egMujeres), varones=sum(egVarones), total=sum (egMujeres, egVarones),porcen= (mu/total * 100))
view(grupo_titulo)

# ¿Cuáles son las carreras de las cuales egresan más Mujeres?
# cantidad de TITULOS mujeres agrupadas por institución para el AÑO 2015
#-------------------------------------------
# ESTE UTILIZO PARA EL GRÁFICO
titulos_mas_mujeres<- grupo_titulo %>% filter(anio==2015) %>% group_by(titulo_grupo)%>% summarise(totalEM= sum(egMujeres),totalEV= sum(egVarones), totalE=sum(egMujeres, egVarones), prom=(round(mean(egMujeres),0)),
                                                                                         porcenEM= (totalEM/totalE * 100), porcenEV=(totalEV/totalE * 100))%>% 
  mutate(brecha = paste0(formato_porc((totalEV-totalEM)/totalE *100), "%"), medio = (totalEV+totalEM)/2)
View(titulos_mas_mujeres)

titulos_mas_MU<- grupo_titulo %>% group_by(anio, titulo_grupo)%>% summarise(totalEM= sum(egMujeres),totalEV= sum(egVarones), totalE=sum(egMujeres, egVarones), prom=(round(mean(egMujeres),0)),
                                                                                                  porcenEM= round((totalEM/totalE * 100),2), porcenEV=round(totalEV/totalE * 100))%>% 
  mutate(brecha = paste0(formato_porc((totalEV-totalEM)/totalE *100), "%"), medio = (totalEV+totalEM)/2)
View(titulos_mas_MU)

#----------------------------------------

# GRÁFICO LLOLIPOP FUNCIONO
# egresadas según carrera para el año 2015

ggplot(titulos_mas_mujeres,aes(x= fct_reorder(titulo_grupo, totalEM), y = totalEM, group = titulo_grupo), label = totalEM) +
  geom_segment(aes(xend=titulo_grupo, yend=0)) +
  geom_segment(data=titulos_mas_hombres, (aes(xend=titulo_grupo, yend=0, color="grey"))) +
  geom_point(size=3.8, color="#a32ea2") +
  geom_text (data = titulos_mas_mujeres, aes(titulo_grupo, (totalEM + 5),  label = totalEM), size=3.5 , color="#a32ea2", vjust = - 0.5,  nudge_y = .1) +
  #coord_flip() +
  labs(title = '¿Cuáles son las carreras relacionadas con programación \n con mayor nro. de egresadas?', x='',y='', color=" ",
       subtitle ="Para el año 2015 en Argentina" , 
       caption = "Fuente: Elaboración propia con datos de Chicas en tecnología") +
  theme_ft_rc() +
  theme(text = element_text(size=14, face = 'bold'),
        plot.title = element_text(size=18,                     #cambiamos el tamaño, fuente y color del título
                                  # family ="Garamond",    
                                  hjust = 0,vjust = 1,
                                  #colour = "#2c204d", 
                                  face = 'bold', 
                                  margin = margin(b = 12 * 1.2)),
        legend.position=" ",
        legend.text= element_text(color="#2c204d", 
                                  size= 12, hjust = 0.5,vjust = 1, family ="Garamond"))

# ¿Cuántos mujeres egresan de carreras relacionadas con programación en Argentina?
# **IMPORTANTE: VERIFICAR BRECHA, si está bien calculada.

# universidades con mayor brecha de género entre varones y mujeres en 2015
#mayor_brecha <- mujeres_est %>% top_n(10) %>% arrange(brecha)
#view(mayor_brecha)


#---------------------------------------------------------
# FUNCIONA LISTO NO TOCAR!!!
# OPCIÓN D1: primer GRÁFICO DE DONA con CELESTE  
#código gracias a pmoracho egresadas según carrera para el año 2015

ggplot(titulos_mas_mujeres,aes(x=2, y= porcenEM, fill=titulo_grupo))+
  geom_bar(stat = "identity",color="white")+   ##2c204d
  coord_polar(theta="y") +
  theme_ft_rc() +
  xlim(0.5,2.5) +
  geom_text(aes(label=percent(porcenEM/100)),  
            position=position_stack(vjust=0.5),color="#2c204d",size= 6) +   #opcion1: size= 6
  scale_fill_manual(values= c("#41b6a6", "#f6e37c","#f5a26b","#51b8df","#713580")) + #opción 1 de colores lila:#713580
  #scale_fill_manual(values= c("#41b6a6", "#753180","#e95c74","#f5a26b","#f6e37c")) +  #opción 2: ,"#51b8df"  #9ccfb1:verde lila:#753180
  labs(title = '¿Cuáles son las carreras relacionadas con \nprogramación con mayor porcentaje de egresadas?', x='',y='', fill=" ",
       subtitle ="Para el año 2015 en Argentina", 
       caption = "Fuente: Elaboración propia con datos de Chicas en tecnología") +
  theme(
    plot.title = element_text(hjust = 0,vjust = 1),                    #cambiamos el tamaño, fuente y color del título
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    legend.position = "bottom") 


ggsave(here("grafico_dona3colores.png"), height = 10, width = 12, units = "in", type='cairo')    #size: 6



# OPCION D2: 
# GRÁFICO DE DONA  FUNCIONA LISTO!!!
#código gracias a pmoracho egresadas según carrera para el año 2015

ggplot(titulos_mas_mujeres,aes(x=2, y= porcenEM, fill=titulo_grupo))+
  geom_bar(stat = "identity",color="white")+   ##2c204d
  coord_polar(theta="y") +
  theme_ft_rc() +
  xlim(0.5,2.5) +
  geom_text(aes(label=percent(porcenEM/100)),  #opcion1: size= 6
            position=position_stack(vjust=0.5),color="#2c204d",size= 8) +
 scale_fill_manual(values= c("#41b6a6", "#753180","#e95c74","#f5a26b","#f6e37c")) +  #opción 2: ,"#51b8df"  #9ccfb1:verde lila:#753180
  labs(title = '¿Cuáles son las carreras relacionadas con \nprogramación con mayor porcentaje de egresadas?', x='',y='', fill=" ",
       subtitle ="Para el año 2015 en Argentina", 
       caption = "Fuente: Elaboración propia con datos de Chicas en tecnología") +
  theme(
    plot.title = element_text(hjust = 0,vjust = 1),                    #cambiamos el tamaño, fuente y color del título
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom") 
 

ggsave(here("grafico_dona3colores_size8.png"), height = 10, width = 12, units = "in", type='cairo')
#--------------------------------------------------------------------------------------------

# FUNCIONA
# diagramas de AREA POLAR o de Florence Nightingale egresadas según carrera para el año 2015
ggplot(titulos_mas_mujeres,aes(x=titulo_grupo, y=porcenEM, fill=titulo_grupo))+
geom_col(width = 1, color = "#2c204d") +
  scale_y_sqrt() +
  #xlim(0.5,2.5) + 
  coord_polar(start=3*pi/2) +
  #scale_fill_manual(values= c("#41b6a6", "#f6e37c","#f5a26b","#51b8df","#713580"))+
  scale_fill_manual(values= c("#41b6a6", "#753180","#e95c74","#f5a26b","#f6e37c")) +  #opción 2: ,"#51b8df"  #9ccfb1:verde lila:#753180
 geom_text(aes(label=percent(porcenEM/100), size= 4),
            position=position_stack(vjust=0.5),color="#2c204d",size= 8) +
  theme_ft_rc() +
  theme_elegante_std(base_family = "Ralleway") +
  labs(title = '¿Cuáles son las carreras relacionadas con programación \n con mayor porcentaje de egresadas?', x='',y='', fill=" ",
       subtitle = "Para el año 2015 en Argentina", 
       caption = "Fuente: Elaboración propia con datos de Chicas en tecnología") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom") 
 
ggsave(here("grafico_areaPolar2.png"), height = 10, width = 12, units = "in", type='cairo')
ggsave(here("grafico_areaPolar2_size8.png"), height = 10, width = 12, units = "in", type='cairo')  #OPCIÓN 2

#-----------------------------------------------------------------------------------
ver <-programadoras %>% group_by(anio, titulo) %>% summarise(suma=sum(egMujeres))
view(ver)
# gráfico de ggridges
install.packages("ggridges")
library("ggridges")
ggplot(data=titulos_mas_MU,aes(x = totalEM, y = titulo_grupo, fill = titulo_grupo)) +
  geom_density_ridges() +
  theme(legend.position = "none") +
  labs(title = '¿Cuáles son las carreras de las \n cuales egresan más Mujeres?', x='',y='', fill=" ",
       subtitle ="Para el año 2015" , 
       caption = "Fuente: Elaboración propia con datos de Chicas en tecnología") +
  facet_wrap(~anio, ncol=1)
  # ) +
  # scale_x_continuous(breaks = c(c(0,5, 10, 20, 50), seq(from=75, to=max(data$casos)+25, by = 25))) +
  # theme_elegante_std(base_family = "Assistant") +
  # theme(legend.position = "none")  

#----------------------------------------------------------------------------------

#------------------------------------------------------------------------------------
view(estudiantes_nivelR)
gestion_mujeres<- estudiantes_nivelR %>% filter(anio==2015)


# no funciona revisar 
#Gráfico de waffles
library("waffle")
ggplot(data= gestion_mujeres, aes(x = est_nivel, values = c(40, 60, 25,75))) +
  geom_waffle(n_rows = 10, size = 0.33, colour = "white", flip = TRUE) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), labels = function(x) x * 10) +
  #scale_x_continuous(breaks = seq(0, 10, by = 1), labels = function(x) x * 10) +
  coord_equal() +
  facet_wrap(~ genero) +
  #scale_fill_manual(values=c("#E69F00", "#56B4E9", "#009E73",  "#0072B2", "#D55E00", "#CC79A7")) +
  theme_elegante_std(base_family = "Assistant") +
  labs(title = '¿Cuáles son las carreras de las \n cuales egresan más Mujeres?', x='',y='', fill=" ",
       subtitle ="Para el año 2015" , 
       caption = "Fuente: Elaboración propia con datos de Chicas en tecnología") +
  theme_elegante_std()

if ("ggelegant" %in% rownames(installed.packages())) {
  library("ggelegant")
} else {
  # devtools::install_github("pmoracho/ggelegant")
  theme_elegante_std <- function(base_family) {}
}


#-----------------------------------------------------------------------------------------
# Referencias: https://pmoracho.github.io/blog/2020/05/01/30-dias-de-graficos-en-r/






mujeres_est_gestion<- grupo_titulo %>% group_by(anio, gestion) %>%
  summarise(totalgeneroxG = sum(estMujeres), totalxG= sum(estMujeres, estVarones), promxG=(round(mean(estMujeres),0)),
            porcengeneroxG = round(totalgeneroxG/totalxG * 100)) %>% mutate(genero='Mujeres')
View(mujeres_est_gestion)
hombres_est_gestion<- grupo_titulo %>% group_by(anio, gestion) %>%
  summarise(totalgeneroxG = sum(estVarones), totalxG= sum(estMujeres, estVarones), promxG=(round(mean(estVarones),0)),
            porcengeneroxG = round(totalgeneroxG/totalxG * 100)) %>% mutate(genero='Hombres') 
View(hombres_est_gestion)

#estudiantes varones y mujeres por gestión
estudiantes_R_gestion<-rbind(mujeres_est_gestion, hombres_est_gestion)%>% arrange(anio, gestion, totalgeneroxG)
view(estudiantes_R_gestion)

