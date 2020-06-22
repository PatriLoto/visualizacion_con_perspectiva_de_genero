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
head(programadoras)
## tamaño del dataset
dim(programadoras)

programadoras$nivel

#---------------------------------
# ESTUDIANTES por año y unidad académica
#--------------------------------
## cantidad de estudiantes hombres y mujeres por anio y por unidad academica

cant_mujeres_anio_inst <- programadoras%>%group_by(institucion, anio)%>% summarise(totalMujeresXInstituto= sum(estMujeres)) 
View(cant_mujeres_anio_inst)

cant_varones_anio_inst <- programadoras%>%group_by(institucion, anio)%>% summarise(totalHombresXinstituto= sum(estVarones)) 
View(cant_varones_anio_inst)

#----------------------------------------------
# cantidad total de estudiantes hombres y mujeres
#-----------------------------------------------

# calculo totalas por genero, promedio, porcentaje
mujeres_anio<- programadoras%>%group_by(anio)%>% summarise(totalgenero= sum(estMujeres), total=sum(estMujeres, estVarones), prom=(round(mean(estMujeres),0)), porcen= round(totalgenero/total*100)) %>% mutate(genero='Mujeres') 
View(mujeres_anio)
hombres_anio<- programadoras %>% group_by(anio) %>% summarise(totalgenero= sum(estVarones), total=sum(estMujeres, estVarones), prom=(round(mean(estVarones),0)), porcen=round(totalgenero/total*100)) %>% mutate(genero='Hombres')
View(hombres_anio)



-------------------------------------------
  # función para redondear a decimales de código Natshu
formato_porce <- function(numero, deci = 1){
  format(round(numero, digits = deci), nsmall = dec, decimal.mark = ",")
}


mujeres_est<- programadoras%>% filter(anio==2015) %>% group_by(institucion)%>% summarise(totalM= sum(estMujeres),totalV= sum(estVarones), tot=sum(estMujeres, estVarones), prom=(round(mean(estMujeres),0)))%>% 
 mutate(brecha = paste0(formato_porc((totalV-totalM)/totalV*100), "%"), medio = (totalV+totalM)/2)
View(mujeres_est)

# hombres_anio<- programadoras%>%group_by(anio)%>% summarise(total= sum(estVarones), totalV= sum(estVarones), tot=sum(estMujeres, estVarones), prom=(round(mean(estVarones),0)))
# %>% mutate(brecha = paste0(formato_porc((totalV-totalM)/totalV*100), "%"))
# View(hombres_anio)

mas_mujeres_est <- mujeres_est %>% top_n(10) %>% arrange(brecha)
view(mas_mujeres_est)

kable(mas_mujeres_est, align = "r") %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "center")




# prueba dumbble
colores = c("#a32ea2","#d8d860") #fct_reorder(name, desc(val))
ggplot(mas_mujeres_est, aes(x = totalM, xend = totalV, y =reorder(institucion,(totalV)), group = institucion, label = brecha)) +
  geom_dumbbell(color= "#808080",
                size_x = 3, size_xend = 3,
                colour_x = colores[1],
                colour_xend = colores[2]) +
  #geom_text(aes(label = brecha), nudge_y = .2)+
  geom_text(aes(medio, institucion, label = brecha), nudge_y = .2)+
  labs(x="Cantidad de estudiantes",
       y=NULL, 
       caption = "Fuente: Elaboración propia en base a datos de chicas en tecnología")+
  scale_color_manual(values= colores)+
  theme_minimal()

# datos 2015
mujeres_ing<- programadoras%>% filter(anio==2015) %>% group_by(institucion)%>% summarise(totalIngM= sum(niMujeres),totalIngV= sum(niVarones), totalIng=sum(niMujeres, niVarones), promIn=(round(mean(niMujeres),0)))%>% 
  mutate(brecha = paste0(formato_porc((totalIngV-totalIngM)/totalIngV*100), "%"), medio = (totalIngM/2))
view(mujeres_ing)
ingresantes <- mujeres_ing %>% top_n(10, totalIngM) %>% mutate(institucion = fct_reorder(institucion, (totalIngM)))
view(ingresantes) # para 2015


# grafico lollipop
ggplot(ingresantes,aes(x=institucion, y =totalIngM, group = institucion), label = totalIngM) +
  geom_segment(aes(xend=institucion, yend=0), colour="#e3e4e5") +
  geom_point(size=4, color="#a32ea2") +
  geom_text (data =ingresantes, aes(institucion, medio,  label = totalIngM), vjust = - 0.5,  nudge_y = .1) +
  coord_flip() +
 labs(title = 'Universidades Argentinas con mayor número de inscriptas \n mujeres', x='',y='', color=" ",
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


ggsave(here("lollipop.png"), height = 8, width = 12, units = "in", type='cairo')

#theme_ipsum_tw()
#theme_ft_rc()
#---------------------------------------------------------------------
# gráfico barras contiguas paradas
library(hrbrthemes)
hrbrthemes::import_titillium_web()
estudiantes_R_barra<-rbind(mujeres_anio, hombres_anio)%>%arrange(desc(totalgenero))
view(estudiantes_R_barra)
# grafico de barras con percentajes
ggplot(data=estudiantes_R_barra, aes(x=anio, y=totalgenero, fill=genero))+
  geom_bar(stat= "count", position = "identity", width= .8, color= "black")+
  #coord_flip()+
  scale_fill_manual(values= c("#d8d860","#a32ea2"), labels= c('Hombres', 'Mujeres'))+  #amarillo claro:#f1fa8c  verde:#41b6a6 lilaoscuro:#713580 
  #scale_x_discrete(breaks= c(2010, 2011, 2012,2013,2014,2015), labels= c('2010', '2011', '2012','2013', '2014','2015'))+
  #geom_text(aes(label=total), size=2)+
  geom_text (data =estudiantes_R_barra, aes(anio, medio=(totalgenero/2),  label = paste0(porcen, "%")), color = "#2c204d", vjust = - 0.5,  nudge_y = .1) +
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
#theme_ipsum_tw()
#theme_ft_rc()
# para theme negro:theme_ft_rc() 
ggsave(here("barras_con_leyenda arriba_2.png"), height = 8, width = 10, units = "in", type='cairo')

# barras con stack pero debería clacular los porcentajes

ggplot(data=estudiantes_R_barra, aes(x=anio, y=total,fill=genero))+
  geom_col(position = "stack", width= .8, color= "black")+
  #coord_flip()+
  scale_fill_manual(values= c("#d8d860","#a32ea2"), labels= c('Hombres', 'Mujeres'))+  #amarillo claro:#f1fa8c  verde:#41b6a6 lilaoscuro:#713580 
  #scale_x_discrete(breaks= c(2010, 2011, 2012,2013,2014,2015), labels= c('2010', '2011', '2012','2013', '2014','2015'))+
  #geom_text(aes(label=total), size=2)+
  geom_text (data =estudiantes_R_barra, aes(anio, medio=(total/2),  label = paste0(porcen, "%")), color = "#2c204d", vjust = - 0.5,  nudge_y = .1) +
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
#theme_ipsum_tw()




#CON FACET WRAP
ggplot(data=estudiantes_R_bar, aes(x=anio, y=total,fill=genero))+
  geom_bar(stat= "identity", position = "dodge", width= .8, color= "black")+
  #coord_flip()+
  scale_fill_manual(values= c("#d8d860","#a32ea2"), labels= c('Hombres', 'Mujeres'))+  #amarillo claro:#f1fa8c  verde:#41b6a6 lilaoscuro:#713580 
  #scale_x_discrete(breaks= c(2010, 2011, 2012,2013,2014,2015), labels= c('2010', '2011', '2012','2013', '2014','2015'))+
  #geom_text(aes(label=total), size=2)+
  labs(title = 'Estudiantes de carreras relacionadas con Programación \n de Universidades Argentinas', x='',y='',fill= ' ',
       subtitle ="Para el período 2010-2015" , 
       caption = "DataSource: Chicas en tecnología") +
  theme_ipsum_tw()+
  theme(text = element_text(size=14, face = 'bold', color = "#2c204d"),
        plot.title = element_text(size=18,                     #cambiamos el tamaño, fuente y color del título
                                  # family ="Garamond",    
                                  hjust = 0,vjust = 1,
                                  colour = "#2c204d", 
                                  face = 'bold', 
                                  margin = margin(b = 12 * 1.2)),
        legend.position= " ",legend.text= element_text(color="#2c204d", 
                                                      size= 12, hjust = 0.5,vjust = 1, family ="Garamond")) +
  #facet_wrap(~genero, scales = 'free_y') # barras_con_facet3png
  facet_grid(anio ~ genero, space= 'free_y', scales = 'free_y')  # barras_con_facet4.png
#theme_ipsum_tw()

ggsave(here("barras_apiladas.png"), height = 8, width = 10, units = "in", type='cairo')


# gráfico barras contiguas acostadas
estudiantes_R_flip<-rbind(mujeres_anio, hombres_anio)%>%arrange((total))
view(estudiantes_R_flip)

ggplot(data=estudiantes_R_flip, aes(x=anio, y=total,fill=genero))+
  geom_bar(stat= "identity", position = "dodge", width= .8, color= "black")+
  #geom_col( position = position_dodge())+
  coord_flip()+
  scale_fill_manual(values= c("#d8d860","#a32ea2"), labels= c('Hombres', 'Mujeres'))+  #amarillo claro:#f1fa8c  verde:#41b6a6 lilaoscuro:#713580 
  #scale_x_discrete(breaks= c(2010, 2011, 2012,2013,2014,2015), labels= c('2010', '2011', '2012','2013', '2014','2015'))+
  #geom_text(aes(label=total), size=2)+
  labs(title = 'Estudiantes de Carreras Exactas de Universidades Argentinas\n', x='',y='',fill= ' ',
       subtitle ="Para el período 2010-2015" , 
       caption = "DataSource: Chicas en tecnología") +
  theme_ipsum_tw()+
  theme(text = element_text(size=14, face = 'bold', color = "#2c204d"),
        plot.title = element_text(size=18,                     #cambiamos el tamaño, fuente y color del título
                                  family ="Garamond",    
                                  hjust = 0,vjust = 1,
                                  colour = "#2c204d", 
                                  face = 'bold', 
                                  margin = margin(b = 12 * 1.2)),
        legend.position="top",legend.text= element_text(color="#2c204d", 
                                                        size= 12, hjust = 0.5,vjust = 1, family ="Garamond"))
#theme_ipsum_tw()
ggsave(here("grafico_barras_nuevos_colores_flip.png"), height = 8, width = 10, units = "in", type='cairo')

#----------------------------------------------------------------------------------------------------
# FUNCIONA
# graficos de líneas con promedios 
estudiantes_R<-rbind(mujeres_anio, hombres_anio)%>%arrange(desc(anio,total))
view(estudiantes_R)


# Gráfico de lineas con promedios por estudiantes por año CON facet por género
ggplot(data=estudiantes_R, aes(x=anio, y=prom, color=genero))+
  geom_line(size=2) +
  scale_colour_manual(values= c("#d8d860","#a32ea2"), labels= c('Mujeres', 'Hombres'))+
  facet_grid(~genero) +  #uno al lado del otro
  labs(title = 'Estudiantes de carreras relacionadas con Programación \n de Universidades Argentinas\n', x='',y='',fill= ' ',
       subtitle ="Para el período 2010-2015" , 
       caption = "Fuente: Elaboración propia con datos de Chicas en tecnología") +
  #theme_ipsum_tw()
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
#theme_ipsum_tw()#

ggsave(here("grafico_prom_estud.png"), height = 8, width = 10, units = "in", type='cairo')

#opcion 2
# Gráfico de líneas con prom. de estudiantes por género
ggplot(data=estudiantes_R, aes(x=anio, y=prom, color=genero))+
  geom_line(size=2) +
  scale_colour_manual(values= c("#d8d860","#a32ea2"), labels= c('Mujeres', 'Hombres'))+
  facet_wrap(~genero, ncol=1) +  #scales="free_y"  #uno arriba del otro
  labs(title = 'Estudiantes de carreras relacionadas con Programación \n de Universidades Argentinas\n', x='',y='',fill= ' ',
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


ggsave(here("grafico_prom_estud_wrap.png"), height = 8, width = 10, units = "in", type='cairo')
#-------------------------------------------------------------------------------------------------------------

# FUNCIONA
# Gráfico con totales de estudiantes por género, escala real
# opción 1
ggplot(data=estudiantes_R, aes(x=anio, y=total, color=genero))+
  geom_line(size=2) +
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
#theme_ipsum_tw()#
ggsave(here("grafico_total_facet.png"), height = 8, width = 10, units = "in", type='cairo')


#opcion2 negro: CON ESCALA FREE_Y, entonces cada gráfico tiene su propia escala
ggplot(data=estudiantes_R, aes(x=anio, y=total, color=genero))+
  geom_line(size=2) +
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

#-----------------------------------------------------------------------------------------------------------------------------------



# cantidad de estudiantes mujeres y varones por anio y nivel  
cant_mujeres_anio_nivel <-programadoras %>% group_by(anio, nivel)%>% summarise(totalMujeresXnivel= sum(estMujeres)) 
view(cant_mujeres_anio_nivel)

cant_hombres_anio_nivel <-programadoras %>% group_by(anio, nivel)%>% summarise(totalHombresXnivel= sum(estVarones)) 
view(cant_hombres_anio_nivel)



estudiantes_mujeres_anio_nivelR <-programadoras %>% group_by(anio, nivel)%>% summarise(est_nivel= sum(estMujeres))%>% mutate(genero='Femenino') 
view(estudiantes_mujeres_anio_nivelR)

estudiantes_hombres_anio_nivelR <-programadoras %>% group_by(anio, nivel)%>% summarise(est_nivel= sum(estVarones))%>% mutate(genero='Masculino') 
view(estudiantes_hombres_anio_nivelR)
# por filas
estudiantes_nivelR<-rbind(estudiantes_mujeres_anio_nivelR, estudiantes_hombres_anio_nivelR)
view(estudiantes_nivelR)


# FUNCIONA
# opción 1 facetado por niveles negro

ggplot(data=estudiantes_nivelR, aes(x=anio, y=est_nivel, colour=genero))+
  geom_line() +
  #facet_grid(anio ~ genero, space= 'free_y', scales = 'free_y') +
  geom_line(size=2) +
  scale_colour_manual(values= c("#a32ea2", "#d8d860"), labels= c('Mujeres', 'Hombres'))+
  facet_wrap(~nivel, ncol=1) + # scales="free_y"
  labs(title = 'Estudiantes de carreras relacionadas con programación \n de Universidades Argentinas por tipo de nivel\n', x='',y='',color= ' ',
       subtitle ="Para el período 2010-2015" , 
       caption = "Fuente: Elaboración propia con datos de Chicas en tecnología") +
  theme_ft_rc() + 
  theme(text = element_text(size=14, face = 'bold'),
        plot.title = element_text(size=18,                     #cambiamos el tamaño, fuente y color del título
                                  family ="Garamond",    
                                  hjust = 0,vjust = 1,
                                  #colour = "#2c204d" 
                                  face = 'bold', 
                                  margin = margin(b = 12 * 1.2)),
        legend.position="top", legend.text= element_text(size= 12, hjust = 0.5,vjust = 1))

ggsave(here("grafico_nivel_Facetnegro.png"), height = 8, width = 10, units = "in", type='cairo')

# opción 2 facetado

ggplot(data=estudiantes_nivelR, aes(x=anio, y=est_nivel, colour=genero))+
  geom_line() +
  #facet_grid(anio ~ genero, space= 'free_y', scales = 'free_y') +
  geom_line(size=2) +
  scale_colour_manual(values= c("#a32ea2", "#d8d860"), labels= c('Mujeres', 'Hombres'))+
  facet_wrap(~nivel, ncol=1, scales="free_y") +
  labs(title = 'Estudiantes de Carreras Exactas de Universidades Argentinas', x='',y='', color= ' ',
       subtitle ="Para el período 2010-2015" , 
       caption = "DataSource: Chicas en tecnología") +
  theme_ipsum_tw() + 
  theme(text = element_text(size=14, face = 'bold', color = "#2c204d"),
        plot.title = element_text(size=18,                     #cambiamos el tamaño, fuente y color del título
                                  family ="Garamond",    
                                  hjust = 0,vjust = 1,
                                  colour = "#2c204d", 
                                  face = 'bold', 
                                  margin = margin(b = 12 * 1.2)),
        legend.position="top", legend.text= element_text(size= 12, hjust = 0.5,vjust = 1, color = "#2c204d"))

ggsave(here("grafico_nivel_Facetclaro.png"), height = 8, width = 10, units = "in", type='cairo')




#facet_wrap(~genero) #, scales="free_y"

#quiero filtrar por nivel y no puedo
# solo_grado<-estudiantes_nivelR %>% filter(nivel=='Grado')
#  view(solo_grado) ver factors 
# programadoras$nivel[nivel=]
# solo_grado <-programadoras %>% filter(nivel==1) %>% group_by(anio)%>% summarise(est_nivel= sum(estVarones))%>% mutate(genero='Masculino') 
# view(solo_grado)
# prueba <-programadoras %>% mutate(nivel2= str_replace_all(nivel,"Grado//s",'Grado')) 
# prueba2 <- prueba %>% ifelse(nivel2=="Grado//s", 'Grado', 'pregrado')   #view(prueba)
# view(prueba2)


ggplot(data=estudiantes_nivelR, aes(x=anio, y=est_nivel,colour=genero))+
  geom_col()
#---------------------------------
# REINSCRIPTOS
#--------------------------------
reins_mujeres_anio <-programadoras %>% group_by(anio)%>% summarise(total_reins_mujeres= sum(reinsMujeres))
view(reins_mujeres_anio)

reins_hombres_anio <-programadoras %>% group_by(anio)%>% summarise(total_reins_varones= sum(reinsVarones))
view(reins_hombres_anio)

# reinscriptos_anio<-programadoras %>% group_by(anio)%>% summarise(total_reins= sum(anio))
# view(reinscriptos_anio)

#UNO REINSCRIPTOS HOMBRES +MUJERES
data_reinscriptos<-left_join(reins_mujeres_anio, reins_hombres_anio)
view(data_reinscriptos)

# se nota la amplia diferencia entre la reinscripción de hombres y mujeres por anio
#lineas
ggplot(data=data_reinscriptos, aes(x=anio, y=total_reins_mujeres))+
  geom_line()

ggplot(data=data_reinscriptos, aes(x=anio, y=total_reins_varones))+
  geom_line()

# teniendo en cuenta ambos datos 
ggplot(data=data_reinscriptos, aes(x=anio, y=total_reins_varones))+
  geom_line()+
  geom_line(data=reins_mujeres_anio, aes(x=anio, y=total_reins_mujeres))

#AGRUPADOS por año y nivel
# cantidad de reinscriptos de mujeres y hombres por anio y nivel
reins_mujeres_anio_nivel <-programadoras %>% group_by(anio, nivel)%>% summarise(M_reins_nivel= sum(reinsMujeres)) 
view(reins_mujeres_anio_nivel)

reins_hombres_anio_nivel <-programadoras %>% group_by(anio, nivel)%>% summarise(H_reins_nivel= sum(reinsVarones)) 
view(reins_hombres_anio_nivel)

reinscriptos_nivelC<-cbind(reins_mujeres_anio_nivel, reins_hombres_anio_nivel)
view(reinscriptos_nivelC)

ggplot(data=reinscriptos_nivel, aes(x=anio, y=M_reins_nivel, colour=nivel))+
  geom_line() +
  facet_wrap(~nivel)


ggplot(data=reinscriptos_nivel, aes(x=anio, y=H_reins_nivel, colour=nivel1))+
  geom_line() +
  facet_wrap(~nivel1)


#---------------------------------------------------
reins_mujeres_anio_nivelR <-programadoras %>% group_by(anio, nivel)%>% summarise(reins_nivel= sum(reinsMujeres))%>% mutate(genero='Femenino') 
view(reins_mujeres_anio_nivelR)

reins_hombres_anio_nivelR <-programadoras %>% group_by(anio, nivel)%>% summarise(reins_nivel= sum(reinsVarones))%>% mutate(genero='Masculino') 
view(reins_hombres_anio_nivelR)
# por filas
reinscriptos_nivelR<-rbind(reins_mujeres_anio_nivelR, reins_hombres_anio_nivelR)
view(reinscriptos_nivelR)

ggplot(data=reinscriptos_nivelR, aes(x=anio, y=reins_nivel, colour=nivel))+
  geom_line() +
  facet_wrap(~genero)


----------------------------------------------
  # EGRESADOS
  #----------------------------------------------

----------------------------------------------
  # cantidad total de egresados hombres y mujeres
  #-----------------------------------------------
mujeres_egresadas<- programadoras%>%group_by(anio)%>% summarise(egresades= sum(egMujeres), total=sum(egMujeres, egVarones), prom=(round(mean(egMujeres),0)))%>% mutate(genero='Mujeres') 
View(mujeres_egresadas)
hombres_egresados<- programadoras%>%group_by(anio)%>% summarise(egresades= sum(egVarones), total=sum(egMujeres, egVarones), prom=(round(mean(egVarones),0)))%>% mutate(genero='Hombres')
View(hombres_egresados)


egresades_anio<-rbind(mujeres_egresadas, hombres_egresados)
view(egresades_anio)

# gráfico barras contiguas paradas
library(hrbrthemes)
hrbrthemes::import_titillium_web()
# estudiantes_R_bar<-rbind(mujeres_anio, hombres_anio)%>%arrange(desc(total))
# view(estudiantes_R_bar)

ggplot(data=egresades_anio, aes(x=anio, y=egresades, fill=genero))+
  geom_bar(stat= "identity", position = "dodge", width= .8, color= "black")+
  #coord_flip()+
  scale_fill_manual(values= c("#d8d860","#a32ea2"), labels= c('Hombres', 'Mujeres'))+  #amarillo claro:#f1fa8c  verde:#41b6a6 lilaoscuro:#713580 
  #scale_x_discrete(breaks= c(2010, 2011, 2012,2013,2014,2015), labels= c('2010', '2011', '2012','2013', '2014','2015'))+
  #geom_text(aes(label=total), size=2)+
  labs(title = 'Egresados de Carreras Exactas de Universidades Argentinas\n', x='',y='',fill= ' ',
       subtitle ="Para el período 2010-2015" , 
       caption = "DataSource: Chicas en tecnología") +
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
#theme_ipsum_tw()
#theme_ft_rc()
# para theme negro:theme_ft_rc() 
ggsave(here("barras_egresades.png"), height = 8, width = 10, units = "in", type='cairo')


ggplot(data=egresades_anio, aes(x=anio, y=egresades, color=genero))+
  geom_line(size=2) +
  scale_colour_manual(values= c("#d8d860","#a32ea2"), labels= c('Mujeres', 'Hombres')) +
  facet_wrap(~genero) +
  labs(title = 'Egresados  de Carreras Exactas de Universidades Argentinas\n', x='',y='', color= ' ',
       subtitle ="Para el período 2010-2015" , 
       caption = "DataSource: Chicas en tecnología") +
  theme_ipsum_tw()+
  theme(text = element_text(size=14, face = 'bold', color = "#2c204d"),
        plot.title = element_text(size=18,                     #cambiamos el tamaño, fuente y color del título
                                  family ="Garamond",    
                                  hjust = 0,vjust = 1,
                                  colour = "#2c204d", 
                                  face = 'bold', 
                                  margin = margin(b = 12 * 1.2)),
        legend.position="  ", legend.text= element_text(color="#2c204d", 
                                                        size= 12, hjust = 0.5,vjust = 1, family ="Garamond"))


#theme_ipsum_tw()#
ggsave(here("grafico_egresados_lineas.png"), height = 8, width = 10, units = "in", type='cairo')
