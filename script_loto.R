## Leemos datos
#setwd("D:/R ladies/MujeresPrograman
library("here")
library("tidyverse")
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
# ESTUDIANTES
#--------------------------------
## cantidad de estudiantes hombres y mujeres por anio y por unidad academica

 cant_mujeres_anio_inst <- programadoras%>%group_by(institucion, anio)%>% summarise(totalMujeresXInstituto= sum(estMujeres)) 
 View(cant_mujeres_anio_inst)
 
cant_varones_anio_inst <- programadoras%>%group_by(institucion, anio)%>% summarise(totalHombresXinstituto= sum(estVarones)) 
 View(cant_varones_anio_inst)


# cantidad total de estudiantes 
 mujeres_anio<- programadoras%>%group_by(anio)%>% summarise(total= sum(estMujeres))%>% mutate(genero='Mujeres') 
 View(mujeres_anio)
 hombres_anio<- programadoras%>%group_by(anio)%>% summarise(total= sum(estVarones))%>% mutate(genero='Hombres')
 View(hombres_anio)
 
 estudiantes_R_bar<-rbind(mujeres_anio, hombres_anio)%>%arrange(desc(total))
 view(estudiantes_R_bar)
 
 library(hrbrthemes)
 hrbrthemes::import_titillium_web()
 
 ggplot(data=estudiantes_R_bar, aes(x=anio, y=total,fill=genero))+
   geom_bar(stat= "identity", position = "dodge", width= .7, color= "black")+
   #geom_col( position = position_dodge())+
   coord_flip()+
   scale_fill_manual(values= c("#713580", "#41b6a6"), labels= c('Mujeres', 'Hombres'))+
   scale_x_discrete(breaks= c(2010, 2011, 2012,2013,2014,2015), labels= c("2010", "2011", "2012","2013", "2014","2015"))+
    #geom_text(aes(label=total), size=2)+
   labs(title = 'Estudiantes de carreras de exactas en universidades \n de toda la Argentina', x='',y='', color= ' ',
        subtitle ="Para el período 2010-2015" , 
        caption = "DataSource: Chicas en tecnología") +
   theme(text = element_text(size=14, face = 'bold', color = "#2c204d"),
         plot.title = element_text(size=18,                     #cambiamos el tamaño, fuente y color del título
                                   family ="Garamond",    
                                   hjust = 0.5,vjust = 1,
                                   colour = "#2c204d", 
                                   face = 'bold', 
                                   margin = margin(b = 12 * 1.2)),
         legend.position="top",legend.text= element_text(color="#2c204d", 
                                                         size= 12, hjust = 0.5,vjust = 1, family ="Garamond"))+
   theme_ipsum_tw()
 ggsave(here("grafico_barras.png"), height = 8, width = 10, units = "in", type='cairo')
 
 
 
 estudiantes_R<-rbind(mujeres_anio, hombres_anio)%>%arrange(desc(anio,total))
 view(estudiantes_R)
 
 ggplot(data=estudiantes_R, aes(x=anio, y=total, color=genero))+
   geom_line(size=2) +
   scale_colour_manual(values= c("#713580", "#41b6a6"), labels= c('Mujeres', 'Hombres'))+
   facet_wrap(~genero) +
   labs(title = 'Estudiantes de carreras de exactas en universidades \n de toda la Argentina', x='',y='', color= ' ',
        subtitle ="Para el período 2010-2015" , 
        caption = "DataSource: Chicas en tecnología") +
   theme_ipsum_tw()#
 
 ggsave(here("grafico_lineas_estudiantes.png"), height = 8, width = 10, units = "in", type='cairo')
 
 
 
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
 
 ggplot(data=estudiantes_nivelR, aes(x=anio, y=est_nivel, colour=nivel))+
   geom_line() +
   facet_wrap(~genero) #, scales="free_y"
   
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
################################################################

## GENERO

mujeres=tapply(programadoras$estMujeres,programadoras$anio,sum)
mujeres
varones=tapply(programadoras$estVarones,programadoras$anio,sum)
varones
plot(2010:2015,varones,ylab = "cantidad de estudiantes",xlab="año",ylim=c(11000,38000),col="green3")
points(2010:2015,mujeres,col="purple3")
legend(2014, 20000, c("mujeres", "varones"), col = c("purple3","green3"), pch = c(19,19), bg = "gray90")

## opiniones?
#################################################################
##TOTALES
tabla=matrix(c(mujeres,varones),2,6,byrow=TRUE)
rownames(tabla)=c("mujeres","varones")
colnames(tabla)=2010:2015
tabla


dev.off()
#funciona
barplot(tabla)
barplot(tabla, main="", xlab="año", col=c("purple3","green3"),
        legend = rownames(tabla)) 

 # plot(cars)
 # par(mfrow=c(2,2)
 # plot(cars)

ggplot(data=tabla, x=anio,aes(mujeres))+
  geom_col
### PROPORCIONES
prop.table(tabla,2)
tabla.porc=prop.table(tabla,2)*100
tabla.porc
barplot(tabla.porc[1,],main="Distribución de estudiantes mujeres",xlab="año", ylab="porentaje",col=c("purple3"))
##########################################################################################
##########################################################################################
## Y los nuevos inscriptos?

mujeres=tapply(programadoras$niMujeres,programadoras$anio,sum)
mujeres
varones=tapply(programadoras$niVarones,programadoras$anio,sum)
varones
###########################
##TOTALES
tabla=matrix(c(mujeres,varones),2,6,byrow=TRUE)
rownames(tabla)=c("mujeres","varones")
colnames(tabla)=2010:2015
tabla

barplot(tabla, main="", xlab="año", col=c("purple3","green3"),
        legend = rownames(tabla)) 

### PROPORCIONES
prop.table(tabla,2)
tabla.porc=prop.table(tabla,2)*100
tabla.porc
barplot(tabla.porc[1,],main="Distribución de nuevas inscriptas",xlab="año", ylab="porentaje",col=c("purple3"))

##################################################################################################
##################################################################################################
## Y las egresadas?

mujeres=tapply(programadoras$egMujeres,programadoras$anio,sum)
mujeres
varones=tapply(programadoras$egVarones,programadoras$anio,sum)
varones
###########################
##TOTALES
tabla=matrix(c(mujeres,varones),2,6,byrow=TRUE)
rownames(tabla)=c("mujeres","varones")
colnames(tabla)=2010:2015
tabla

barplot(tabla, main="", xlab="año", col=c("purple3","green3"),
        legend = rownames(tabla)) 

### PROPORCIONES
prop.table(tabla,2)
tabla.porc=prop.table(tabla,2)*100
tabla.porc
barplot(tabla.porc[1,],main="Distribución de egresadas",xlab="año", ylab="porcentaje",col=c("purple3"))

############################################################################
###########################################################################
# de las estudiantes mujeres, qué proporción de las ingresantes egresa?
### SIMPLIFICACION!! OJO,NO ESTA BIEN ESTRICTAMENTE!!!

mujeres.ni=tapply(programadoras$niMujeres,programadoras$anio,sum)
mujeres.ni

mujeres.egre=tapply(programadoras$egMujeres,programadoras$anio,sum)
mujeres.egre
prop.mujeres=mujeres.egre/mujeres.ni*100
prop.mujeres

## idem varones
varones.ni=tapply(programadoras$niVarones,programadoras$anio,sum)
varones.ni

varones.egre=tapply(programadoras$egVarones,programadoras$anio,sum)
varones.egre
prop.varones=varones.egre/varones.ni*100
prop.varones

##LO vemos en un gráfico

plot(2010:2015,prop.varones,ylab = "porcentaje de egresados",xlab="año",ylim=c(0,40),col="green3",pch=19)
points(2010:2015,prop.mujeres,col="purple3",pch=19)
legend(2010,10, c("mujeres", "varones"), col = c("purple3","green3"), pch = c(19,19))

plot(2010:2015,prop.varones,ylab = "porcentaje de egresados",xlab="año",ylim=c(0,40),col="green3",pch=19)
points(2010:2015,prop.mujeres,col="purple3",pch=19)
legend("bottomleft", c("mujeres", "varones"), col = c("purple3","green3"), pch = c(19,19),cex = 1.2, bty = "n", inset = c(0.1, 0.1))

#############################################################################
### CONCLUSION?