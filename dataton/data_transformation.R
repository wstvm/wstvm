### Data visualization version 2 ###

#creando nueva variable edad para hacer cambios
pagadores$edad_v1<-pagadores$edad

#cambiando edad negativa dado analisis de hipotesis
pagadores$edad_v1[pagadores$edad_v1==-975]<-25

#cambiando edades mayores o igualesa 108 como "null" dado analisis de hipotesis
pagadores$edad_v1[pagadores$edad_v1>108]<-NA

#creando una columna de flag para registros que presentan inconsistencias para mas adelante
#analizar sus transacciones y mirar si son registros buenos o presentan errores
pagadores$flag<-NA

# personas de edad 108 (acumulacion atipica)
pagadores$flag[pagadores$edad_v1==108]<-"si"

#menores a 14 sin ocupaci?n 2 (Estudiante) O (Otra)  y S (Desempleado sin ingresos)
pagadores$flag[pagadores$edad_v1<=14 & pagadores$ocupacion != "2" & pagadores$ocupacion != "O" &  
                 pagadores$ocupacion != "S"] <- "si"

#menores a 14 sin nivel acad?mico Missing, I (No informa), P (Primaria), N (Ninguno) y B (Bachillerato)
pagadores$flag[pagadores$edad_v1<=14 & !is.na(pagadores$nivel_academico)
               & pagadores$nivel_academico != "I" & pagadores$nivel_academico != "P"
               & pagadores$nivel_academico != "N" & pagadores$nivel_academico != "B"] <- "si"


#menores a 14 sin estado Civil S (Soltero), I (No informa), Missing, O (Otro)
pagadores$flag[pagadores$edad_v1<=14 & !is.na(pagadores$estado_civil)
               & pagadores$estado_civil != "S" & pagadores$estado_civil != "I"
               & pagadores$estado_civil != "O"] <- "si"
                                         
pagadores$flag[pagadores$edad_v1<14 & pagadores$estado_civil == "D"] <- "si"

pagadores$flag[is.na(pagadores$flag)]<-"no"

### version 3 ####
#Convirtiendo los NA en categoria "no informa" o "no disponible " o si no hubiese en 
#una nueva categoria dentro de cada feature


#Para ocupacion crearemos una nueva variable llamada 'M' missing
pagadores$ocupacion[is.na(pagadores$ocupacion)]<-"M"

#Para cada feature agregaremos a la variable llamada 'I' NO INFORMA
pagadores$tipo_vivienda[is.na(pagadores$tipo_vivienda)]<-"I"
pagadores$nivel_academico[is.na(pagadores$nivel_academico)]<-"I"
pagadores$estado_civil[is.na(pagadores$estado_civil)]<-"I"
pagadores$genero[is.na(pagadores$genero)]<-"I"



### version 4 ####
#Agregando dos nuevas columnas ingreso_min e ingreso_max
pagadores$ingreso_min<-NA
pagadores$ingreso_max<-NA

pagadores$ingreso_min <-(ifelse(pagadores$ingreso_rango=="0",0,
                         ifelse(pagadores$ingreso_rango=="a. (0  1.1MM]",0.01,
                         ifelse(pagadores$ingreso_rango=="b. (1.1  2.2MM]",1100001.01, 
                         ifelse(pagadores$ingreso_rango=="c. (2.2  3.3MM]",2200001.01,
                         ifelse(pagadores$ingreso_rango=="d. (3.3  4.4MM]",3300001.01,
                         ifelse(pagadores$ingreso_rango=="e. (4.4  5.5MM]",4400001.01,
                         ifelse(pagadores$ingreso_rango=="f. (5.5  6.6MM]",5500001.01,        
                         ifelse(pagadores$ingreso_rango=="g. (6.6  7.6MM]",6600001.01,
                         ifelse(pagadores$ingreso_rango=="h. (7.6  8.7MM]",7600001.01,
                         ifelse(pagadores$ingreso_rango=="i. (8.7  Inf)",8700001.01,NA)))))))))))

pagadores$ingreso_max <-(ifelse(pagadores$ingreso_rango=="0",0,
                         ifelse(pagadores$ingreso_rango=="a. (0  1.1MM]",1100000,
                         ifelse(pagadores$ingreso_rango=="b. (1.1  2.2MM]",2200000, 
                         ifelse(pagadores$ingreso_rango=="c. (2.2  3.3MM]",3300000,
                         ifelse(pagadores$ingreso_rango=="d. (3.3  4.4MM]",4400000,
                         ifelse(pagadores$ingreso_rango=="e. (4.4  5.5MM]",5500000,
                         ifelse(pagadores$ingreso_rango=="f. (5.5  6.6MM]",6600000,
                         ifelse(pagadores$ingreso_rango=="g. (6.6  7.6MM]",7600000,
                         ifelse(pagadores$ingreso_rango=="h. (7.6  8.7MM]",8700000,
                         ifelse(pagadores$ingreso_rango=="i. (8.7  Inf)",10e100,NA)))))))))))





