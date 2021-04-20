# --------------------------------------------------------------------------------------- #
# ANALISIS EXLORATORIO DE DATOS: TITANIC
# --------------------------------------------------------------------------------------- #
# Definicion de columnas del dataset (fuente: https://www.kaggle.com/hesh97/titanicdataset-traincsv)
# --------------------------------------------------------------------------------------- #
# 1. PassengerId (numeric)
# 2. Survived (numeric, 0 = No Survived, 1 = Survived)
# 3. Pclass (numeric: 1 - Baja, 2 - Media, 3 - Alta)
# 4. Name (text)
# 5. Sex (text: Male, Female)
# 6. Age (numeric)
# 7. Sibsp (numeric: 0 = 0 Hermanos / Conyuges a bordo, 1, 2 ,3, 4, 5, 6 ,7 ,8)
# 8. Parch (numeric: 0 = 0 Padres/NiÃ±os  a Bordo, 1, 2 , 3 ,4 ,5 6)
# 9. Ticket (text)
# 10. Fare (Numeric)
# 11. Cabin (text)
# 12. Embarked (text)

# --------------------------------------------------------------------------------------- #
# Librerias
# --------------------------------------------------------------------------------------- #

library(tidyverse)
library(miscset)
library(ggplot2)
library(corrplot) 
library(GGally)
library(readr)  
library(dplyr)  
library(crayon) 
library(modeest)


# --------------------------------------------------------------------------------------- #
# Direccion
# --------------------------------------------------------------------------------------- #

dir()
setwd("C:/Users/fgamarra/Desktop/Maestria/Explotacion de datos para negocios/TP2")

# --------------------------------------------------------------------------------------- #
# Importacion del Data Set
# --------------------------------------------------------------------------------------- #

Titanic = read.csv("train.csv", stringsAsFactors = F, na.strings = "") 
# se convierten en NA los no reportados


# --------------------------------------------------------------------------------------- #
# Dimension del Data Set
# --------------------------------------------------------------------------------------- #

dim_data <- dim(Titanic)

cat("El dataset tiene una dimension de", bold(dim_data[1]),"filas y",bold(dim_data[2]),"columnas.\n")

# --------------------------------------------------------------------------------------- #
# Descripcion de cada una de las variables del set de datos
# --------------------------------------------------------------------------------------- #

Cont_var<-0 #Contador de variables cuantitativas
disc_var<-0 #Contador de variables cualitativas
mycatlist<- c() #Lista que almcena las variables categoricas
mycualist<- c() #Lista que almcena las variables cualitativas

for (i in colnames(Titanic))
{
  cat("la columna",bold(i),"es del tipo",bold(typeof(Titanic[[i]])), "\n")
  if (is.character(Titanic[[i]])){
    disc_var=disc_var+1
    mycatlist <- c(mycatlist, i)
    
  } else {Cont_var=Cont_var+1
          mycualist <- c(mycualist, i)
          }
  
}

cat("Hay",bold(Cont_var),"Variables", bold("cuantitativas"), "y",bold(disc_var),"variables", bold("discretas"), "en el set de datos")


cat("Las variables categoricas a analizar son:",length(mycatlist),"el detalle:",mycatlist)
cat("Las variables cualitativas a analizar son:",length(mycualist),"el detalle:", mycualist)

#Observacion: Aca nos percatamos que hay features catalogadas como variables cualitativas cuando en realidad son categoricas,
#Como el caso de Pclass o Survived, donde si bien sus datos son numericos, en si son valores que se asocian a una etiqueta en particular.
#Para ello, mas adelante se procedera a realizar un analisis y correccion de dichas variables para sincerar el tipo de dato
#al caul corresponde cada uno.

# --------------------------------------------------------------------------------------- #
# Total y porcentaje de valores nulos de cada variable del dataset
# --------------------------------------------------------------------------------------- #

col_names<-names(Titanic)
for (i in col_names)
{
  na_sum = sum(is.na(Titanic[i]))
  na = na_sum
  na_percent = na_sum
  if (na_sum > 0) {
    na = red(na_sum)
    na_percent = red(100 * na_sum / length(Titanic[[i]]) )
  }
  cat("La variable",bold(i),"tiene",bold(na),"registros nulos, el",bold(na_percent),"% del total de sus registros.\n")
}


# --------------------------------------------------------------------------------------- #
# Visualizamos el dataset y su estructura 
# --------------------------------------------------------------------------------------- #


glimpse(Titanic) # Da un vistazo rapido del dataset

view(Titanic)#Visualizacion completa del Dataset

summary(Titanic) #Realiza un analisis estadistico por cada variable del dataset

# --------------------------------------------------------------------------------------- #
# Cambio de nombre de las variables
# --------------------------------------------------------------------------------------- #

names(Titanic)

nombres=c("Id_Pasajero", "Sobrevivio", "Clase" , "Nombre", "Sexo", "Edad", "Hermanos/Conyuges_a_Bordo" , 
          "Padres/NiÃ±os_a_Bordo" ,"Ticket", "Tarifa", "Cabina", "Embarque")
colnames(Titanic) <- nombres

# --------------------------------------------------------------------------------------- #
# Columnas renombradas
# --------------------------------------------------------------------------------------- #
# 1. Id_Pasajero
# 2. Sobrevivio
# 3. Clase_Ticket
# 4. Nombre
# 5. Sexo
# 6. Edad
# 7. Hermanos/Conyuges a Bordo
# 8. Padres/NiÃ±os  a Bordo
# 9. Ticket
# 10. Tarifa
# 11. Cabina
# 12. Embarque

Titanic$Sobrevivio = factor(Titanic$Sobrevivio,levels = (0:1), labels = c("No","Si"))
table(Titanic$Sobrevivio, useNA='always')

Titanic$Clase = factor(Titanic$Clase, levels = (1:3), labels = c("Alta", "Media", "Baja"))
table(Titanic$Clase,useNA='always')

Titanic$Embarque = factor(Titanic$Embarque, levels = c("C","Q","S"), labels = c("Cherbourg", "Queenstown", "Southampton"))
table(Titanic$Embarque,useNA='always')


table(Titanic$Sexo,useNA='always')#No se convierte a factores en esta instancia
# --------------------------------------------------------------------------------------- #
# Descarte de Features
# --------------------------------------------------------------------------------------- #


#Se elimina la columna cabina por poseer casi un 80% de valores nulos
#Se elimina las columnas ID_Pasajero, nombre y ticket por no dar informacion relevante para determinar alguna
#caracteristica o estructura que pueda dar informacion sobre las mismas

Titanic = subset(Titanic, select = -c(Id_Pasajero,Nombre,Cabina,Ticket) )

# --------------------------------------------------------------------------------------- #
# Imputacion de valores faltantes en variables Edad y Embarque
# --------------------------------------------------------------------------------------- #

sd(Titanic$Edad,na.rm = TRUE)
var(Titanic$Edad,na.rm = TRUE)

#Dado que la feature Embarque es una variable categorica el cual solo tiene una proporcion de datos faltantes
#de 0.22%, se podria imputar dicho valor por la moda, el cual se determino en su tabla de contingencia 
#que es el valor de Southampton. Por otro Lado para el caso de la Variable Edad, esta cuenta con
#aproximadamente 20% de sus datos faltantes, siendo esto un cantidad representativa dentro del conjunto de
#de datos, por lo que en una primera instancia en terminos de analisis exploratorio se le inputara, a 
#a posteriori se debe, dado que EDAD posee una varianza y Desviacion alta, se debe verificar que dicho 
#Metodo de imputacion no genere una alteracion significativa su distristribucion y en la capcidad
#predictiva de algun modelo para alguna prediccion o calsificadio de datos

Titanic$Embarque[is.na(Titanic$Embarque)] = "Southampton"

Titanic$Edad[is.na(Titanic$Edad)] = median(Titanic$Edad, na.rm = T)

#Se verifica a continuaCION en DF limpio

col_names<-names(Titanic)
for (i in col_names)
{
  na_sum = sum(is.na(Titanic[i]))
  na = na_sum
  na_percent = na_sum
  if (na_sum > 0) {
    na = red(na_sum)
    na_percent = red(100 * na_sum / length(Titanic[[i]]) )
  }
  cat("La variable",bold(i),"tiene",bold(na),"registros nulos, el",bold(na_percent),"% del total de sus registros.\n")
}

#Se evidencia que ahora no contamos con valores nulos en ninguna columna

# --------------------------------------------------------------------------------------- #
# Analisis de variables luego de la limpieza del set de datos
# --------------------------------------------------------------------------------------- #

mycatlist<- c() #Lista que almcena las variables categoricas
mycualist<- c() #Lista que almcena las variables cualitativas


for (i in colnames(Titanic))
{
  if (is.character(Titanic[[i]]) || is.factor(Titanic[[i]])){mycatlist <- c(mycatlist, i)}
  else {mycualist <- c(mycualist, i)}
  
}

# --------------------------------------------------------------------------------------- #
# VARIABLES CUANTITATIVAS
# --------------------------------------------------------------------------------------- #
cat("Las variables cualitativas a analizar son:",length(mycualist),"el detalle:", mycualist)


for (w in mycualist){ 
  cat("La estadistica de la feature",bold(w),"es:\n",
                          "Minimo:", bold(min(Titanic[[w]])),"\n",
                          "Mediana:", bold(median(Titanic[[w]])),"\n",
                          "Maximo:", bold(max(Titanic[[w]])),"\n",
                          "Promedio:", bold(mean(Titanic[[w]])),"\n",
                          "Varianza:", bold(var(Titanic[[w]])),"\n",
                          "Desv. Std:", bold(sd(Titanic[[w]])),"\n",
                          "Rango Int.:", bold(IQR(Titanic[[w]])),"\n",
                          "Coef. Var(%):", bold(100*sd(Titanic[[w]])/mean(Titanic[[w]])),"\n"
      
      )
  
  
}

#Deteccion de Outliers

O_edad<-boxplot(Titanic$Edad)$out
O_PNABz<-boxplot(Titanic$`Padres/NiÃ±os_a_Bordo`)$out
o_hcb<-boxplot(Titanic$`Hermanos/Conyuges_a_Bordo`)$out
O_tarifa<-boxplot(Titanic$Tarifa)$out

cat("los Outliers de la variable Edad son:",length(O_edad),"detalle",sort(O_edad))
cat("los Outliers de la variable Hermanos y conyugues a bordo son:",length(o_hcb),"detalle",sort(o_hcb))
cat("los Outliers de la variable Padres/ Nietos a bordo son:",length(O_PNABz),"detalle",sort(O_PNABz))
cat("los Outliers de la variable tarifa son:",length(O_tarifa),"detalle",sort(O_tarifa))

cor_data <- Titanic %>%
  select(mycualist) %>%
  cor(method='pearson')%>%
  corrplot( method="number", type="upper")

#1-La variable Edad es la feature menos dispersa en realcion al resto, donde las demas presentan un coef. de variacion superior al 100%
#2-Las personas dentro del titanic eran personas jovenes con media de edad de aproximadamente 30 años, sin acompañantes.
#Existe poca correlacion entre las variables


# --------------------------------------------------------------------------------------- #
# VARIABLES CUAntitativas
# --------------------------------------------------------------------------------------- #

cat("Las variables categoricas a analizar son:",length(mycatlist),"el detalle:",mycatlist)

round(100*prop.table(table(Titanic$Clase,useNA='always')),2)
#La mayoria de las personas era de clase Baja.

round(100*prop.table( table(Titanic$Embarque,useNA='always')),2)
#La mayoria embarcaron en el  puerto de Southampton

round(100*prop.table(table(Titanic$Sexo,useNA='always')),2)
#La mayoria de las personas eran mujeres

round(100*prop.table( table(Titanic$Sobrevivio,useNA='always')),2)
#Solo Sobrevivio un 38% aproximadamente de lo que embarcaron


#########################################################################################
# ......................................................................................#
# Analisis de asociacion entre variables
# ......................................................................................#
#########################################################################################

# Distretizacion de la variable Edad


Titanic = Titanic %>% 
  mutate(Edades = case_when(between(Edad, 0, 18) ~ "0 a 18 Años",
                            between(Edad, 19, 45) ~ "19 a 45 Años",
                            between(Edad, 45, 81) ~ "46 y mas Años",
                            TRUE ~ "V"))

# Sobrevivientes por Clase de Voleto 

Titanic %>% 
  select(Clase, Sobrevivio, Embarque, Edades) %>% 
  ggplot() +
  geom_bar(aes(x=Clase, fill=Sobrevivio), position = "dodge") +
  theme_bw() +ggtitle ("La mayoria de los  que murieron eran de clase baja")


# Sobrevivientes por Clase de boleto separado por rango de edad

Titanic %>% 
  select(Clase, Sobrevivio, Embarque, Edades) %>% 
  ggplot() +
  geom_bar(aes(x=Clase, fill=Sobrevivio), position = "dodge") +
  theme_bw() +
  facet_wrap(.~Edades)+ggtitle ("La mayoria de las personas eran de clase baja con rango de edades entre los 19 y 45 años")

# Relacion entre la edad y la tarifa: No se encuentra relacion alguna


plot(x = Titanic$Edad, y = Titanic$Tarifa, xlab = "Edad", ylab = "Tarifa")
mtext("No existe correlacion entre la tarifa y la edad de los tripulantes")

# Sobrevivientes por Clase de boleto separado por sexo

Titanic %>% 
  select(Clase, Sobrevivio, Embarque, Edades, Sexo) %>% 
  ggplot() +
  geom_bar(aes(x=Clase, fill=Sobrevivio), position = "dodge") +
  theme_bw() +
  facet_wrap(.~Sexo)+ggtitle ("La mayoria de las personas que sobrevivieron fueron mujeres de clase alta")

Titanic %>% 
  select(Sobrevivio, Embarque, Edades) %>% 
  ggplot() +
  geom_bar(aes(x=Edades, fill=Sobrevivio), position = "dodge") +
  theme_bw() +
  facet_wrap(.~Embarque)+ggtitle ("La mayoria de las personas persona que sobrevivieron son jovenes con media de 30 años")

Titanic %>% 
  select(Clase, Embarque,Sobrevivio) %>% 
  ggplot() +
  geom_bar(aes(x=Clase), position = "dodge") +
  theme_bw() +
  facet_wrap(.~Embarque)+
  ggtitle ("Clasificando por el lugar de embarque y sobreviviente, Southampton cuenta con la mayor proporcion de personas que sobrevivio
y tambien es el lugar con mayor cantidad de personas Pobres y Ricas, siendo este el lugar donde Embarco la gran mayoria de la tripulacion.")
 
Titanic %>% 
  select(Tarifa,Clase, Sobrevivio, Edades) %>% 
  filter(Tarifa>=66)%>% #Se determino que a partir de este valor inician los outlier de tarifa
  ggplot() +
  geom_bar(aes(x=Clase, fill=Sobrevivio), position = "dodge") +
  theme_bw() +
  facet_wrap(.~Edades)+ggtitle ("La gran Mayoria de los que tienen valores outliers en tarifa alta sobrevivieron")

# Validando la Relacion entre variables categoricas

# Relacion entre las variables cuantitativas

summary(table(Titanic$Sexo, Titanic$Sobrevivio))
#Dado que el P-valor es menor que 0.05 se acepta la H0 por ende hay una realcion entre el sexo y si sobrevivio o no

summary(table(Titanic$Clase, Titanic$Sobrevivio))
#Dado que el P-valor es menor que 0.05 se acepta la H0 por ende hay una realcion entre la clase social y si sobrevivio o no

summary(table(Titanic$Clase, Titanic$Sobrevivio,Titanic$Sexo))
#Dado que el P-valor es menor que 0.05 se acepta la H0 por ende hay una realcion entre la clase social y si sobrevivio o no y el sexo del tripulante

summary(table(Titanic$Clase, Titanic$Embarque))
#Dado que el P-valor es menor que 0.05 se acepta la H0 por ende hay una realcion entre la clase social y el lugar de Embarque.

#########################################################################################
# ......................................................................................#
# CONCLUCIONES GENERALES
# ......................................................................................#
#########################################################################################

#1-Para una persona de clase alta, mujer y joven su probabilidad de sobrevivir era mucho mas alta que la del resto
#2-Hay mas chance de sobrevivir si eras de clase alta y estabas alrrededor de la de promedio de 30 años
#Existe una fuerte correlacion entre todas las variables categoricas.
#3-Solo Sobrevivio un 38% aproximadamente de lo que embarcaron
#4-La mayoria parte de la tripulacion eran mujeres
