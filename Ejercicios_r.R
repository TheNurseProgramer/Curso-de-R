library(readxl)
library (dplyr)
library(ggplot2)
### Proyecto/Tarea Final

#LOS SIGUIENTES PUNTOS SE CONSIDERARAN PARA LA EVALUACION:
#- Buenas practicas
#- Codigo limpio y legible
#- Esfuerzo
#- Resultados
#- Comentarios y observaciones del analisis que vayan haciendo

#- HINT GENERAL: Si no lo sabes hacer, googlealo o revisa los scripts de las clases
#- Mas informacion sobre el dataset utilizado: https://datos.gob.mx/busca/dataset/informacion-referente-a-casos-covid-19-en-mexico

#1) Descarga el csv de los datos del COVID http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip,
# importa los datos en R
# RECOMENDACION: usar read_csv

covid_dataset <- read_excel("covid_dataset.xlsx")
View(covid_dataset)


#2) Extrae una muestra aleatoria de 100k registros y asignala en una nueva variable. A partir de ahora trabaja con este dataset
# HINT: usar funcion sample_n de dplyr

muestra_covid <- slice_sample(covid_dataset,n = 33000)

#3)Haz un resumen estadistico del dataset y tambien muestra los tipos de datos por columna

summary.data.frame(muestra_covid)

#4)Filtra los renglones que dieron positivo para SARS-COVID y calcula el numero de registros
## Los casos positivos son aquellos que en la columna CLASIFICACION_FINAL tienen 1, 2 o 3

View(subset(muestra_covid,CLASIFICACION_FIL == 1 |CLASIFICACION_FIL==2|CLASIFICACION_FIL==3))


#5)Cuenta el numero de registros nulos por columna (HINT: Usar sapply o m
apply(is.na(muestra_covid),2,sum)


#6)
##a)Calcular la media de edades de los contagiados de covid
x =subset(muestra_covid,CLASIFICACION_FIL == 1 |CLASIFICACION_FIL==2|CLASIFICACION_FIL==3)
mean(x$EDAD)

##b)Realiza un Histograma de las edades de los contagiados 
hist(x$EDAD,main = "Histograma de edades de contagiados",ylab = "",xlab = "Edades" )

##c)Realiza una grafica de densidad de edades de los contagiados
qplot(x=EDAD,data = x,geom = "density")


#7)Agregar una columna nueva al dataframe que tenga valor 1 cuando la fecha de defuncion no es valor nulo y 0 cuando es nulo 
## La columna que contiene la fecha de defuncion se llama FECHA_DEF 
## HINT: Usa mutate, ifelse e is.na
x<-mutate(muestra_covid,FECHA_DEFUNCION = ifelse(is.na(FECHA_DEF),0,1))
View(x)

#8)Hacer un boxplot de edades de los muertos por covid vs lo que no murieron para ver si detectamos diferencias y escribe tus conclusiones
x<-mutate(muestra_covid,FECHA_DEFUNCION = ifelse(is.na(FECHA_DEF),"Finado","Vivo"))
qplot(x = FECHA_DEFUNCION, y=EDAD,data = x,geom = "boxplot")
#los finados eran mas jovenes que los vivos

#9)Transforma la columna CLASIFICACION_FINAL, que tenga valor de 1 si tiene 1, 2 o 3 como valor y que tenga 0 en cualquier otro caso
## HINT: Usar transform o mutate

y<-mutate(muestra_covid,POSITIVOS = ifelse(CLASIFICACION_FIL == 1 |CLASIFICACION_FIL==2|CLASIFICACION_FIL==3,1,0))

#10)Cuenta el numero de casos positivos agrupado por estado y realiza una grafica de barras de los 10 estados con mas casos
## HINT: Usar groupby, summarize, n(), y ggplot2

paso_diez<-group_by(muestra_covid,ENTIDAD_UM)
hist(paso_diez$ENTIDAD_UM)
#11)Renombra la columna llamada CLASIFICACION FINAL para que ahora su nombre sea: "CONTAGIADO"
colnames(muestra_covid)[36]<-"GONTAGIADO"
#12)Realiza una funcion que al aplicarla nos diga el procentaje del total de registros que estan contagiados por Covid
#Ejemplo: al correr la funcion porcentaje_contagios(mi_dataframe) el resultado sea: 20.5%

porcentaje_contagiados <- function(mi_dataframe) {
  x<-0
  y<-sum (mi_dataframe)
  for (i in mi_dataframe) {
    if (i==1|i==2|i==3){
      x<-x+1
    }
  }
  z<-(x*100)/y
  print (z)
}
porcentaje_contagiados(muestra_covid$GONTAGIADO)
  print(sum(muestra_covid$GONTAGIADO))
#13)Realiza una matriz de corrrelacion entre las variables numericas y concluye
## HINT: https://stackoverflow.com/questions/5863097/selecting-only-numeric-columns-from-a-data-frame

#14)Realiza algun analisis, conteo por grupo y/o grafica que te parezca relevante para complementar el estudio y concluye


