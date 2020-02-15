library(foreign)#importar base datos
library(epiDisplay)#analisis
library(readxl) #importar bases en excel
library(dplyr)#datamaing
library(tidyr)#datamaing
getwd()#ver el directorio de trabajo de R
search()#ver q librerias estan en la memoria

#Cargando base de datos
data=base_datos_UCI
head(data)
tail(data)
### Corrigiendo errores en la data
tab1(data$age)
data$age[data$age==-10] <-10
tab1(data$age)
data$age[data$age==-1]<-1
tab1(data$age)
# Evaluando control de calidad de los datos
summ(data)

# Media, mediana, DS, Min, Max y quintiles de edad
summary(data$age)
sd(data$age)

# Gráfico de cajas para Plaquetas
boxplot(data$platelel,main = "Plaquetas")

# Gráfico de cajas de plaquetas por sexo
attach(data)
boxplot(platelel ~sex, col = 'blue', main="Plaquetas según sexo")

### CONSTRUCCIÓN DE SOFA 
#Escala de coma de Glasgow
data$neuro<- ifelse(data$gcs>14,0,
                    ifelse(data$gcs>=13,1,
                           ifelse(data$gcs>=10,2,
                                  ifelse(data$gcs>=6,3,4))))
tab1(data$neuro)
#Bilirrubina
data$liver<-ifelse(data$bilirubin<20,0,
                   ifelse(data$bilirubin<=32,1,
                          ifelse(data$bilirubin<=101,2,
                                 ifelse(data$bilirubin<=204,3,4))))
summ(data$bilirubin)
tab1(data$liver)
View(data)

#Edad
summ(data$age)
data$AgeGroup<-cut(data$age,breaks = c(0,20,40,60,80,90),
                   labels = c("1 a 20 años","21 a 40 años",
                              "40 a 60 años","60 a 80 años",
                              "80 a 90 años"))
tab1(data$AgeGroup)
View(data)

#Cardio
data$map2<- ifelse(data$map<70,1,0)
data$dopamina<- ifelse(data$dop<5,2,
                       ifelse(data$dop>15,4,3))
data$epi2<- ifelse(data$epi<0.1,3,4)
View(data)

data$cardio<-pmax(data$map2,data$dopamina,data$epi2)
tab1(data$cardio)
View(data)

#Coagulation
data$coagulacion<-ifelse(data$platelel<20,4,
                         ifelse(data$platelel<50,3,
                                ifelse(data$platelel<100,2,
                                       ifelse(data$platelel<150,1,0))))
tab1(data$coagulacion)

#Renal
data$renal<-ifelse(data$cr<110,0,
                   ifelse(data$cr<170,1,
                          ifelse(data$cr<229,2,
                                 ifelse(data$cr<440,3,4))))
tab1(data$renal)

# Respiratory
rel_Pao2_Fio2<-data$pao2/data$fio2
rel_Pao2_Fio2
data$respiratory<-ifelse(rel_Pao2_Fio2<100,4,
                         ifelse(rel_Pao2_Fio2<200,3,
                                ifelse(rel_Pao2_Fio2<300,2,
                                       ifelse(rel_Pao2_Fio2<400,1,0))))
tab1(data$respiratory)
View(data)

#SUMATORIA DE PUNTAJES
data$SOFAsuma<-data$neuro+data$liver+
                data$cardio+data$coagulacion+
                data$renal+data$respiratory
View(data)

### CÁLCULO DE MORTALIDAD= SOFA
data$CalcMortalidad<-ifelse(data$SOFAsuma<6,10,
                            ifelse(data$SOFAsuma<9,20,
                                   ifelse(data$SOFAsuma<12,50,
                                          ifelse(data$SOFAsuma<14,60,
                                                 ifelse(data$SOFAsuma<15,80,
                                                        ifelse(data$SOFAsuma<24,90))))))
View(data)
