#Script R con el código de la práctica de 
#Maria Sonia Rodriguez Cepedano y Carlos E. Jimenez Gomez


library(knitr)
library(rockchalk)
library(nortest)
library(normtest)
library(moments)
library(car)
library(ggplot2)
library(dplyr)

#Cargamos los datos
surveyMentalHealth<-read.csv("survey.csv", sep=",",na.strings = "NA")
#Mostramos las primeras filas
head(surveyMentalHealth) 

#Eliminación de las variables
surveyMentalHealth$Timestamp<-NULL
surveyMentalHealth$state<-NULL
surveyMentalHealth$comments<-NULL

#Pedimos resumen y estructura
summary(surveyMentalHealth)
colnames(surveyMentalHealth)
str(surveyMentalHealth) 

#Número de fila del fichero. 
nrow(surveyMentalHealth)

sapply(surveyMentalHealth,class)

#En el momento de la lectura del fichero establecemos que si se encuenta un valor perdido los asigne por NA 
(na.strings = "NA")
table(is.na(surveyMentalHealth$Age))
table(is.na(surveyMentalHealth$Gender))
table(is.na(surveyMentalHealth$Country ))
table(is.na(surveyMentalHealth$self_employed))
table(is.na(surveyMentalHealth$family_history))
table(is.na(surveyMentalHealth$treatment))
table(is.na(surveyMentalHealth$work_interfere))
table(is.na(surveyMentalHealth$no_employees))
table(is.na(surveyMentalHealth$remote_work))
table(is.na(surveyMentalHealth$tech_company))
table(is.na(surveyMentalHealth$benefits))
table(is.na(surveyMentalHealth$care_options))
table(is.na(surveyMentalHealth$ wellness_program))
table(is.na(surveyMentalHealth$seek_help))
table(is.na(surveyMentalHealth$anonymity))
table(is.na(surveyMentalHealth$leave))
table(is.na(surveyMentalHealth$mental_health_consequence))
table(is.na(surveyMentalHealth$phys_health_consequence))
table(is.na(surveyMentalHealth$coworkers))
table(is.na(surveyMentalHealth$supervisor))
table(is.na(surveyMentalHealth$mental_health_interview))
table(is.na(surveyMentalHealth$phys_health_interview))
table(is.na(surveyMentalHealth$mental_vs_physical))
table(is.na(surveyMentalHealth$obs_consequence))
surveyMentalHealth$Age[which(surveyMentalHealth$Age==0) ]

barplot(table(surveyMentalHealth$Age))
datos_atipicos<-subset(surveyMentalHealth[1:1], surveyMentalHealth$Age<16 | surveyMentalHealth$Age>75)
datos_atipicos
surveyMentalHealth_clean<-subset(surveyMentalHealth, surveyMentalHealth$Age>16 & surveyMentalHealth$Age<75)
nrow(surveyMentalHealth_clean)
summary(surveyMentalHealth_clean$Age)
boxplot(surveyMentalHealth_clean$Age)

#Media Aritmetica 
Media_Age<-mean(surveyMentalHealth_clean$Age)

#Mediana
Mediana_Age<-median(surveyMentalHealth_clean$Age)

#Media Recortada
Media_Recortada_Age<-mean(surveyMentalHealth_clean$Age, trim=0.05)

#Desviación estándar
Desviacion_estandar_Age<-sd(surveyMentalHealth_clean$Age)

#Rango Intercuartilico (RIC)
RIC_Age<-IQR(surveyMentalHealth_clean$Age)

#Desviación Absoluta Respecto de la Mediana
Desviacion_Absoluta_Mediana_Age<-mad(surveyMentalHealth_clean$Age)

#Tabla 
kable(rbind(Media_Age,Mediana_Age,Media_Recortada_Age,
            Desviacion_estandar_Age,RIC_Age,
            Desviacion_Absoluta_Mediana_Age))

#Niveles de variable Gender
levels(surveyMentalHealth_clean$Gender)

#Asignación de niveles adecuada para la variable Gender
surveyMentalHealth_clean$Gender<-as.character(surveyMentalHealth_clean$Gender)
surveyMentalHealth_clean$Gender<-replace(surveyMentalHealth_clean$Gender,surveyMentalHealth_clean$Gender=="Cis Female"|surveyMentalHealth_clean$Gender=="cis-female/femme"|surveyMentalHealth_clean$Gender=="f"|surveyMentalHealth_clean$Gender=="femail"|surveyMentalHealth_clean$Gender=="Femake"|surveyMentalHealth_clean$Gender=="female"|surveyMentalHealth_clean$Gender=="Female"|surveyMentalHealth_clean$Gender=="Female "|surveyMentalHealth_clean$Gender=="Female (cis)"|surveyMentalHealth_clean$Gender=="Female (trans)"|surveyMentalHealth_clean$Gender=="Woman"|surveyMentalHealth_clean$Gender=="woman","F")

surveyMentalHealth_clean$Gender<-replace(surveyMentalHealth_clean$Gender,surveyMentalHealth_clean$Gender=="Male"|surveyMentalHealth_clean$Gender=="male"|surveyMentalHealth_clean$Gender=="Cis Male"|surveyMentalHealth_clean$Gender=="m"|surveyMentalHealth_clean$Gender=="Mail"|surveyMentalHealth_clean$Gender=="Make"|surveyMentalHealth_clean$Gender=="male leaning androgynous"|surveyMentalHealth_clean$Gender=="Malr"|surveyMentalHealth_clean$Gender=="msle"|surveyMentalHealth_clean$Gender=="ostensibly male, unsure what that really means"|surveyMentalHealth_clean$Gender=="something kinda male?"|surveyMentalHealth_clean$Gender=="Androgyne"|surveyMentalHealth_clean$Gender=="cis male"|surveyMentalHealth_clean$Gender=="Cis Man"|surveyMentalHealth_clean$Gender=="Guy (-ish) ^_^"|surveyMentalHealth_clean$Gender=="maile"|surveyMentalHealth_clean$Gender=="Mal"|surveyMentalHealth_clean$Gender=="Male (CIS)"|surveyMentalHealth_clean$Gender=="Male-ish"|surveyMentalHealth_clean$Gender=="Man"|surveyMentalHealth_clean$Gender=="Male ","M")

surveyMentalHealth_clean$Gender<-replace(surveyMentalHealth_clean$Gender,surveyMentalHealth_clean$Gender=="A little about you"|surveyMentalHealth_clean$Gender=="Agender"|surveyMentalHealth_clean$Gender=="All"|surveyMentalHealth_clean$Gender=="Enby"|surveyMentalHealth_clean$Gender=="fluid"|surveyMentalHealth_clean$Gender=="Genderqueer"|surveyMentalHealth_clean$Gender=="Nah"|surveyMentalHealth_clean$Gender=="Neuter"|surveyMentalHealth_clean$Gender=="non-binary"|surveyMentalHealth_clean$Gender=="p"|surveyMentalHealth_clean$Gender=="queer"|surveyMentalHealth_clean$Gender=="queer/she/they"|surveyMentalHealth_clean$Gender=="Trans woman"|surveyMentalHealth_clean$Gender=="Trans-female",NA)

surveyMentalHealth_clean$Gender<-as.factor(surveyMentalHealth_clean$Gender)
levels(surveyMentalHealth_clean$Gender)
surveyMentalHealth_clean<-subset(surveyMentalHealth_clean, surveyMentalHealth_clean$Gender!="NA")
nrow(surveyMentalHealth_clean)

#Comprobación de niveles para variable Country y resumen
levels(surveyMentalHealth_clean$Country)
summary(surveyMentalHealth_clean$Country)

#Gráficos sobre las variables
barplot(table(surveyMentalHealth_clean$Country), 
        main="Paises")
levels(surveyMentalHealth_clean$family_history)
summary(surveyMentalHealth_clean$family_history)

barplot(table(surveyMentalHealth_clean$family_history), 
        main="Antecedentes familiares")
levels(surveyMentalHealth_clean$treatment)
summary(surveyMentalHealth_clean$treatment)

barplot(table(surveyMentalHealth_clean$treatment), 
        main="Ha sido tratado de alguna enfermedad mental")
levels(surveyMentalHealth_clean$work_interfere)
summary(surveyMentalHealth_clean$work_interfere)

barplot(table(surveyMentalHealth_clean$work_interfere), 
        main="La enfermendad mental interfiere en su trabajo")
levels(surveyMentalHealth_clean$no_employees)
summary(surveyMentalHealth_clean$no_employees)

barplot(table(surveyMentalHealth_clean$no_employees), 
        main="Número de empleados de la compañia u organizacion")
levels(surveyMentalHealth_clean$remote_work)
summary(surveyMentalHealth_clean$remote_work)

barplot(table(surveyMentalHealth_clean$remote_work), 
        main="Teletrabajo al menos el 50% del tiempo")
levels(surveyMentalHealth_clean$tech_company)
summary(surveyMentalHealth_clean$tech_company)

barplot(table(surveyMentalHealth_clean$tech_company), 
        main="La Organización es Tecnologica")
levels(surveyMentalHealth_clean$benefits)
summary(surveyMentalHealth_clean$benefits)

barplot(table(surveyMentalHealth_clean$benefits), 
        main="La Organizacion provee de beneficios de salud Mental")
levels(surveyMentalHealth_clean$care_options)
summary(surveyMentalHealth_clean$care_options)

barplot(table(surveyMentalHealth_clean$care_options), 
        main="Conoce Opciones de cuidado mental de su compañia médica")
levels(surveyMentalHealth_clean$wellness_program)

summary (surveyMentalHealth_clean$wellness_program)
barplot(table(surveyMentalHealth_clean$wellness_program), 
        main="Información del conocimiento de programas especificos")
levels(surveyMentalHealth_clean$seek_help)
summary (surveyMentalHealth_clean$seek_help)

barplot(table(surveyMentalHealth_clean$seek_help), 
        main="Información de recursos y ayuda desde la organización")
levels(surveyMentalHealth_clean$anonymity)
summary (surveyMentalHealth_clean$anonymity)

barplot(table(surveyMentalHealth_clean$anonymity), 
        main="Privacidad de beneficios sobre enfermedades mentales")
levels(surveyMentalHealth_clean$leave)
summary (surveyMentalHealth_clean$leave)

barplot(table(surveyMentalHealth_clean$leave), 
        main="Posibilidad de baja en enfermedades mentales")
levels(surveyMentalHealth_clean$mental_health_consequence)
summary (surveyMentalHealth_clean$mental_health_consequence)

barplot(table(surveyMentalHealth_clean$mental_health_consequence), 
        main="Consecuencias por hablar de salud mental")
levels(surveyMentalHealth_clean$phys_health_consequence)
summary (surveyMentalHealth_clean$phys_health_consequence)

barplot(table(surveyMentalHealth_clean$phys_health_consequence), 
        main="Consecuencias por hablar de salud física")
levels(surveyMentalHealth_clean$coworkers)
summary (surveyMentalHealth_clean$coworkers)

barplot(table(surveyMentalHealth_clean$mental_health_consequence), 
        main="Hablaria de salud mental con compañeros")
levels(surveyMentalHealth_clean$supervisor)
summary (surveyMentalHealth_clean$supervisor)

barplot(table(surveyMentalHealth_clean$supervisor), 
        main="Hablaria de salud mental con su jefe")
levels(surveyMentalHealth_clean$mental_health_interview)
summary (surveyMentalHealth_clean$mental_health_interview)

barplot(table(surveyMentalHealth_clean$mental_health_interview), 
        main="Hablaria de salud mental en una entrevista laboral")
levels(surveyMentalHealth_clean$phys_health_interview)
summary (surveyMentalHealth_clean$phys_health_interview)

barplot(table(surveyMentalHealth_clean$mental_health_interview), 
        main="Hablaria de salud física en una entrevista laboral")
levels(surveyMentalHealth_clean$mental_vs_physical)
summary (surveyMentalHealth_clean$mental_vs_physical)

barplot(table(surveyMentalHealth_clean$mental_vs_physical), 
        main="Importacia en la Organizacion de la salud mental sobre la física")
levels(surveyMentalHealth_clean$obs_consequence)
summary (surveyMentalHealth_clean$obs_consequence)

barplot(table(surveyMentalHealth_clean$obs_consequence), 
        main="Consecuencias laboral por padecer enfermedad mental ")

#Gráfica comparativa 1
par(mfrow=c(1,2))
barplot(table(surveyMentalHealth_clean$mental_health_interview), 
        main="Mención mental en entrevista laboral")
barplot(table(surveyMentalHealth_clean$phys_health_interview), 
        main="Mención física en entrevista laboral")

#Gráfica comparativa 2
par(mfrow=c(1,2))
barplot(table(surveyMentalHealth_clean$mental_health_consequence), 
        main="Mental:¿consecuencias negativas?")
barplot(table(surveyMentalHealth_clean$phys_health_consequence), 
        main="Física:¿consecuencias negativas?")
#Calculo Media
mean(surveyMentalHealth_clean$Age)

#Calculo Mediana
median(surveyMentalHealth_clean$Age)

#Sumario de los cinco números (Mínimo, Q1, Mediana, Q3, Maximo)
fivenum(surveyMentalHealth_clean$Age)

#Diagrama de caja (Boxplot)
boxplot(surveyMentalHealth_clean$Age, main="Box Plot Age")

#Calculamos el numero de intervalor
k_Age<- round(sqrt(length(surveyMentalHealth_clean$Age)))
k_Age
hist(surveyMentalHealth_clean$Age ,main="Edad individuos Observados",
     breaks=k_Age, col="blue")
hh_Age<-hist(surveyMentalHealth_clean$Age ,main="Edad individuos Observados",
             breaks=k_Age, col="blue")
hh_Age

#Calculo de la función de densidad
den_Age<- density(surveyMentalHealth_clean$Age)
plot(den_Age ,main="Edad Individuos Observados")
polygon(den_Age , col="blue", border="red")

#Superposición de las gráficas
hist(surveyMentalHealth_clean$Age ,main="Edad individuos Observados",
     col="gold",freq=FALSE)
lines(den_Age,col="blue",lwd=4)
qqnorm(surveyMentalHealth_clean$Age)
qqline(surveyMentalHealth_clean$Age)
ks.test(x=surveyMentalHealth_clean$Age,"pnorm", mean(surveyMentalHealth_clean$Age), sd(surveyMentalHealth_clean$Age))

lillie.test((x=surveyMentalHealth_clean$Age))
jb.norm.test(x=surveyMentalHealth_clean$Age)
Age_Trans<-(sqrt(sqrt(1/surveyMentalHealth_clean$Age)))
#Calculo de la función de densidad
den_Age_Trans<- density(Age_Trans)
plot(den_Age_Trans ,main="Edad de los individuos Observados")
polygon(den_Age_Trans , col="blue", border="red")

#Superposición de las gráficas
hist(Age_Trans ,main="Edad de los individuos Observados",
     col="gold",freq=FALSE)
lines(den_Age_Trans ,col="blue",lwd=4)
qqnorm(Age_Trans)
qqline(Age_Trans)

lillie.test((x=Age_Trans))
Age_Mental<-subset(surveyMentalHealth_clean$Age, surveyMentalHealth_clean$mental_vs_physical=="Yes")
Age_Fisica<-subset(surveyMentalHealth_clean$Age, surveyMentalHealth_clean$mental_vs_physical=="No")
Age_Desconoce<-subset(surveyMentalHealth_clean$Age, surveyMentalHealth_clean$mental_vs_physical=="Don't know")
bartlett.test(list(Age_Mental,Age_Fisica,Age_Desconoce))
ggplot(surveyMentalHealth_clean, aes(x = mental_vs_physical, y = Age, colour =mental_vs_physical)) + geom_boxplot() + theme_bw()
Age_Tratamiento<-subset(surveyMentalHealth_clean$Age, surveyMentalHealth_clean$treatment=="Yes")
Age_NTratamiento<-subset(surveyMentalHealth_clean$Age, surveyMentalHealth_clean$treatment=="No")
bartlett.test(list(Age_Tratamiento,Age_NTratamiento))
ggplot(surveyMentalHealth_clean, aes(x = treatment, y = Age, colour =treatment)) + geom_boxplot() + theme_bw()
kruskal.test(Age~ treatment, data=surveyMentalHealth_clean)
fit2=lm(Age~ mental_vs_physical, surveyMentalHealth_clean)
aov(fit2)
summary(aov(fit2))

#Se guardan los cambios realizados
write.csv(surveyMentalHealth_clean, file="survey_clean.csv")
