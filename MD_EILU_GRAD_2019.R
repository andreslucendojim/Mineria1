library(haven) #Import dta
library(ggplot2) #Better graphs
library(paletteer) #Color palette
library(ggpubr) #Combine plots
library(corrplot)

data <- read_dta("~/Desktop/MineriaDatos/Mineria1/EILU_GRAD_2019.dta") #Import dataset
data_na <- na.omit(read.csv("~/Desktop/MineriaDatos/Mineria1/graduados.csv", sep = ";"))

"""
ANÁLISIS DESCRIPTIVO
"""

#Father - Mother studies

"""
1 <- No sabe leer ni escribir
2 <- Educación primaria incompleta (ha asistido menos de 5 años a la escuela)
3 <- Educación primaria completa
4 <- Primera etapa de Enseñanza Secundaria, con o sin título (ESO, EGB, Bachillerato Elemental)
5 <- Estudios de Bachillerato (Bachillerato LOGSE, BUP, COU, Preu)
6 <- Enseñanzas profesionales de grado medio o equivalentes
7 <- Enseñanzas profesionales de grado superior o equivalentes
8 <- Estudios universitarios (Diplomatura, Licenciatura, Doctorado) o equivalentes
9 <- NS/NC
"""

#Father histogram -> ESUDIOS_PADRE, ESTUDIOS_MADRE
father <- data$ESTUDIOS_PADRE #Create a vector
father <- as.numeric(father) #Change to numerico form
father <- father[!is.na(father)] #Remove NA
father <- father[!father == 9] #Remove 9 value


hf <- ggplot() + aes(x = father) + geom_bar(fill = paletteer_c("ggthemes::Blue", 8), 
                                                  color = "black") + 
  ggtitle("Histograma estudios del padre") + xlab("Estudios padre") + ylab("Cantidad")

hf

#Mother histogram

mother <- data$ESTUDIOS_MADRE #Create a vector
mother <- as.numeric(mother) #Change to numerico form
mother <- mother[!is.na(mother)] #Remove NA
mother <- mother[!mother == 9] #Remove 9 value

hm <- ggplot() + aes(x = mother)  + geom_bar(fill = paletteer_c("ggthemes::Green", 8), 
                                                  color = "black") + 
  ggtitle("Histograma estudios de la madre") + xlab("Estudios madre") + ylab("Cantidad")
hm

#Combined histograms
histo <- ggarrange(hf, hm, ncol = 1, nrow = 2)
histo

summary(father)
summary(mother)

#Scolarship -> EST_B2_2

"""
1 <- Si
2 <- No
"""
scolarship <- data$EST_B2_2 #Create a vector
scolarship <- as.numeric(scolarship) #Change to numerico form
scolarship <- scolarship[!scolarship == 9] #Remove 9 value

ggplot() + aes(scolarship)+ geom_bar(colour="black", fill=paletteer_c("ggthemes::Classic Area Green", 2))
summary(scolarship)

#Erasmus -> EST_M1
"""
1 <- Si
2 <- No
"""
erasmus <- data$EST_M1 #Create a vector
erasmus <- as.numeric(erasmus) #Change to numerico form
erasmus <- erasmus[!erasmus == 9] #Remove 9 value


ggplot() + aes(erasmus)+ geom_bar(colour="black", fill=paletteer_c("ggthemes::Classic Area Red", 2))
summary(erasmus)

#Master -> EST_B11_2
"""
1 <- Si
2 <- No
"""
master <- data$EST_B11_2
ggplot() + aes(master)+ geom_bar(colour="black", fill=paletteer_c("ggthemes::Purple", 2))
summary(master)
#Doctor -> EST_B11_3
"""
1 <- Si
2 <- No
"""
doctor <- as.numeric(data$EST_B11_3)
ggplot() + aes(doctor)+ geom_bar(colour="black", fill=paletteer_c("ggthemes::Gray", 2))

summary(as.numeric(doctor))


#Correlación

father_na <- data_na$ESTUDIOS_PADRE
mother_na <- data_na$ESTUDIOS_MADRE
scolarship_na <- data_na$EST_B11_2
erasmus_na <- data_na$EST_M1
master_na <- data_na$EST_B11_2
doctor_na <- data_na$EST_B11_3

cor <- data.frame(as.numeric(data_na$ESTUDIOS_PADRE), as.numeric(data_na$ESTUDIOS_PADRE), 
                  as.numeric(data_na$EST_B2_2), as.numeric(data_na$EST_M1), 
                  as.numeric(data_na$EST_B11_2), as.numeric(data_na$EST_B11_3))
colnames(cor) <- c("Estudios padre", "Estudios Madre", "Beca de Excelencia", 
                   "Movilidad", "Master", "Doctorado")

corrplot(cor(cor), type = "upper")
