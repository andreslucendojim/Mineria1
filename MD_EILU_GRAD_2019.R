library(haven) #Import dta
library(ggplot2) #Better graphs
library(paletteer) #Color palette
library(ggpubr) #Combine plots
data <- read_dta("~/Desktop/MineriaDatos/Mineria1/EILU_GRAD_2019.dta") #Import dataset
View(data)


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
hf <- ggplot(data, aes(x = ESTUDIOS_PADRE)) + geom_bar(fill = paletteer_c("ggthemes::Blue", 9), 
                                                  color = "black") + 
  ggtitle("Histograma estudios del padre") + xlab("Estudios padre") + ylab("Cantidad")

#Mother histogram
hm <- ggplot(data, aes(x = ESTUDIOS_MADRE)) + geom_bar(fill = paletteer_c("ggthemes::Green", 9), 
                                                  color = "black") + 
  ggtitle("Histograma estudios de la madre") + xlab("Estudios madre") + ylab("Cantidad")

#Combined histograms
histo <- ggarrange(hf, hm, ncol = 1, nrow = 2)
histo

#Scolarship -> EST_B2_2
"""
1 <- Si
2 <- No
"""
scolarship <- data$EST_B2_2 #Create a vector
scolarship <- as.numeric(scolarship) #Change to numerico form
scolarship <- scolarship[!is.na(scolarship)] #Remove NA
scolarship <- scolarship[!scolarship == 9] #Remove 9 value
hist(scolarship)

ggplot() + aes(scolarship)+ geom_bar(colour="black", fill=paletteer_c("ggthemes::Classic Area Green", 2))

#Erasmus -> EST_M1
"""
1 <- Si
2 <- No
"""
erasmus <- data$EST_M1 #Create a vector
erasmus <- as.numeric(erasmus) #Change to numerico form
erasmus <- erasmus[!is.na(erasmus)] #Remove NA
erasmus <- erasmus[!erasmus == 9] #Remove 9 value
erasmus

ggplot() + aes(erasmus)+ geom_bar(colour="black", fill=paletteer_c("ggthemes::Classic Area Red", 2))

#Master -> EST_B11_2
"""
1 <- Si
2 <- No
"""
master <- data$EST_B11_2
ggplot() + aes(master)+ geom_bar(colour="black", fill=paletteer_c("ggthemes::Purple", 2))

#Doctor -> EST_B11_3
"""
1 <- Si
2 <- No
"""
doctor <- data$EST_B11_3
ggplot() + aes(doctor)+ geom_bar(colour="black", fill=paletteer_c("ggthemes::Gray", 2))


