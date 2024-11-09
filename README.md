# proyectof1

#descarga de DATOS
ir a la URL: https://www.ine.gob.gt/educacion/
-seleccionar base de datos en el menu laterla
-seleccionar anio 2023 y periodo anual
-descargar la base de datos con extenison sav, Llamada: Base de educación formal

# Instalar el paquete foreign si no lo tienes
install.packages("foreign")

# Cargar el paquete
library(foreign)

# Leer el archivo .sav

-----se debe cambiar la ruta segun donde este descargado el archivo con extension .sav
data2 <- read.spss("/Users/oliverrodas/Documents/maestria/2024/MINERIA DE DATOS/proyecto1/baseformal2023.sav", to.data.frame = TRUE)

#cambio para sexo
data2$Sexo <- factor(data2$Sexo, levels = c("Hombre", "Mujer"), labels = c(1, 2))

#cambio para Sector
data2$Sector <- factor(data2$Sector, levels = c("Público", "Privado", "Municipal", "Cooperativa"), labels = c(1, 2, 3, 4))

#cambio para Area
data2$Área <- factor(data2$Área, levels = c("Urbana", "Rural", "Ignorado"), labels = c(1, 2, 9))

#cambio Jornada
data2$Jornada_Est <- factor(data2$Jornada_Est, levels = c("Matutina", "Vespertina", "Noturna", "Doble", "Internmedia", "Sin jornada"), labels = c(1, 2, 3, 4, 5, 9))

#cambio Resultado Final
data2$Resultado_F <- factor(data2$Resultado_F, levels = c("Promovido", "Vigente", "Retirado por traslado", "Retirado definitivo", "No promovido", "Ignorado"), labels = c(1, 2, 3, 4, 5, 9))

#plan de estudio
data2$Plan_Est <- factor(data2$Plan_Est, levels = c("Diario", "Fin de semana", "Virtual a distancia", "Semipresencial", "Mixto"), labels = c(1, 2, 3, 4, 5))

#Nivel
data2$Nivel <- factor(data2$Nivel, levels = c("Preprimaria", "Primaria", "Básicos", "Diversificado", "Primaria de adultos"), labels = c(1, 2, 3, 4, 5))

#Pueblo de pertenencia
data2$Pueblo_Per <- factor(data2$Pueblo_Per, levels = c("Ladino/Mestizo", "Maya", "Garífuna", "Xinka", "Extranjero", "Ignorado"), labels = c(1, 2, 3, 4, 5,9))


#departamentos
data2$Departamento_F <- factor(data2$Departamento_F, levels = c("Guatemala", "El Progreso", "Sacatepéquez", "Chimaltenango", "Escuintla", "Santa Rosa", "Sololá", "Totonicapán", "Quetzaltenango", "Suchitepéquez", "Retalhuleu", "San Marcos", "Huehuetenango", "Quiché", "Baja Verapaz", "Alta Verapaz", "Péten", "Izabal", "Zacapa", "Chiquimula", "Jalapa", "Jutiapa"), labels = 1:22)


# Verifica los primeros valores
head(data2$Departamento)





################ reglas #######################

# Instalar y cargar el paquete arules
install.packages("arules")

library("arules")


# Quitar la columna "Año"
data2 <- subset(data2, select = -Año)

# Aplicar el algoritmo Apriori para descubrir reglas de asociación
reglas <- apriori(data2, parameter = list(support=0.2, confidence=0.5 ))

# Inspeccionar las reglas
inspect(reglas[100:200])

# Convertir las reglas a un data frame
reglasframe <- as(reglas, "data.frame")


################# Reglas de asociación FP-Growth ############

#quitar anio 2023
dataFP2 <- data2[, c(2,3,4,5,6,7,8,9,10,11,12,13,14)]

dataFP2 <- data.frame(dataFP2)

# Aplicar el algoritmo Apriori nuevamente
reglasFP <- apriori(dataFP2, parameter = list(supp = 0.2, conf = 0.5, target = "rules"))

# Convertir las reglas a un data frame
reglasframeFP <- as(reglasFP, "data.frame")



#segmentacion por departamento 
# se debe ejecutar por departamento ya que al aplicar la regla solo se usa ReglasFP

#Qiche
dataFP2D <- subset(dataFP2, Departamento_F == 14)
#Jutiapa
dataFP2D <- subset(dataFP2, Departamento_F == 22)
#Alta verapaz
dataFP2D <- subset(dataFP2, Departamento_F == 16)

#Sacatepequez
dataFP2D <- subset(dataFP2, Departamento_F == 3)

# Aplicar el algoritmo Apriori a la segmentación
reglasFP <- apriori(dataFP2D, parameter = list(supp = 0.2, conf = 0.5, target = "rules"))

reglasframeFP <- as(reglasFP, "data.frame")

############### kmeans ###############

# Instalar y cargar las bibliotecas necesarias
install.packages("arules")
library(arules)
install.packages("genero")
library(genero)

install.packages("ggplot2")
library(ggplot2)

#sexo sector

datamsc2 <- data2[, c(5,7)]

#Conteo de NA
any(is.na(datamsc2))

#quitar posibles NA
datamsc2 <- na.omit(datamsc2)

# Aplicar K-means
cluster <- kmeans(na.omit(datamsc2) , centers=2)

# Visualizar el cluster
ggplot(datamsc2, aes(x = Pueblo_Per, y = Nivel, color = as.factor(cluster$cluster)))+
  geom_point()+
  geom_point(data = as.data.frame(cluster$centers), aes(x=Pueblo_Per, y = Nivel), color = "black", size=4, shape=17)+
  labs(title = "Sexo por sector estudiantil")+
  theme_minimal()

#pueblo perteneciente vs nivel escolar

datamsc2 <- data2[, c(9,10)]

#Conteo de NA
any(is.na(datamsc2))
#quitar posibles NA
datamsc2 <- na.omit(datamsc2)

# Aplicar K-means
cluster <- kmeans(na.omit(datamsc2) , centers=4)

# Visualizar el cluster
ggplot(datamsc2, aes(x = Pueblo_Per, y = Nivel, color = as.factor(cluster$cluster)))+
  geom_point()+
  geom_point(data = as.data.frame(cluster$centers), aes(x=Pueblo_Per, y = Nivel), color = "black", size=4, shape=17)+
  labs(title = "Pueblo Perteneciente por Nivel academico")+
  theme_minimal()




#se debe utilizar el archivo excel diccionario educacion formal 2023 para comparar las reglas