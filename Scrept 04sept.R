cities<-read.csv("cities.csv",header=TRUE,sep=",")
class(cities)
matriz_cities<-data.matrix(cities[,2:12])
rownames(matriz_cities)<-as.matrix(cities[,1])
cor(matriz_cities) #es una funcion que me permite calcular la matriz para correlaciones
library(corrplot)
corrplot(cor(matriz_cities))

# Prueba KMO
KMO(matriz_cities)
library(psych)
KMO(matriz_cities)
cortest.bartlett(matriz_cities)

# Codigo 1
data_estand<-scale(matriz_cities)
pca1<-princomp(data_estand)

pca1$loadings

# Grafico
biplot(pca1)

---------------------------------------
  
 library(readxl)
cpv <- read_excel("cpv_2010.xlsx", col_names=TRUE)
matriz_cpv<-data.matrix(cpv[ ,2:13])
rownames(matriz_cpv)<- as.matrix(cpv[,1])

corrplot(cor(matriz_cpv))
KMO(matriz_cpv)
cortest.bartlett(matriz_cpv)

# CODIGO 2
library("FactoMineR")
library("factoextra")

resultado_pca<-PCA(matriz_cpv, scale.unit=T,graph=F)
resultado_pca$eig

summary(resultado_pca)

fviz_pca_ind(resultado_pca)

