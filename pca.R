install.packages("pacman")
library(pacman)

p_load(haven, dplyr, factoextra, FactoMineR, readr, rgl, fpc, psych, xlsx,
       corrplot, readxl)

# ----------------------
# BASE DE DATOS data_pca
# ----------------------
data_pca <- read.csv("OCTAVO/CÓMPUTO/data_pca.csv", sep = ";", dec = ",")
View(data_pca)

# Normaloizar datos
d1 <- scale(data_pca[, -16])
View(d1)

#--------------
# REALIZAR PCA
#--------------

# Diagnóstico para el PCA
# Si el resultado tiende a 0 se puede aplicar
det(cor(d1))

# el valor si tiende a 0, por ende puede realizarse
pca1 <- princomp(d1)
pca1$loadings

# Revisar varianza y eigenvalores

summary(pca1)
# se observa que los principales componentes que aportan en la varianza son del 1 al 6.

fviz_eig(pca1, choice = "variance", addlabels = T, barfill = "paleturquoise4",
         barcolor = "paleturquoise4", linecolor = "#00838f")
# puede comprobarse que la componente uno a seis aportan mayor varianza

fviz_eig(pca1, choice = "eigenvalue", addlabels = T, barfill = "paleturquoise4",
         barcolor = "paleturquoise4", linecolor = "#00838f")
# los eigen valores deben ser mayores a 1 para ser considerados; nuevamente, 
# solo seis componentes tienen un eigenvalor mayor a 1

#-----------------
# ANÁLISIS GRÁFICO
#-----------------

# Gráfico de las puntuaciones factoriales y su representación
fviz_pca_ind(pca1, col.ind = "cos2",
             gradient.cols = c("red", "orange", "yellow", "green"),
             repel = T)

# Gráfico de las cargas 
fviz_pca_var(pca1, col.var = "contrib", 
             gradient.cols =  c("red", "yellow", "green"),
             repel = T)

# Para visualizar puntuaciones se emplea un biplot
fviz_pca_biplot(pca1,
                col.var = "red",
                col.ind = "black",
                select.ind = list(name = c("75", "34", "199")))

d1[75,]
d1[34,]
d1[199,]

# Análisis como lo proporciona spss

# Para visualizar la correlación de los datos
x11()
psych::cor.plot(d1)
corrplot::corrplot(cor(d1),method = 'shade', col = COL2("BrBG"),  
                   addCoef.col = 'gray28', number.cex = 0.7,
                   tl.col = "black", tl.cex = 0.9)

# -------------------------------
# Resultado del pca por factores 
# -------------------------------

# La rotación más común es VARIMAX
pca2 <- psych::principal(d1, nfactors = 6, residuals = FALSE, rotate = "varimax", 
                         scores =TRUE, oblique.scores=FALSE, method="regression", use= "pairwise", cor="cor", weight=NULL)
pca2

# Matriz de coeficientes para las puntuaciones de los componentes. 
pca2$weights[,1]
pca2$weights[,2]
pca2$weights[,3]
pca2$weights[,4]
pca2$weights[,5]
pca2$weights[,6]

# Nuevas variables obtenidas cuya principal caracteristica es que son ortogonales, 
# es decir, linealmente independientes.
pca2$scores

# --------------------------
# BASE DE DATOS PoblacionUSA
# --------------------------

USA <- read_excel("OCTAVO/CÓMPUTO/PoblacionUSA.xlsm")
View(USA)

# Crear las bases para los análisis pca
d_2020 <- USA[, c(2, 3, 5, 7, 9, 11, 13, 15, 17, 19)]
colnames(d_2020) <- c("CRTP", "RTPE", "NDM", "F/C_MA", "NIM", "PB", "PD", "RU65", "R65+", "RES")
d_2021 <- USA[, c(4, 6, 8, 10, 12, 14, 16, 18, 20)]
colnames(d_2021) <- c("RTPE", "NDM", "F/C_MA", "NIM", "PB", "PD", "RU65", "R65+", "RES")

# ------
#  2020
# ------

# Normaloizar datos
d1_2020 <- scale(d_2020)

#--------------
# REALIZAR PCA
#--------------

# Diagnóstico para el PCA
# Si el resultado tiende a 0 se puede aplicar
det(cor(d1_2020))
# el valor si tiende a 0, por ende puede realizarse

# Calcular factor de adecuación muestral Kaiser - Meyer - Olkin
psych::KMO(d1_2020)


pca1_2020 <- princomp(d1_2020)
pca1_2020$loadings

# Revisar varianza y eigenvalores

summary(pca1_2020)
# se observa que los principales componentes que aportan en la varianza son del 1 al 2.

fviz_eig(pca1_2020, choice = "variance", addlabels = T, barfill = "paleturquoise4",
         barcolor = "paleturquoise4", linecolor = "#00838f")
# puede comprobarse que la componente uno a dos aportan mayor varianza

fviz_eig(pca1_2020, choice = "eigenvalue", addlabels = T, barfill = "paleturquoise4",
         barcolor = "paleturquoise4", linecolor = "#00838f")
# los eigen valores deben ser mayores a 1 para ser considerados; nuevamente, 
# solo dos componentes tienen un eigenvalor mayor a 1

#-----------------
# ANÁLISIS GRÁFICO
#-----------------


# Gráfico de las puntuaciones factoriales y su representación
fviz_pca_ind(pca1_2020, col.ind = "cos2",
             gradient.cols = c("red", "orange", "yellow", "green"),
             repel = T)

# Gráfico de las cargas 
fviz_pca_var(pca1_2020, col.var = "contrib", 
             gradient.cols =  c("red", "yellow", "green"),
             repel = T)

# Para visualizar puntuaciones se emplea un biplot
fviz_pca_biplot(pca1_2020,
                col.var = "red",
                col.ind = "black",
                select.ind = list(name = c("34", "47", "11")))

d1_2020[11,]
d1_2020[34,]
d1_2020[47,]

# Análisis como lo proporciona spss

# Para visualizar la correlación de los datos
x11()
corrplot::corrplot(cor(d1_2020),method = 'shade', col = COL2("BrBG"),  
                   addCoef.col = 'gray28', number.cex = 0.7,
                   tl.col = "black", tl.cex = 0.9)

# -------------------------------
# Resultado del pca por factores 
# -------------------------------

# La rotación más común es VARIMAX
pca2_2020 <- psych::principal(d1_2020, nfactors = 2, residuals = FALSE, rotate = "varimax", 
                         scores =TRUE, oblique.scores=FALSE, method="regression", use= "pairwise", cor="cor", weight=NULL)
pca2_2020

# Matriz de coeficientes para las puntuaciones de los componentes. 
pca2_2020$weights[,1]
pca2_2020$weights[,2]

pca2_2020$scores

# ------
#  2021
# ------

# Normaloizar datos
d1_2021 <- scale(d_2021)

#--------------
# REALIZAR PCA
#--------------

# Diagnóstico para el PCA
# Si el resultado tiende a 0 se puede aplicar
det(cor(d1_2021))
# el valor si tiende a 0, por ende puede realizarse

# Calcular factor de adecuación muestral Kaiser - Meyer - Olkin
psych::KMO(d1_2021)


pca1_2021 <- princomp(d1_2021)
pca1_2021$loadings

# Revisar varianza y eigenvalores

summary(pca1_2021)
# se observa que los principales componentes que aportan en la varianza son del 1 al 2.

fviz_eig(pca1_2021, choice = "variance", addlabels = T, barfill = "paleturquoise4",
         barcolor = "paleturquoise4", linecolor = "#00838f")
# puede comprobarse que la componente uno a dos aportan mayor varianza

fviz_eig(pca1_2021, choice = "eigenvalue", addlabels = T, barfill = "paleturquoise4",
         barcolor = "paleturquoise4", linecolor = "#00838f")

#-----------------
# ANÁLISIS GRÁFICO
#-----------------


# Gráfico de las puntuaciones factoriales y su representación
fviz_pca_ind(pca1_2021, col.ind = "cos2",
             gradient.cols = c("red", "orange", "yellow", "green"),
             repel = T)

# Gráfico de las cargas 
fviz_pca_var(pca1_2021, col.var = "contrib", 
             gradient.cols =  c("red", "yellow", "green"),
             repel = T)

# Para visualizar puntuaciones se emplea un biplot
fviz_pca_biplot(pca1_2021,
                col.var = "red",
                col.ind = "black",
                select.ind = list(name = c("11", "14")))

d1_2021[11,]
d1_2021[14,]


# Análisis como lo proporciona spss

# Para visualizar la correlación de los datos
x11()
corrplot::corrplot(cor(d1_2021), method = 'shade', col = COL2("BrBG"),  
                   addCoef.col = 'gray28', number.cex = 0.7,
                   tl.col = "black", tl.cex = 0.9)

# -------------------------------
# Resultado del pca por factores 
# -------------------------------

# La rotación más común es VARIMAX
pca2_2021 <- psych::principal(d1_2021, nfactors = 2, residuals = FALSE, rotate = "varimax", 
                              scores =TRUE, oblique.scores=FALSE, method="regression", use= "pairwise", cor="cor", weight=NULL)
pca2_2021

# Matriz de coeficientes para las puntuaciones de los componentes. 
pca2_2020$weights[,1]
pca2_2020$weights[,2]

pca2_2021$scores