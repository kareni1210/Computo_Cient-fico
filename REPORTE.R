#Instalar y llamar librerías
install.packages(c("insuranceData", "dplyr", "ggplot2", "visdat", "skimr"))
install.packages("extrafont")

library(insuranceData)
library(dplyr)
library(skimr)
library(visdat)
library(ggplot2)
library(extrafont)
library(kableExtra)

#Usar dataCar para trabajar
data(dataCar)
#Estructura de dataCar
str(dataCar)
#Mostrar un resumen
summary(dataCar)
#Resumen más detallado 
skim(dataCar)
head(dataCar, 4) #por default solo aparecen 6 observaciones, pero se puede elegir el número deseado
dim(dataCar) #Dimensión del arreglo
tail(dataCar)
#Características del data frame
glimpse(dataCar)
#Conocer los nombres de las columnas
colnames(dataCar)
names(dataCar)
# Verificar si hay datos faltantes, si llegase a resultar un TRUE se puede usar un which
miss <- any(is.na(dataCar))
miss
#Gráfica de los datos faltantes
vis_dat(dataCar)
#Porcentaje de los datos faltantes
vis_miss(dataCar)


#Para conocer cuantos datos existen y las reclamaciones
pol <- length(dataCar$numclaims)
sum(dataCar$clm >= 1)
claims <- sum(dataCar$numclaims >= 1)
(claims/pol)*100

#Mayor número de reclamaciones por tipo de vehículo, top 5
claims_tipo <- dataCar %>%
  group_by(veh_body) %>%
  summarise(tot_claims = sum(numclaims)) %>% #Crear nueva variable
  arrange(desc(tot_claims)) #Ordenar de forma descendente
head(claims_tipo,5)

#Número de pólizas por tipo de vehículos
num_pol_by_tv <- dataCar %>%
  group_by(veh_body) %>%
  summarise(total_pol =n()) %>%
  arrange(desc(total_pol))

num_pol_by_tv

#Vehículos con mayor monto de reclamaciones, top 10
claims_mont <- dataCar %>%
  group_by(veh_body) %>%
  summarise(total_mont = sum(claimcst0)) %>% #Crear nueva variable
  arrange(desc(total_mont)) #Ordenar de forma descendente

head(claims_mont, 10)

#ANÁLISIS CONTEMPLANDO EL GÉNERO

#Monto total de reclamaciones por género y modelo
claims_by_vehb_gen <- dataCar %>%
  group_by(gender,veh_body) %>%
  summarise(monto_total = sum(claimcst0)) %>%
  arrange(desc(monto_total))

claims_by_vehb_gen

#Número de reclamaciones por edad, género y tipo de vehículo
claims_by_gen_age_vehb<- dataCar %>%
  group_by(gender, veh_body, agecat) %>%
  summarise(nclaims_total = sum(numclaims)) %>%
  arrange(desc(nclaims_total))

head(claims_by_gen_age_vehb, 20)

#USANDO GGPLOT

#Ejemplo 1
ggplot(dataCar, aes(x = numclaims, y = claimcst0)) + geom_point()

#Ejemplo 2 Gráfico de Dispersión
ggplot(dataCar, aes(x = exposure, y = veh_value))+ geom_point(color = "plum")+
  labs(title = "Gráfico de Dispersión: Exposición vs Valor de Vehiculo", 
       x = "Exposición", y = "Valor Vehiculo")+
  theme_minimal()
#Hacer consultas con base al gráfico
#Valor máximo del vehículo por tipo, exposición y costo de reclamación
consulta <- dataCar %>%
  group_by(veh_body, claimcst0, exposure) %>%
  summarise(maxi = max(veh_value)) %>%
  arrange(desc(maxi))
consulta
#Monto de reclamación máximo por tipo, exposición y valor del vehículo
consulta1 <- dataCar %>%
  group_by(veh_body, veh_value, exposure) %>%
  summarise(maxi = max(claimcst0)) %>%
  arrange(desc(maxi))
consulta1

#Ejemplo 3
ggplot(claims_by_gen_age_vehb, aes(x = reorder(veh_body, -nclaims_total), 
                                   y = nclaims_total, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Gráfico 2. Tipo de Vehículos Más Reclamados por Género",
       x = "Tipo de Vehículos", y = "Número de Reclamaciones") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("F" = "lightpink2", "M" = "cadetblue")) + 
  theme(plot.title = element_text(family = "serif", face = "bold", size = 15, 
                                  hjust = 0.6, vjust = 1, lineheight = 1), 
        plot.title.position = "plot", legend.position = "bottom") +
  guides(fill = guide_legend(title = "Género"))

######

#Estadísticas generales
summary(dataCar)
resumen<- data.frame(`Categoría` = c("Valor del Vehículo", "Exposición", "Ocurrencia de reclamación", 
                                     "Número de reclamaciones", "Monto de la reclamación", 
                                     "Edad Vehículo", "Grupo de edad del Conductor"),
                     `Mínimo` = c(0, 0.002738, 0, 0, 0, 1, 1),
                     `Primer cuantil` = c(1.010, 0.219028, 0, 0, 0, 2, 2),
                     `Promedio`= c(1.5, 0.468651, 0.06814, 0.07276, 137.3, 2.674, 3.485),
                     `Tercer cuantil` = c(2.150, 0.709103, 0, 0, 0, 4, 5),
                     `Máximo` = c(34.560, 0.999316, 1, 4, 55922.1, 4, 6),
                     check.names = FALSE)
resumen  %>%
  kbl(caption = "<b><span style='font-size:18px; display:block; text-align:center'>Tabla 1. Resumen Estadístico</span></b>", 
      align = "c", na = "") %>%
  kable_classic_2(full_width = F, html_font = "Cambria") %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "center", font_size = 12) %>%
  row_spec(0, bold = T, font_size = 15)


#Análisis con base al género
pol_by_gen <- as.data.frame(table(dataCar$gender))
colnames(pol_by_gen) <- c("Gender", "Count") 

ggplot(pol_by_gen, aes(x = Gender, y = Count)) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  geom_text(aes(y = Count, label = Count), vjust = 1.35, size = 3, color = "white") +
  labs(title = "Gráfico 1. Distribución por Género", y = "Frecuencia", x = "Género") +
  theme_light() +
  theme(plot.title = element_text(family = "serif", face = "bold", size = 15, 
                                  hjust = 0.6, vjust = 1, lineheight = 1), 
        plot.title.position = "plot")

#Exposición por tipo de vehículo
expo_by_vehb <- dataCar %>%
  group_by(veh_body) %>%
  summarise(exp_tot = sum(exposure)) %>%
  arrange(desc(exp_tot))
head(expo_by_vehb, 5)

ggplot(expo_by_vehb, aes(x = veh_body, y = exp_tot))+ geom_point(color = "plum", size = 2.5)+
  labs(title = "Gráfio 3. Exposición por Tipo de Vehiculo", 
       x = "Tipo de Vehículo", y = "Exposición total")+
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(family = "serif", face = "bold", size = 15, 
                                  hjust = 0.6, vjust = 1, lineheight = 1), 
        plot.title.position = "plot", legend.position = "bottom") 


