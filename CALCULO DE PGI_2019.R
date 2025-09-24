#PGI - 2019  -------------------------------------------------------------------------------------------
library(EconGeo)

## generate a region - industry matrix
set.seed(31)
mat <- matrix(sample(0:100, 20, replace = TRUE), ncol = 4)
rownames(mat) <- c("R1", "R2", "R3", "R4", "R5")
colnames(mat) <- c("I1", "I2", "I3", "I4")

## a vector of GDP of regions
vec <- c(5, 10, 15, 25, 50)
## run the function
prody(mat, vec)

###########################################################

# Proyecto BID CONO SUR --PRODUCTOS A 6 DIGITOS

install.packages("haven")
library(haven)
library(dplyr)
library(EconGeo)
datos1<-read_dta("country_hsproduct6digit_year-001.dta")
View(datos1)

# filtrando Año 2019
datos2019 = datos1 [datos1$year == "2019" ,]
write.csv(datos2019, file = "datos2019_6DIG.csv") 
# match entre exportaciones y gini
green_products = read.csv("GINI_2019_F.csv")
all_products = read.csv("datos2019_6DIG.csv")
merged.full.1 <-  merge(green_products, all_products, by="location_code", all.x= T)
write.csv(merged.full.1, file = "exportaciones2019_match.csv")

#Similarly to Hartmann et al. (2017), countries with an average export value under 1 billion
#dollars were excluded from the analysis to avoid taking into account small countries. T

ExportByCountry = merged.full.1 %>%                                  
  group_by(location_code) %>% 
  dplyr::summarise(suma_exports  = sum(export_value, na.rm = TRUE),count= n())
write.csv(ExportByCountry, file = "ExportByCountry.csv")

#importar vectores
EL <- read.csv("exportadores.csv")
MM = get_matrix(EL)
class(MM)
head (MM[,1:10])
dim (MM)
write.csv(MM, file = "MatrizExportadores.csv")

# match entre paises ordenados
green_products = read.csv("GINI_2019_F.csv")
all_products = read.csv("id_orden.csv")
merged.full.1 <-  merge(all_products, green_products, by="location_code", all.x= T)
write.csv(merged.full.1, file = "ordendelgini.csv")

# cargar vector de gini
vectoriza <- c(30.1, 42.9, 30, 30.2, 27.2, 40.3, 25.3, 41.6, 53.5, 31.7, 38.2, 51.3, 48.2, 31.2, 25.3, 31.7, 27.7, 41.9, 45.7, 31.9, 34.3, 30.8, 27.7, 30.7, 31.2, 32.8, 35.9, 33.1, 48.2, 28.9, 30, 37.6, 35, 30.8, 40.9, 34.6, 29.7, 37.7, 35.3, 34.2, 34.5, 26, 29.3, 35.5, 33.5, 31, 50.5, 38.5, 29.2, 27.7, 49.8, 41.6, 28.8, 32.8, 45.7, 34.8, 37.7, 38.8, 34.5, 23.2, 24.4, 29.3, 34.9, 41.9, 42.7, 26.6, 39.7, 41.5, 50.3)
vec <- as.data.frame(vectoriza)
class(vec)
dim(vec)
vectorfinal = (t(vec))
dim(vectorfinal)
matriz = as.matrix(
  read.csv("MatrizExportadores2.csv", 
           sep = ",", 
           header = T, 
           row.names = 1))
write.csv(matriz)

dim (matriz)
#Step 3 
p1 <- prody(matriz, vectorfinal)
p2 <- as.data.frame(t(p1))
write.csv(p2, "PGI_2019calculada.csv") 

