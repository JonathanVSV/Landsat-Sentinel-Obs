#### //////////////////////////////////////////////////////
#### Avaibility of Landsat and Sentinel images in Brasil
#### //////////////////////////////////////////////////////
#### Analyze the table based on medadata obtained by GEE scripts
#### Calculatethe total numberof scenes by path/row
#### calculate scene number by year / sensor, elaborate histogram
#### J-F Mas, CIGA-UNAM jfmas@ciga.unam.mx

# SIRGAS 2000
#
# Projection:         longlat
# EPSG code:          4989 (3D) or 4674 (2D)
# PROJ string:        '+proj=longlat +ellps=GRS80 +towgs84=0,0,0 +no_defs'
# Reference:          http://www.epsg-registry.org/ (unofficial reference)
# Note:               Considered identical to WGS84

### SETTINGS #############################################################################

proj_ibge = "+proj=aea +lat_1=-2 +lat_2=-22 +lat_0=-12 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

### Funtions

substr_inter1a4 <- function(x){
  substr(x, 1, 4)
}

substr_inter6a7 <- function(x){
  substr(x, 6, 7)
}

substr_inter1a10 <- function(x){
  substr(x, 1, 10)
}

substr_inter1a7 <- function(x){
  substr(x, 1, 7)
}

substr_inter28a32 <- function(x){
  substr(x, 28, 32)
}

substr_inter34a37 <- function(x){
  substr(x, 34, 37)
}

# Workspace / packages
library(dplyr)
library(sf)
library(ggplot2)
#library(rgdal)
setwd("/home/Disponbilidade_imagenes")

opcion <- "T1"  # "T2"  Tier 1 o 2

## Read shapes
# Map of States (Entidades federais) from IBGE
Edos <- st_read("shapes/EstadosBR_IBGE_LLWGS84.shp")
# Map of Descending WRS grid (path/row) Landsat
Landsat <- st_read("shapes/WRS2_descending.shp")
head(Landsat)
Landsat$pathrow <- paste(Landsat$PATH,Landsat$ROW,sep="/")

## Select the path/row which coincide with the country of interest
mat <- st_intersects(Landsat,Edos,sparse = T)
intersecta  <- apply(mat, 1, any)
## We discard some of them which correspond to islands or small part of images already covered by adjacent images
## this is based on visual inspection
algunas <- c("6/63","214/68","224/82","221/84","219/78","219/79","219/80","213/63","215/75","208/74")
length(algunas)
length(unique(intersecta))
# Path/row which intersect with the map of Brasil
pathrow_br <- Landsat$pathrow[intersecta]
length(unique(pathrow_br))
pathrow_br <- setdiff(unique(pathrow_br),algunas)

Landsat_br <- Landsat[Landsat$pathrow %in% pathrow_br,]
Landsat_br$pathrow
plot(st_geometry(Landsat_br))
plot(st_geometry(Edos),add=T)
nrow(Landsat_br)  # 380 imagens


## Read the table
if (opcion == "T1") {tab <- read.csv("tablas/LandsatAll_1970-2019_CCL100.csv")} else { 
  tab <- read.csv("tablas/LandsatAllT2_1970-2019_CCL100.csv")}

head(tab)
summary(tab)
## calcule a field path/row
tab$pathrow <- paste(tab$path,tab$row,sep="/")
## Cut to the shape of Edos (because images have been selected with marco_br which is a bit larger)
tab <- tab[tab$pathrow %in% unique(pathrow_br),]
head(tab)
## extract year (ano) and month (mes)
tab$ano <- as.numeric(substr_inter1a4(tab$date))
tab$mes <- as.numeric(substr_inter6a7(tab$date))

# Calculate total scene numberby pathrow y total number by pathrow/sensor
tab_frec <- data.frame(pathrow = sort(unique(tab$pathrow)))
tabL4 <- tab[tab$sensor == "LANDSAT_4",]
frecL4 <- aggregate(sensor ~ pathrow, data = tabL4, FUN = length)
names(frecL4) <- c("pathrow","L4")
tab_frec <- left_join(tab_frec,frecL4,by="pathrow")

tabL5 <- tab[tab$sensor == "LANDSAT_5",]
frecL5 <- aggregate(sensor ~ pathrow, data = tabL5, FUN = length)
names(frecL5) <- c("pathrow","L5")
tab_frec <- left_join(tab_frec,frecL5,by="pathrow")

tabL7 <- tab[tab$sensor == "LANDSAT_7",]
frecL7 <- aggregate(sensor ~ pathrow, data = tabL7, FUN = length)
names(frecL7) <- c("pathrow","L7")
tab_frec <- left_join(tab_frec,frecL7,by="pathrow")

tabL8 <- tab[tab$sensor == "LANDSAT_8",]
frecL8 <- aggregate(sensor ~ pathrow, data = tabL8, FUN = length)
names(frecL8) <- c("pathrow","L8")
tab_frec <- left_join(tab_frec,frecL8,by="pathrow")
tab_frec[is.na(tab_frec)] <- 0
tab_frec$total <- tab_frec$L4 + tab_frec$L5 + tab_frec$L7 + tab_frec$L8

# Calculate total scenes number by pathrow y total number by pathrow/year
for (ano in 1984:2019){ 
tab_ano <- tab[tab$ano == ano,]
frec_ano <- aggregate(sensor ~ pathrow, data = tab_ano, FUN = length)
names(frec_ano) <- c("pathrow",paste("n",ano,sep=""))
tab_frec <- left_join(tab_frec,frec_ano,by="pathrow")
}

head(tab_frec)
if (opcion == "T1") {write.csv(tab_frec, "tablas/tab_num_escenas_por_pathrow_br.csv")} else {
  write.csv(tab_frec, "tablas/tab_num_escenasT2_por_pathrow_br.csv")
}
# paste table to map
Landsat_br <- left_join(Landsat_br,tab_frec,by="pathrow")

# Scenes with cloud <50% Calculate total scenes number by pathrow y toatal number by pathrow/sensor
tab_frec2 <- data.frame(pathrow = sort(unique(tab$pathrow)))
tabL4 <- tab[tab$sensor == "LANDSAT_4" & tab$cloud_cover_land < 50,]
frecL4 <- aggregate(sensor ~ pathrow, data = tabL4, FUN = length)
names(frecL4) <- c("pathrow","L4sn")
tab_frec2 <- left_join(tab_frec2,frecL4,by="pathrow")

tabL5 <- tab[tab$sensor == "LANDSAT_5" & tab$cloud_cover_land < 50,]
frecL5 <- aggregate(sensor ~ pathrow, data = tabL5, FUN = length)
names(frecL5) <- c("pathrow","L5sn")
tab_frec2 <- left_join(tab_frec2,frecL5,by="pathrow")

tabL7 <- tab[tab$sensor == "LANDSAT_7" & tab$cloud_cover_land < 50,]
frecL7 <- aggregate(sensor ~ pathrow, data = tabL7, FUN = length)
names(frecL7) <- c("pathrow","L7sn")
tab_frec2 <- left_join(tab_frec2,frecL7,by="pathrow")

tabL8 <- tab[tab$sensor == "LANDSAT_8" & tab$cloud_cover_land < 50,]
frecL8 <- aggregate(sensor ~ pathrow, data = tabL8, FUN = length)
names(frecL8) <- c("pathrow","L8sn")
tab_frec2 <- left_join(tab_frec2,frecL8,by="pathrow")
tab_frec2[is.na(tab_frec2)] <- 0
tab_frec2$totalsn <- tab_frec2$L4sn + tab_frec2$L5sn + tab_frec2$L7sn + tab_frec2$L8sn
head(tab_frec2)
names(tab_frec2) <- c("pathrow", "L4sn",  "L5sn",  "L7sn",  "L8sn", "totalsn")
if (opcion == "T1") {write.csv(tab_frec2, "tablas/tab_num_escenas_conpocanube_por_pathrow_br.csv")} else {
  write.csv(tab_frec2, "tablas/tab_num_escenasT2_conpocanube_por_pathrow_br.csv")
}

# paste table to map
Landsat_br <- left_join(Landsat_br,tab_frec2,by="pathrow")

if (opcion == "T1") {st_write(Landsat_br, "shapes/Landsat_br.shp")} else 
{st_write(Landsat_br, "shapes/Landsat_brT2.shp")}

# Calculate total scenes number by year and by sensor
por_ano_sensor <- group_by(tab, .dots=c("ano","sensor"))
tab_ano_sensor <- summarise(por_ano_sensor,Num_escenas = n())
tab_ano_sensor <- as.data.frame(tab_ano_sensor)
head(tab_ano_sensor)
if (opcion == "T1") {write.csv(tab_ano_sensor, "tablas/tab_num_escenas_por_ano_por_sensor_br.csv")} else 
{write.csv(tab_ano_sensor, "tablas/tab_num_escenasT2_por_ano_por_sensor_br.csv")}

### bar graph

ggplot(tab_ano_sensor, aes(fill=sensor, y=Num_escenas, x=ano)) + 
  geom_bar(position="stack", stat="identity")


## ///////////////////////////////////////////////////////////////////////////////////  
  
  
###################################################################################
######### SENTINEL

sentinel <- st_read("shapes/sentinel_2_index_shapefile.shp")
names(sentinel)
head(sentinel)
# Name                       geometry
# 1 01CCV POLYGON Z ((180 -73.05974 0...

## Read table obtained by script GEE Sentinel-2_1C_ValidObsJulianD
tab <- read.csv("tablas/Sentinel-2_1C_Metadata_2015-2019_br_5-4-2020.csv")
substr_inter28a32("S2A_MSIL1C_20150801T131806_N0204_R138_T22JHT_20150801T131809")
tab$ano <- lapply(as.character(tab$date),FUN=substr_inter1a4)
tab$anomes <- lapply(as.character(tab$date),FUN=substr_inter1a7)
tab$orbitaN <- as.character(lapply(as.character(tab$ID),FUN=substr_inter28a32))
tab$orbitaR <- as.character(lapply(as.character(tab$ID),FUN=substr_inter34a37))

nrow(tab)  ## 114 copernicis dice 73

## For each tile we have to identify if they correspoden to 2 tracks R and take into account only one of them
## because the tiles located between two orbits have a part without data and there is a larger number of them
names(tab)
class(tab)
por_cenaR <- group_by(tab,.dots=c("mgrsTile","orbitaR"))
NxcenaR <- data.frame(summarise(por_cenaR, N = n()))
por_cenaR2 <- group_by(NxcenaR,.dots=c("mgrsTile"))
Nxcena <- data.frame(summarise(por_cenaR2, N = max(N))) # scenes number by tile
names(Nxcena) <- c("Name","N")

# with less than 50% of clouds
tab <- tab[tab$cloud_pixel_percentage<50,]
por_cenaR <- group_by(tab,.dots=c("mgrsTile","orbitaR"))
NxcenaR <- data.frame(summarise(por_cenaR, N = n()))
por_cenaR2 <- group_by(NxcenaR,.dots=c("mgrsTile"))
Nxcena2 <- data.frame(summarise(por_cenaR2, N = max(N))) # Num de escenas por tiles
names(Nxcena2) <- c("Name","Nsn")

Nsxcena <- left_join(Nxcena,Nxcena2,by="Name")
head(Nsxcena)
write.csv(Nsxcena, "tablas/NsxcenaSentinel_br.csv")
sentinel_br <- merge(sentinel,Nsxcena,by="Name")
plot(sentinel_br)


## selectionate the tiles which coincide with the country of interest
mat <- st_intersects(sentinel_br,Edos,sparse = T)
intersecta  <- apply(mat, 1, any)

length(unique(intersecta))
# Path/row which intersecte with the map of Brasil
sentinel_br <- sentinel_br[intersecta,]
plot(sentinel_br)
class(sentinel_br)
st_write(sentinel_br, "shapes/Sentinel_br.shp")
### ERROR Geometry type of `3D Polygon' not supported in shapefiles
# To fix it I have to remove the 3rd dimension by:
sentinel_br2 <- st_zm(sentinel_br, drop=T, what='ZM')
  st_write(sentinel_br2, "shapes/Sentinel_br.shp")
# Calcula num total de escenas por año y por sensor
tab$ano <- substr_inter1a4(tab$date)

por_ano_sensor <- group_by(tab, .dots=c("ano","sensor"))
tab_ano_sensor <- summarise(por_ano_sensor,Num_escenas = n())
tab_ano_sensor <- as.data.frame(tab_ano_sensor)
head(tab_ano_sensor)
write.csv(tab_ano_sensor, "tablas/tab_num_escenas_por_ano_por_sensor_br_sentinel.csv")
