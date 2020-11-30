#### //////////////////////////////////////////////////////
#### Disponibilidade imagens Landsat e Sentinel por pixel 
#### //////////////////////////////////////////////////////
#### Analiza as imagens sobre dados válidos de Landsat por ano e por mês
#### JF Mas CIGA UNAM  jfmas@ciga.unam.mx

### SETTINGS #############################################################################

# ESPACO DE TRABALHO / pacotes
library(raster)
library(sf)
library(doParallel)  #Foreach Parallel Adaptor 
library(foreach)     #Provides foreach looping construct

## raster opcoes
rasterOptions(tmpdir="/tmp")
rasterOptions(maxmemory = 1e+09)
setwd("/home/Disponbilidade_imagenes")
getwd()

## Funcoes
mask <- function(r1,r2){ifelse(is.na(r1),NA,r2)}

# Determina a rota do espaço de trabalho
setwd("/home/Disponbilidade_imagenes")
ruta2 <- "/home/Disponbilidade_imagenes/validObsLandsatbr1800" 
ruta3 <- "/home/Disponbilidade_imagenes/Sentinel-2_BR_1800m" 

## Paralizacao
#Define how many cores you want to use
UseCores <- detectCores() -1
#Register CoreCluster
cl <- makeCluster(UseCores)
registerDoParallel(cl)


################################################################################################
#### LANDSAT ###################################################################################
################################################################################################


############### dados anuais ###########################################
biomar <-raster(paste(ruta2,"/biomar1800.tif",sep=""))
plot(biomar)
mask <- biomar
mask[mask>=0]<-1
plot(mask)

rasters <- list()
ano <- 2019
i <- 0
## periodo
t1 <- 2014  #1984
t2 <- 2019
#foreach(ano=2015:2019) %dopar% {
 for (ano in t1:t2){
  i <- i + 1     
  mapano <- paste(ruta2,"/L4-8_SR_T1_MXjDayVObs_MonthStack",ano,"_1800m.tif",sep="")
  Ntotano <- sum(stack(mapano))
  Ntotano <- mask*Ntotano
  plot(Ntotano)
  print(maxValue(Ntotano))
  #writeRaster(Ntotano, filename=paste(ruta2,"/Landsat_Nvalsum",ano,".tif",sep=""), datatype='INT2U', format="GTiff", overwrite=TRUE)
  rasters[[i]] <- Ntotano
}

stac <- stack(rasters)
sum <- sum(stac)
plot(sum)
print(maxValue(sum))
writeRaster(sum, filename=paste(ruta2,"/Landsat_Nvalsum",t1,"-",t2,".tif",sep=""), datatype='INT2U', format="GTiff", overwrite=TRUE)

############### dados mensais ###########################################
rasters <- list()
mes <- 1
i <- 0
t1 <- 1984  #1984
t2 <- 2019
#
for (mes in 1:12) {
  i <- 0
  for (ano in t1:t2) { 
  i <- i + 1
  mapano <- paste(ruta2,"/L4-8_SR_T1_MXjDayVObs_MonthStack",ano,"_1800m.tif",sep="")
  rasters[[i]] <- raster(mapano, band = mes)*mask
} # sobre anos
  stac <- stack(rasters)
  sum <- sum(stac)/length(rasters)
# writeRaster(sum, filename=paste(ruta2, "/Landsat_Nvalsum_2018-9mes",mes,".tif",sep=""), datatype='FLT4S', format="GTiff", overwrite=TRUE)
writeRaster(sum, filename=paste(ruta2, "/Landsat_Nvalsum_mes",t1,"-",t2,"-",mes,".tif",sep=""), datatype='FLT4S', format="GTiff", overwrite=TRUE)
plot(sum, main=mes)
} # sobre meses



### operacao adicional para checar o total de observacoes em 2018-19

NL1819 <- raster("/home/Disponbilidade_imagenes/validObsLandsatbr1800/Landsat_Nvalsum2018-2019.tif")
plot(NL1819)
cellStats(NL1819, mean) # 26.8



################################################################################################
#### SENTINEL ##################################################################################
################################################################################################

############### dados anuais ###########################################
# 2018 y 2019 sao os 2 anos completos com os 2 satelites em funcionamento (Sentinel 2A e B)
rasters <- list()
ano <- 2019
## periodo
t1 <- 2015  #2018
t2 <- 2019
i <- 0
#foreach(ano=2015:2019) %dopar% {
for (ano in t1:t2){
  i <- i + 1     
  mapano <- stack(paste(ruta3,"/S2_1C_MXjDayVObs_MonthStack",ano,"_1800m.tif",sep=""))
  #plot(mapano, main=ano)
  rasters[[i]] <- sum(stack(mapano))*mask
}

stac <- brick(rasters)
plot(stac)
plot(stac[[2]])
sum <- sum(stac, na.rm=TRUE)
sum <- calc(stac, sum)
overlay(s1, s2, fun=function(x) sum(x, na.rm=TRUE))
plot(sum)

writeRaster(sum, filename=paste(ruta3,"/Sentinel_Nvalsum_",t1,"-",t2,".tif",sep=""), datatype='INT2U', format="GTiff", overwrite=TRUE)

############### dados mensais ###########################################
# visualiza os dados
ano <- 2018
mapano <- paste(ruta3,"/S2_1C_MXjDayVObs_MonthStack",ano,"_1800m.tif",sep="")
for (mes in 1:12) {
  plot(raster(mapano, band = mes))
} # sobre meses



## Calcula o numero medio de obs válidas / mes
rasters <- list()
mes <- 1
i <- 0
#
## periodo
t1 <- 2018  #1984
t2 <- 2019

for (mes in 1:12) {
  i <- 0
  for (ano in t1:t2) { 
    i <- i + 1
    mapano <- paste(ruta3,"/S2_1C_MXjDayVObs_MonthStack",ano,"_1800m.tif",sep="")
    rasters[[i]] <- raster(mapano, band = mes)*mask
  } # sobre anos
  stac <- stack(rasters)
  sum <- sum(stac)/length(rasters)
  writeRaster(sum, filename=paste(ruta3, "/Sentinel_Nvalsum_mes",t1,"-",t2,"-",mes,".tif",sep=""), datatype='FLT4', format="GTiff", overwrite=TRUE)
  plot(sum)
} # sobre meses

### operacao adicional para checar o total de observacoes em 2018-19

NS1819 <- raster("/home/Disponbilidade_imagenes/Sentinel-2_BR_1800m/Sentinel_Nvalsum_2018-2019.tif")
plot(NS1819)
cellStats(NS1819, mean) # 26.8


#### Para fazer umas estatísticas
### Landsat
t1 <- 1984  #1984
t2 <- 2019
sumL <- raster(paste(ruta2,"/Landsat_Nvalsum",t1,"-",t2,".tif",sep=""))
sum(sumL)
plot(sumL)
res(sumL)
cellStats(sumL,mean)  # num de obs válida media
cellStats(sumL,sum)*1.800*1.800/1000000  # 3605 milloes de km2 tomadas con inf válida

### Sentinel
t1 <- 2015  #1984
t2 <- 2019
sumL <- raster(paste(ruta3, "/Sentinel_Nvalsum_",t1,"-",t2,".tif",sep=""))  
sumL <- raster(paste(ruta3, "/Sentinel_Nvalsum_2015-19.tif",sep="")) 
sum(sumL)
plot(sumL)
res(sumL)
cellStats(sumL,mean)  # num de obs válida media 112
cellStats(sumL,sum)*1.800*1.800/1000000  # 990 miloes de km2 tomadas com inf válida



###############################################
## soma total de obs mensais sobre 1984:2019 totais anuais


ruta2 <- "/home/Disponbilidade_imagenes/validObsLandsatbr600.pormes" 

foreach(ano=1984:2019) %dopar% {
padrao <- paste("LandsatMonth_*_",ano,"*",sep="")
lista <- list.files(path = ruta2, pattern = glob2rx(padrao), full.names=TRUE)
library(raster)

#creates unique filepath for temp directory
dir.create (file.path(paste("/tmp/temp",ano,sep="")), showWarnings = FALSE)

#sets temp directory
rasterOptions(tmpdir=paste("/tmp/temp",ano,sep=""))

rasters <- lapply(lista, raster)
stac <- stack(rasters)
sum <- sum(stac)
#plot(sum)
sum <- overlay(biomar,sum,fun=mask)
writeRaster(sum, filename=paste("Landsat_",area,"/NvalYeartot",ano,".tif",sep=""), datatype='INT2U', format="GTiff", overwrite=TRUE)
unlink(file.path(paste("/tmp/temp",ano,sep="")), recursive = TRUE)
}  # loop sobre año

###############################################
## soma total de obs mensais sobre 1984:2019 totais mensais


ruta2 <- "/home/Disponbilidade_imagenes/validObsLandsatbr600.pormes" 

foreach(mes=1:12) %dopar% {
  padrao <- paste("LandsatMonth_",mes,"_*",sep="")
  lista <- list.files(path = ruta2, pattern = glob2rx(padrao), full.names=TRUE)
  library(raster)
  
  #creates unique filepath for temp directory
  dir.create (file.path(paste("/tmp/temp",mes,sep="")), showWarnings = FALSE)
  
  #sets temp directory
  rasterOptions(tmpdir=paste("/tmp/temp",mes,sep=""))
  
  rasters <- lapply(lista, raster)
  stac <- stack(rasters)
  sum <- sum(stac)
  sum <- overlay(biomar,sum,fun=mask)
  #plot(sum)
  writeRaster(sum, filename=paste("Landsat_",area,"/NvalMonthtot",mes,".tif",sep=""), datatype='INT2U', format="GTiff", overwrite=TRUE)
  unlink(file.path(paste("/tmp/temp",mes,sep="")), recursive = TRUE)
}  # loop sobre año




########## Para checar somamos os mensais
## soma total de obs val sobre 1984:2019
padrao <- "*Nval_mes*.tif"
lista <- list.files(path = "Landsat_Brasil600", pattern = glob2rx(padrao), full.names=TRUE)

rasters <- lapply(lista, raster)
stac <- stack(rasters)
sum <- sum(stac)
plot(sum)
writeRaster(sum, filename=paste("Landsat_",area,"/Nvalssum.tif",sep=""), datatype='INT2U', format="GTiff", overwrite=TRUE)

## soma total de obs val sobre 1984:2019
padrao <- "*Ntot_mes*.tif"
lista <- list.files(path = "Landsat_Brasil600", pattern = glob2rx(padrao), full.names=TRUE)

rasters <- lapply(lista, raster)

stac <- stack(rasters)
sum <- sum(stac)
plot(sum)
writeRaster(sum, filename=paste("Landsat_",area,"/Ntotmessum.tif",sep=""), datatype='INT2U', format="GTiff", overwrite=TRUE)

# ## soma total de obs sobre 1984:2019 (disco fue cheio se se faz em uma operacao só)
# padrao <- "*Nval*.tif"
# lista <- list.files(path = "Landsat_Brasil600", pattern = glob2rx(padrao), full.names=TRUE)
# # 1 6 11 16 21 26 31 36
# registerDoParallel(cl)
# foreach(l=c(1,6,11,16)) %dopar% {
# library(raster)
# rasters <- lapply(lista[l:(l+5)], raster)
# #rasters <- do.call(raster::raster, lista[l:(l+5)])
# # Then, use the Reduce function with the accumulate argument set to TRUE
# sum <- Reduce("+", rasters, accumulate = TRUE)
# writeRaster(sum, filename=paste("Landsat_Brasil/sum",l,".tif",sep=""), datatype='INT2U', format="GTiff", overwrite=TRUE)
# }
# plot(sum)



######### PARTE II Imagens por mes
## Calculo da media de observacoes válidas por mes por bioma
## zonal(mapa,zonas,"mean"), dados  para la elaboracao de heatmaps

bioma <- st_read("shapes/bioma.shp")
st_tr
biomar <- rasterize(bioma,sum,"NOME")

writeRaster(biomar, filename=paste("Landsat_Brasil600/biomar600.tif",sep=""), datatype='INT1U', overwrite=TRUE)
biomar <- raster("Landsat_Brasil600/biomar600.tif")
# dirty
crs(biomar) <- crs(Nval)

z <- matrix(ncol=1,nrow=6,1:6)
for (mes in 1:12){
Nval <- raster(paste("Landsat_",area,"/Nval_mes",mes,".tif",sep=""))

znew <- zonal(Nval,biomar,"mean")
z <- cbind(z,znew[,2])
print(z)
}


extension <- extent(-63,-62,-6,-4)
disp2019 <- stack("Landsat_Brasil/LandsatYear_2019_120m-0000000000-0000000000.tif" )
summary(disp2019[[1]])
Nval2019 <- crop(raster("Landsat_Brasil/LandsatYear_2019_120m-0000000000-0000000000.tif",4 ),extension)
Ntot2019 <- crop(raster("Landsat_Brasil/LandsatYear_2019_120m-0000000000-0000000000.tif",5 ),extension)
Pval2019 <- crop(raster("Landsat_Brasil/LandsatYear_2019_120m-0000000000-0000000000.tif",6 ),extension)

plot(Pval2019,axes=T)
crop(raster("LandsatYear_2019_120m-0000000000-0000000000.tif",6 ),extension)
