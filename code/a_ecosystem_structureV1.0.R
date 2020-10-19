#Load packages
library(raster)
library(landscapemetrics)
library(sf)
library(sp)
library(rgdal)
library(dplyr)
library(rgeos)
library(dplyr)
library(mice)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(segmented)
library(forecast)
library(nlme)
library(lme4)
library(car)
library(lattice)
library(MASS)
library(ggeffects)
library(splines)
library(piecewiseSEM)

#Load land cover raster
lcm = raster("data/lcm2015gb25m.tif")      
crs(lcm) 

#Load local authortiy polygons
la <- st_read("data/Local_Authority_Districts__December_2017__Boundaries_in_Great_Britain.shp")
crs(la)
class(la)

#Transform local auhtority polygons to same projection as the land cover raster
la_proj <-st_transform(la, crs(lcm))

#Identify 10 random coordinates in each local authority polygon
point_n = 10
crds_comb = NULL
for(a in 1:nrow(la_proj)){
  crds = as.data.frame(matrix(unlist(st_sample(la_proj[a, ], point_n)), nrow = point_n, byrow = T))
  crds$lad17nm = la_proj$lad17nm[a]
  crds$run = 1:point_n
  crds_comb = rbind(crds_comb, crds)
}
colnames(crds_comb)[1:2] = c("lon", "lat") 
spdf <- SpatialPointsDataFrame(coords = crds_comb[,1:2],
                               data = data.frame(lad17nm = crds_comb[,3]), 
                               proj4string = crs(lcm))

#Sample greenness, urbanness and fragmentation of greenness
met = 2500
green_cnt = NULL
for(b in 1:nrow(crds_comb)){
  print(b)
  buff = gBuffer(spdf[b,], width = 2500, capStyle="SQUARE")
  trim = crop(lcm, buff)
  green = trim
  green_cnt_tmp = data.frame(
    total = sum(table(green[!is.na(green)])),
    green = sum(table(green[
      green == 1 | 
        green == 2 |
        green == 4 |
        green == 5 |
        green == 6 |
        green == 7 |
        green == 8 |
        green == 9 |
        green == 10 |
        green == 11])),
    urban = sum(table(green[green > 19])))
  green[
    green == 1 | 
      green == 2 |
      green == 4 |
      green == 5 |
      green == 6 |
      green == 7 |
      green == 8 |
      green == 9 |
      green == 10 |
      green == 11] <- 1
  green[
    green == 3 | 
      green == 12 |
      green == 13 |
      green == 14 |
      green == 15 |
      green == 16 |
      green == 17 |
      green == 18 |
      green == 19 |
      green == 20 |
      green == 21 ] <- 0
  green_cnt = rbind(green_cnt, green_cnt_tmp)
}
crds_comb = cbind(crds_comb, green_cnt)
dir.create("data/produced")
dir.create("plots")
#save greenness to newly produced directory. 
write.csv(crds_comb, "data/produced/green_sum.csv")




