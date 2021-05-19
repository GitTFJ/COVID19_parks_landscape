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
library(tidyverse)
library(mgcv)
library(zoo)
library(geoR)
library(ape)
library(mgcv.helper)


#Load land cover raster
pd_res = raster("data/pop_density.asc")
crs(pd_res) 
res(pd_res)
lcm = raster("data/land_cover.tif")      
crs(lcm) 
res(lcm)


#Load local authortiy polygons
la <- st_read("data/la_boundary.shp")
crs(la)
class(la)

#Transform local auhtority polygons to same projection as the land cover raster
la_proj <-st_transform(la, crs(lcm))
la_proj = la_proj[c(1:326),] #keep only England



green_cnt = NULL
popden = NULL
for(b in 1:nrow(la_proj)){
  print(b)
  buf = st_buffer(la_proj[b,], dist = 1000)
  green = crop(lcm, buf)
  green_cnt_tmp = data.frame(
    lad17nm = la_proj$lad17nm[b],
    total = length((green[!is.na(green)])),
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
  green_cnt = rbind(green_cnt, green_cnt_tmp)
  trim_pd_res = crop(pd_res, la_proj[b,])
  tmp_df = data.frame(
    name = la_proj$lad17nm[b],
    pd_res = mean(values(trim_pd_res), na.rm = T),
    sa_res = Moran(trim_pd_res),
    N = length(values(trim_pd_res)))
  popden = rbind(popden, tmp_df)
}
dir.create("data/produced")
dir.create("plots")
#save greenness to newly produced directory. 
write.csv(green_cnt, "data/produced/green_sum.csv")
write.csv(popden, "data/produced/popden.csv")
rm(list = ls())


