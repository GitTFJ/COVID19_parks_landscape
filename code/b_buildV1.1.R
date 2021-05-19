#Load data
cases = read.csv("data/case.csv")
mobil = read.csv("data/assorted/2020_GB_Region_Mobility_Report.csv")
mobil_link = read.csv("data/assorted/match_mobility_locations.csv")

#Produce time series for every local authority from 1st of Jan
cmb = NULL
term = unique(cases[,c(1,2)])
for(a in 1:380){
  print(a)
  tmp_df = data.frame(
    date = seq(as.Date("2020/02/15"), by = "day", length.out = 290),
    date_num = 1:290,
    code = term[a,1],
    name = term[a,2])
  cmb = rbind(cmb, tmp_df)
}

cases$date = as.Date(paste(substr(as.character(cases$date), 7, 10),
                            substr(as.character(cases$date), 4, 5),
                            substr(as.character(cases$date), 1, 2), 
                            sep = "-"))
cmb = left_join(cmb, cases[,c(1,3,4)], by = c("code", "date"))

#Clean dates for mobility
mobil$region = mobil$sub_region_2
mobil$region = ifelse(mobil$region == "", mobil$sub_region_1, mobil$region)
mobil = left_join(mobil, mobil_link, by = c("region" = "name"))
mobil$date = as.Date(paste(
  substr(as.character(mobil$date), 7, 10),
  substr(as.character(mobil$date), 4, 5),
  substr(as.character(mobil$date), 1, 2), 
  sep = "-"))
cmb = left_join(cmb, mobil[c(9:15,17)], by = c("code", "date"))
colnames(cmb) = c(
  "date",
  "date_num",
  "la_code",
  "name",
  "case",
  "ret_la",
  "food_la",
  "park_la",
  "tran_la",
  "work_la",
  "resid_la")
cmb$case = ifelse(is.na(cmb$case), 0, cmb$case)
cmb$country = substr(cmb$la_code, 1, 1)
cmb = subset(cmb, country == "E")
cmb$country = NULL

### Add day of the week as continuos scale
week = data.frame( # Monday = 1
  date_num = 1:290,
  day = c(6,7,rep(1:7,41), 1)
)
cmb<-left_join(cmb,week,by="date_num") # join on
cmb$weekend = ifelse(cmb$day < 6, "no", "yes")

#Calculate local authority population density
la = read.csv("data/la_info.csv")
pop = read.csv("data/pop_size.csv")
colnames(pop) = c("name", "pop")
pop$name = trimws(as.character(pop$name))
pop$pop = as.numeric(pop$pop)
cmb = left_join(cmb, la[,c(3, 7:9)], by = c("name" = "lad17nm"))
cmb = left_join(cmb, pop)
cmb$pop_density = (cmb$pop*1000)/(cmb$st_areashape/100000)
rm(la,pop)

#Population clustering
popden = read.csv("data/produced/popden.csv")[,c(2,4)]
colnames(popden)[2] = "clust"
cmb = left_join(cmb, popden)
rm(popden)


#Unemployment
emply = read.csv("data/unemployed.csv")
cmb = left_join(cmb, emply, by = c("name" = "la"))
rm(emply)

#Health
helth = read.csv("data/health.csv")[,c(2,5)]
colnames(helth) = c("name", "health")
cmb = left_join(cmb, helth)
rm(helth)

#Age
age = read.csv("data/age.csv")
age$X2018 = as.numeric(as.character(gsub(",","",age$X2018)))
age$X2019 = as.numeric(as.character(gsub(",","",age$X2019)))
temp = age[which(age$AGE.GROUP != "All ages"),] %>%
  group_by(CODE) %>%
  dplyr::summarise(total = sum(X2019))
temp2 = age[which(age$AGE.GROUP == "All ages"),c(1,5)]
temp = left_join(temp, temp2, by = "CODE")
temp$perc_70plus = (temp$total/temp$X2019)*100
cmb = left_join(cmb, temp[,c(1,4)], by = c("la_code" = "CODE"))
rm(age, temp, temp2)

#Greenness
grn = read.csv("data/produced/green_sum.csv")
grn$green_perc = (grn$green/grn$total)*100
cmb = left_join(cmb, grn[,c(2,6)], by = c("name" = "lad17nm"))
cmb$green_m_pp = (((cmb$green_perc/100)*cmb$st_areashape))/(cmb$pop*1000)
cmb$st_areashape = NULL
cmb$pop = NULL
cmb$green_perc = NULL
rm(grn)

#Gardens
gard = read.csv("data/garden.csv")[,c(5,10)]
colnames(gard) = c("la_code", "perc_address_garden")
gard$perc_address_garden = as.numeric(substr(gard$perc_address_garden,1,nchar(gard$perc_address_garden)-1))
cmb = left_join(cmb, gard)
rm(gard)

#Park access
park = read.csv("data/parks.csv")
park2 = park %>%
  group_by(name) %>%
  summarise_at(vars(ave_freq_1k), funs(mean(., na.rm=TRUE)))
cmb = left_join(cmb, park2)
rm(park, park2)







