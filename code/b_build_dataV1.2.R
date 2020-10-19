#Load data
cases = read.csv("data/local_authority_cases.csv")
mobil = read.csv("data/mobility.csv")
gard = read.csv("data/garden.csv")
emply = read.csv("data/unemployed.csv")
size = read.csv("data/local_authority.csv")
grn = read.csv("data/produced/green_sum.csv")
age = read.csv("data/age.csv")
helth = read.csv("data/health.csv")[,c(2,5)]
colnames(helth) = c("name", "health")
popden = read.csv("data/produced/density_cluster.csv")
pop = read.csv("data/pop_size.csv")
park = read.csv("data/parks.csv")

#Extract heirarchal information from gard
hei = gard[,c(3,4,5,6)]

#Produce time series for every local authority from 1st of Jan
cmb = NULL
for(a in unique(hei$LAD.name)){
  print(a)
  tmp_df = data.frame(
    date = seq(as.Date("2020/01/01"), by = "day", length.out = 180),
    date_num = 1:180,
    Name = a)
  cmb = rbind(cmb, tmp_df)
}
cmb = subset(cmb, Name != "")
cmb = left_join(cmb, hei, by = c("Name" = "LAD.name"))



#Clean dates for cases
cases$date = as.Date(paste(
  substr(as.character(cases$Specimen_Date), 7, 10),
  substr(as.character(cases$Specimen_Date), 4, 5),
  substr(as.character(cases$Specimen_Date), 1, 2), 
  sep = "-"))
cmb = left_join(cmb, cases[,c(1,4,5)], by = c("Name", "date"))

#Clean dates for mobility
mobil$date = as.Date(paste(
  substr(as.character(mobil$date), 7, 10),
  substr(as.character(mobil$date), 4, 5),
  substr(as.character(mobil$date), 1, 2), 
  sep = "-"))
cmb = left_join(cmb, mobil[c(2,8:14)], by = c("LAD.code" = "Code", "date"))
cmb = left_join(cmb, mobil[c(2,8:14)], by = c("Region.code" = "Code", "date"))
colnames(cmb) = c(
  "date",
  "date_num",
  "name",
  "region_code",
  "region",
  "la_code",
  "case",
  "ret_la",
  "food_la",
  "park_la",
  "tran_la",
  "work_la",
  "resid_la",
  "ret_reg",
  "food_reg",
  "park_reg",
  "tran_reg",
  "work_reg",
  "resid_reg")

pop$Area.Name = trimws(as.character(pop$Area.Name))
cmb = left_join(cmb, pop, by = c("name" = "Area.Name"))
colnames(cmb)[20] = "pop"
cmb$pop = as.numeric(as.character(gsub(",","",cmb$pop)))
cmb = left_join(cmb, emply, by = c("name" = "la"))
cmb = left_join(cmb, gard[,6:8], by = c("name" = "LAD.name"))



#Claculate key greenenss, urban anfd fragmentation metrics
sum_grn = grn %>%
  group_by(lad17nm) %>%
  dplyr::summarise(green_med = median(green), urb_med = median(urban))
sum_grn$green_perc = (sum_grn$green_med/40000)*100
sum_grn$urb_perc = (sum_grn$urb_med/40000)*100
cmb = left_join(cmb, sum_grn[,c(1,4,5)], by = c("name" = "lad17nm"))
cmb = left_join(cmb, size[,c(2:4, 7:9)], by = c("name" = "lad17nm"))

colnames(cmb)[21:23] = c(
  "unmply_perc",
  "addresses",
  "addresses_garden")
cmb$addresses = as.numeric(as.character(gsub(",","",cmb$addresses)))
cmb$addresses_garden = as.numeric(as.character(gsub(",","",cmb$addresses_garden)))
cmb$perc_address_garden = (cmb$addresses_garden/cmb$addresses)*100


#Add age
age$X2018 = as.numeric(as.character(gsub(",","",age$X2018)))
age$X2019 = as.numeric(as.character(gsub(",","",age$X2019)))
temp = age[which(age$AGE.GROUP != "All ages"),] %>%
  group_by(CODE) %>%
  dplyr::summarise(total = sum(X2019))
temp2 = age[which(age$AGE.GROUP == "All ages"),c(1,5)]
temp = left_join(temp, temp2, by = "CODE")
temp$perc_70plus = (temp$total/temp$X2019)*100
cmb = left_join(cmb, temp[,c(1,4)], by = c("lad17cd" = "CODE"))

#Add health, envir, pop den
cmb = left_join(cmb, helth)
cmb = left_join(cmb, popden[,c(2,4,6,8,9)])
cmb$clust = (cmb$sa_work + cmb$sa_res)/2
cmb$den = (cmb$work_den + cmb$res_den)/2

#Add parks
park2 = park %>%
  group_by(name) %>%
  summarise_at(vars(dist, ave_size, ave_freq_1k, ave_size_1k), funs(mean(., na.rm=TRUE)))
park2 = park2[-1,]
cmb = left_join(cmb, park2[,c(1,4)])

#Tidy

cmb$case[is.na(cmb$case)] = 0
cmb = cmb%>%
  group_by(name) %>%
  filter(sum(case > 0, na.rm = T) > 0)
cmb = cmb[,c(5,3,29,28,30,1,2,7,8:21,24,31:33,38:40)]

write.csv(cmb, "data/produced/data.csv")
rm(list=ls())
