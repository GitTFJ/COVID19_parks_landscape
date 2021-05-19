#Add lags
cmb2 = NULL
for(a in unique(cmb$name)){
  tmp = subset(cmb, name == a)
  tmp$ret_la_lg = c(NA,tmp$ret_la[1:nrow(tmp) - 1])
  tmp$food_la_lg = c(NA,tmp$food_la[1:nrow(tmp) - 1])
  tmp$park_la_lg = c(NA,tmp$park_la[1:nrow(tmp) - 1])
  tmp$tran_la_lg = c(NA,tmp$tran_la[1:nrow(tmp) - 1])
  tmp$work_la_lg = c(NA,tmp$work_la[1:nrow(tmp) - 1])
  cmb2 = rbind(cmb2, tmp)
}
cmb2$date = as.Date(cmb2$date)

#Add weather
weth = read.csv("data/temp_rainfall.csv")
weth$date = as.Date(paste(substr(as.character(weth$date), 7, 10),
                            substr(as.character(weth$date), 4, 5),
                            substr(as.character(weth$date), 1, 2), 
                            sep = "-"))
cmb2 = left_join(cmb2, weth)

#Add national cases
nat = read.csv("data/national_cases.csv")
nat$date = as.Date(paste(substr(as.character(nat$date), 7, 10),
                   substr(as.character(nat$date), 4, 5),
                   substr(as.character(nat$date), 1, 2), 
                   sep = "-"))
cmb2 = left_join(cmb2, nat)

cmb2$date_num_p2 = cmb2$date_num^2
cmb2$date_num_p3 = cmb2$date_num^3
cmb2$name_num = as.numeric(as.factor(cmb2$name))
dat = cmb2[,c(34,2,32,33,13:15,5,31,6:11,24:28,16:23,29,30)]
write.csv(dat[which(dat$name_num == 1),c(2,9)], "data/national_cases2.csv")


imp0 <- mice(dat, maxit=0)
predM <- imp0$predictorMatrix
predM[,"name_num"] = c(0, rep(-2,29))
tmp <- mice(dat, m = 10, predictorMatrix = predM, 
            method =  "2l.pan", maxit=20, paniter=20)

plot(tmp)


saveRDS(tmp, "data/produced/imp2.rds")
datalist = list()
datalist[["data_missing"]] = dat
for(a in 1:10) {
  datalist[[paste0("data_imp",a)]] = mice::complete(tmp,a)
}



saveRDS(datalist, "data/produced/imp_data.rds")

dat_join = rbind(
  datalist[[2]], 
  datalist[[3]], 
  datalist[[4]], 
  datalist[[5]], 
  datalist[[6]],
  datalist[[7]],
  datalist[[8]],
  datalist[[9]],
  datalist[[10]],
  datalist[[11]]
  )
dat_join$name_num = as.factor(dat_join$name_num)
dat_join$date_num = as.factor(dat_join$date_num)
dat_join = dat_join %>%
  group_by(name_num, date_num) %>%
  dplyr::summarise(
    ret_m = mean(ret_la),
    ret_l = mean(ret_la) - (1.96*sd(ret_la)/sqrt(length(ret_la))),
    ret_u = mean(ret_la) + (1.96*sd(ret_la)/sqrt(length(ret_la))),
    food_m = mean(food_la),
    food_l = mean(food_la) - (1.96*sd(food_la)/sqrt(length(food_la))),
    food_u = mean(food_la) + (1.96*sd(food_la)/sqrt(length(food_la))),
    park_m = mean(park_la),
    park_l = mean(park_la) - (1.96*sd(park_la)/sqrt(length(park_la))),
    park_u = mean(park_la) + (1.96*sd(park_la)/sqrt(length(park_la))),
    tran_m = mean(tran_la),
    tran_l = mean(tran_la) - (1.96*sd(tran_la)/sqrt(length(tran_la))),
    tran_u = mean(tran_la) + (1.96*sd(tran_la)/sqrt(length(tran_la))),
    work_m = mean(work_la),
    work_l = mean(work_la) - (1.96*sd(work_la)/sqrt(length(work_la))),
    work_u = mean(work_la) + (1.96*sd(work_la)/sqrt(length(work_la)))
  )

dat_join$date_num = as.numeric(dat_join$date_num)


case = ggplot() +
  geom_line(data = cmb2[which(cmb2$name == "Adur"),], aes(x = date_num, y = nat_case)) +
  geom_rect(aes(xmin = 38, xmax = 77, ymin = 0, ymax = Inf), fill = "red", alpha = 0.2) +
  geom_rect(aes(xmin = 260, xmax = 290, ymin = 0, ymax = Inf), fill = "red", alpha = 0.2) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = c(16,46,76,107,138,164,200,230,260), labels = c("March","April","May", "June", "July","August","September", "October", "November"),guide = guide_axis(n.dodge=2)) +
  labs(x = "", y = "Cases", title = "New daily national cases") +
  theme_classic() +
  theme(
    axis.text.x = element_blank()
  )

park = ggplot(dat_join) +
  geom_hline(aes(yintercept = 0), linetype = "dotted", size = 1.5) +
  geom_line(aes(x = date_num, y = park_m, group = name_num), alpha = 0.02) +
  coord_cartesian(ylim = c(-100, 300)) +
  scale_x_continuous(breaks = c(16,46,76,107,138,164,200,230,260), labels = c("March","April","May", "June", "July","August","September", "October", "November"),guide = guide_axis(n.dodge=2)) +
  labs(x = "", y = " ", title = "Parks") +
  theme_classic() +
  theme(
    axis.text.x = element_blank()
  )

groc = ggplot() +
  geom_hline(aes(yintercept = 0), linetype = "dotted", size = 1.5) +
  geom_line(data = dat_join,
            aes(x = date_num, y = food_m, group = name_num), alpha = 0.02) +
  coord_cartesian(ylim = c(-100, 100)) +
  scale_x_continuous(breaks = c(16,46,76,107,138,164,200,230,260), labels = c("March","April","May", "June", "July","August","September", "October", "November"),guide = guide_axis(n.dodge=2)) +
  labs(x = "", y = " ", title = "Grocery & Pharmacy Stores") +
  theme_classic() +
  theme(
    axis.text.x = element_blank()
  )

ret = ggplot() +
  geom_hline(aes(yintercept = 0), linetype = "dotted", size = 1.5) +
  geom_line(data = dat_join,
            aes(x = date_num, y = ret_m, group = name_num), alpha = 0.02) +
  coord_cartesian(ylim = c(-100, 100)) +
  scale_x_continuous(breaks = c(16,46,76,107,138,164,200,230,260), labels = c("March","April","May", "June", "July","August","September", "October", "November"),guide = guide_axis(n.dodge=2)) +
  labs(x = "", y = "Relative mobility (%)", title = "Retail & Recreation") +
  theme_classic() +
  theme(
    axis.text.x = element_blank()
  )

tran = ggplot() +
  geom_hline(aes(yintercept = 0), linetype = "dotted", size = 1.5) +
  geom_line(data = dat_join,
            aes(x = date_num, y = tran_m, group = name_num), alpha = 0.02) +
  coord_cartesian(ylim = c(-100, 100)) +
  scale_x_continuous(breaks = c(16,46,76,107,138,164,200,230,260), labels = c("March","April","May", "June", "July","August","September", "October", "November"),guide = guide_axis(n.dodge=2)) +
  labs(x = "", y = " ", title = "Transport") +
  theme_classic() +
  theme(
    axis.text.x = element_blank()
  )

work = ggplot() +
  geom_hline(aes(yintercept = 0), linetype = "dotted", size = 1.5) +
  geom_line(data = dat_join,
            aes(x = date_num, y = work_m, group = name_num), alpha = 0.02) +
  coord_cartesian(ylim = c(-100, 100)) +
  scale_x_continuous(breaks = c(16,46,76,107,138,164,200,230,260), labels = c("March","April","May", "June", "July","August","September", "October", "November"),guide = guide_axis(n.dodge=2)) +
  labs(x = "", y = " ", title = "Workplaces") +
  theme_classic() 


ggarrange(case, park, groc, ret, tran, work, ncol = 1, nrow = 6, labels = c("a,", "b,", " ", " ", " ", " "))
ggsave("plots/changes_in mobility.png", width = 7, height = 10.5, units = "in", dpi = 400)
cmb$name_num = as.factor(as.numeric(as.factor(cmb$name)))
cmb = left_join(cmb, dat_join[,c(1,2,3,6,9,12,15)])
cmb = left_join(cmb, cmb2[,c(2,3,31,32)])
write.csv(cmb, "data/data_2.csv")


miss = cmb2 %>%
  group_by(date_num) %>%
  dplyr::summarise(
    park_mi = sum(is.na(park_la))/length(unique(name))*100,
    food_mi = sum(is.na(food_la))/length(unique(name))*100,
    tran_mi = sum(is.na(tran_la))/length(unique(name))*100,
    work_mi = sum(is.na(work_la))/length(unique(name))*100,
    ret_mi = sum(is.na(ret_la))/length(unique(name))*100,
  )

park_mi = ggplot(miss) +
  geom_line(aes(x = date_num, y = park_mi)) +
  geom_smooth(aes(x = date_num, y = park_mi)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = c(16,46,76,107,138,164,200,230,260), labels = c("March","April","May", "June", "July","August","September", "October", "November"),guide = guide_axis(n.dodge=2), expand = c(0,0)) +
  coord_cartesian(ylim = c(0,100)) +
  labs(y = "Missing (%)", x = "", title = "Parks") +
  theme_classic()

groc_mi = ggplot(miss) +
  geom_line(aes(x = date_num, y = food_mi)) +
  geom_smooth(aes(x = date_num, y = food_mi)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = c(16,46,76,107,138,164,200,230,260), labels = c("March","April","May", "June", "July","August","September", "October", "November"),guide = guide_axis(n.dodge=2), expand = c(0,0)) +
  coord_cartesian(ylim = c(0,100)) +
  labs(y = "Missing (%)", x = "", title = "Grocery & Pharmacy Stores") +
  theme_classic()

ret_mi = ggplot(miss) +
  geom_line(aes(x = date_num, y = ret_mi)) +
  geom_smooth(aes(x = date_num, y = ret_mi)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = c(16,46,76,107,138,164,200,230,260), labels = c("March","April","May", "June", "July","August","September", "October", "November"),guide = guide_axis(n.dodge=2), expand = c(0,0)) +
  coord_cartesian(ylim = c(0,100)) +
  labs(y = "Missing (%)", x = "", title = "Retail & Recreation") +
  theme_classic()

tran_mi = ggplot(miss) +
  geom_line(aes(x = date_num, y = tran_mi)) +
  geom_smooth(aes(x = date_num, y = tran_mi)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = c(16,46,76,107,138,164,200,230,260), labels = c("March","April","May", "June", "July","August","September", "October", "November"),guide = guide_axis(n.dodge=2), expand = c(0,0)) +
  coord_cartesian(ylim = c(0,100)) +
  labs(y = "Missing (%)", x = "", title = "Transport") +
  theme_classic()

work_mi = ggplot(miss) +
  geom_line(aes(x = date_num, y = work_mi)) +
  geom_smooth(aes(x = date_num, y = work_mi)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = c(16,46,76,107,138,164,200,230,260), labels = c("March","April","May", "June", "July","August","September", "October", "November"),guide = guide_axis(n.dodge=2), expand = c(0,0)) +
  coord_cartesian(ylim = c(0,100)) +
  labs(y = "Missing (%)", x = "", title = "Workplaces") +
  theme_classic()


ggarrange(park_mi, groc_mi, ret_mi, tran_mi, work_mi, ncol = 1, nrow = 5)
ggsave("plots/missing mobility.png", width = 7, height = 9.5, units = "in", dpi = 400)

warrington_park = ggplot() +
  geom_line(data = dat_join[which(dat_join$name_num == 289),], aes(x = date_num, y = park_m)) +
  geom_ribbon(data = dat_join[which(dat_join$name_num == 289),], aes(x = date_num, ymin = park_l, ymax = park_u), alpha = 0.4, fill = "red") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = c(16,46,76,107,138,164,200,230,260), labels = c("March","April","May", "June", "July","August","September", "October", "November"),guide = guide_axis(n.dodge=2), expand = c(0,0)) +
  labs(x = "", y = "Mobility change (%)", title = paste0("Parks - ", round((sum(dat_join[which(dat_join$name_num == 289),"park_l"] != dat_join[which(dat_join$name_num == 289),"park_u"])/290)*100,1), "% missing")) +
  theme_classic()

warrington_groc = ggplot() +
  geom_line(data = dat_join[which(dat_join$name_num == 289),], aes(x = date_num, y = food_m)) +
  geom_ribbon(data = dat_join[which(dat_join$name_num == 289),], aes(x = date_num, ymin = food_l, ymax = food_u), alpha = 0.4, fill = "red") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = c(16,46,76,107,138,164,200,230,260), labels = c("March","April","May", "June", "July","August","September", "October", "November"),guide = guide_axis(n.dodge=2), expand = c(0,0)) +
  labs(x = "", y = "Mobility change (%)", title = paste0("Grocery & Pharmacy Stores - ", round((sum(dat_join[which(dat_join$name_num == 289),"food_l"] != dat_join[which(dat_join$name_num == 289),"food_u"])/290)*100,1), "% missing")) +
  theme_classic()

warrington_ret = ggplot() +
  geom_line(data = dat_join[which(dat_join$name_num == 289),], aes(x = date_num, y = ret_m)) +
  geom_ribbon(data = dat_join[which(dat_join$name_num == 289),], aes(x = date_num, ymin = ret_l, ymax = ret_u), alpha = 0.4, fill = "red") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = c(16,46,76,107,138,164,200,230,260), labels = c("March","April","May", "June", "July","August","September", "October", "November"),guide = guide_axis(n.dodge=2), expand = c(0,0)) +
  labs(x = "", y = "Mobility change (%)", title = paste0("Retail & Recreation - ", round((sum(dat_join[which(dat_join$name_num == 289),"ret_l"] != dat_join[which(dat_join$name_num == 289),"ret_u"])/290)*100,1), "% missing")) +
  theme_classic()

warrington_tran = ggplot() +
  geom_line(data = dat_join[which(dat_join$name_num == 289),], aes(x = date_num, y = tran_m)) +
  geom_ribbon(data = dat_join[which(dat_join$name_num == 289),], aes(x = date_num, ymin = tran_l, ymax = tran_u), alpha = 0.4, fill = "red") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = c(16,46,76,107,138,164,200,230,260), labels = c("March","April","May", "June", "July","August","September", "October", "November"),guide = guide_axis(n.dodge=2), expand = c(0,0)) +
  labs(x = "", y = "Mobility change (%)", title = paste0("Transport - ", round((sum(dat_join[which(dat_join$name_num == 289),"tran_l"] != dat_join[which(dat_join$name_num == 289),"tran_u"])/290)*100,1), "% missing")) +
  theme_classic()

warrington_work = ggplot() +
  geom_line(data = dat_join[which(dat_join$name_num == 289),], aes(x = date_num, y = work_m)) +
  geom_ribbon(data = dat_join[which(dat_join$name_num == 289),], aes(x = date_num, ymin = work_l, ymax = work_u), alpha = 0.4, fill = "red") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = c(16,46,76,107,138,164,200,230,260), labels = c("March","April","May", "June", "July","August","September", "October", "November"),guide = guide_axis(n.dodge=2), expand = c(0,0)) +
  labs(x = "", y = "Mobility change (%)", title = paste0("Workplaces - ", round((sum(dat_join[which(dat_join$name_num == 289),"work_l"] != dat_join[which(dat_join$name_num == 289),"work_u"])/290)*100,1), "% missing")) +
  theme_classic()



ggarrange(warrington_park, warrington_groc, warrington_ret, warrington_tran, warrington_work, ncol = 1, nrow = 5)
ggsave("plots/warrington mobility.png", width = 7, height = 9.5, units = "in", dpi = 400)

rm(list = ls())
