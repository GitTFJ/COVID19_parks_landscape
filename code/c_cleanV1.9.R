is.nan.data.frame <- function(x){
  do.call(cbind, lapply(x, is.nan))
}

ihs <- function(x) {
  y <- log(x + sqrt(x ^ 2 + 1))
  return(y)
}


cmb = read.csv("data/produced/data.csv")


adjusteddataframe<- NULL
for (j in unique(cmb$name)) {
  thisdat<- filter(cmb,name== j)
  thisdat$case_check = ifelse(thisdat$case == 0,0,1)
  if(sum(is.na(thisdat$case)) > 1){
    message(paste0("skipping ", j))
  } else {
    consec <- c()
    for(i in 1:(nrow(thisdat)-6)){
      dt<-thisdat[i:(i+6),]
      conday<-sum(dt$case_check)
      consec<-c(consec,conday)
    }
    
    simpcon<-ifelse(consec > 5,1,0)
    if(sum(simpcon)< 1){
      message(paste0("skipping ", j))
    } else {
      ifelse(any(simpcon[match(1,simpcon):length(simpcon)] ==0),
             thisdat2<-thisdat[match(1,simpcon):(match(1,simpcon)+match(0,simpcon[match(1,simpcon):length(simpcon)])),],
             thisdat2<-thisdat[match(1,simpcon):length(simpcon),])
      
      adjusteddataframe<-rbind(adjusteddataframe,thisdat2)
    }
  }
}
adjusteddataframe$X = NULL
adjusteddataframe = adjusteddataframe[complete.cases(adjusteddataframe[,c(5)]),]
# data match names 
join_df = NULL
for (i in unique(adjusteddataframe$name)){
  print(i)
  trim <- filter(adjusteddataframe, name==i)
  trim2 = subset(trim, case != 0)
  if(nrow(trim2[1:which.max(trim2$case),]) > 4 & nrow(trim2[which.max(trim2$case):nrow(trim2),]) > 4){
  trim$relmob = rowMeans(trim[c(9:14)], na.rm = T)
  trim$relmob_reg = rowMeans(trim[c(15:20)], na.rm = T)
  pre_p = lm(log(case + 1) ~ date_num, data = trim[1:which.max(trim$case),])
  pos_p = lm(log(case + 1) ~ date_num, data = trim[which.max(trim$case):nrow(trim),])
  pre_p_miss = lm(log(case) ~ date_num, data = trim2[1:which.max(trim$case),])
  pos_p_miss = lm(log(case) ~ date_num, data = trim2[which.max(trim$case):nrow(trim),])
  temp_df = data.frame(
      reg = trim$region[1],
      la = i,
      slope1 = coef(pre_p)[2],
      slope1_miss = coef(pre_p_miss)[2],
      var1 = summary(pre_p)$coefficients[2,2]**2,
      var1_miss = summary(pre_p_miss)$coefficients[2,2]**2,
      n1 = nrow(trim[1:which.max(trim$case),]),
      slope2 = coef(pos_p)[2],
      slope2_miss = coef(pos_p_miss)[2],
      var2 = summary(pos_p)$coefficients[2,2]**2,
      var2_miss = summary(pos_p_miss)$coefficients[2,2]**2,
      n2 = nrow(trim[which.max(trim$case):nrow(trim),]),
      start_date = min(trim$date_num),
      breakpoint = trim[which.max(trim$case),]$date_num,
      end_date = max(trim$date_num),
      tot_case = sum(trim$case),
      max_case = max(trim$case),
      meanrelpark1 = mean(trim[1:which.max(trim$case),]$park_la, na.rm = T),
      meanrelmob1 = mean(trim[1:which.max(trim$case),]$relmob, na.rm = T),
      meanrelpark2 = mean(trim[which.max(trim$case):nrow(trim),]$park_la, na.rm = T),
      meanrelmob2 = mean(trim[which.max(trim$case):nrow(trim),]$relmob, na.rm = T),
      meanrelpark1_r = mean(trim[1:which.max(trim$case),]$park_reg, na.rm = T),
      meanrelmob1_r = mean(trim[1:which.max(trim$case),]$relmob_reg, na.rm = T),
      meanrelpark2_r = mean(trim[which.max(trim$case):nrow(trim),]$park_reg, na.rm = T),
      meanrelmob2_r = mean(trim[which.max(trim$case):nrow(trim),]$relmob_reg, na.rm = T),
      unemply_perc = mean(trim$unmply_perc, na.rm = T),
      address_garden_perc = mean(trim$perc_address_garden, na.rm = T),
      green_km_pp = (mean(trim$green_perc, na.rm = T)/100)*mean(trim$st_areashape, na.rm = T)/(mean(trim$pop, na.rm = T)*1000),
      ave_freq_1k = mean(trim$ave_freq_1k, na.rm = T),
      pop_density_km = mean(trim$den, na.rm = T),
      pop_clust = mean(trim$clust, na.rm = T),
      old = mean(trim$perc_70plus, na.rm = T),
      health = mean(trim$health, na.rm = T),
      lt = mean(trim$lat, na.rm = T),
      ln = mean(trim$long, na.rm = T))
    join_df = rbind(join_df, temp_df)
  } else {
    print("skipping")
  }
}

join_df[is.nan(join_df)] = NA

tmp_df = join_df
tmp_df$meanrelpark1_imp = ifelse(is.na(tmp_df$meanrelpark1), tmp_df$meanrelpark1_r, tmp_df$meanrelpark1)
tmp_df$meanrelmob1_imp = ifelse(is.na(tmp_df$meanrelmob1) | is.na(tmp_df$meanrelpark1), tmp_df$meanrelmob1_r, tmp_df$meanrelmob1)
tmp_df$meanrelpark2_imp = ifelse(is.na(tmp_df$meanrelpark2), tmp_df$meanrelpark2_r, tmp_df$meanrelpark2)
tmp_df$meanrelmob2_imp = ifelse(is.na(tmp_df$meanrelmob2)| is.na(tmp_df$meanrelpark2), tmp_df$meanrelmob2_r, tmp_df$meanrelmob2)

tmp_df = subset(tmp_df, 
                  slope1 > 0 &
                  slope2 < 0)


#Transmission models
tmp_df$pop_den_log = log10(tmp_df$pop_density_km)
tmp_df$la = as.character(tmp_df$la)
tmp_df = tmp_df[complete.cases(tmp_df[,c(1,3,5,8,10,13:17,26:40)]),]
tmp_df$slope1 = (exp(tmp_df$slope1)-1)*100
tmp_df$slope2 = (exp(tmp_df$slope2)-1)*100

green_slp1_base = lme(
  slope1 ~
    scale(pop_den_log) +
    scale(pop_clust) +
    scale(meanrelmob1_imp) +
    scale(health) +
    scale(old) + 
    scale(unemply_perc) +
    scale(pop_den_log)*scale(pop_clust) +
    scale(pop_den_log)*scale(meanrelmob1_imp) +
    scale(pop_clust)*scale(meanrelmob1_imp) 
  ,  
  random = ~ 1| reg,
  correlation=corExp(form=~lt+ln,nugget=T),
  weights = varFixed(~ var1),  
  data=tmp_df,
  method = "ML")
summary(green_slp1_base)
intervals(green_slp1_base, which = "fixed")
qqnorm(green_slp1_base, abline = c(0,1))
plot(green_slp1_base)
vif(green_slp1_base)



green_slp1_sim = lme(
  slope1 ~
    scale(pop_den_log) +
    scale(pop_clust) +
    scale(meanrelmob1_imp) +
    scale(pop_den_log)*scale(pop_clust) +
    scale(pop_den_log)*scale(meanrelmob1_imp) +
    scale(pop_clust)*scale(meanrelmob1_imp) 
  ,  
  random = ~ 1| reg,
  correlation=corExp(form=~lt+ln,nugget=T),
  weights = varFixed(~ var1),  
  data=tmp_df,
  method = "ML")
intervals(green_slp1_sim, which = "fixed")


mydf <- ggpredict(green_slp1_base, terms = c("meanrelmob1_imp [-50:0]"))
ggplot(mydf, aes(x, predicted)) +
  geom_line(aes(group = group), size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .15) +
  labs(x = expression ("Pre-peak mobility change (%)") , y = "Pre-peak case rate (%)") +
  scale_y_continuous(breaks = c(0,10,20,30)) +
  coord_cartesian(ylim = c(0,30)) +
  theme_classic() +
  theme(
    text = element_text(size=16),
    legend.title =  element_text(size=11.5),
    legend.text =  element_text(size=11, face="italic"),
    legend.position = "top"
  )
ggsave(file="Graphs/trans_slp1_mob.png", width = 90, height = 80, dpi = 600, units = "mm", device='png') 

mydf <- ggpredict(green_slp1_base,  terms = c("meanrelmob1_imp [-50:0]", "pop_clust [0.35,0.7]"))
a = ggplot(mydf, aes(x, predicted)) +
  geom_line(aes(group = group, colour = group), size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = .15) +
  labs(x = expression ("Pre-peak mobility change (%)") , y = "Pre-peak case rate (%)") +
  scale_color_manual(name = expression ("Population clustering"),
                     labels = c("Low","High"),
                     values = c("#a6611a", "#018571")) +
  scale_fill_manual(guide=FALSE,
                    values = c("#a6611a", "#018571")) +
  scale_y_continuous(breaks = c(0,10,20,30)) +
  coord_cartesian(ylim = c(0,30)) +
  theme_classic() +
  theme(
    text = element_text(size=16),
    legend.title =  element_text(size=11.5),
    legend.text =  element_text(size=11, face="italic"),
    legend.position = "top"
  )
a
ggsave(file="Graphs/trans_slp1_mob_clust.png", width = 90, height = 80, dpi = 600, units = "mm", device='png') 


green_slp2_sim = lme(
  slope2 ~
    scale(max_case) +
    scale(meanrelmob1_imp) +
    scale(pop_den_log) +
    scale(pop_clust) +
    scale(meanrelmob2_imp) +
    scale(pop_den_log)*scale(pop_clust) +
    scale(pop_den_log)*scale(meanrelmob2_imp) +
    scale(pop_clust)*scale(meanrelmob2_imp) 
  ,  
  random = ~ 1| reg,
  correlation=corExp(form=~lt+ln,nugget=T),
  weights = varFixed(~ var2),  
  data=tmp_df,
  method = "ML",
  control = lmeControl(opt = 'optim'))
intervals(green_slp2_sim, which = "fixed")


green_slp2_base = lme(
  slope2 ~
    scale(max_case) +
    scale(meanrelmob1_imp) +
    scale(pop_den_log) +
    scale(pop_clust) +
    scale(meanrelmob2_imp) +
    scale(health) +
    scale(old) + 
    scale(unemply_perc) +
    scale(pop_den_log)*scale(pop_clust) +
    scale(pop_den_log)*scale(meanrelmob2_imp) +
    scale(pop_clust)*scale(meanrelmob2_imp) 
  ,  
  random = ~ 1| reg,
  correlation=corExp(form=~lt+ln,nugget=T),
  weights = varFixed(~ var2),  
  data=tmp_df,
  method = "ML",
  control = lmeControl(opt = 'optim'))
summary(green_slp2_base)
intervals(green_slp2_base, which = "fixed")
plot(green_slp2_base)
qqnorm(green_slp2_base, abline = c(0.5,1))
vif(green_slp2_base)

mydf <- ggpredict(green_slp2_base, terms = c("max_case [0:150]"))
ggplot(mydf, aes(x, predicted)) +
  geom_line(aes(group = group), size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .15) +
  labs(x = expression ("Cases at peak") , y = "Post-peak case rate (%)") +
  scale_y_continuous(breaks = c(-5,0,5)) +
  coord_cartesian(ylim = c(-8,5)) +
  theme_classic() +
  theme(
    text = element_text(size=16),
    legend.title =  element_text(size=11.5),
    legend.text =  element_text(size=11, face="italic"),
    legend.position = "top"
  )
ggsave(file="Graphs/trans_slp2_max_case.png", width = 90, height = 80, dpi = 600, units = "mm", device='png') 

mydf <- ggpredict(green_slp2_base, terms = c("meanrelmob1_imp [-50:0]"))
ggplot(mydf, aes(x, predicted)) +
  geom_line(aes(group = group), size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .15) +
  labs(x = expression ("Pre-peak mobility change (%)") , y = "Post-peak case rate (%)") +
  scale_y_continuous(breaks = c(-5,0,5)) +
  coord_cartesian(ylim = c(-8,5)) +
  theme_classic() +
  theme(
    text = element_text(size=16),
    legend.title =  element_text(size=11.5),
    legend.text =  element_text(size=11, face="italic"),
    legend.position = "top"
  )
ggsave(file="Graphs/trans_slp2_mob1.png", width = 90, height = 80, dpi = 600, units = "mm", device='png') 

mydf <- ggpredict(green_slp2_base, terms = c("meanrelmob2_imp [-50:0]"))
ggplot(mydf, aes(x, predicted)) +
  geom_line(aes(group = group), size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .15) +
  labs(x = expression ("Post-peak mobility change (%)") , y = "Post-peak case rate (%)") +
  scale_y_continuous(breaks = c(-5,0,5)) +
  coord_cartesian(ylim = c(-8,5)) +
  theme_classic() +
  theme(
    text = element_text(size=16),
    legend.title =  element_text(size=11.5),
    legend.text =  element_text(size=11, face="italic"),
    legend.position = "top"
  )
ggsave(file="Graphs/trans_slp2_mob2.png", width = 90, height = 80, dpi = 600, units = "mm", device='png') 


mydf <- ggpredict(green_slp2_base, terms = c("pop_clust"))
ggplot(mydf, aes(x, predicted)) +
  geom_line(aes(group = group), size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .15) +
  labs(x = expression ("Population clustering") , y = "Post-peak case rate (%)") +
  scale_y_continuous(breaks = c(-5,0,5)) +
  coord_cartesian(ylim = c(-8,5)) +
  theme_classic() +
  theme(
    text = element_text(size=16),
    legend.title =  element_text(size=11.5),
    legend.text =  element_text(size=11, face="italic"),
    legend.position = "top"
  )
ggsave(file="Graphs/trans_slp2_pop_clust.png", width = 90, height = 80, dpi = 600, units = "mm", device='png') 

mydf <- ggpredict(green_slp2_base, terms = c("meanrelmob2_imp [-50:0]", "pop_den_log [2.69,3.69]"))
b = ggplot(mydf, aes(x, predicted)) +
  geom_line(aes(group = group, colour = group), size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = .15) +
  labs(x = expression ("Post-peak mobility change (%)") , y = "Post-peak case rate (%)") +
  scale_color_manual(name = expression ("Population density"),
                     labels = c(expression ("500/"*km^2),expression ("5000/"*km^2)),
                     values = c("#a6611a", "#018571")) +
  scale_fill_manual(guide=FALSE,
                    values = c("#a6611a", "#018571")) +
  scale_y_continuous(breaks = c(-5,0,5)) +
  coord_cartesian(ylim = c(-8,5)) +
  theme_classic() +
  theme(
    text = element_text(size=16),
    legend.title =  element_text(size=11.5),
    legend.text =  element_text(size=11, face="italic"),
    legend.position = "top"
  )
b
ggsave(file="Graphs/trans_slp2_mob_pop.png", width = 100, height = 80, dpi = 600, units = "mm", device='png')


mydf <- ggpredict(green_slp2_base, terms = c("pop_clust", "pop_den_log [2.69,3.69]"))
c = ggplot(mydf, aes(x, predicted)) +
  geom_line(aes(group = group, colour = group), size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = .15) +
  labs(x = expression ("Population clustering") , y = "Post-peak case rate (%)") +
  scale_color_manual(name = expression ("Population density"),
                     labels = c(expression ("500/"*km^2),expression ("5000/"*km^2)),
                     values = c("#a6611a", "#018571")) +
  scale_fill_manual(guide=FALSE,
                    values = c("#a6611a", "#018571")) +
  scale_y_continuous(breaks = c(-5,0,5)) +
  coord_cartesian(ylim = c(-8,5)) +
  theme_classic() +
  theme(
    text = element_text(size=16),
    legend.title =  element_text(size=11.5),
    legend.text =  element_text(size=11, face="italic"),
    legend.position = "top"
  )
c
ggsave(file="Graphs/trans_slp2_clust_pop.png", width = 100, height = 80, dpi = 600, units = "mm", device='png') 



mydf <- ggpredict(green_slp2_base, terms = c("meanrelmob2_imp [-50:0]", "pop_clust [0.35,0.7]"))
d = ggplot(mydf, aes(x, predicted)) +
  geom_line(aes(group = group, colour = group), size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = .15) +
  labs(x = expression ("Post-peak mobility change (%)") , y = "Post-peak case rate (%)") +
  scale_color_manual(name = expression ("Population clustering"),
                     labels = c("Low","High"),
                     values = c("#a6611a", "#018571")) +
  scale_fill_manual(guide=FALSE,
                    values = c("#a6611a", "#018571")) +
  scale_y_continuous(breaks = c(-5,0,5)) +
  coord_cartesian(ylim = c(-8,5)) +
  theme_classic() +
  theme(
    text = element_text(size=16),
    legend.title =  element_text(size=11.5),
    legend.text =  element_text(size=11, face="italic"),
    legend.position = "top"
  )
d
ggsave(file="Graphs/trans_slp2_mob_clust.png", width = 90, height = 80, dpi = 600, units = "mm", device='png') 

ggarrange(a,b,d,c, ncol = 2, nrow = 2, labels = c("a", "c", "b", "d"))
ggsave(file="Graphs/base_slopes.png", width = 205, height = 160, dpi = 600, units = "mm", device='png') 

tmp_df$grn_slp1_res = residuals(green_slp1_base)
tmp_df$grn_slp2_res = residuals(green_slp2_base)
tmp_df$park1_res = residuals(lm(meanrelpark1_imp ~ meanrelmob1_imp, data = tmp_df))
tmp_df$park2_res = residuals(lm(meanrelpark2_imp ~ meanrelmob2_imp, data = tmp_df))
tmp_df$park1_res = ihs(tmp_df$park1_res)
tmp_df$park2_res = ihs(tmp_df$park2_res)
tmp_df$green_log = log10(tmp_df$green_km_pp)

park1_mod = lme(
  park1_res ~  
    scale(green_log)*scale(ave_freq_1k) +
    scale(address_garden_perc) +
    scale(ave_freq_1k) +
    scale(green_log)
  ,  
  random = ~ 1| reg,
  correlation=corExp(form=~lt+ln,nugget=T),
  data=tmp_df,
  method = "ML")
summary(park1_mod)
intervals(park1_mod, which = "fixed")
plot(park1_mod)
qqnorm(park1_mod, abline = c(0,1))
vif(park1_mod)

library(scales)
tn <- trans_new("ihs",
                function(x) ihs(x),
                function(y) sinh(y),
                domain=c(-Inf, Inf))

mydf <- ggpredict(park1_mod,  terms = c("green_log"))
mydf$predicted = sinh(mydf$predicted)
mydf$conf.low = sinh(mydf$conf.low)
mydf$conf.high = sinh(mydf$conf.high)
a = ggplot(mydf, aes(10^x, predicted)) +
  geom_line(aes(group = group), size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .15) +
  labs(x = expression("Green space per person"~m^2) , y = "Pre-peak park use (%)") +
  scale_x_log10() +
  coord_trans(y = tn, ylim = c(-10,10)) +
  scale_y_continuous(breaks = c(-10,-2, 0, 2, 10)) +
  theme_classic() +
  theme(
    text = element_text(size=16),
    legend.title =  element_text(size=11.5),
    legend.text =  element_text(size=11, face="italic"),
    legend.position = "top"
  )
a
ggsave(file="Graphs/trans_prk1_grn.png", width = 90, height = 80, dpi = 600, units = "mm", device='png') 


park2_mod = lme(
  park2_res ~  
    scale(green_log)*scale(ave_freq_1k) +
    scale(address_garden_perc) +
    scale(ave_freq_1k) +
    scale(green_log)
  ,  
  random = ~ 1| reg,
  correlation=corExp(form=~lt+ln,nugget=T),
  data=tmp_df,
  method = "ML")
summary(park2_mod)
intervals(park2_mod, which = "fixed")
plot(park2_mod)
qqnorm(park2_mod, abline = c(0,1))
vif(park2_mod)

mydf <- ggpredict(park2_mod, terms = c("green_log"))
mydf$predicted = sinh(mydf$predicted)
mydf$conf.low = sinh(mydf$conf.low)
mydf$conf.high = sinh(mydf$conf.high)
b = ggplot(mydf, aes(10^x, predicted)) +
  geom_line(aes(group = group), size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .15) +
  labs(x = expression("Green space per person"~m^2) , y = "Post-peak park use (%)") +
  scale_x_log10() +
  coord_trans(y = tn, ylim = c(-10,10)) +
  scale_y_continuous(breaks = c(-10,-2, 0, 2, 10)) +
  theme_classic() +
  theme(
    text = element_text(size=16),
    legend.title =  element_text(size=11.5),
    legend.text =  element_text(size=11, face="italic"),
    legend.position = "top"
  )
b
ggsave(file="Graphs/trans_prk2_grn.png", width = 90, height = 80, dpi = 600, units = "mm", device='png') 


mydf <- ggpredict(park2_mod, terms = c("ave_freq_1k[0:10]", "green_log[1, 2.69897]"))
mydf$predicted = sinh(mydf$predicted)
mydf$conf.low = sinh(mydf$conf.low)
mydf$conf.high = sinh(mydf$conf.high)
c = ggplot(mydf, aes(x, predicted)) +
  geom_line(aes(group = group, colour = group), size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = .15) +
  labs(x =  expression("Patchiness"), y = "Post-peak park use (%)") +
  scale_color_manual(name = expression ("Green space \n per person"),
                     labels = c(expression ("10"~m^2),expression ("500"~m^2)),
                     values = c("#a6611a", "#018571")) +
  scale_fill_manual(guide=FALSE,
                    values = c("#a6611a", "#018571")) +
  coord_trans(y = tn, ylim = c(-10,10)) +
  scale_y_continuous(breaks = c(-10,-2, 0, 2, 10)) +
  scale_x_continuous(breaks = c(0, 5, 10), labels = c("0", "5", "10")) +
  theme_classic() +
  theme(
    text = element_text(size=16),
    legend.title =  element_text(size=11.5),
    legend.text =  element_text(size=11, face="italic"),
    legend.position = "top"
  )
c
ggsave(file="Graphs/trans_prk2_ave_grn.png", width = 90, height = 80, dpi = 600, units = "mm", device='png') 


green1_mod = lme(
  grn_slp1_res ~  
    scale(park1_res)*scale(green_log) +
    scale(park1_res)*scale(ave_freq_1k) +
    scale(park1_res) +
    scale(ave_freq_1k) +
    scale(green_log)
  ,  
  random = ~ 1| reg,
  weights = varFixed(~ var1),  
  data=tmp_df,
  method = "ML")
summary(green1_mod)
intervals(green1_mod, which = "fixed")
plot(green1_mod)
qqnorm(green1_mod, abline = c(0,1))
vif(green1_mod)


mydf <- ggpredict(green1_mod, terms = c("park1_res"))
d = ggplot(mydf, aes(sinh(x), predicted)) +
  geom_line(aes(group = group), size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .15) +
  labs(x =  expression("Pre-peak park use (%)"), y = "Pre-peak case rate (%)") +
  theme_classic() +
  coord_trans(x = tn, ylim = c(-5,5)) +
  scale_x_continuous(breaks = c(-10,-2, 0, 2, 10)) +
  scale_y_continuous(breaks = c(-4,-2,0,2,4)) +
  #coord_cartesian(ylim = c(-5,5)) +
  theme(
    text = element_text(size=16),
    legend.title =  element_text(size=11.5),
    legend.text =  element_text(size=11, face="italic"),
    legend.position = "top"
  )
d
ggsave(file="Graphs/trans_grn_prk.png", width = 90, height = 80, dpi = 600, units = "mm", device='png') 


mydf <- ggpredict(green1_mod, terms = c("park1_res", "green_log[1, 2.69897]"))
e = ggplot(mydf, aes(sinh(x), predicted)) +
  geom_line(aes(group = group, colour = group), size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = .15) +
  labs(x =  expression("Pre-peak park use (%)"), y = "Pre-peak case rate (%)") +
  theme_classic() +
  coord_trans(x = tn, ylim = c(-5,5)) +
  scale_x_continuous(breaks = c(-10,-2, 0, 2, 10)) +
  scale_y_continuous(breaks = c(-4,-2,0,2,4)) +
  #coord_cartesian(ylim = c(-5,5)) +
  scale_color_manual(name = expression ("Green space \n per person"),
                     labels = c(expression ("10"~m^2),expression ("500"~m^2)),
                     values = c("#a6611a", "#018571")) +
  scale_fill_manual(guide=FALSE,
                    values = c("#a6611a", "#018571")) +
  theme_classic() +
  theme(
    text = element_text(size=16),
    legend.title =  element_text(size=11.5),
    legend.text =  element_text(size=11, face="italic"),
    legend.position = "top"
  )
e
ggsave(file="Graphs/trans_grn_prk_grn.png", width = 90, height = 80, dpi = 600, units = "mm", device='png') 

mydf <- ggpredict(green1_mod, terms = c("park1_res", "ave_freq_1k[1,10]"))
f = ggplot(mydf, aes(sinh(x), predicted)) +
  geom_line(aes(group = group, colour = group), size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = .15) +
  labs(x =  expression("Pre-peak park use (%)"), y = "Pre-peak case rate (%)") +
  theme_classic() +
  coord_trans(x = tn, ylim = c(-5,5)) +
  scale_x_continuous(breaks = c(-10,-2, 0, 2, 10)) +
  scale_y_continuous(breaks = c(-4,-2,0,2,4)) +
  #coord_cartesian(ylim = c(-5,5)) +
  scale_color_manual(name = expression ("Patchiness"),
                     labels = c("1", "10"),
                     values = c("#a6611a", "#018571")) +
  scale_fill_manual(guide=FALSE,
                    values = c("#a6611a", "#018571")) +
  theme_classic() +
  theme(
    text = element_text(size=16),
    legend.title =  element_text(size=11.5),
    legend.text =  element_text(size=11, face="italic"),
    legend.position = "top"
  )
f
ggsave(file="Graphs/trans_grn_prk_ave.png", width = 90, height = 80, dpi = 600, units = "mm", device='png') 


green2_mod = lme(
  grn_slp2_res ~  
    scale(park2_res)*scale(green_log) +
    scale(park2_res)*scale(ave_freq_1k) +
    scale(park2_res) +
    scale(ave_freq_1k) +
    scale(green_log)
  ,  
  random = ~ 1| reg,
  weights = varFixed(~ var2),  
  data=tmp_df,
  method = "ML")
summary(green2_mod)
intervals(green2_mod, which = "fixed")
plot(green2_mod)
qqnorm(green2_mod, abline = c(0,1))
vif(green2_mod)


ggarrange(d,f,a,e,c,b,ncol = 3, nrow = 2, labels = c("a","c","e","b","d","f"), align = "h")
ggsave(file="Graphs/green_plots.png", width = 295, height = 195, dpi = 600, units = "mm", device='png') 



ggpredict(green_slp1_base, terms = c("meanrelmob1_imp [0]")) [2] + ggpredict(green1_mod, terms = c("park1_res [3.91]"))[2] #25% park preference
ggpredict(green_slp1_base, terms = c("meanrelmob1_imp [0]")) [2] + ggpredict(green1_mod, terms = c("park1_res [-3.91]"))[2] #25% park preference

ggpredict(green_slp1_base, terms = c("meanrelmob1_imp [0]")) 
ggpredict(green_slp1_base, terms = c("meanrelmob1_imp [-50]")) 
16.47-2.85

ggpredict(green_slp1_base, terms = c("meanrelmob1_imp [0]")) [2] + ggpredict(green1_mod, terms = c("park1_res [3.91]", "green_log[1]"))[2] #25% park preference
ggpredict(green_slp1_base, terms = c("meanrelmob1_imp [0]")) [2] + ggpredict(green1_mod, terms = c("park1_res [-3.91]", "green_log[1]"))[2] #25% park preference


MetricsWeighted::mae(green_slp1_base$data$slope1, green_slp1_base$fitted[,1], 1/green_slp1_base$data$var1)
MetricsWeighted::mae(green_slp1_base$data$slope1, green_slp1_base$fitted[,1] + green1_mod$fitted[,1], 1/green_slp1_base$data$var1)

MetricsWeighted::r_squared(green_slp1_base$data$slope1, green_slp1_base$fitted[,1], 1/green_slp1_base$data$var1)
MetricsWeighted::r_squared(green_slp1_base$data$slope1, green_slp1_base$fitted[,1] + green1_mod$fitted[,1], 1/green_slp1_base$data$var1)

MetricsWeighted::mae(green_slp2_base$data$slope2, green_slp2_base$fitted[,1], 1/green_slp2_base$data$var2)
MetricsWeighted::mae(green_slp2_base$data$slope2, green_slp2_base$fitted[,1] + green2_mod$fitted[,1], 1/green_slp2_base$data$var2)

MetricsWeighted::r_squared(green_slp2_base$data$slope2, green_slp2_base$fitted[,1], 1/green_slp2_base$data$var2)
MetricsWeighted::r_squared(green_slp2_base$data$slope2, green_slp2_base$fitted[,1] + green2_mod$fitted[,1], 1/green_slp2_base$data$var2)




library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
require(gridExtra)


low_col = "#a6611a"
med_col = "grey"
med_col_map = "white"
high_col = "#018571"

la <- st_read("data/Local_Authority_Districts__December_2017__Boundaries_in_Great_Britain.shp")
la_proj <-st_transform(la,"+proj=longlat +datum=WGS84")
colnames(la_proj)[3] = "la"
la_proj = merge(la_proj, tmp_df, by = "la")
uk = subset(ne_countries(scale = "medium", returnclass = "sf"), name == "United Kingdom")


mean(la_proj$slope1)
sd(la_proj$slope1)
mean(la_proj$slope2)
sd(la_proj$slope2)
mean(la_proj$meanrelmob1_imp)
sd(la_proj$meanrelmob1_imp)
mean(la_proj$meanrelmob2_imp)
sd(la_proj$meanrelmob2_imp)
(98-sum(la_proj$meanrelmob1_imp > la_proj$meanrelmob2_imp))/98
(98-sum(la_proj$park1_resid > la_proj$park2_resid))/98
range(sinh(la_proj$park1_res))
range(sinh(la_proj$park2_res))
la_proj$park_diff = sinh(la_proj$park2_res) - sinh(la_proj$park1_res)


a = ggplot(data = uk) +
  geom_sf() +
  geom_sf(data = la_proj, aes(fill = slope1)) +
  geom_rect(mapping=aes(xmin=-0.5, xmax=0.5, ymin=51.25, ymax=51.75), color="black", fill = NA, alpha = 0.5) +
  
  scale_fill_gradient2(name = " ",low = low_col, mid = med_col, high = high_col, midpoint = 0, breaks = c(-10,0, 20), limits = c(-12,22)) +
  scale_x_continuous(breaks = c(-6,-3,0)) +
  scale_y_continuous(breaks = c(52, 55, 58)) +
  coord_sf(xlim = c(-8, 3), ylim = c(50,59), expand = FALSE) +
  labs(title = "Pre-peak") +
  theme_classic() +
  theme(
    legend.position = "none",
    text = element_text(size=17),
    plot.title =  element_text(size=13),
    legend.text =  element_text(size=13, face="italic")
  )
a
b = ggplot(data = uk) +
  geom_sf() +
  geom_sf(data = la_proj, aes(fill = slope1)) +
  scale_fill_gradient2(name = "Pre-peak daily case\nrate change (%)",low = low_col, mid = med_col, high = high_col, midpoint = 0, breaks = c(-10,0, 20), limits = c(-12,22)) +
  coord_sf(xlim = c(-0.5, 0.5), ylim = c(51.25,51.75), expand = FALSE) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size=14, hjust = 0.5),
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    text = element_text(size=12.5)) +
  labs(title = "London")


b
png(filename="Graphs/map_slop1.png", width = 900, height = 1200, units = "px", res = 250) 
grid.arrange(a, b, layout_matrix =rbind(c(1, 1, 1, 1, 1, 1),
                                        c(1, 1, 1, 1, 2, 2),
                                        c(1, 1, 1, 1, 2, 2),
                                        c(1, 1, 1, 1, 1, 1),
                                        c(1, 1, 1, 1, 1, 1),
                                        c(1, 1, 1, 1, 1, 1)))
dev.off()

a = ggplot(data = uk) +
  geom_sf() +
  geom_sf(data = la_proj, aes(fill = slope2)) +
  geom_rect(mapping=aes(xmin=-0.5, xmax=0.5, ymin=51.25, ymax=51.75), color="black", fill = NA, alpha = 0.5) +
  scale_fill_gradient2(name = "Daily case\nrate change (%)",low = low_col, mid = med_col, high = high_col, midpoint = 0, breaks = c(-10,0, 20), limits = c(-12,22)) + 
  scale_x_continuous(breaks = c(-6,-3,0)) +
  scale_y_continuous(breaks = c(52, 55, 58)) +
  coord_sf(xlim = c(-8, 3), ylim = c(50,59), expand = FALSE) +
  labs(title = "Post-peak") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    text = element_text(size=17),
    plot.title =  element_text(size=13),
    legend.text =  element_text(size=13, face="italic")
  )
a

b = ggplot(data = uk) +
  geom_sf() +
  geom_sf(data = la_proj, aes(fill = slope2)) +
  scale_fill_gradient2(name = "Post-peak daily case\nrate change (%)",low = low_col, mid = med_col, high = high_col, midpoint = 0, breaks = c(-10,0, 20), limits = c(-12,22)) +
  coord_sf(xlim = c(-0.5, 0.5), ylim = c(51.25,51.75), expand = FALSE) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size=14, hjust = 0.5),
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    text = element_text(size=12.5)) +
  labs(title = "London")
b


png(filename="Graphs/map_slop2.png", width = 900, height = 1400, units = "px", res = 250) 
grid.arrange(a, b, layout_matrix =rbind(c(1, 1, 1, 1, 1, 1),
                                        c(1, 1, 1, 1, 2, 2),
                                        c(1, 1, 1, 1, 2, 2),
                                        c(1, 1, 1, 1, 1, 1),
                                        c(1, 1, 1, 1, 1, 1),
                                        c(1, 1, 1, 1, 1, 1),
                                        c(1, 1, 1, 1, 1, 1)))
dev.off()

a = ggplot(data = uk) +
  geom_sf() +
  geom_sf(data = la_proj, aes(fill = meanrelmob1_imp)) +
  geom_rect(mapping=aes(xmin=-0.5, xmax=0.5, ymin=51.25, ymax=51.75), color="black", fill = NA, alpha = 0.5) +
  
  scale_fill_gradient2(name = " ",low = low_col, mid = med_col, high = high_col, midpoint = -40, breaks = c(-60,-40, -20), limits = c(-60,-20)) +
  scale_x_continuous(breaks = c(-6,-3,0)) +
  scale_y_continuous(breaks = c(52, 55, 58)) +
  coord_sf(xlim = c(-8, 3), ylim = c(50,59), expand = FALSE) +
  labs(title = "Pre-peak") +
  theme_classic() +
  theme(
    legend.position = "none",
    text = element_text(size=17),
    plot.title =  element_text(size=13),
    legend.text =  element_text(size=13, face="italic")
  )
a
b = ggplot(data = uk) +
  geom_sf() +
  geom_sf(data = la_proj, aes(fill = meanrelmob1_imp)) +
  scale_fill_gradient2(name = "Pre-peak mobility\nchange (%)",low = low_col, mid = med_col, high = high_col, midpoint = -40, breaks = c(-60,-40, -20), limits = c(-60,-20)) +
  coord_sf(xlim = c(-0.5, 0.5), ylim = c(51.25,51.75), expand = FALSE) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size=14, hjust = 0.5),
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    text = element_text(size=12.5)) +
  labs(title = "London")



png(filename="Graphs/map_mob1.png", width = 900, height = 1200, units = "px", res = 250) 
grid.arrange(a, b, layout_matrix =rbind(c(1, 1, 1, 1, 1, 1),
                                        c(1, 1, 1, 1, 2, 2),
                                        c(1, 1, 1, 1, 2, 2),
                                        c(1, 1, 1, 1, 1, 1),
                                        c(1, 1, 1, 1, 1, 1),
                                        c(1, 1, 1, 1, 1, 1)))
dev.off()

a = ggplot(data = uk) +
  geom_sf() +
  geom_sf(data = la_proj, aes(fill = meanrelmob2_imp)) +
  geom_rect(mapping=aes(xmin=-0.5, xmax=0.5, ymin=51.25, ymax=51.75), color="black", fill = NA, alpha = 0.5) +
  
  scale_fill_gradient2(name = "Mobility\nchange (%)  ",low = low_col, mid = med_col, high = high_col, midpoint = -40, breaks = c(-60,-40, -20), limits = c(-60,-20)) +
  scale_x_continuous(breaks = c(-6,-3,0)) +
  scale_y_continuous(breaks = c(52, 55, 58)) +
  coord_sf(xlim = c(-8, 3), ylim = c(50,59), expand = FALSE) +
  labs(title = "Post-peak") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    text = element_text(size=17),
    plot.title =  element_text(size=13),
    legend.text =  element_text(size=13, face="italic")
  )

b = ggplot(data = uk) +
  geom_sf() +
  geom_sf(data = la_proj, aes(fill = meanrelmob2_imp)) +
  scale_fill_gradient2(name = "Post-peak mobility\nchange (%)",low = low_col, mid = med_col, high = high_col, midpoint = -40, breaks = c(-60,-40, -20), limits = c(-60,-20)) +
  coord_sf(xlim = c(-0.5, 0.5), ylim = c(51.25,51.75), expand = FALSE) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size=14, hjust = 0.5),
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    text = element_text(size=12.5)) +
  labs(title = "London")



png(filename="Graphs/map_mob2.png", width = 900, height = 1400, units = "px", res = 250) 
grid.arrange(a, b, layout_matrix =rbind(c(1, 1, 1, 1, 1, 1),
                                        c(1, 1, 1, 1, 2, 2),
                                        c(1, 1, 1, 1, 2, 2),
                                        c(1, 1, 1, 1, 1, 1),
                                        c(1, 1, 1, 1, 1, 1),
                                        c(1, 1, 1, 1, 1, 1),
                                        c(1, 1, 1, 1, 1, 1)))
dev.off()

la_proj$park1_resid = sinh(la_proj$park1_res)
la_proj$park2_resid = sinh(la_proj$park2_res)

a = ggplot(data = uk) +
  geom_sf() +
  geom_sf(data = la_proj, aes(fill = park1_resid)) +
  geom_rect(mapping=aes(xmin=-0.5, xmax=0.5, ymin=51.25, ymax=51.75), color="black", fill = NA, alpha = 0.5) +
  
  scale_fill_gradient2(name = " ",low = low_col, mid = med_col, high = high_col, midpoint = 0, breaks = c(-40, 0, 30), limits = c(-45,31)) +
  scale_x_continuous(breaks = c(-6,-3,0)) +
  scale_y_continuous(breaks = c(52, 55, 58)) +
  coord_sf(xlim = c(-8, 3), ylim = c(50,59), expand = FALSE) +
  labs(title = "Pre-peak") +
  theme_classic() +
  theme(
    legend.position = "none",
    text = element_text(size=17),
    plot.title =  element_text(size=13),
    legend.text =  element_text(size=13, face="italic")
  )

b = ggplot(data = uk) +
  geom_sf() +
  geom_sf(data = la_proj, aes(fill = park1_resid)) +
  scale_fill_gradient2(name = "Pre-peak park\npreference (%)       ",low = low_col, mid = med_col, high = high_col, midpoint = 0, breaks = c(-40, 0, 30), limits = c(-45,31)) +
  coord_sf(xlim = c(-0.5, 0.5), ylim = c(51.25,51.75), expand = FALSE) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size=14, hjust = 0.5),
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    text = element_text(size=12.5)) +
  labs(title = "London")



png(filename="Graphs/map_prk1.png", width = 900, height = 1200, units = "px", res = 250) 
grid.arrange(a, b, layout_matrix =rbind(c(1, 1, 1, 1, 1, 1),
                                        c(1, 1, 1, 1, 2, 2),
                                        c(1, 1, 1, 1, 2, 2),
                                        c(1, 1, 1, 1, 1, 1),
                                        c(1, 1, 1, 1, 1, 1),
                                        c(1, 1, 1, 1, 1, 1)))
dev.off()

a = ggplot(data = uk) +
  geom_sf() +
  geom_sf(data = la_proj, aes(fill = park2_resid)) +
  geom_rect(mapping=aes(xmin=-0.5, xmax=0.5, ymin=51.25, ymax=51.75), color="black", fill = NA, alpha = 0.5) +
  
  scale_fill_gradient2(name = "Park use\n (%)  ",low = low_col, mid = med_col, high = high_col, midpoint = 0, breaks = c(-40, 0, 30), limits = c(-45,31)) +
  scale_x_continuous(breaks = c(-6,-3,0)) +
  scale_y_continuous(breaks = c(52, 55, 58)) +
  coord_sf(xlim = c(-8, 3), ylim = c(50,59), expand = FALSE) +
  labs(title = "Post-peak") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    text = element_text(size=17),
    plot.title =  element_text(size=13),
    legend.text =  element_text(size=13, face="italic")
  )

b = ggplot(data = uk) +
  geom_sf() +
  geom_sf(data = la_proj, aes(fill = park2_resid)) +
  scale_fill_gradient2(name = "Post-peak park\npreference (%)       ",low = low_col, mid = med_col, high = high_col, midpoint = 0, breaks = c(-40, 0, 30), limits = c(-45,31)) +
  coord_sf(xlim = c(-0.5, 0.5), ylim = c(51.25,51.75), expand = FALSE) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size=14, hjust = 0.5),
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    text = element_text(size=12.5)) +
  labs(title = "London")



png(filename="Graphs/map_prk2.png", width = 900, height = 1400, units = "px", res = 250) 
grid.arrange(a, b, layout_matrix =rbind(c(1, 1, 1, 1, 1, 1),
                                        c(1, 1, 1, 1, 2, 2),
                                        c(1, 1, 1, 1, 2, 2),
                                        c(1, 1, 1, 1, 1, 1),
                                        c(1, 1, 1, 1, 1, 1),
                                        c(1, 1, 1, 1, 1, 1),
                                        c(1, 1, 1, 1, 1, 1)))
dev.off()
