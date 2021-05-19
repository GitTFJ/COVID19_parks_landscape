is.nan.data.frame <- function(x){
  do.call(cbind, lapply(x, is.nan))
}

ihs <- function(x) {
  y <- log(x + sqrt(x ^ 2 + 1))
  return(y)
}

cmb = read.csv("data/data_2.csv")[,-1]

cmb = subset(cmb, name != "Aylesbury Vale" & name != "Chiltern" & name != "Isles of Scilly" & name != "City of London" & name != "South Bucks"  & name != "Wycombe" & name != "Castle Point" & name != "Hartlepool" & name != "Kettering"  & name != "Rutland" & name != "Tamworth")

### Derive additional covariates
cmb$relmob = rowMeans(cmb[,c(9:13)]) # mean on non-residential mobility
cmb$relmob_imp = rowMeans(cmb[,c(28:32)]) # mean on non-residential mobility

cmb2 = NULL
for(a in unique(cmb$name)){
  print(a)
  tmp = subset(cmb, name == a)
  cmb_df = NULL
  for(b in 1:(nrow(tmp)-1)){
    tmp_lm = lm(log(case + 1) ~ date_num, data = tmp[c(b:(b+6)),])
    tmp_int = coef(tmp_lm)[1]
    tmp_slp = coef(tmp_lm)[2]
    tmp_vr = summary(tmp_lm)$coefficients[2,2]**2
    tmp_r2 = rsquared(tmp_lm)[5]
    tmp_df = data.frame(
      date_num = b + 3,
      slp = tmp_slp,
      vr = tmp_vr,
      r2 = tmp_r2,
      date_min = b,
      pred_min = predict(tmp_lm)[1],
      date_max = b + 6,
      pred_max = predict(tmp_lm)[7]
    )
    cmb_df = rbind(cmb_df, tmp_df)
  }
  cmb_df$slp_lag1 = lag(cmb_df$slp)
  
  tmp = left_join(tmp, cmb_df)
  roll_mob = c(
    rep(NA,10), 
    zoo::rollmean(tmp$relmob, k = 9, na.rm = T, align = "left")[1:(length(tmp$relmob)-10)])
  roll_mob_imp = c(
    rep(NA,10), 
    zoo::rollmean(tmp$relmob_imp, k = 9, na.rm = T, align = "left")[1:(length(tmp$relmob_imp)-10)])#mobiity from day 2 to 10 before case
  roll_park = c(
    rep(NA,10), 
    zoo::rollmean(tmp$park_la, k = 9, na.rm = T, align = "left")[1:(length(tmp$park_la)-10)]) #park mobiity from day 2 to 10 before case
  roll_park_imp = c(
    rep(NA,10), 
    zoo::rollmean(tmp$park_m, k = 9, na.rm = T, align = "left")[1:(length(tmp$park_m)-10)])
  tmp = cbind(tmp, roll_mob, roll_mob_imp, roll_park, roll_park_imp)
  
  parks = data.frame(
    date_num = as.numeric(names(resid(lm(tmp$roll_park ~ tmp$roll_mob)))),
    resid_park = resid(lm(tmp$roll_park ~ tmp$roll_mob))
  )
  parks$resid_park_lag = lag(parks$resid_park)
  
  parks_imp = data.frame(
    date_num = as.numeric(names(resid(lm(tmp$roll_park_imp ~ tmp$roll_mob_imp)))),
    resid_park_imp = resid(lm(tmp$roll_park_imp ~ tmp$roll_mob_imp))
  )
  parks_imp$resid_park_lag_imp = lag(parks_imp$resid_park_imp)
  
  tmp = left_join(tmp, parks)
  tmp = left_join(tmp, parks_imp)
  
  
  tmp[is.nan(tmp)] = NA
  cmb2 = rbind(cmb2, tmp)
}

tmp_loc = subset(cmb2, name == "Oxford")
tmp_loc_predcase = data.frame(
  id = c(tmp_loc$date_num,tmp_loc$date_num),
  x = c(tmp_loc$date_min, tmp_loc$date_max),
  y = c(tmp_loc$pred_min, tmp_loc$pred_max)
)

slp_fit = ggplot() +
  geom_line(data = tmp_loc[which(tmp_loc$date_num> 16),], aes(x = date_num, y = case)) +
  geom_line(data = tmp_loc_predcase[which(tmp_loc_predcase$x>16),], aes(x = x, y = exp(y), group = id), alpha =  0.4, colour = "blue") +
  scale_x_continuous(breaks = c(16,46,76,107,138,164,200,230,260), labels = c("March","April","May", "June", "July","August","September", "October", "November"),guide = guide_axis(n.dodge=2)) +
  scale_y_continuous(expand = c(0,0)) +
  #coord_cartesian(xlim = c(80,110)) +
  labs(x = " ", y = "Cases") +
  theme_classic() 

slp_fit2 = ggplot() +
  geom_line(data = tmp_loc, aes(x = date_num, y = case)) +
  geom_line(data = tmp_loc_predcase, aes(x = x, y = exp(y), group = id), alpha =  0.4, colour = "blue") +
  scale_x_continuous(breaks = c(16,46,76,107,138,164,200,230,260), labels = c("March","April","May", "June", "July","August","September", "October", "November"),guide = guide_axis(n.dodge=2)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(16,75)) +
  labs(x = " ", y = " ") +
  theme_classic() 

ggarrange(slp_fit, slp_fit2, ncol = 2, nrow = 1, labels = c("a,", "b,"), widths = c(1,0.75))
ggsave("plots/slp_fits.png", width = 7, height = 4, dpi = 400)


cmb2[is.nan(cmb2)] = NA
is.na(cmb2) <- sapply(cmb2, is.infinite)
cmb2$name = as.factor(cmb2$name)
cmb2$slp = ihs((exp(cmb2$slp)-1)*100)
cmb2$slp_lag1 = ihs((exp(cmb2$slp_lag1)-1)*100)
cmb2$pop_density =log10(cmb2$pop_density)
cmb2$green_m_pp = log10(cmb2$green_m_pp)
cmb3 = cmb2[,c(1:7,14:25,32,33,36,43,44,46,48,49)]
cmb3 = cmb3[complete.cases(cmb3),]
cmb4 = cmb2[,c(1:7,14:25,32,33,36,43,45,47,50,51)]
cmb4 = cmb4[complete.cases(cmb4),]
colnames(cmb4)[24:27] = c("roll_mob", "roll_park", "resid_park", "resid_park_lag")

write.csv(cmb3, "data/data_complete.csv")
write.csv(cmb4, "data/data_imputation.csv")
rm(list = ls())
