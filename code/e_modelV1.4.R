dat = list(
  comp = read.csv("data/data_complete.csv")[,-1],
  imp = read.csv("data/data_imputation.csv")[,-1],
  comp_trim = read.csv("data/data_complete.csv")[,-1],
  imp_trim = read.csv("data/data_imputation.csv")[,-1]
  )

dat[[3]] = dat[[3]][which(dat[[3]]$date_num > 107),]
dat[[4]] = dat[[4]][which(dat[[4]]$date_num > 107),]

base_m_df = NULL
green_m_df = NULL
for(a in 1:4){
  df = dat[[a]]
  df$name = as.factor(df$name)
  base_m = gam(slp ~ 
                 scale(slp_lag1) +
                 scale(pop_density) + 
                 scale(clust) + 
                 scale(roll_mob) +
                 scale(health) + 
                 scale(perc_70plus) + 
                 scale(unemployed_perc) + 
                 scale(pop_density)*scale(clust) + 
                 scale(pop_density)*scale(roll_mob) + 
                 scale(clust)*scale(roll_mob) +
                 s(name, bs="re") +  
                 s(day, k = 7, bs = "cc"),
               data=df)
  output_base_m = summary(base_m)
  tmp_base_m_df = data.frame(
    term = names(output_base_m$p.coeff),
    coef = output_base_m$p.coeff,
    se = output_base_m$se[1:11],
    run = a
  )
  base_m_df = rbind(base_m_df, tmp_base_m_df)
  
  green_m = gam(
    (resid(base_m)) ~  
      scale(resid_park)*scale(green_m_pp) +
      scale(resid_park)*scale(ave_freq_1k) +
      scale(resid_park) +
      scale(ave_freq_1k) +
      scale(green_m_pp) +
      s(name, bs="re"),  
    data=df)
  output_green_m = summary(green_m)
  tmp_green_m_df = data.frame(
    term = names(output_green_m$p.coeff),
    coef = output_green_m$p.coeff,
    se = output_green_m$se[1:6],
    run = a
  )
  green_m_df = rbind(green_m_df, tmp_green_m_df)
}
base_m_df$lci = base_m_df$coef - (1.96*base_m_df$se)
base_m_df$uci = base_m_df$coef + (1.96*base_m_df$se)
term_nice = data.frame(
  term = unique(base_m_df$term),
  nice = c(
    "Intercept",
    "Case rate lag",
    "Population density",
    "Population clustering",
    "Mobility change",
    "Health",
    "Elderly population",
    "Unemployed population",
    "Population density * clustering",
    "Population density * Mobility change",
    "Population clustering * Mobility change"
  )
)
base_m_df = left_join(base_m_df, term_nice)
base_m_df$run = ifelse(base_m_df$run == 1, "Complete obs.", base_m_df$run)
base_m_df$run = ifelse(base_m_df$run == 2, "Imputed obs.", base_m_df$run)
base_m_df$run = ifelse(base_m_df$run == 3, "Complete obs. (trim)", base_m_df$run)
base_m_df$run = ifelse(base_m_df$run == 4, "Imputed obs. (trim)", base_m_df$run)

sens_imp_base_m = ggplot() +
  geom_pointrange(data = base_m_df,
                  aes(x = nice, ymin = lci, y = coef, ymax = uci, colour = run),
                  position = position_dodge(width = 0.5)) +
  geom_hline(aes(yintercept = 0), linetype = "dotted", size = 1.2) +
  scale_colour_manual(values = c("black", "grey", "dark red", "pink"), name = NULL) +
  coord_flip() +
  labs(x = " ", y = " ") +
  theme_classic() +
  theme(
    legend.position = c(0.6,0.9)
  )




green_m_df$lci = green_m_df$coef - (1.96*green_m_df$se)
green_m_df$uci = green_m_df$coef + (1.96*green_m_df$se)


term_nice2 = data.frame(
  term = unique(green_m_df$term),
  nice = c(
    "Intercept",
    "Park use",
    "Green space",
    "Patchiness",
    "Park use * Green space",
    "Park use * Patchiness"
  )
)
green_m_df = left_join(green_m_df, term_nice2)
green_m_df$run = ifelse(green_m_df$run == 1, "Complete obs.", green_m_df$run)
green_m_df$run = ifelse(green_m_df$run == 2, "Imputed obs.", green_m_df$run)
green_m_df$run = ifelse(green_m_df$run == 3, "Complete obs. (trim)", green_m_df$run)
green_m_df$run = ifelse(green_m_df$run == 4, "Imputed obs. (trim)", green_m_df$run)

sens_imp_green_m = ggplot() +
  geom_pointrange(data = green_m_df,
                  aes(x = nice, ymin = lci, y = coef, ymax = uci, colour = run),
                  position = position_dodge(width = 0.5)) +
  geom_hline(aes(yintercept = 0), linetype = "dotted", size = 1.2) +
  scale_colour_manual(values = c("black", "grey", "dark red", "pink"), name = NULL) +
  coord_flip() +
  labs(x = " ", y = "Standardised coefficient") +
  theme_classic() +
  theme(
    legend.position = "none"
  )



ggarrange(sens_imp_base_m, sens_imp_green_m, ncol = 1, nrow = 2, labels = c("a,", "b,"), align = "hv", heights = c(1,0.65))
ggsave("plots/sens_anal_imp.png", width = 13, height = 7, dpi = 400)

base_m_df = NULL
green_m_df = NULL
for(a in 9:46){
  print(a)
  df = dat[[2]]
  df$week = as.numeric(strftime(df$date, format = "%V"))
  df = df[which(df$week >= a & df$week <= a+3),]
  df$name = as.factor(df$name)
  base_m = gam(slp ~ 
                 scale(slp_lag1) +
                 scale(pop_density) + 
                 scale(clust) + 
                 scale(roll_mob) +
                 scale(health) + 
                 scale(perc_70plus) + 
                 scale(unemployed_perc) + 
                 scale(pop_density)*scale(clust) + 
                 scale(pop_density)*scale(roll_mob) + 
                 scale(clust)*scale(roll_mob) +
                 s(name, bs="re") +  
                 s(day, k = 7, bs = "cc"),
               data=df)
  output_base_m = summary(base_m)
  tmp_base_m_df = data.frame(
    term = names(output_base_m$p.coeff),
    coef = output_base_m$p.coeff,
    se = output_base_m$se[1:11],
    run = a
  )
  base_m_df = rbind(base_m_df, tmp_base_m_df)
  
  green_m = gam(
    (resid(base_m)) ~  
      scale(resid_park)*scale(green_m_pp) +
      scale(resid_park)*scale(ave_freq_1k) +
      scale(resid_park) +
      scale(ave_freq_1k) +
      scale(green_m_pp) +
      s(name, bs="re"),  
    data=df)
  output_green_m = summary(green_m)
  tmp_green_m_df = data.frame(
    term = names(output_green_m$p.coeff),
    coef = output_green_m$p.coeff,
    se = output_green_m$se[1:6],
    run = a
  )
  green_m_df = rbind(green_m_df, tmp_green_m_df)
}

base_m_df$lci = base_m_df$coef - (1.96*base_m_df$se)
base_m_df$uci = base_m_df$coef + (1.96*base_m_df$se)
term_nice = data.frame(
  term = unique(base_m_df$term),
  nice = c(
    "Intercept",
    "Case rate lag",
    "Population density",
    "Population clustering",
    "Mobility change",
    "Health",
    "Elderly population",
    "Unemployed population",
    "Population density * clustering",
    "Population density * Mobility change",
    "Population clustering * Mobility change"
  )
)
base_m_df = left_join(base_m_df, term_nice)

sens_time_base_m = ggplot() +
  geom_pointrange(data = base_m_df,
                  aes(x = nice, ymin = lci, y = coef, ymax = uci, group = run),
                  position = position_dodge(width = 0.5), alpha = 0.1) +
  geom_boxplot(data = base_m_df,
                  aes(x = nice, ymin = lci, y = coef, ymax = uci),
               alpha = 0.1) +
  geom_hline(aes(yintercept = 0), linetype = "dotted", size = 1.2) +
  coord_flip() +
  labs(x = " ", y = " ") +
  theme_classic() 

mobility_time = ggplot() +
  geom_pointrange(data = base_m_df[which(base_m_df$nice == "Mobility change"),],
            aes(x = run, ymin = lci, y = coef, ymax = uci), alpha = 0.2) +
  geom_line(data = base_m_df[which(base_m_df$nice == "Mobility change"),],
                  aes(x = run, y = coef)) +
  geom_rect(aes(xmin = 23, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "blue", alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  labs(x = " ", y = "Standardised coefficient", title = "Mobility change") +
  theme_classic() +
  theme(
    axis.text.x = element_blank()
  )

case_lag_time = ggplot() +
  geom_pointrange(data = base_m_df[which(base_m_df$nice == "Case rate lag"),],
                  aes(x = run, ymin = lci, y = coef, ymax = uci), alpha = 0.2) +
  geom_line(data = base_m_df[which(base_m_df$nice == "Case rate lag"),],
            aes(x = run, y = coef)) +
  geom_rect(aes(xmin = 23, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "blue", alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  labs(x = " ", y = "Standardised coefficient", title = "Case rate lag") +
  theme_classic() +
  theme(
    axis.text.x = element_blank()
  )

health_time = ggplot() +
  geom_pointrange(data = base_m_df[which(base_m_df$nice == "Health"),],
                  aes(x = run, ymin = lci, y = coef, ymax = uci), alpha = 0.2) +
  geom_line(data = base_m_df[which(base_m_df$nice == "Health"),],
            aes(x = run, y = coef)) +
  geom_rect(aes(xmin = 23, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "blue", alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  labs(x = " ", y = "Standardised coefficient", title = "Health") +
  theme_classic() +
  theme(
    axis.text.x = element_blank()
  )

cmb = read.csv("data/national_cases2.csv")[,-1]
case = ggplot() +
  geom_line(data = cmb2, aes(x = date_num, y = nat_case)) +
  geom_rect(aes(xmin = 38, xmax = 77, ymin = 0, ymax = Inf), fill = "red", alpha = 0.2) +
  geom_rect(aes(xmin = 260, xmax = 290, ymin = 0, ymax = Inf), fill = "red", alpha = 0.2) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = c(16,84,164,260)) +
  labs(x = "", y = "Cases", title = "New daily national cases") +
  theme_classic() +
  theme(
    axis.text.x = element_blank()
  )


green_m_df$lci = green_m_df$coef - (1.96*green_m_df$se)
green_m_df$uci = green_m_df$coef + (1.96*green_m_df$se)


term_nice2 = data.frame(
  term = unique(green_m_df$term),
  nice = c(
    "Intercept",
    "Park use",
    "Green space",
    "Patchiness",
    "Park use * Green space",
    "Park use * Patchiness"
  )
)
green_m_df = left_join(green_m_df, term_nice2)

sens_time_green_m = ggplot() +
  geom_pointrange(data = green_m_df,
                  aes(x = nice, ymin = lci, y = coef, ymax = uci, group = run),
                  position = position_dodge(width = 0.5), alpha = 0.1) +
  geom_boxplot(data = green_m_df,
               aes(x = nice, ymin = lci, y = coef, ymax = uci),
               alpha = 0.1) +
  geom_hline(aes(yintercept = 0), linetype = "dotted", size = 1.2) +
  coord_flip() +
  labs(x = " ", y = "Standardised coefficient") +
  theme_classic() 

park_use_time = ggplot() +
  geom_pointrange(data = green_m_df[which(green_m_df$nice == "Park use"),],
                  aes(x = run, ymin = lci, y = coef, ymax = uci), alpha = 0.2) +
  geom_line(data = green_m_df[which(green_m_df$nice == "Park use"),],
            aes(x = run, y = coef)) +
  geom_rect(aes(xmin = 23, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "blue", alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  labs(x = "Week", y = "Standardised coefficient", title = "Park use") +
  theme_classic()

ggarrange(ggarrange(sens_time_base_m, sens_time_green_m, ncol = 1, nrow = 2, align = "hv", heights = c(1,0.65), labels = c("a,", "b,")),
ggarrange(case, mobility_time, health_time, case_lag_time, park_use_time, ncol = 1, nrow = 5, align = "hv", labels = c("    c,", "    d,", "    e,", "    f,", "    g,")), 
nrow = 1, ncol = 2)
ggsave("plots/sens_anal_time.png", width = 10, height = 10, dpi = 400)




df = dat[[4]] # Using imputed data
df2 = NULL
for(a in unique(df$name)){
  tmp = subset(df, name == a)
  tmp$case_avg = c(rep(NA,3),zoo::rollmean(tmp$case, k = 7, na.rm = T, align = "left"), rep(NA,3))
  df2 = rbind(df2, tmp)
}
df = df2
df = df[complete.cases(df),]
df$case_avg =log10(df$case_avg + 1)

df$name = as.factor(df$name)
base_m = gam(slp ~ 
               scale(slp_lag1) +
               scale(pop_density) + 
               scale(clust) + 
               scale(roll_mob)*scale(case_avg) +
               scale(health) + 
               scale(perc_70plus) + 
               scale(unemployed_perc) + 
               scale(pop_density)*scale(clust) + 
               scale(pop_density)*scale(roll_mob) + 
               scale(clust)*scale(roll_mob) +
               s(name, bs="re") +  
               s(day, k = 7, bs = "cc"),
             data=df)
summary(base_m) # seems to explain a lot more of the variation 
plot(base_m)
qqnorm(resid(base_m))
qqline(resid(base_m))
concurvity(base_m)

df$res = resid(base_m)


vg_cmb = NULL
mn_cmb = NULL
for(a in min(df$date_num):max(df$date_num)){
  vg1 = variog(coords = df[which(df$date_num == a),c("lat", "long")], data = df[which(df$date_num == a),c("res")])
  vg1 = data.frame(distance = vg1$u, vari = vg1$v, date = a)
  vg_cmb = rbind(vg_cmb, vg1)
  dists <- as.matrix(dist(df[which(df$date_num == a),c("long", "lat")]))
  dists.inv <- 1/dists
  diag(dists.inv) <- 0
  mn_tmp = data.frame(
    date = a,
    mn = Moran.I(df[which(df$date_num == a),]$res, dists.inv)$p.value)
  mn_cmb = rbind(mn_cmb, mn_tmp)
}


# well this is new! I think this is OK because its all super close to 0
spat_corr = ggplot() +
  geom_smooth(data = vg_cmb, aes(x = distance, y = vari)) +
  geom_line(data = vg_cmb, aes(x = distance, y = vari, group = date), alpha = 0.1) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(limits = c(0,10), expand = c(0,0)) +
  labs(y = "Semi-variance", x = "Distance (decimal degrees)", title = paste0("Moran's autocorrelation p-value: ", round(median(mn_cmb$mn),2)) )+
  theme_classic()


# think this is probably the best its looked and I think is probably OK?

corr_cmb = NULL
for(a in unique(df$name)){
  corr_tmp = data.frame(
    name = a,
    corr = acf(df[which(df$name == a),]$res)[[1]],
    lag = acf(df[which(df$name == a),]$res)[[4]]
  )
  corr_cmb = rbind(corr_cmb, corr_tmp)
}

temp_corr = ggplot() +
  geom_smooth(data = corr_cmb, aes(x = lag, y = corr)) +
  geom_boxplot(data = corr_cmb, aes(x = lag, y = corr, group = lag), alpha = 0.05) +
  geom_hline(aes(yintercept = 0)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(limits = c(-1,1), expand = c(0,0)) +
  labs(y = "Autocorrelation function", x = "Lag (days)" )+
  theme_classic()

ggarrange(spat_corr, temp_corr, ncol = 2, nrow = 1, labels = c("a,", "b,"), align ="hv")
ggsave("plots/correlation.png", width = 8, height = 5, dpi = 400)


green_m = gam(
  resid(base_m) ~  
    scale(resid_park)*scale(green_m_pp) +
    scale(resid_park)*scale(ave_freq_1k) +
    scale(ave_freq_1k) +
    scale(green_m_pp) +
    s(name, bs="re"),  
  data=df)

summary(green_m) # seems to explain a lot more of the variation 
plot(green_m)
qqnorm(resid(green_m))
qqline(resid(green_m))
concurvity(green_m)

df$res2 = resid(green_m)

df$pred = predict(base_m)
df$pred2 = predict(green_m)
df$mae = abs(sinh(df$slp) - sinh(df$pred))
df$mae2 = abs(sinh(df$slp) - sinh(df$pred + df$pred2))
mean(df$mae)
mean(df$mae2)

mydf = ggpredict(base_m, terms = c("roll_mob [-70:70, by = 1]", "case_avg[1, 2]"))
plt_mob_case = ggplot(mydf) +
  geom_line(aes(x = x, y = sinh(predicted), colour = group)) +
  geom_ribbon(aes(x = x, ymin = sinh(conf.low), ymax = sinh(conf.high), fill = group), alpha = 0.2) +
  scale_x_continuous(breaks = c(-50,0,50), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(-70,70), ylim = c(-10,2.5)) +
  scale_fill_manual(name = "Community\ncases", labels = c("10", "100"), values = c("red", "blue")) +
  scale_colour_manual(name = "Community\ncases", labels = c("10", "100"), values = c("red", "blue")) +
  labs(x = "Mobility change (%)", y = " ") +
  theme_classic()  +
  theme(
    text = element_text(size=14),
    legend.title =  element_text(size=11, face = "bold"),
    legend.text =  element_text(size=10.5, face="italic"),
    legend.position = c(0.6,0.25)
  )


mydf = ggpredict(base_m, terms = c("roll_mob [-70:70, by = 1]", "clust[0.2,0.7]", "case_avg[0.83]"))
plt_mob_clust = ggplot(mydf) +
  geom_line(aes(x = x, y = sinh(predicted), colour = group)) +
  geom_ribbon(aes(x = x, ymin = sinh(conf.low), ymax = sinh(conf.high), fill = group), alpha = 0.2) +
  scale_x_continuous(breaks = c(-50,0,50), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(-70,70), ylim = c(-3.5,0)) +
  scale_fill_manual(name = "Population\nclustering", labels = c("Low", "High"), values = c("red", "blue")) +
  scale_colour_manual(name = "Population\nclustering", labels = c("Low", "High"), values = c("red", "blue")) +
  labs(x = "Mobility change (%)", y = "Case rate (%)") +
  theme_classic()  +
  theme(
    text = element_text(size=14),
    legend.title =  element_text(size=11, face = "bold"),
    legend.text =  element_text(size=10.5, face="italic"),
    legend.position = c(0.6,0.25)
  )


mydf = ggpredict(base_m, terms = c("health [-2:2, by = 0.1]", "case_avg[0.83]"))
plt_hel = ggplot(mydf) +
  geom_line(aes(x = x, y = sinh(predicted))) +
  geom_ribbon(aes(x = x, ymin = sinh(conf.low), ymax = sinh(conf.high)), alpha = 0.2) +
  scale_x_continuous(breaks = c(-1.5,0,1.5), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(-1.7,1.7), ylim = c(-1.6,-1.1)) +
  labs(x = "Health index", y = "Case rate (%)") +
  theme_classic()  +
  theme(
    text = element_text(size=14)
  )

mydf = ggpredict(base_m, terms = c("perc_70plus [1:30, by = 0.1]", "case_avg[0.83]"))
plt_age = ggplot(mydf) +
  geom_line(aes(x = x, y = sinh(predicted))) +
  geom_ribbon(aes(x = x, ymin = sinh(conf.low), ymax = sinh(conf.high)), alpha = 0.2) +
  scale_x_continuous(breaks = c(5,15,25), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(1,30), ylim = c(-1.8,-0.8)) +
  labs(x = "Retirement age (%)", y = " ") +
  theme_classic()  +
  theme(
    text = element_text(size=14)
  )


mydf = ggpredict(green_m, terms = c("resid_park [-80:80, by = 1]", "green_m_pp [1,4]"))
plt_park_grn = ggplot(mydf) +
  geom_line(aes(x = x, y = sinh(predicted), colour = group)) +
  geom_ribbon(aes(x = x, ymin = sinh(conf.low), ymax = sinh(conf.high), fill = group), alpha = 0.2) +
  scale_x_continuous(breaks = c(-50,0,50), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(-70,70), ylim = c(-1,1)) +
  scale_fill_manual(name = expression(bold("Green space"~m^2)), labels = c("10","10,000"), values = c("red", "blue")) +
  scale_colour_manual(name = expression(bold("Green space"~m^2)), labels = c("10","10,000"), values = c("red", "blue")) +
  labs(x = "Park use (%)", y = " ") +
  theme_classic()  +
  theme(
    text = element_text(size=14),
    legend.title =  element_text(size=11, face = "bold"),
    legend.text =  element_text(size=10.5, face="italic"),
    legend.position = c(0.6,0.85)
  )


mydf = ggpredict(green_m, terms = c("resid_park [-80:80, by = 1]", "ave_freq_1k[1,8]"))
plt_prk_clst = ggplot(mydf) +
  geom_line(aes(x = x, y = sinh(predicted), colour = group)) +
  geom_ribbon(aes(x = x, ymin = sinh(conf.low), ymax = sinh(conf.high), fill = group), alpha = 0.2) +
  scale_x_continuous(breaks = c(-50,0,50), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(-70,70), ylim = c(-1,1)) +
  scale_fill_manual(name = "Patchiness", labels = c("Low","High"), values = c("red", "blue")) +
  scale_colour_manual(name = "Patchiness", labels = c("Low","High"), values = c("red", "blue")) +
  labs(x = "Park use (%)", y = " ") +
  theme_classic() +
  theme(
    text = element_text(size=14),
    legend.title =  element_text(size=11, face = "bold"),
    legend.text =  element_text(size=10.5, face="italic"),
    legend.position = c(0.6,0.85)
  )


ggarrange(plt_hel, plt_age, plt_mob_case, plt_mob_clust, plt_prk_clst, plt_park_grn, ncol = 3, nrow = 2, labels = c("a,", "b,", "c,", "d,", "e,", "f,"))
ggsave("plots/m_plt.png", width = 10, height = 6.5, dpi = 400)




df = dat[[2]] # Using imputed data

plt_slp = ggplot() +
  geom_line(data = df, aes(date_num, y = sinh(slp), group = name), alpha = 0.02) +
  geom_hline(aes(yintercept = 0), linetype = "dotted", colour = "black", size = 1.1) +
  geom_line(data = df[which(df$name == "Oxford"),], aes(date_num, y = sinh(slp), group = name), alpha = 0.3, colour = "red", size = 1.05)  +
  scale_x_continuous(breaks = c(16,46,76,107,138,164,200,230,260), labels = c("March","April","May", "June", "July","August","September", "October", "November"),guide = guide_axis(n.dodge=2), expand = c(0,0), limits = c(15, 290)) +
  labs(x = "", y = "Case rate (%)") +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    text = element_text(size=13)
  )

plt_mob = ggplot() +
  geom_line(data = df, aes(date_num, y = roll_mob, group = name), alpha = 0.03)  +
  geom_line(data = df[which(df$name == "Oxford"),], aes(date_num, y = roll_mob, group = name), alpha = 0.4, colour = "red", size = 1.05)  +
  geom_hline(aes(yintercept = 0), linetype = "dotted", colour = "black", size = 1.1) +
  scale_x_continuous(breaks = c(16,46,76,107,138,164,200,230,260), labels = c("March","April","May", "June", "July","August","September", "October", "November"),guide = guide_axis(n.dodge=2), expand = c(0,0), limits = c(15, 290)) +
  labs(x = "", y = "Mobility change (%)") +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    text = element_text(size=13)
  )

plt_prk = ggplot() +
  geom_line(data = df, aes(date_num, y = resid_park, group = name), alpha = 0.03)  +
  geom_line(data = df[which(df$name == "Oxford"),], aes(date_num, y = resid_park, group = name), alpha = 0.4, colour = "red", size = 1.05)  +
  geom_hline(aes(yintercept = 0), linetype = "dotted", colour = "black", size = 1.1) +
  scale_x_continuous(breaks = c(16,46,76,107,138,164,200,230,260), labels = c("March","April","May", "June", "July","August","September", "October", "November"),guide = guide_axis(n.dodge=2), expand = c(0,0), limits = c(15, 290)) +
  labs(x = "", y = "Park use (%)") +
  theme_classic() +
  theme(
    text = element_text(size=13)
  )

ggarrange(plt_slp, plt_mob, plt_prk, ncol = 1, nrow = 3, labels = c("a,", "b,", "c,"))
ggsave("plots/temporal_pattern.png", width = 8, height = 8, dpi = 400)


df2 = NULL
for(a in unique(df$name)){
  tmp = subset(df, name == a)
  tmp$case_avg = c(rep(NA,3),zoo::rollmean(tmp$case, k = 7, na.rm = T, align = "left"), rep(NA,3))
  df2 = rbind(df2, tmp)
}
df = df2
df = df[complete.cases(df),]
df$case_avg =log10(df$case_avg + 1)

sum_df = NULL
for(a in c("base", "mobil", "park")){
  for(b in c("mean", "lci", "uci")){
    for(c in unique(df$name)){
      print(c)
      tst = df[which(df$name == c & df$date_num > 15),]
      if(a == "base"){
      } else if (a == "mobil"){
        tst$roll_mob = tst$roll_mob - 20
      } else {
        tst$resid_park = tst$resid_park + 20
      }
      tst$case_avg = NA
      tst$slp_lag1 = NA
      for(d in 1:nrow(tst)){
        if(d == 1){
          tst$case_avg[d] = log10(10)
          tst$slp_lag1[d] = 0.55
        } else {
          tst$case_avg[d] = log10(new_val)
          tst$slp_lag1[d] = slp
        }
        pred_mob = predict(base_m, newdata = tst[d,], se = T)
        if(a == "park"){
          pred_grn = predict(green_m, newdata = tst[d,], se = T)
          if(b == "mean"){
            slp = pred_mob$fit
            slp = ifelse(slp < -4.40, -4.40, slp)
            slp = ifelse(slp > 4.95, 4.95, slp)
            slp_res = pred_grn$fit
            new_val = 10^(tst$case_avg[d])*((sinh(slp + slp_res) + 100)/100)
          } else if (b == "lci"){
            slp = pred_mob$fit - (1.96 * pred_mob$se.fit)
            slp = ifelse(slp < -4.40, -4.40, slp)
            slp = ifelse(slp > 4.95, 4.95, slp)
            new_val = 10^(tst$case_avg[d])*((sinh(slp + slp_res) + 100)/100)
          } else {
            slp = pred_mob$fit + (1.96 * pred_mob$se.fit)
            slp = ifelse(slp < -4.40, -4.40, slp)
            slp = ifelse(slp > 4.95, 4.95, slp)
            new_val = 10^(tst$case_avg[d])*((sinh(slp + slp_res) + 100)/100)
          }
        } else {
          if(b == "mean"){
            slp = pred_mob$fit
            slp = ifelse(slp < -4.40, -4.40, slp)
            slp = ifelse(slp > 4.95, 4.95, slp)
            new_val = 10^(tst$case_avg[d])*((sinh(slp) + 100)/100)
          } else if (b == "lci"){
            slp = pred_mob$fit - (1.96 * pred_mob$se.fit)
            slp = ifelse(slp < -4.40, -4.40, slp)
            slp = ifelse(slp > 4.95, 4.95, slp)
            new_val = 10^(tst$case_avg[d])*((sinh(slp) + 100)/100)
          } else {
            slp = pred_mob$fit + (1.96 * pred_mob$se.fit)
            slp = ifelse(slp < -4.40, -4.40, slp)
            slp = ifelse(slp > 4.95, 4.95, slp)
            new_val = 10^(tst$case_avg[d])*((sinh(slp) + 100)/100)
          }
        }

      }
      tmp_df = data.frame(
        scenario = a,
        ci = b,
        name = c,
        case = sum(tst$case_avg),
        pd = 10^mean(tst$pop_density)
      )
      sum_df = rbind(sum_df, tmp_df)
    }

  }
}


sum_df_base = subset(sum_df, ci == "mean" & scenario == "base")
sum_df_1 = subset(sum_df, ci == "mean" & scenario == "mobil")
sum_df_2 = subset(sum_df, ci == "mean" & scenario == "park")
sum_df_cmb = cbind(sum_df_base[,c(3,4)], sum_df_1[,c(4)], sum_df_2[,c(4,5)])
colnames(sum_df_cmb)[2:4] = c("case_base", "case_mobil", "case_park")

sum_df_cmb$case_change = ((sum_df_cmb$case_mobil/sum_df_cmb$case_base)-1)*100
sum_df_cmb$case_change2 = ((sum_df_cmb$case_park/sum_df_cmb$case_base)-1)*100



scen1 = ggplot() +
  geom_density(data = sum_df_cmb, aes(x = case_change), fill = "red", alpha = 0.2) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(-100, 10)) +
  labs(x = "Case change (%)", y = "Density", title = "Mobility change: -20%\nCase change (%): -51.0 [-88.7, -29.7]") + 
  theme_classic()


scen2 = ggplot() +
  geom_density(data = sum_df_cmb, aes(x = case_change2), fill = "blue", alpha = 0.2) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(-100, 10)) +
  labs(x = "Case change (%)", y = "Density", title = "Park use: +20%\nCase change (%): -5.4 [-17.3, 0.6]") + 
  theme_classic()

cmb_tst = NULL
for(a in c("base", "mobil", "park")){
  print(a)
  for(b in c("mean", "lci", "uci")){
    print(b)
    for(c in "Oxford"){
      print(c)
      tst = df[which(df$name == c & df$date_num > 15),]
      if(a == "base"){
      } else if (a == "mobil"){
        tst$roll_mob = tst$roll_mob - 20
      } else {
        tst$resid_park = tst$resid_park + 20
      }
      tst$case_avg = NA
      tst$slp_lag1 = NA
      for(d in 1:nrow(tst)){
        message(d)
        if(d == 1){
          tst$case_avg[d] = log10(10)
          tst$slp_lag1[d] = 0.55
        } else {
          tst$case_avg[d] = log10(new_val)
          tst$slp_lag1[d] = slp
        }
        pred_mob = predict(base_m, newdata = tst[d,], se = T)
        if(a == "park"){
          pred_grn = predict(green_m, newdata = tst[d,], se = T, level = 0.5)
          if(b == "mean"){
            slp = pred_mob$fit
            slp = ifelse(slp < -4.40, -4.40, slp)
            slp = ifelse(slp > 4.95, 4.95, slp)
            slp_res = pred_grn$fit
            new_val = 10^(tst$case_avg[d])*((sinh(slp + slp_res) + 100)/100)
          } else if (b == "lci"){
            slp = pred_mob$fit - (1.96 * pred_mob$se.fit)
            slp = ifelse(slp < -4.40, -4.40, slp)
            slp = ifelse(slp > 4.95, 4.95, slp)
            slp_res = pred_grn$fit - (1.96*pred_grn$se.fit)
            new_val = 10^(tst$case_avg[d])*((sinh(slp + slp_res) + 100)/100)
          } else {
            slp = pred_mob$fit + (1.96 * pred_mob$se.fit)
            slp = ifelse(slp < -4.40, -4.40, slp)
            slp = ifelse(slp > 4.95, 4.95, slp)
            slp_res = pred_grn$fit + (1.96*pred_grn$se.fit)
            new_val = 10^(tst$case_avg[d])*((sinh(slp + slp_res) + 100)/100)
          }
        } else {
          if(b == "mean"){
            slp = pred_mob$fit
            slp = ifelse(slp < -4.40, -4.40, slp)
            slp = ifelse(slp > 4.95, 4.95, slp)
            new_val = 10^(tst$case_avg[d])*((sinh(slp) + 100)/100)
          } else if (b == "lci"){
            slp = pred_mob$fit - (1.96 * pred_mob$se.fit)
            slp = ifelse(slp < -4.40, -4.40, slp)
            slp = ifelse(slp > 4.95, 4.95, slp)
            new_val = 10^(tst$case_avg[d])*((sinh(slp) + 100)/100)
          } else {
            slp = pred_mob$fit + (1.96 * pred_mob$se.fit)
            slp = ifelse(slp < -4.40, -4.40, slp)
            slp = ifelse(slp > 4.95, 4.95, slp)
            new_val = 10^(tst$case_avg[d])*((sinh(slp) + 100)/100)
          }
        }
      }
      tst$scen = a
      tst$level = b
      cmb_tst = rbind(cmb_tst, tst)
    }
  }
}

base_proj = cbind(
  subset(cmb_tst, level == "mean" & scen == "base")[,c(1:28)],
  subset(cmb_tst, level == "lci" & scen == "base")[,c(28)],
  subset(cmb_tst, level == "uci" & scen == "base")[,c(28)]
)
colnames(base_proj)[28:30] = c("mn", "lci", "uci")

mob_proj = cbind(
  subset(cmb_tst, level == "mean" & scen == "mobil")[,c(1:28)],
  subset(cmb_tst, level == "lci" & scen == "mobil")[,c(28)],
  subset(cmb_tst, level == "uci" & scen == "mobil")[,c(28)]
)
colnames(mob_proj)[28:30] = c("mn", "lci", "uci")

park_proj = cbind(
  subset(cmb_tst, level == "mean" & scen == "park")[,c(1:28)],
  subset(cmb_tst, level == "lci" & scen == "park")[,c(28)],
  subset(cmb_tst, level == "uci" & scen == "park")[,c(28)]
)
colnames(park_proj)[28:30] = c("mn", "lci", "uci")


project= ggplot() +
  geom_line(data = base_proj, aes(x= date_num, y = 10^mn)) +
  geom_ribbon(data = base_proj, aes(x = date_num, ymin = 10^lci, ymax = 10^uci), alpha = 0.2) +
  geom_line(data = mob_proj, aes(x= date_num, y = 10^mn), colour = "red") +
  geom_ribbon(data = mob_proj, aes(x = date_num, ymin = 10^lci, ymax = 10^uci), alpha = 0.2, fill = "red") +
  geom_line(data = park_proj, aes(x= date_num, y = 10^mn), colour = "blue") +
  geom_ribbon(data = park_proj, aes(x = date_num, ymin = 10^lci, ymax = 10^uci), alpha = 0.2, fill = "blue") +
  scale_x_continuous(breaks = c(16,46,76,107,138,164,200,230,260), labels = c("March","April","May", "June", "July","August","September", "October", "November"),guide = guide_axis(n.dodge=2), expand = c(0,0)) +
  labs(x = " ", y = "Cases", title = "Projected cases in Oxford") +
  theme_classic()

ggarrange(project, ggarrange(scen1, scen2, ncol = 1, nrow = 2, labels = c("b,", "c,")), ncol = 2, nrow = 1, labels = c("a,", " "))
ggsave("plots/projection.png", width = 8, height = 5, dpi = 400)


library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
require(gridExtra)

tot_case = df %>%
  group_by(name) %>%
  dplyr::summarise(sum_case = sum(case))

sum_df_cmb = left_join(sum_df_cmb, tot_case)



high_col = "#ffe6e6"
med_col = "#ff4d4d"
med_col_map = "white"
low_col = "#b30000"

la <- st_read("data/la_boundary.shp")
la_proj <-st_transform(la,"+proj=longlat +datum=WGS84")
colnames(la_proj)[3] = "la"
colnames(sum_df_cmb)[1] = "la"
la_proj = merge(la_proj, sum_df_cmb, by = "la")
uk = subset(ne_countries(scale = "medium", returnclass = "sf"), name == "United Kingdom")
la_proj$pop = la_proj$pd * (la_proj$st_areasha/1000000)
la_proj$case_cap = (la_proj$sum_case / la_proj$pop)*100

a = ggplot(data = uk) +
  geom_sf() +
  geom_sf(data = la_proj, aes(fill = case_change)) +
  geom_rect(mapping=aes(xmin=-0.5, xmax=0.5, ymin=51.25, ymax=51.75), color="black", fill = NA, alpha = 0.5) +
  
  scale_fill_gradient2(name = "Case change\n(%)",low = low_col, mid = med_col, high = high_col, midpoint = -50, breaks = c(-100,-50,0), limits = c(-100,0)) +
  scale_x_continuous(breaks = c(-6,-3,0)) +
  scale_y_continuous(breaks = c(52, 55, 58)) +
  coord_sf(xlim = c(-8, 3), ylim = c(50,59), expand = FALSE) +
  labs(title = " ") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    text = element_text(size=11),
    plot.title =  element_text(size=13),
    legend.text =  element_text(size=11, face="italic")
  )

b = ggplot(data = uk) +
  geom_sf() +
  geom_sf(data = la_proj, aes(fill = case_change)) +
  scale_fill_gradient2(name = "Pre-peak daily case\nrate change (%)",low = low_col, mid = med_col, high = high_col, midpoint = -50, breaks = c(-100,-50, -0), limits = c(-100,0)) +
  coord_sf(xlim = c(-0.5, 0.5), ylim = c(51.25,51.75), expand = FALSE) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size=14, hjust = 0.5),
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    text = element_text(size=11.5)) +
  labs(title = "London")



png(filename="plots/map_case_change.png", width = 900, height = 1200, units = "px", res = 250) 
grid.arrange(a, b, layout_matrix =rbind(c(1, 1, 1, 1, 2, 2),
                                        c(1, 1, 1, 1, 2, 2),
                                        c(1, 1, 1, 1, 1, 1),
                                        c(1, 1, 1, 1, 1, 1),
                                        c(1, 1, 1, 1, 1, 1),
                                        c(1, 1, 1, 1, 1, 1)))
dev.off()

la_proj$case_change2 = ifelse(la_proj$case_change2 < -20, -20, 0)

a = ggplot(data = uk) +
  geom_sf() +
  geom_sf(data = la_proj, aes(fill = case_change2)) +
  geom_rect(mapping=aes(xmin=-0.5, xmax=0.5, ymin=51.25, ymax=51.75), color="black", fill = NA, alpha = 0.5) +
  scale_fill_gradient2(name = "Case change\n(%)",low = "#4d4dff", mid = "#9999ff", high = "#ccccff", midpoint = -10, breaks = c(-20,-10,0), limits = c(-20,5)) + 
  scale_x_continuous(breaks = c(-6,-3,0)) +
  scale_y_continuous(breaks = c(52, 55, 58)) +
  coord_sf(xlim = c(-8, 3), ylim = c(50,59), expand = FALSE) +
  labs(title = " ") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    text = element_text(size=11),
    plot.title =  element_text(size=13),
    legend.text =  element_text(size=11, face="italic")
  )
a

b = ggplot(data = uk) +
  geom_sf() +
  geom_sf(data = la_proj, aes(fill = case_change2)) +
  scale_fill_gradient2(name = "Case change\n(%)",low = "#4d4dff", mid = "#9999ff", high = "#ccccff", midpoint = -10, breaks = c(-20,-10,0), limits = c(-20,5)) + 
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


png(filename="plots/map_adj_case.png", width = 900, height = 1200, units = "px", res = 250) 
grid.arrange(a, b, layout_matrix =rbind(c(1, 1, 1, 1, 2, 2),
                                        c(1, 1, 1, 1, 2, 2),
                                        c(1, 1, 1, 1, 1, 1),
                                        c(1, 1, 1, 1, 1, 1),
                                        c(1, 1, 1, 1, 1, 1),
                                        c(1, 1, 1, 1, 1, 1),
                                        c(1, 1, 1, 1, 1, 1)))
dev.off()

a = ggplot(data = uk) +
  geom_sf() +
  geom_sf(data = la_proj, aes(fill = case_cap)) +
  geom_rect(mapping=aes(xmin=-0.5, xmax=0.5, ymin=51.25, ymax=51.75), color="black", fill = NA, alpha = 0.5) +

  scale_fill_gradient2(name = "Cases per\ncapita (%)",low = "#f9e6ff", mid = "#d24dff", high = "#600080", midpoint = 30, breaks = c(0,30,60), limits = c(0,62.5)) +
  scale_x_continuous(breaks = c(-6,-3,0)) +
  scale_y_continuous(breaks = c(52, 55, 58)) +
  coord_sf(xlim = c(-8, 3), ylim = c(50,59), expand = FALSE) +
  labs(title = " ") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    text = element_text(size=11),
    plot.title =  element_text(size=13),
    legend.text =  element_text(size=11, face="italic")
  )
a
b = ggplot(data = uk) +
  geom_sf() +
  geom_sf(data = la_proj, aes(fill = case_cap)) +
  scale_fill_gradient2(name = "Cases per\ncapita (%)",low = "#f9e6ff", mid = "#d24dff", high = "#600080", midpoint = 30, breaks = c(0,30,60), limits = c(0,62.5)) +
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



png(filename="plots/case_cap.png", width = 900, height = 1200, units = "px", res = 250) 
grid.arrange(a, b, layout_matrix =rbind(c(1, 1, 1, 1, 2, 2),
                                        c(1, 1, 1, 1, 2, 2),
                                        c(1, 1, 1, 1, 1, 1),
                                        c(1, 1, 1, 1, 1, 1),
                                        c(1, 1, 1, 1, 1, 1),
                                        c(1, 1, 1, 1, 1, 1)))
dev.off()

