# one way ANOVA
# Migration strategy chapter
# SJC
# March 2022

# Data
setwd('F:\\shorebirds\\analysis_code\\migration_strategies\\metric_results')

# 2 hour duty cycles
met <- read.csv('MC_Shorebird_Metric_Data_All_021624.csv')

amav_tbl <- data.frame(matrix(NA, nrow=3, ncol=ncol(amav)))
names(amav_tbl) <- colnames(amav)
for (i in 1:ncol(amav)){
  amav_tbl[1,i] <- mean(amav[,i], na.rm=T)
  amav_tbl[2,i] <- sd(amav[,i], na.rm=T)
}

bbpl_tbl <- data.frame(matrix(NA, nrow=3, ncol=ncol(bbpl)))
names(bbpl_tbl) <- colnames(bbpl)
for (i in 1:ncol(bbpl)){
  bbpl_tbl[1,i] <- mean(bbpl[,i], na.rm=T)
  bbpl_tbl[2,i] <- sd(bbpl[,i], na.rm=T)
}

hugo_tbl <- data.frame(matrix(NA, nrow=3, ncol=ncol(hugo)))
names(hugo_tbl) <- colnames(hugo)
for (i in 1:ncol(hugo)){
  hugo_tbl[1,i] <- mean(hugo[,i], na.rm=T)
  hugo_tbl[2,i] <- sd(hugo[,i], na.rm=T)
}


# 1 day duty cycles

# Chosen migration metrics (* = could also do for 24h duty cycle)
# Distance*
# number of stopovers*
# Duration*
# Proportion of time in stopover
# Departure date*
# arrival date*
# Stop Duration
# Added travel distance to stopover ratio ** (2024)

# Things seem reasonably normally distributed...

# ANOVA for each
# **************************************************************************************************
head(met)

# Migration Distance
dist_aov <- anova(lm(mig_dist_km~species, data=met))
dist_aov
# number of stopovers
nstop_aov <- anova(lm(nstop~species, data=met))
nstop_aov
# Migration duration
migdur_aov <- anova(lm(mig_duration~species, data=met))
migdur_aov
# proportion of time in stopover
prstop_aov <- anova(lm(prstop~species, data=met))
prstop_aov
# departure
depart_aov <- anova(lm(depart_time~species, data=met))
depart_aov
# arrival
arrive_aov <- anova(lm(arrive_time~species, data=met))
arrive_aov
# stopover duration
stopdur_aov <- anova(lm(mean_stop_duration~species, data=met))
stopdur_aov
# travel to stopover ratio
tts_aov <- anova(lm(travel_to_stopover~species, data=met))
tts_aov

# T-Test Pairwise for each
# *************************************************************************************************
# Migration distance
dist_t <- pairwise.t.test(met$mig_dist_km, met$species, p.adj='bonferroni')
dist_t
# Migration distance
nstop_t <- pairwise.t.test(met$nstop, met$species, p.adj='bonferroni')
nstop_t
# Migration duration
migdur_t <- pairwise.t.test(met$mig_duration, met$species, p.adj='bonferroni')
migdur_t
# Proportion of time in Stopover
prstop_t <- pairwise.t.test(met$prstop, met$species, p.adj='bonferroni')
prstop_t
# Departure
depart_t <- pairwise.t.test(met$depart_time, met$species, p.adj='bonferroni')
depart_t
# Arrival
arrive_t <- pairwise.t.test(met$arrive_time, met$species, p.adj='bonferroni')
arrive_t
# Stopover duration
stopdur_t <- pairwise.t.test(met$mean_stop_duration, met$species, p.adj='bonferroni')
stopdur_t
# Stopover duration
tts_t <- pairwise.t.test(met$travel_to_stop, met$species, p.adj='bonferroni')
tts_t


