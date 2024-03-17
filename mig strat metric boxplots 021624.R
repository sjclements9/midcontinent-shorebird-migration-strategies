# preliminary results - raw data
# mig strategy chapter
# SJC
# March 2022

# data
# everything in this commented out section I'm pretty sure goes into the MC data all file...
#setwd('G:\\My Drive\\shorebirds\\analysis_code\\migration_strategies\\metric_results')
#amav <- read.csv('amav_mig_metrics_030322_2hr.csv')
#bbpl <- read.csv('bbpl_mig_metrics_030322_2hr.csv')
#hugo <- read.csv('hugo_mig_metrics_030322_2hr.csv')

#amav$travel_to_stopover <- readRDS('G:\\My Drive\\shorebirds\\analysis_code\\migration_strategies\\updated_analysis_feb24\\amav_travel_to_stopover.rds')
#bbpl$travel_to_stopover <- readRDS('G:\\My Drive\\shorebirds\\analysis_code\\migration_strategies\\updated_analysis_feb24\\bbpl_travel_to_stopover.rds')
#hugo$travel_to_stopover <- readRDS('G:\\My Drive\\shorebirds\\analysis_code\\migration_strategies\\updated_analysis_feb24\\hugo_travel_to_stopover.rds')

#amav$total_stop_duration <- readRDS('G:\\My Drive\\shorebirds\\analysis_code\\migration_strategies\\updated_analysis_feb24\\amav_total_stop_duration.rds')
#bbpl$total_stop_duration <- readRDS('G:\\My Drive\\shorebirds\\analysis_code\\migration_strategies\\updated_analysis_feb24\\bbpl_total_stop_duration.rds')
#hugo$total_stop_duration <- readRDS('G:\\My Drive\\shorebirds\\analysis_code\\migration_strategies\\updated_analysis_feb24\\hugo_total_stop_duration.rds')

#met <- rbind(amav, bbpl, hugo)
#head(met)
#nrow(met)
# 45

#met$dist_per_day <- met$mig_dist_km/met$mig_duration
#met$dist_per_day[which(met$id==190092)] <- NA

#write.csv(met, "MC_Shorebird_Metric_Data_All_021624.csv")

met <- read.csv("MC_Shorebird_Metric_Data_All_021624.csv")

boxplot(travel_to_stopover~species, data=met) # hugo have more travel per day of stopover, amav have the most variation (the crazy one is legit)
# makes sense given the hardcore first flight of HUGO

test_sub <- met[which(met$id!=190096),]
boxplot(travel_to_stopover~species, data=test_sub) # it doesn't change a ton even without the nutcase avocet

# 45 total
# 24 BBPL
# 14 AMAV
# 7 HUGO

# list of metrics
# nstop = count of stops after leaving winter and before entering breeding area
# prstop = proportion of migration duration spent in stopover (based on time, not # of points)
# mig duration = time between day the bird left wintering area and time the bird entered breeding area
# depart and arrive times = ordinal day of departure from wintering and arrival to breeding
# mean/sd stop duration = based on time spent in individual stopovers
# stop step = based on step lenghts across all individual stopovers
# migration distance = sum of daily displacements between winter and breeding (migration points) daily
#  displacements were used to reduce effect of within-stopover movmements, similar to the 5 day subset
#  Amy used for WFG total migration distance to minimize effect of stopovers for longer WFG migration

# New added 2024 - 
# travel_to_stopover is ratio of stopover days to travel distance, supposed to help differentiate between time minimizing and
# energy minimizing strategies

# plots 
# 2. One box plot for each migration metric with one box for each species (9 total)

# Migration distance
#boxplot(mig_dist_km~species, data=met)

boxplot(amav$mig_dist_km, bbpl$mig_dist_km, hugo$mig_dist_km, 
        main="A) Migration Distance", outline=F, ylim=c(1000, 16000),
        col=c('gray40', 'gray40', 'gray40'), border=c(rep('black', times=3)),
        xaxt="n", axes=T,boxwex=0.3, whisklty=1, staplelwd=0, staplewex=0, whisklwd=2.5, boxlwd=2, medcol="black",
        cex.axis=1.2)
mtext('AMAV', side=1, line=1, at=1, cex=1.5)
mtext('BBPL', side=1, line=1, at=2, cex=1.5)
mtext('HUGO', side=1, line=1, at=3, cex=1.5)
mtext('Kilometers', side=2, line=2.2, cex=1.2)
#legend('topleft', legend = c("Louisiana", "Texas"), fill=c("gray40", "black"), bty='n',  y.intersp = 0.5)
lines(x=c(1,3), y=c(16000,16000), lwd=2) # lines are present if there is a significant difference
lines(x=c(2,3), y=c(15500,15500), lwd=2)
lines(x=c(1,2), y=c(15000,15000), lwd=2)
box()


# Number of stopovers
#boxplot(amav$nstop, bbpl$nstop, hugo$nstop)

boxplot(amav$nstop, bbpl$nstop, hugo$nstop,
        main="B) Number of Stopovers", outline=F, ylim=c(2,16),
        col=c('gray40', 'gray40', 'gray40'), border=c(rep('black', times=3)),
        xaxt="n", axes=T,boxwex=0.3, whisklty=1, staplelwd=0, staplewex=0, whisklwd=2.5, boxlwd=2, medcol="black",
        cex.axis=1.2)
mtext('AMAV', side=1, line=1, at=1, cex=1.5)
mtext('BBPL', side=1, line=1, at=2, cex=1.5)
mtext('HUGO', side=1, line=1, at=3, cex=1.5)
mtext('Number of Stopovers', side=2, line=2.2, cex=1.2)
lines(x=c(1,3), y=c(16,16), lwd=2)
lines(x=c(2,3), y=c(15.5,15.5), lwd=2)
box()


# Migration Duration
#boxplot(amav$mig_duration, bbpl$mig_duration, hugo$mig_duration)

boxplot(amav$mig_duration, bbpl$mig_duration, hugo$mig_duration, ylim=c(0,45),
        main="C) Migration Duration", outline=F, 
        col=c('gray40', 'gray40', 'gray40'), border=c(rep('black', times=3)),
        xaxt="n", axes=T,boxwex=0.3, whisklty=1, staplelwd=0, staplewex=0, whisklwd=2.5, boxlwd=2, medcol="black",
        cex.axis=1.2)
mtext('AMAV', side=1, line=1, at=1, cex=1.5)
mtext('BBPL', side=1, line=1, at=2, cex=1.5)
mtext('HUGO', side=1, line=1, at=3, cex=1.5)
mtext('Days', side=2, line=2.2, cex=1.2)
lines(x=c(1,3), y=c(45,45), lwd=2)
lines(x=c(2,3), y=c(44,44), lwd=2)
box()

# Proportion of time in Stopover
#boxplot(amav$prstop, bbpl$prstop, hugo$prstop)

boxplot(amav$prstop, bbpl$prstop, hugo$prstop, ylim=c(0,1),
        main="D) Proportion of time in Stopover", outline=F, 
        col=c('gray40', 'gray40', 'gray40'), border=c(rep('black', times=3)),
        xaxt="n", axes=T,boxwex=0.3, whisklty=1, staplelwd=0, staplewex=0, whisklwd=2.5, boxlwd=2, medcol="black",
        cex.axis=1.2)
mtext('AMAV', side=1, line=1, at=1, cex=1.5)
mtext('BBPL', side=1, line=1, at=2, cex=1.5)
mtext('HUGO', side=1, line=1, at=3, cex=1.5)
mtext('Proportion', side=2, line=2.2, cex=1.2)
lines(x=c(2,3), y=c(1,1), lwd=2)
box()


# depart time
#boxplot(depart_time~species, data=met, horizontal=T)

boxplot(amav$depart_time, bbpl$depart_time, hugo$depart_time, ylim=c(75, 145),
        main="A) Departure Time", outline=F, 
        col=c('gray40', 'gray40', 'gray40'), border=c(rep('black', times=3)),
        axes=T,boxwex=0.3, whisklty=1, staplelwd=0, staplewex=0, whisklwd=2.5, boxlwd=2, medcol="black",
        cex.axis=1.2, horizontal=T)
mtext('AMAV', side=2, line=1, at=1, cex=1.2)
mtext('BBPL', side=2, line=1, at=2, cex=1.2)
mtext('HUGO', side=2, line=1, at=3, cex=1.2)
mtext('Day of Year', side=1, line=2, cex=1.2)
lines(y=c(1,3), x=c(74,74), lwd=2)
lines(y=c(1,2), x=c(75,75), lwd=2)
lines(y=c(2,3), x=c(76,76), lwd=2)
box()

# arrive time
#boxplot(arrive_time~species, data=met, horizontal=T)

boxplot(amav$arrive_time, bbpl$arrive_time, hugo$arrive_time, ylim=c(115,165),
        main="C) Arrival Time", outline=F, 
        col=c('gray40', 'gray40', 'gray40'), border=c(rep('black', times=3)),
        axes=T,boxwex=0.3, whisklty=1, staplelwd=0, staplewex=0, whisklwd=2.5, boxlwd=2, medcol="black",
        cex.axis=1.2, horizontal=T)
mtext('AMAV', side=2, line=1, at=1, cex=1.2) 
mtext('BBPL', side=2, line=1, at=2, cex=1.2)
mtext('HUGO', side=2, line=1, at=3, cex=1.2)
mtext('Day of Year', side=1, line=2, cex=1.2)
lines(y=c(1,2), x=c(115,115), lwd=2)
lines(y=c(2,3), x=c(116,116), lwd=2)
box()


# stop duration
#boxplot(mean_stop_duration~species, data=met)

boxplot(amav$mean_stop_duration, bbpl$mean_stop_duration, hugo$mean_stop_duration,
        main="E) Mean Stop Duration", outline=F, 
        col=c('gray40', 'gray40', 'gray40'), border=c(rep('black', times=3)),
        xaxt="n", axes=T,boxwex=0.3, whisklty=1, staplelwd=0, staplewex=0, whisklwd=2.5, boxlwd=2, medcol="black",
        cex.axis=1.2)
mtext('AMAV', side=1, line=1, at=1, cex=1.5)
mtext('BBPL', side=1, line=1, at=2, cex=1.5)
mtext('HUGO', side=1, line=1, at=3, cex=1.5)
mtext('Hours', side=2, line=2.2, cex=1.2)
box()


#par(mfrow=c(4,2))


# NOT USING IN PAPER-------------------------

# stop step length
boxplot(mean_stop_step~species, data=met)

boxplot(amav$mean_stop_step, bbpl$mean_stop_step, hugo$mean_stop_step,
        main="Mean Stopover Step length", outline=F, 
        col=c('gray40', 'gray40', 'gray40'), border=c(rep('black', times=3)),
        xaxt="n", axes=T,boxwex=0.3, whisklty=1, staplelwd=0, staplewex=0, whisklwd=2.5, boxlwd=2, medcol="black",
        cex.axis=1.2)
mtext('AMAV', side=1, line=1, at=1, cex=1.5)
mtext('BBPL', side=1, line=1, at=2, cex=1.5)
mtext('HUGO', side=1, line=1, at=3, cex=1.5)
mtext('Meters (per ~2 hours)', side=2, line=2.2, cex=1.2) 
#legend('topleft', legend = c("Louisiana", "Texas"), fill=c("gray40", "black"), bty='n',  y.intersp = 0.5)
box()


boxplot(dist_per_day~species, data=met)

boxplot(met$dist_per_day[which(met$species=='AMAV')], met$dist_per_day[which(met$species=='BBPL')], met$dist_per_day[which(met$species=='HUGO')],
        main="Distance per Day", outline=F, 
        col=c('gray40', 'gray40', 'gray40'), border=c(rep('black', times=3)),
        xaxt="n", axes=T,boxwex=0.3, whisklty=1, staplelwd=0, staplewex=0, whisklwd=2.5, boxlwd=2, medcol="black",
        cex.axis=1.2)
mtext('AMAV', side=1, line=1, at=1, cex=1.5)
mtext('BBPL', side=1, line=1, at=2, cex=1.5)
mtext('HUGO', side=1, line=1, at=3, cex=1.5)
mtext('km', side=2, line=2.2, cex=1.2)
#legend('topleft', legend = c("Louisiana", "Texas"), fill=c("gray40", "black"), bty='n',  y.intersp = 0.5)
box()

# try to make something draw lines

mt <- met[,c(2,7:8,15)]
head(mt)
mt$number <- seq(1, nrow(mt), by=1)
mt
day_seq <- seq(min(mt$depart_time), max(mt$arrive_time), 1)

plot(x=day_seq, y=rep(0, times=length(day_seq)), ylim=c(0,50), type='n', ylab="", xlab="")
for (i in 1:nrow(mt)){
  if (mt$species[i]=="AMAV"){
    abline(a=100, b=140, col='black')
    #abline(a=mt$depart_time[i], b=mt$arrive_time[i], h=mt$number[i], col='deepskyblue4')
  }
}

ggplot(mt, aes(group=id)) + 
  geom_segment(aes(x=depart_time, y=number, xend=arrive_time, yend=number, color=species),size=1.2, data=mt)+
  scale_color_manual(values=c('deepskyblue4', 'black', 'darkorange3'))+
  theme_bw()+
  xlab("Day of Year")+
  ylab("Individual")+
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x=element_text(size=18))
  
  
  
