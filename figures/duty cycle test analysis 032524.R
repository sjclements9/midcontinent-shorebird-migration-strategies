# PLOTS - DATA SUBSET TEST
# Migration Strategy Comparing
# SJC
# March 2022

library(ggplot2)
library(cowplot)
library(ggpubr)
library(lme4)
library(ciTools)
library(rsq)
library(modelr)
library(lmerTest)
# data

pd6 <- read.csv('G:\\My Drive\\shorebirds\\analysis_code\\migration_strategies\\metric_results\\stop_table_6pd.csv')
pd3 <- read.csv('G:\\My Drive\\shorebirds\\analysis_code\\migration_strategies\\metric_results\\stop_table_3pd.csv')
pd1 <- read.csv('G:\\My Drive\\shorebirds\\analysis_code\\migration_strategies\\metric_results\\stop_table_1pd.csv')
pd12 <-read.csv('G:\\My Drive\\shorebirds\\analysis_code\\migration_strategies\\metric_results\\stop_table_12pd.csv')

dt <- rbind(pd1, pd3, pd6, pd12)

am12 <- pd12[which(pd12$species=='AMAV'),]
bb12 <- pd12[which(pd12$species=='BBPL'),]
hu12 <- pd12[which(pd12$species=='HUGO'),]

am6 <- pd6[which(pd6$species=='AMAV'),]
bb6 <- pd6[which(pd6$species=='BBPL'),]
hu6 <- pd6[which(pd6$species=='HUGO'),]

am3 <- pd3[which(pd3$species=='AMAV'),]
bb3 <- pd3[which(pd3$species=='BBPL'),]
hu3 <- pd3[which(pd3$species=='HUGO'),]

am1 <- pd1[which(pd1$species=='AMAV'),]
bb1 <- pd1[which(pd1$species=='BBPL'),]
hu1 <- pd1[which(pd1$species=='HUGO'),]


head(pd6)
head(pd3)
head(pd1)
head(pd12)

dt <- dt[-41,] # one bird on 1-day duty cycle gets weird
#dt <- dt %>% dplyr::arrange(., ppd)
# means and sds
mean(am12$nstop)
sd(am12$nstop)
mean(am6$nstop)
sd(am6$nstop)
mean(am3$nstop)
sd(am3$nstop)
mean(am1$nstop)
sd(am1$nstop)

mean(bb12$nstop)
sd(bb12$nstop)
mean(bb6$nstop)
sd(bb6$nstop)
mean(bb3$nstop)
sd(bb3$nstop)
mean(bb1$nstop)
sd(bb1$nstop)

mean(hu12$nstop)
sd(hu12$nstop)
mean(hu6$nstop)
sd(hu6$nstop)
mean(hu3$nstop)
sd(hu3$nstop)
mean(hu1$nstop)
sd(hu1$nstop)


#head(dt)
nstoplm <- lm(nstop ~ as.factor(ppd), data=dt)
summary(nstoplm )
nstoplm$coefficients
confint(nstoplm )
rsquare(nstoplm , dt) # [1] 0.439
#res_nstop <- data.frame(ppd=c(1,3,6,12), duty_cycle=c('24-Hr', "8-Hr", "4-Hr", "2-Hr"),
#                      mean=c(0, as.numeric(nstoplm$coefficients)[2:4]),
#                      lci=c(0, as.numeric(confint(nstoplm)[,1])[2:4]),
#                      hci=c(0, as.numeric(confint(nstoplm)[,2])[2:4]))
res_nstop <- data.frame(ppd=c(3,6,12), duty_cycle=c( "8-Hr", "4-Hr", "2-Hr"),
                      mean=as.numeric(nstoplm$coefficients)[2:4],
                      lci=as.numeric(confint(nstoplm)[,1])[2:4],
                      hci=as.numeric(confint(nstoplm)[,2])[2:4])
pa <- ggplot(res_nstop)+
  geom_point(aes(x=as.factor(ppd), y=mean), size=4)+
  theme_bw(base_size=14)+
  geom_errorbar(aes(x=as.factor(ppd), ymin=lci, ymax=hci, width=0.1), linewidth=1)+
  scale_x_discrete(labels=c('8','4','2'))+
  xlab('')+
  ylab('Effect Sizes')+
  ggtitle('(A) Number of\n     Stopovers')+
  ylim(c(0,8.8))+
  geom_hline(yintercept=0, color='gray50', linewidth=1, linetype='dashed')+
  annotate("text", label='24 hours', size=5,hjust=0,x=0.5, y=0.3, color='gray50')+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        axis.text.x=element_text(size=16), axis.text.y=element_text(size=14),
        plot.title=element_text(size=14))
pa
  

migdurlm <- lm(mig_duration ~ as.factor(ppd), data=dt)
summary(migdurlm)
migdurlm$coefficients
confint(migdurlm)
rsquare(migdurlm, dt) # [1] 0.001


stopdurlm <- lm(mean_stop_duration ~ as.factor(ppd), data=dt)
summary(stopdurlm)
stopdurlm$coefficients
confint(stopdurlm)
rsquare(stopdurlm, dt) # [1] 0.16
res_stopdur <- data.frame(ppd=c(3,6,12), duty_cycle=c( "8-Hr", "4-Hr", "2-Hr"),
                        mean=as.numeric(stopdurlm$coefficients)[2:4],
                        lci=as.numeric(confint(stopdurlm)[,1])[2:4],
                        hci=as.numeric(confint(stopdurlm)[,2])[2:4])
pb <- ggplot(res_stopdur)+
  geom_point(aes(x=as.factor(ppd), y=mean), size=4)+
  theme_bw(base_size=14)+
  geom_errorbar(aes(x=as.factor(ppd), ymin=lci, ymax=hci, width=0.1), linewidth=1)+
  scale_x_discrete(labels=c('8','4','2'))+
  xlab('Time between locations (hours)')+
  ylab('')+
  ggtitle('(B) Mean Stopover\n     Duration')+
  #ylim(c(0,8.8))+
  geom_hline(yintercept=0, color='gray50', linewidth=1, linetype='dashed')+
  annotate("text", label='24 hours', size=5,hjust=0,x=0.5, y=-1.2, color='gray50')+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        axis.text.x=element_text(size=16), axis.text.y=element_text(size=14),
        plot.title=element_text(size=14), axis.title.x=element_text(size=13))
pb


departlm <- lm(depart_time ~ as.factor(ppd), data=dt[-41,])
summary(departlm)
departlm$coefficients
confint(departlm)
rsquare(departlm, dt[-41,]) # [1] 0.16

stopsteplm <- lm(mean_stop_step/1000 ~ as.factor(ppd), data=dt)
summary(stopsteplm)
stopsteplm$coefficients
confint(stopsteplm)
rsquare(stopsteplm, dt) # 0.16
res_stopstep <- data.frame(ppd=c(3,6,12), duty_cycle=c( "8-Hr", "4-Hr", "2-Hr"),
                          mean=as.numeric(stopsteplm$coefficients)[2:4],
                          lci=as.numeric(confint(stopsteplm)[,1])[2:4],
                          hci=as.numeric(confint(stopsteplm)[,2])[2:4])
pc <- ggplot(res_stopstep)+
  geom_point(aes(x=as.factor(ppd), y=mean), size=4)+
  theme_bw(base_size=14)+
  geom_errorbar(aes(x=as.factor(ppd), ymin=lci, ymax=hci, width=0.1), linewidth=1)+
  scale_x_discrete(labels=c('8','4','2'))+
  xlab('')+
  ylab('')+
  ggtitle('(C) Mean Stopover\n     Step Length')+
  #ylim(c(0,8.8))+
  geom_hline(yintercept=0, color='gray50', linewidth=1, linetype='dashed')+
  annotate("text", label='24 hours', size=5,hjust=0,x=0.5, y=-0.15, color='gray50')+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        axis.text.x=element_text(size=16), axis.text.y=element_text(size=14), 
        plot.title=element_text(size=14))
pc

prstoplm <- lm(prstop ~ as.factor(ppd), data=dt)
summary(prstoplm)
prstoplm$coefficients
confint(prstoplm)
rsquare(prstoplm, dt) # 0.16

ggarrange(pa, pb, pc, nrow=1, ncol=3)

#test2 <- lm(mig_duration ~ species + as.factor(ppd), data=dt)
#summary(test2)

#test <- lm(nstop ~ species + as.factor(ppd) + species*as.factor(ppd), data=dt)
#summary(test)

#dist_t <- pairwise.t.test(dt$mig_duration, as.factor(dt$ppd), p.adj='bonferroni')
#dist_t

#amav_tbl <- data.frame(matrix(NA, nrow=3, ncol=ncol(amav)))
#names(amav_tbl) <- colnames(amav)
#for (i in 1:ncol(amav)){
#  amav_tbl[1,i] <- mean(amav[,i], na.rm=T)
#  amav_tbl[2,i] <- sd(amav[,i], na.rm=T)
#}


pcr4 <- glm(pc4~start_lat+start_lon+end_lat+end_lon, data=amwo_pca_data)
summary(pcr4)
pcr4$coefficients
confint(pcr4)
rsquare(pcr4, amwo_pca_data) # [1] 0.4662899
rsq.partial(pcr4)


# list of metrics
# nstop = count of stops after leaving winter and before entering breeding area
# prstop = proportion of migration duration spent in stopover (based on time, not # of points)
# mig duration = time between day the bird left wintering area and time the bird entered breeding area
# depart and arrive times = ordinal day of departure from wintering and arrival to breeding
# mean/sd stop duration = based on time spent in individual stopovers
# stop step = based on step lengths across all individual stopovers

# plots 
# 2. One box plot for each migration metric by species (not depart # arrive but all otehrs)

# nstop
boxplot(am12$nstop, am6$nstop, am3$nstop, am1$nstop,
        bb12$nstop, bb6$nstop, bb3$nstop, bb1$nstop,
        hu12$nstop, hu6$nstop, hu3$nstop, hu1$nstop)

boxplot(am12$nstop, am6$nstop, am3$nstop, am1$nstop,
        bb12$nstop, bb6$nstop, bb3$nstop, bb1$nstop,
        hu12$nstop, hu6$nstop, hu3$nstop, hu1$nstop,
        main="Number of Stopovers", outline=F, names=rep(c('12', '6', '3', '1'), times=3),
        col=c(rep(c('black','gray20', 'gray40',"gray60"), times=4)), border=c(rep(c('black','gray20', 'gray40',"gray60"), times=4)),
        xaxt="n", axes=T,boxwex=0.3, whisklty=1, staplelwd=0, staplewex=0, whisklwd=3, boxlwd=1, medcol="white",
        cex.axis=1.2)
mtext('AMAV', side=1, line=1.2, at=2.5, cex=1.5)
mtext('BBPL', side=1, line=1.2, at=6.5, cex=1.5)
mtext('HUGO', side=1, line=1.2, at=10.5, cex=1.5)
mtext('Number of Stopovers', side=2, line=2.2, cex=1.2)
abline(v=c(4.5,8.5))
legend('topleft', legend = c("2-Hour", "4-Hour", "8-Hour", "24-Hour"), fill=c('black','gray20', 'gray40',"gray60"), bty='n', y.intersp = 0.6)
box()




# prstop
boxplot(am12$prstop, am6$prstop, am3$prstop, am1$prstop,
        bb12$prstop, bb6$prstop, bb3$prstop, bb1$prstop,
        hu12$prstop, hu6$prstop, hu3$prstop, hu1$prstop)

boxplot(am12$prstop, am6$prstop, am3$prstop, am1$prstop,
        bb12$prstop, bb6$prstop, bb3$prstop, bb1$prstop,
        hu12$prstop, hu6$prstop, hu3$prstop, hu1$prstop,
        main="Proportion of Time in Stopover", outline=F, names=rep(c('12', '6', '3', '1'), times=3),
        col=c(rep(c('black','gray20', 'gray40',"gray60"), times=4)), border=c(rep(c('black','gray20', 'gray40',"gray60"), times=4)),
        xaxt="n", axes=T,boxwex=0.3, whisklty=1, staplelwd=0, staplewex=0, whisklwd=3, boxlwd=1, medcol="white",
        cex.axis=1.2)
mtext('AMAV', side=1, line=1.2, at=2.5, cex=1.5)
mtext('BBPL', side=1, line=1.2, at=6.5, cex=1.5)
mtext('HUGO', side=1, line=1.2, at=10.5, cex=1.5)
mtext('Proportion of time in Stopover', side=2, line=2.2, cex=1.2)
abline(v=c(4.5,8.5))
legend('topright', legend = c("2-Hour", "4-Hour", "8-Hour", "24-Hour"), fill=c('black','gray20', 'gray40',"gray60"), bty='n', y.intersp = 0.6)
box()


# Duration
boxplot(am12$mig_duration, am6$mig_duration, am3$mig_duration, am1$mig_duration,
        bb12$mig_duration, bb6$mig_duration, bb3$mig_duration, bb1$mig_duration,
        hu12$mig_duration, hu6$mig_duration, hu3$mig_duration, hu1$mig_duration)

boxplot(am12$mig_duration, am6$mig_duration, am3$mig_duration, am1$mig_duration,
        bb12$mig_duration, bb6$mig_duration, bb3$mig_duration, bb1$mig_duration,
        hu12$mig_duration, hu6$mig_duration, hu3$mig_duration, hu1$mig_duration,
        main="Migration Duration", outline=F, names=rep(c('12', '6', '3', '1'), times=3),
        col=c(rep(c('black','gray20', 'gray40',"gray60"), times=4)), border=c(rep(c('black','gray20', 'gray40',"gray60"), times=4)),
        xaxt="n", axes=T,boxwex=0.3, whisklty=1, staplelwd=0, staplewex=0, whisklwd=3, boxlwd=1, medcol="white",
        cex.axis=1.2)
mtext('AMAV', side=1, line=1.2, at=2.5, cex=1.5)
mtext('BBPL', side=1, line=1.2, at=6.5, cex=1.5)
mtext('HUGO', side=1, line=1.2, at=10.5, cex=1.5)
mtext('Migration Duration (Days)', side=2, line=2.2, cex=1.2)
abline(v=c(4.5,8.5))
legend('topleft', legend = c("2-Hour", "4-Hour", "8-Hour", "24-Hour"), fill=c('black','gray20', 'gray40',"gray60"), bty='n', y.intersp = 0.6)
box()

# mean_stop_step
boxplot(am12$mean_stop_step, am6$mean_stop_step, am3$mean_stop_step, am1$mean_stop_step,
        bb12$mean_stop_step, bb6$mean_stop_step, bb3$mean_stop_step, bb1$mean_stop_step,
        hu12$mean_stop_step, hu6$mean_stop_step, hu3$mean_stop_step, hu1$mean_stop_step)

boxplot(am12$mean_stop_step, am6$mean_stop_step, am3$mean_stop_step, am1$mean_stop_step,
        bb12$mean_stop_step, bb6$mean_stop_step, bb3$mean_stop_step, bb1$mean_stop_step,
        hu12$mean_stop_step, hu6$mean_stop_step, hu3$mean_stop_step, hu1$mean_stop_step,
        main="Mean Stopover Step Length", outline=F, names=rep(c('12', '6', '3', '1'), times=3),
        col=c(rep(c('black','gray20', 'gray40',"gray60"), times=4)), border=c(rep(c('black','gray20', 'gray40',"gray60"), times=4)),
        xaxt="n", axes=T,boxwex=0.3, whisklty=1, staplelwd=0, staplewex=0, whisklwd=3, boxlwd=1, medcol="black",
        cex.axis=1.2)
mtext('AMAV', side=1, line=1.2, at=2.5, cex=1.5)
mtext('BBPL', side=1, line=1.2, at=6.5, cex=1.5)
mtext('HUGO', side=1, line=1.2, at=10.5, cex=1.5)
mtext('Meters', side=2, line=2.2, cex=1.2)
abline(v=c(4.5,8.5))
legend('topleft', legend = c("2-Hour", "4-Hour", "8-Hour", "24-Hour"), fill=c('darkorange3','gray40', 'deepskyblue4',"white"), bty='n', y.intersp = 0.5)
box()


# ggplot version
nstop_plot <- ggplot(dt)+
  theme_bw(base_size=18)+
  aes(x=as.factor(ppd), y=nstop, fill=as.factor(ppd))+ #, fill=as.factor(ppd)
  facet_wrap(~species)+
  geom_boxplot(aes(group=as.factor(ppd), y=nstop))+ #, fill=as.factor(ppd)
  geom_point(color='black', position = position_jitterdodge(dodge.width = 0.2, jitter.width = 1), 
             size = 1.5, alpha=0.4)+
  scale_fill_manual("", values=rep('gray80', 4), 
                    labels=c('', "", "", ""))+
  theme(legend.position='')+
  theme(axis.text = element_text(size = 14))+
  theme(axis.title = element_text(size = 12)) +
  xlab('Time between locations (hours)')+
  ylab('Number of Stopovers')+
  scale_x_discrete(labels=c('24', '8', '4', '2'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle('A')
nstop_plot

prstop_plot <- ggplot(dt)+
  theme_bw(base_size=18)+
  aes(x=as.factor(ppd), y=prstop, fill=as.factor(ppd))+ #, fill=as.factor(ppd)
  facet_wrap(~species)+
  geom_boxplot(aes(group=as.factor(ppd), y=prstop))+ #, fill=as.factor(ppd)
  geom_point(color='black', position = position_jitterdodge(dodge.width = 0.2, jitter.width = 1), 
             size = 1.5, alpha=0.4)+
  scale_fill_manual("", values=rep('gray80', 4), 
                    labels=c('', "", "", ""))+
  theme(legend.position='')+
  theme(axis.text = element_text(size = 14))+
  theme(axis.title = element_text(size = 12)) +
  xlab('Time between locations (hours)')+
  ylab('Proportion of time in stopover')+
  scale_x_discrete(labels=c('24', '8', '4', '2'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle('F')
prstop_plot

mig_duration_plot <- ggplot(dt)+
  theme_bw(base_size=18)+
  aes(x=as.factor(ppd), y=mig_duration, fill=as.factor(ppd))+ #, fill=as.factor(ppd)
  facet_wrap(~species)+
  geom_boxplot(aes(group=as.factor(ppd), y=mig_duration))+ #, fill=as.factor(ppd)
  geom_point(color='black', position = position_jitterdodge(dodge.width = 0.2, jitter.width = 1), 
             size = 1.5, alpha=0.4)+
  scale_fill_manual("", values=rep('gray80', 4), 
                    labels=c('', "", "", ""))+
  theme(legend.position='')+
  theme(axis.text = element_text(size = 14))+
  theme(axis.title = element_text(size = 12)) +
  xlab('Time between locations (hours)')+
  ylab('Migration duration (days)')+
  scale_x_discrete(labels=c('24', '8', '4', '2'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle('B')
mig_duration_plot

stop_step_plot <- ggplot(dt)+
  theme_bw(base_size=18)+
  aes(x=as.factor(ppd), y=mean_stop_step/1000, fill=as.factor(ppd))+ #, fill=as.factor(ppd)
  facet_wrap(~species)+
  geom_boxplot(aes(group=as.factor(ppd), y=mean_stop_step/1000))+ #, fill=as.factor(ppd)
  geom_point(color='black', position = position_jitterdodge(dodge.width = 0.2, jitter.width = 1), 
             size = 1.5, alpha=0.4)+
  scale_fill_manual("", values=rep('gray80', 4), 
                    labels=c('', "", "", ""))+
  theme(legend.position='')+
  theme(axis.text = element_text(size = 14))+
  theme(axis.title = element_text(size = 12)) +
  xlab('Time between locations (hours)')+
  ylab('Mean within-stopover step length (m)')+
  scale_x_discrete(labels=c('24', '8', '4', '2'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle('E')
stop_step_plot

depart_time_plot <- ggplot(dt)+
  theme_bw(base_size=18)+
  aes(x=as.factor(ppd), y=depart_time, fill=as.factor(ppd))+ #, fill=as.factor(ppd)
  facet_wrap(~species)+
  geom_boxplot(aes(group=as.factor(ppd), y=depart_time))+ #, fill=as.factor(ppd)
  geom_point(color='black', position = position_jitterdodge(dodge.width = 0.2, jitter.width = 1), 
             size = 1.5, alpha=0.4)+
  scale_fill_manual("", values=rep('gray80', 4), 
                    labels=c('', "", "", ""))+
  theme(legend.position='')+
  theme(axis.text = element_text(size = 14))+
  theme(axis.title = element_text(size = 12)) +
  xlab('Time between locations (hours)')+
  ylab('Departure time (ordinal date)')+
  scale_x_discrete(labels=c('24', '8', '4', '2'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle('D')
depart_time_plot

stop_duration_plot <- ggplot(dt)+
  theme_bw(base_size=18)+
  aes(x=as.factor(ppd), y=mean_stop_duration/24, fill=as.factor(ppd))+ #, fill=as.factor(ppd)
  facet_wrap(~species)+
  geom_boxplot(aes(group=as.factor(ppd), y=mean_stop_duration/24))+ #, fill=as.factor(ppd)
  geom_point(color='black', position = position_jitterdodge(dodge.width = 0.2, jitter.width = 1), 
             size = 1.5, alpha=0.4)+
  scale_fill_manual("", values=rep('gray80', 4), 
                    labels=c('', "", "", ""))+
  theme(legend.position='')+
  theme(axis.text = element_text(size = 14))+
  theme(axis.title = element_text(size = 12)) +
  xlab('Time between locations (hours)')+
  ylab('Mean stopover duration (days)')+
  scale_x_discrete(labels=c('24', '8', '4', '2'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle('C')
stop_duration_plot

ggarrange(nstop_plot, mig_duration_plot, stop_duration_plot, depart_time_plot, stop_step_plot, prstop_plot, nrow=3, ncol=2)

ggarrange(nstop_plot, mig_duration_plot, stop_duration_plot, depart_time_plot, stop_step_plot, prstop_plot, nrow=2, ncol=3)
