# Migration Metrics - 2 hour duty cycle
# Migration Strategy Chapter
# SJC
# Feb 2022

#library(raster)
library(lubridate)
library(amt)
library(dplyr)
library(ggplot2)
library(Rcpp)
library(move)

# data 
birds <- read.csv('G:\\My Drive\\shorebirds\\analysis_code\\migration_strategies\\filtered_data\\SPRING_MIG_2HR_STOP_FINAL.csv')
birds$DateTime <- as_datetime(birds$DateTime)

setwd('G:\\My Drive\\shorebirds\\analysis_code\\migration_strategies\\updated_analysis_feb24')
# just bbpl first

bbpl <- birds[which(birds$species=='BBPL'),]

brd <- list()
ids <- unique(bbpl$id)
for (i in 1:length(ids)){
  brd[[i]] <- bbpl[which(bbpl$id==ids[i]),]
  brd[[i]]$DateTime <- as_datetime(brd[[i]]$DateTime)
}


# just do for bbpl for now..

stop_tables <- list()

# general migration metrics
# migration duration*
# number of stopovers
# propoartion of time in stopover
# departure date
# arrival date
# total migration distance - do this separately bc it will involve once per day subset
# mean distance between stops

stop_tabled <- matrix(NA, nrow=length(brd), ncol=11)
stop_tabled <- data.frame(stop_tabled)
names(stop_tabled) <- c('id', 'device','nstop', 'prstop', "mig_duration", "depart_time", "arrive_time", "mean_stop_duration",
                        'sd_stop_duration', 'mean_stop_step', 'sd_stop_step')
for (i in 1:nrow(stop_tabled)){
  stop_tabled$depart_time[i] <- max(brd[[i]]$yday[which(brd[[i]]$label==1)])
  stop_tabled$arrive_time[i] <- min(brd[[i]]$yday[which(brd[[i]]$label==444)])
  tmp <- brd[[i]][which(brd[[i]]$label!=0),]
  mx <- 444 #max(tmp$label) # ORDER IS IMPORTANT HERE - get breeding and wintering (mx and mn)
  mn <- min(tmp$label)
  tmp <- tmp[which(tmp$label!=mx),] # get rid of last stop, breeding for count
  tmp <- tmp[which(tmp$label!=mn),] # get rid of first stop, wintering for count
  tmp2 <- brd[[i]][which(brd[[i]]$label!=mx),] # whole migration rid of breeding (for mig duration)
  tmp2 <- tmp2[which(tmp2$label!=mn),] # whole migration rid of wintering (for mig duration)
  stop_tabled$id[i] <- brd[[i]]$id[1] # fill in id
  stop_tabled$nstop[i] <- length(unique(tmp$label)) # count unique labels of stops
  #stop_tabled$prstop[i] <- nrow(tmp)/nrow(brd[[i]][which(brd[[i]]$label!=mx&brd[[i]]$label!=mn),])
  stop_tabled$mig_duration[i] <- stop_tabled$arrive_time[i] - stop_tabled$depart_time[i]
    #abs(difftime(stop_tabled$depart_time[i], stop_tabled$arrive_time[i], units='days')) # difference in time beginning to end
  
  
    tbl <- matrix(NA, nrow=length(unique(tmp$label)), ncol=5) #add columns here
    tbl <- data.frame(tbl)
    names(tbl) <- c('id', 'label', 'id_label','mean_step', 'duration') #add column labels here
    tbl$id <- rep(tmp$id[1], times=nrow(tbl))
    tbl$label <- unique(tmp$label)#[2:length(unique(tmp$label))] #don't need for this one bc winter already excluded
    tbl$id_label <- paste0(tbl$id,'_',tbl$label)
    
    for (j in 1:nrow(tbl)){ # anything else you want to calculate by individual stopover you put in hear
      yam <- tmp[which(tmp$label==tbl$label[j]),]
      
      # step lengths
      PloBursts<-make_track(yam, lon, lat, DateTime, id, yday, dist, label)
      PloBursts$y_<-as.double(PloBursts$y_)
      PloBursts$x_<-as.double(PloBursts$x_)
      #calculate step length
      #PloBursts$sl_ <- step_lengths(PloBursts, lonlat=TRUE)
      #PloBursts <-PloBursts %>% mutate(sl_ = step_lengths(., lonlat = TRUE))
      #resamples with bursts
      #PloBursts<-PloBursts %>% track_resample(rate=hours(2), tolerance=minutes(20))
     # class(PloBursts$sl_)
      #tbl$mean_step[j] <- mean(PloBursts$sl_, na.rm = TRUE)
      # maximum, etc
      # duration
      tbl$duration[j] <- abs(difftime(yam$DateTime[1], yam$DateTime[nrow(yam)], tz="UTC", units='hours'))
    
    }
    
  stop_tables[[i]] <- tbl # this puts tbl for each bird in list
  stop_tabled$prstop[i] <- (sum(tbl$duration)/24)/stop_tabled$mig_duration[i]
  stop_tabled$mean_stop_duration[i] <- mean(tbl$duration)
  stop_tabled$sd_stop_duration[i] <- sd(tbl$duration)
  stop_tabled$mean_stop_step[i] <- mean(tbl$mean_step)
  stop_tabled$sd_stop_step[i] <- sd(tbl$mean_step)
  stop_tabled$total_stop_duration[i] <- (sum(tbl$duration)/24)
  print(i)
}
stop_tabled

# there were 2 cases where there was a weird thing happening producing NAs, this gets rid of it
stop_tabled$mean_stop_step[15] <- mean(stop_tables[[15]]$mean_step[1:8])
stop_tabled$sd_stop_step[15] <- sd(stop_tables[[15]]$mean_step[1:8])

stop_tables[[23]]
stop_tabled$mean_stop_step[23] <- mean(stop_tables[[23]]$mean_step[c(1:3, 5:6)])
stop_tabled$sd_stop_step[23] <- sd(stop_tables[[23]]$mean_step[c(1:3, 5:6)])

# Then need the other shit - total migration distance
# need to subset to once per day and add up daily displacements between winter and breeding

# subset, calculate distance

birds2 <- list()
# now use the move package to subset to once per day
# this is similar to what Amy did with the thintracktime function
for (i in 1:length(brd)){
  tmp <- move(x=brd[[i]]$lon, y=brd[[i]]$lat, time=brd[[i]]$DateTime, data=brd[[i]], 
              proj=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"), animal=brd[[i]]$id)
  yam <- thinTrackTime(tmp, interval = as.difftime(24, units = 'hours'), 
                       tolerance = as.difftime(2, units = 'hours'))
  tmpdf <- as.data.frame(yam)
  tmpdf$id <- brd[[i]]$id[1] # move format doesn't keep id or species in with the data so add
  tmpdf$species <- 'BBPL'
  tmpdf <- tmpdf %>% filter(burstId=="selected")
  birds2[[i]] <- tmpdf
  print(i)
}

# then distance , also with move package
head(birds2[[1]])
# make each piece of the move into a temporary move object for distance calculations
birds3 <- list()
for (i in 1:length(brd)){
  tmp <- birds2[[i]]
  tmpmove <- move(x=tmp$lon, y=tmp$lat, time=tmp$DateTime, data=tmp, 
                  proj=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"), animal=tmp$id)
  tmp$dist[nrow(tmp)] <- 0
  tmp$dist[1:(nrow(tmp)-1)] <- move::distance(tmpmove)
  birds3[[i]] <- tmp
  print(i)
}
head(birds3[[1]],20)

# birds3 has daily displacement as calculated by move in the dist column
# so sum of that between last winter point and first breeding point is going to be migration distance

for (i in 1:length(brd)){
  sub <- birds3[[i]][which(birds3[[i]]$yday>=stop_tabled$depart_time[i]),]
  sub <- sub[which(sub$yday<=stop_tabled$arrive_time[i]),]
  stop_tabled$mig_dist[i] <- sum(sub$dist)
}

stop_tabled$mig_dist_km <- stop_tabled$mig_dist/1000
stop_tabled$travel_to_stopover <- stop_tabled$mig_dist_km/stop_tabled$total_stop_duration
stop_tabled$device <- "6g"
stop_tabled$species <- "BBPL"
stop_tabled

saveRDS(stop_tabled$travel_to_stopover, file='bbpl_travel_to_stopover.rds')
saveRDS(stop_tabled$total_stop_duration, file='bbpl_total_stop_duration.rds')

#write.csv(stop_tabled, 'bbpl_mig_metrics_030322_2hr.csv')

# summarizing and plotting
stopover_id_map<- function(x){
  test <- x[which(x$label!=0),] # all points which are not 0 (0 is not a stopover)
  test <- test[which(test$label!=444),] # all points which are not 999 (999 is breeding and later)
  test <- test[which(test$label!=1),] # 1 should be wintering for most, check to make sure there aren't extras
  ll <- unique(x$label) # list of unique stopover labels
  test$label <- factor(test$label) # make a factor for mapping
  pal <- colorFactor(rainbow(length(ll)), test$label) # color scheme, one color for each unique stopover
  m <- leaflet(test) %>% 
    addTiles() %>% 
    setView( lng = -85, lat = 40, zoom = 2) %>% 
    addProviderTiles("Esri.WorldImagery") %>%
    addScaleBar(position='bottomright') %>% 
    addPolylines(lat=x$lat, lng=x$lon, col='white') %>%
    addControl(paste(test$id[1]), position = "topleft", className="map-title")%>%
    addCircleMarkers(lat=test$lat, lng=test$lon, color=~pal(label), radius=2, label=paste(test$DateTime, test$label), labelOptions=labelOptions(textsize="20px"))%>%
    addLegend('topright', pal=pal, values=~label)
  #saveWidget(m, file=paste0('D:\\shorebirds\\analysis_code\\migration_metrics\\stopover_101421\\stopover_id_maps_101521\\bbpl_', test$id[1], '_stops.html'))
  m
}

stopover_id_map(brd[[8]])

bbpl_for_analysis <- bind_rows(brd)

write.csv(bbpl_for_analysis, 'bbpl_full_mig_final.csv')



