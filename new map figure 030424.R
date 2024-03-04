# Map of AMAV, BBPL, and HUGO migrations
# Migration strategy chapter
# SJC (Map based on the one SAC made for geese)
# March 2022 - Converted to new spatial packages march 2024

library(tidyverse)
#library(rworldmap)
#library(sp)
#library(rgdal)
library(ggspatial)
library(lubridate)
library(move)
library(rnaturalearth)
library(sf)
library(terra)
library(cowplot)

# world map

mp <- ne_countries(type='countries', returnclass='sf')
#st_crs(mp) <- 4326 # it already is this
azeq <- 'PROJCS["ProjWiz_Custom_Azimuthal_Equidistant",
 GEOGCS["GCS_WGS_1984",
  DATUM["D_WGS_1984",
   SPHEROID["WGS_1984",6378137.0,298.257223563]],
  PRIMEM["Greenwich",0.0],
  UNIT["Degree",0.0174532925199433]],
 PROJECTION["Azimuthal_Equidistant"],
 PARAMETER["False_Easting",0.0],
 PARAMETER["False_Northing",0.0],
 PARAMETER["Central_Meridian",-100.55],
 PARAMETER["Latitude_Of_Origin",6.85],
 UNIT["Meter",1.0]]'#crs(54032)
#mp <- st_transform(mp, azeq)

# birds (use 2 hr migration data, then subset)

birds <- read.csv('G:\\My Drive\\shorebirds\\analysis_code\\migration_strategies\\filtered_data\\spring_mig_2hr.csv')
birds1 <- birds

# subset birds to 1 day
brd <- list()
ids <- unique(birds$id)
for (i in 1:length(ids)){
  brd[[i]] <- birds[which(birds$id==ids[i]),]
  brd[[i]]$DateTime <- as_datetime(brd[[i]]$DateTime)
}

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
  tmpdf$species <- brd[[i]]$species[1]
  tmpdf <- tmpdf %>% filter(burstId=="selected")
  birds2[[i]] <- tmpdf
  print(i)
}

birds <- bind_rows(birds2)
birds_df <- as.data.frame(bind_rows(birds2))
#birds <- fortify(birds)
birds_sf <- st_as_sf(birds, coords=c('lon', 'lat'), crs=4326)
#birds_sf <- st_transform(birds_sf, azeq)
birds_border <- data.frame(x=c(-170,-20), y=c(-55,85))%>%
  st_as_sf(., coords=c('x', 'y'), crs=4326) %>%
 # st_transform(., azeq) %>%
  st_bbox()
mp1 <- st_crop(mp,birds_border)
plot(mp1$geometry)
plot(birds_sf$geometry, add=T)

mp1 <- st_transform(mp1, azeq) # change to azimuthal equidistant
birds_sf1 <- st_transform(birds_sf, azeq)
plot(mp1$geometry)
plot(birds_sf1, add=T)

# make a combined continent map
mp1_combine <- mp1 %>% group_by %>% summarize() 

# Plot latitude by yday
inset <- ggplotGrob(ggplot(birds) + geom_line(aes(x=yday, y=lat, color=species, group=id), size=1) + 
                      theme_classic() +
                      scale_color_manual(values=c("#2166ac","black", "darkorange3")) + 
                      ylab("Latitude") + xlab("Day of Year") +
                      scale_x_continuous(breaks=c(30,60,90,120,150,180)) + #, labels=c("Mar","Apr""May","May","Jun", "Jul" )
                      theme(panel.border=element_rect(color="black", fill=NA, size=0.5),
                            axis.text=element_text(size=12), axis.title=element_text(size=12),
                            legend.position="none")
)

# map of birds by species
fig2 <- ggplot()+
  geom_sf(data=mp1_combine, mapping=aes(), fill='white', color='black')+
  geom_sf(data=birds_sf1, aes(color=species), alpha=0.5)+
  scale_color_manual('Species', values=c("#2166ac", "black", "darkorange3"))+
  #theme(panel.background = element_rect(fill="white"))+
  #theme_map()
 # theme_gray(base_size=14) + 
  theme(axis.title.x=element_blank(), panel.grid.major=element_line(color='gray40'),axis.text=element_text(size=14),
                          axis.title.y=element_blank(), panel.border=element_rect(color="black", fill=NA, linewidth=0.5),
                          legend.justification=c(1,1), legend.position=c(1,1), legend.text=element_text(size=14),
                          legend.title=element_text(size=14, face="bold"), panel.background = element_rect(fill="gray80"),
                          legend.background=element_rect(fill="white", colour="black")) +
  annotation_custom(grob=inset, xmin=-6704483, xmax=550000, ymin=-7790544,  ymax=0)+
 # ggspatial::annotation_north_arrow( # no north arrow for this bc north is different on differnet parts of the map
#    location = "tl", which_north = "true",
#    height=unit(0.5, 'in'), width=unit(0.5, 'in'),
#    pad_x = unit(0.15, "in"), pad_y = unit(0.25, "in"),
#  )+
  ggspatial::annotation_scale(
    location = 'tl',
    bar_cols = c("black", "white"),
  )
fig2 
ggsave(fig2, filename='figure_2_new.png', path='.\\figures\\', width=7, height=8, dpi=300)
