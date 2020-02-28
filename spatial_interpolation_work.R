

# these are the packages/libraries I need.
# install.packages("raster")
# install.packages("gstat")
library(gstat)
library(sp)
library(maptools)
#install.packages("spatial.tools")
library(rgeos)
library(ggplot2)
#install.packages("remotes")
#remotes::install_github("MariekeDirk/GeoInterpolation")
library(GeoInterpolation)

# JC: I added these. Might mess with some of the above. Has name collisions with regeos
library(dplyr)
library(lubridate)
library(sf)
library(stars)

#import lighting data for the state of MN#
#data is read in as date yyyymmdd long lat count of 
#strikes of lightning for a given day, for a 0.1 degree grid
#surroundig the listed centroid.

working.dir <- "C:/Users/jchan/Dropbox/Teaching/OtherStuff/tstorm/"
#working.dir <- "Z:/personal/luke/climate_data_all/lightning data/mn_lightning_data/"
mydata <- read.csv(paste0(working.dir,"ALLMSPNLDN.csv"))

#restrict the data to a bounded box that is a bit larger than the twin cities
#which is my study area.
mydata1 <-  mydata[ which(mydata$centerlon > -94.5
                            & mydata$centerlon < -92
                            & mydata$centerlat > 44
                            & mydata$centerlat < 46), ]
# in dplyr, this would be
mydata1 <- mydata %>% # ask me about the pipe operator if you're not familar
  filter(between(centerlon,-94.5,-92),
         between(centerlat,44,46))


#Import shape files for a selected area around the twin cities
#assign as shape poly, and set the projection
#this imports a selection of shape files; these are the zipcodes for the 
# area in and around minneapolis.  it has a number of variables that I don't need.
#import, and assign a projection
d <- readShapePoly(paste0(working.dir,"msp_zip_select.shp"),
                   proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
# You named this "q" which normally is the name of the function that quits. Changing it to
# d. Also be careful of "t" and "c".


# JC: I'm going to change the dates to dates.
mydata1 <- mydata1 %>% 
  mutate(zday = ymd(zday))

#subset for one day
#do outside of loop
#msp_nldn <- subset(mydata1,zday==20000508)
msp_nldn <- mydata1 %>% # subset is slow, so doing dplyr version
  filter(zday=="2000-05-08")


#assign as lat long the coordinate system of the .csv of lightning data.
#do outside of loop?  
#tell R that variables centerlon and centerlat are gridded long lat
# JC: this is cool. I've never seen a construction like this
coordinates(msp_nldn) <- ~centerlon+centerlat

#assign a projection type (map projection)
proj4string(msp_nldn) <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
#fyi, command proj4string(df) will return the coordinate ref system


#plot lightning data (for one day) over the zip codes
#this is just a visualization.  
plot(d)
plot (msp_nldn, add=TRUE)

#this is just a summary plot to put some 'bubbles' 
#that represent the relative 'weight' of the lilghtning strikes 
#over my gridded study area.
plot(msp_nldn)
bubble(msp_nldn,"total_count")

#meat and potatoes.
# idw is a function in the gstat package.  this predicts the 
# count for each area n the grid newdata=d,
# d is my shape file; so each zipcode has a value that is the
# average of all the points contained by it.
# nmax = 8 tells it to use the 8 surrounding points of my grid.  
# fwiw, everyone says that the grid should 
# be definied in degrees, so that I could use maxdist = 0.14 
# (the hypotenuse of a 0.1 degree grid) to do the same thing
# which would include the surrounding points -- in fact if i 
# assume KM as the unit, and use maxdist = 14
# i run it 
# it works just fine.  go figure. 

# JC: R is case sensitive. You'll make yourself crazy mixing caps and not.
# I recommend this style guide http://adv-r.had.co.nz/Style.html
lightning.idw  <- idw(total_count ~ 1, locations=msp_nldn, newdata=d, nmax=8) 
lightning.idw2 <- idw(total_count ~ 1, locations=msp_nldn, newdata=d, maxdist=12) 

#this will plot it for me.  ugh.
idw_res<- st_as_sf(lightning.idw) # added indicator to "sf" library
plot(idw_res)
ggplot(data=idw_res)+geom_sf(aes(fill=var1.pred))

#this is a 'spatial Cbind'.  if i use 'simple features' I can just use cbind.
lightning.dat<-spCbind(d,lightning.idw$var1.pred)

#this is a way to investigate the relationship across points of varied distances.
#not important
nldn.vgm <- variogram(total_count ~1,msp_nldn)
nldn.vgm

############################################################
##### This is the only part I've made changes to in V2 #####
############################################################

#Let's do the same thing with simple features
############################################################

# import a shape file that covers just the restricted area called 
# p <- msp_zip_select.shp
# import a gridded shape file layer with data yyyymmdd, long, 
# lat, total_count = daily count strikes lightning
# for a given 0.1 degree grid, at that centroid.
# same idea, this just uses the new 'simple features' process, 
# which should simplify things.

p <- st_read(paste0(working.dir,"msp_zip_select.shp"))
nldn_shp <- st_read(paste0(working.dir,"msp_nldn.shp")) %>%
  mutate(zday=ymd(zday))

nldn_dat <- nldn_shp %>% filter(zday=="2000-05-08")

plot(nldn_dat)

ggplot() + 
  geom_sf(data=p) + 
  geom_point(data=nldn_dat,
             aes(x=centerlon,
                 y=centerlat))
  
lightning.idw3 <- idw(total_coun ~ 1, locations=nldn_dat, newdata=p, nmax=8) 
lightning.idw4 <- idw(total_coun ~ 1, locations=nldn_dat, newdata=p, maxdist=12) 
#import station data

ggplot(data=lightning.idw3) + 
  geom_sf(aes(fill=var1.pred)) + 
  geom_point(data=nldn_dat,
             aes(x=centerlon,y=centerlat))
  
lightning.dat2 <- cbind(p,lightning.idw3$var1.pred)
  
head(lightning.dat2)

  
##################################
#nearest neighbor stuff.
#  in this it's another form of spatial interpolation.  but this data has a 
# series of points where weather 
# stations exist in the area.  using arc GIS, i've restricted the data 
# to an area in and around MSP.
# this data has separate year, month, day columns 
# for date -- I could convert all data to the same convention.
#import msp meso
station<- st_read(paste0(working.dir,"MSP_meso.shp"))

# JC: let's add date column as dt
station <- station %>% 
  mutate(dt = ymd(paste(year,month,day)))

station_day <- station %>% 
  filter(dt=="2000-05-08") 

gs <- doNearestNeighbor(formula=dayP01~1, 
                        station_day,
                        p,
                        debug=1)

ggplot(data=gs) + 
  geom_sf(aes(fill=var1.pred)) + 
  geom_point(data=station,
             aes(x=lon,y=lat),
             pch=24,
             cex=3,
             color="red",
             bg="red") + 
  # JC, adding some stuff you might like later.
  labs(x="Longitude",y="Latitude",
       fill="Scale Title",
       title="Main Title Words Go Here",
       subtitle="More words. Love words.",
       caption=paste("But if you really love words,",
                     "Then sometimes you want a caption",
                     "talking about what you did. I",
                     "*love* words.",sep="\n"))

#i'll just cbind it to my already existing data frame?
#so now i have rain, and lightning (and other stuff, for one day (05082000))
#it just doestn' say that anywhere in my data
  
alldat.test2 <- cbind(lightning.dat2,gs$var1.pred)

ggplot(data=alldat.test2) + 
  geom_sf(aes(fill=gs.var1.pred)) + 
  geom_point(data=station,
             aes(x=lon,y=lat),
             pch=24,
             cex=3,
             color="red",
             bg="red")
  
#here i can add a datenum variable.
alldat.test2$datenum<-"2000-05-08" # JC sticking with real dates


#now i just need to loop through 
# yyyymmdd="20000301-20181130", 
# for all zips (looks like there are 344 of them in this case)
  
# JC: New stuff below here
date.seq <- seq(ymd("2000-03-01"),
                ymd("2018-11-30"),
                by="days") %>% 
  tibble::enframe(name=NULL,value=c("dt")) 
  # weird way to get vector to dplyr-able data frame
  # I put the dates in the data frame to do the fancy 
  # filter on the next line, but it makes things awkwarder
  # below. Shrug. Anyway, now you know why. 

date.seq <- date.seq %>% 
  filter(between(month(dt),3,11))

zips <- unique(p$ZIP_CODE)

# JC: this is kind of new. Basically, I make a `this.results` and 
# then I'll glue all those together after each date.seq loop.

this.results <- expand.grid(zip=zips,
                       dt=date.seq$dt,
                       variable="precip_in_mm",
                       value=0.0,
                       stringsAsFactors = F) # almost always do this when 
                                             # you make a dataframe.

this.results <- as_tibble(this.results)

# This is biggish
dim(this.results)

for(dt.idx in 1:nrow(date.seq)){
  this.dt <- date.seq$dt[dt.idx]
  
  # Remove this next for loop. it's causing us to only 
  # do March so I don't have to wait for this to run. 
  if(!(year(this.dt) == 2000 & 
       month(this.dt) == 8 & 
       day(this.dt) %in% c(6,7,8))) {
    next
  }
 
  station.day <- station %>% 
    filter(dt==this.dt) 
  
  
  if(nrow(station.day) > 0) {
    # GS holds interpolation for all zips, 
    # but doesn't have zip code in it. 
    # Let's add it in and then join
    gs <- doNearestNeighbor(formula= dayP01 ~ 1, 
                            station.day,
                            p,
                            debug=1)
  } else {
    warning(paste0("Skipping ",this.dt,". Is this expected?"))
    next
  }
  
  gs.for.join <- gs %>% 
    as.data.frame %>% 
    select(var1.pred) %>% 
    rename(interpolation = var1.pred) %>% 
    mutate(zip=p$ZIP_CODE,
           dt=ymd(this.dt))
  
  # Kind of a weird trick to use joins (which are faster)
  # to get data into a data.frame that you've already set up.
  
  this.results <- this.results %>% 
    left_join(gs.for.join,
              by=c("zip","dt"))
    
  # Copy data over
  this.results <- this.results %>% 
    mutate(value = if_else(dt==this.dt,
                           interpolation,
                           value)) 

  # and get rid of the join column
  this.results$interpolation <- NULL
  
  print(paste("Just finished with",this.dt))
  flush.console() # need this to print in loops
}

# TODO: saving this.results seems smart. 
# I like `readr`. You may need to run
# install.packages("readr")
readr::write_tsv(this.results,
                 paste0(working.dir,"20200226_precip.txt"))

if (!exists("results")){
  results <- this.results
} else {
  stop("Whoa, why does results already exist?\nNot sure if it's okay to write over it.")
}


############  RELATIVE HUMIDITY ######################################

this.results <- expand.grid(zip=zips,
                            dt=date.seq$dt,
                            variable="relative_humidity",
                            value=0.0,
                            stringsAsFactors = F) # almost always do this when 
# you make a dataframe.

this.results <- as_tibble(this.results)

# This is biggish
dim(this.results)

for(dt.idx in 1:nrow(date.seq)){
  this.dt <- date.seq$dt[dt.idx]
  
  # Remove this next for loop. it's causing us to only 
  # do March so I don't have to wait for this to run. 
  if(!(year(this.dt) == 2000 & 
       month(this.dt) == 8 & 
       day(this.dt) %in% c(6,7,8))) {
    next
  }
  
  station.day <- station %>% 
    filter(dt==this.dt) 
  
  
  if(nrow(station.day) > 0) {
    # GS holds interpolation for all zips, 
    # but doesn't have zip code in it. 
    # Let's add it in and then join
    # JC: change the formula below in each block
    gs <- doNearestNeighbor(formula= relh ~ 1, 
                            station.day,
                            p,
                            debug=1)
  } else {
    warning(paste0("Skipping ",this.dt,". Is this expected?"))
    next
  }
  
  gs.for.join <- gs %>% 
    as.data.frame %>% 
    select(var1.pred) %>% 
    rename(interpolation = var1.pred) %>% 
    mutate(zip=p$ZIP_CODE,
           dt=ymd(this.dt))
  
  # Kind of a weird trick to use joins (which are faster)
  # to get data into a data.frame that you've already set up.
  
  this.results <- this.results %>% 
    left_join(gs.for.join,
              by=c("zip","dt"))
  
  # Copy data over
  this.results <- this.results %>% 
    mutate(value = if_else(dt==this.dt,
                           interpolation,
                           value)) 
  
  # and get rid of the join column
  this.results$interpolation <- NULL
  
  print(paste("Just finished with",this.dt))
  flush.console() # need this to print in loops
}

# TODO: saving this.results seems smart. 
readr::write_tsv(this.results,
                 paste0(working.dir,"20200226_relative_humidity.txt"))

if (exists("results")){
  results <- results %>% 
    bind_rows(this.results)
  # Let me know if you'd prefer these bound as columns. It's 
  # just a `left_join` in dplyr. 
} else {
  stop("Where's `results`? I thought we already built it. ")
}

# Note: at 1.7M rows per `this.result`, this is going to get pretty 
# big pretty fast. Looks like about 50-60 MB per "section".
