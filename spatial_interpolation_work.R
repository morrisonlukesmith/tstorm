

# these are the packages/libraries I need.
# install.packages("raster")
# install.packages("gstat")
library(gstat)
library(sp)
library(maptools)
#install.packages("spatial.tools")
library("rgeos")
library("ggplot2")
#install.packages("remotes")
remotes::install_github("MariekeDirk/GeoInterpolation")
library("GeoInterpolation")

#import lighting data for the state of MN#
#data is read in as date yyyymmdd long lat count of 
#strikes of lightning for a given day, for a 0.1 degree grid
#surroundig the listed centroid.
mydata = read.csv("Z:/personal/luke/climate_data_all/lightning data/mn_lightning_data/ALLMSPNLDN.csv")

#restrict the data to a bounded box that is a bit larger than the twin cities
#which is my study area.
mydata1 <-  mydata[ which(mydata$centerlon > -94.5
                            & mydata$centerlon < -92
                            & mydata$centerlat > 44
                            & mydata$centerlat < 46), ]

#Import shape files for a selected area around the twin cities
#assign as shape poly, and set the projection
#this imports a selection of shape files; these are the zipcodes for the 
# area in and around minneapolis.  it has a number of variables that I don't need.
#import, and assign a projection
q<-readShapePoly("Z:/personal/luke/dissertation/testing_storms/msp_zip_select.shp",proj4string =CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

#subset for one day
#do outside of loop
msp_nldn <- subset(mydata1,zday==20000508)

#assign as lat long the coordinate system of the .csv of lightning data.
#do outside of loop?  
#tell R that variables centerlon and centerlat are gridded long lat
coordinates(msp_nldn) <- ~centerlon+centerlat
#assign a projection type (map projection)
proj4string(msp_nldn)=CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
#fyi, command proj4string(df) will return the coordinate ref system


#plot lightning data (for one day) over the zip codes
#this is just a visualization.  
plot(q)
plot (msp_nldn, add=TRUE)

#this is just a summary plot to put some 'bubbles' 
#that represent the relative 'weight' of the lilghtning strikes 
#over my gridded study area.
plot(msp_nldn)
bubble (msp_nldn,"total_count")

#meat and potatoes.
#idw is a function in the gstat package.  this predicts the count for each area n the grid newdata=q,
#q is my shape file; so each zipcode has a value that is the average of all the points contained by it.
#nmax = 8 tells it to use the 8 surrounding points of my grid.  fwiw, everyone says that the grid should 
#be definied in degrees, so that I could use maxdist = 0.14 (the hypotenuse of a 0.1 degree grid) to do the same thing
#which would include the surrounding points -- in fact if i assume KM as the unit, and use maxdist = 14
#i run it 
#it works just fine.  go figure. 

Lightning.IDW<-idw(total_count ~ 1, locations=msp_nldn, newdata=q, nmax=8) 
Lightning.IDW2<-idw(total_count ~ 1, locations=msp_nldn, newdata=q, maxdist=12) 

#this will plot it for me.  ugh.
idw_res<- st_as_sf(Lightning.IDW)
plot(idw_res
)
ggplot(data=idw_res)+geom_sf(aes(fill=var1.pred))


     )

#this is a 'spatial Cbind'.  if i use 'simple features' I can just use cbind.
lightning.dat<-spCbind(q,Lightning.IDW$var1.pred)

#this is a way to investigate the relationship across points of varied distances.
#not important
nldn.vgm=variogram(total_count ~1,msp_nldn)
nldn.vgm


#Let's do the same thing with simple features
############################################################


#import a shape file that covers just the restricted area called p<-msp_zip_select.shp
#import a gridded shape file layer with data yyyymmdd, long, lat, total_count = daily count strikes lightning
#for a given 0.1 degree grid, at that centroid.
#same idea, this just uses the new 'simple features' process, which should simplify things.

p <- st_read("Z:/personal/luke/dissertation/testing_storms/msp_zip_select.shp")
nldn_shp <- st_read("Z:/personal/luke/dissertation/testing_storms/msp_nldn.shp")
 nldn_dat <- subset(nldn_shp,zday==20000508)
  plot(nldn_dat)
  ggplot()+geom_sf(data=p)+geom_point(data=nldn_dat,aes(x=centerlon,y=centerlat))
  
  Lightning.IDW3<-idw(total_coun ~ 1, locations=nldn_dat, newdata=p, nmax=8) 
  Lightning.IDW4<-idw(total_coun ~ 1, locations=nldn_dat, newdata=p, maxdist=12) 
#import station data

  ggplot(data=Lightning.IDW3)+geom_sf(aes(fill=var1.pred))+geom_point(data=nldn_dat,aes(x=centerlon,y=centerlat))
  
  lightning.dat2<-cbind(p,Lightning.IDW3$var1.pred)
  head(lightning.dat2)

  
  ##################################
  #nearest neighbor stuff.
#  in this it's another form of spatial interpolation.  but this data has a series of points where weather 
  #stations exist in the area.  using arc GIS, i've restricted the data to an area in and around MSP.
  #this data has separate year, month, day columns for date -- I could convert all data to the same convention.
  #
  #import msp meso
  station<- st_read("Z:/personal/luke/dissertation/testing_storms/MSP_meso.shp")
  station_day<-station[which(station$month== 5 & station$day==8 & station$year == 2000),]
  
  gs <- doNearestNeighbor(formula=dayP01~1, station_day,p,debug=1)
  ggplot(data=gs)+geom_sf(aes(fill=var1.pred))+geom_point(data=station,aes(x=lon,y=lat),pch=24,cex=3,color="red",bg="red")

  #i'll just cbind it to my already existing data frame?
  #so now i have rain, and lightning (and other stuff, for one day (05082000))
  #it just doestn' say that anywhere in my data
  
    alldat.test2<-cbind(lightning.dat2,gs$var1.pred)
  ggplot(data=alldat.test2)+geom_sf(aes(fill=gs.var1.pred))+geom_point(data=station,aes(x=lon,y=lat),pch=24,cex=3,color="red",bg="red")
  
#here i can add a datenum variable.
alldat.test2$datenum<-20000508


#now i just need to loop through yyyymmdd="20000301-20181130", for all zips (looks like there are 344 of them in this case)
  

