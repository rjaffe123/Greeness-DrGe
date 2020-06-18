library(raster)
library(maptools)
library(spatstat)
library(dplyr)
library(sparr)
library(rgdal)

## FOR Ottawa


# tt_total <- read.csv("tt_total.csv")
# tt_log <- tt_total %>% mutate(case_control = ifelse(is.na(travel_time),0,1))
# tt_log<- tt_log %>% mutate(case_control = as.numeric(case_control))
# tt_log_ottawa <- tt_log %>% filter(CDNAME == "Ottawa")
tt_log_2019 <- read.csv("/home/rjaffe/tt_ottawa.csv")
shp.ottawa <- shapefile("/home/rjaffe/Ottawa_Dissemination_Areas-shp/ottawa.shp")
lonlat <- cbind(tt_log_2019$longitude, tt_log_2019$latitude)
sp.case <- SpatialPoints(lonlat,CRS("+proj=longlat +datum=WGS84"))
newcrs <- proj4string(shp.ottawa)
sp.case <- spTransform(sp.case,	CRS(newcrs))

## Parks
#driving
tt_2019_park <- tt_log_2019 %>% filter(year == 2019 & mode == "driving" & green_type == "park")
lonlat <- cbind(tt_2019_park$longitude, tt_2019_park$latitude)
sp.case <- SpatialPoints(lonlat,CRS("+proj=longlat +datum=WGS84"))
newcrs <- proj4string(shp.ottawa)
sp.case<-spTransform(sp.case, CRS(newcrs))
spdf.case <- SpatialPointsDataFrame(sp.case,	data=tt_2019_park)
regions <- slot(shp.ottawa, "polygons")
regions <- lapply(regions, function(x){ SpatialPolygons(list(x))})
windows <- lapply(regions, as.owin)
shp.ottawa1 <-tess(tiles=windows)
w.toronto <- as.owin(shp.ottawa1)
xy <- coordinates(spdf.case)
pp.case <- ppp(xy[, 1], xy[, 2], window = w.toronto, marks = as.factor(spdf.case$case_control))
ds.case <- density(pp.case)
spp.case <- split(pp.case)
pp.access <- spp.case$"1"
pp.noaccess <- spp.case$"0"
hpilot.access <- BOOT.density(pp.access)
hpilot.noaccess <- BOOT.density(pp.noaccess)
hglobal <- OS(pp.case, nstar = "geometric")

rho.hat <- risk(f = pp.access, g = pp.noaccess, h0 = hglobal, adapt = c(hpilot.access, hpilot.noaccess), tolerate = TRUE)


pdf("/home/rjaffe/ottawa_park_driving.pdf", width = 10, height = 8)
plot(rho.hat, tol.args = list(levels = c(0.01, 0.05), lty = 1:2), xlab = "Easting", ylab = "Northing")
dev.off()

## Forest
#driving
tt_2019_forest <- tt_log_2019 %>% filter(year == 2019 & mode == "driving" & green_type == "forest")
lonlat <- cbind(tt_2019_forest$longitude, tt_2019_forest$latitude)
sp.case <- SpatialPoints(lonlat,CRS("+proj=longlat +datum=WGS84"))
newcrs <- proj4string(shp.ottawa)
sp.case<-spTransform(sp.case, CRS(newcrs))
spdf.case <- SpatialPointsDataFrame(sp.case,	data=tt_2019_forest)
regions <- slot(shp.ottawa, "polygons")
regions <- lapply(regions, function(x){ SpatialPolygons(list(x))})
windows <- lapply(regions, as.owin)
shp.ottawa1 <-tess(tiles=windows)
w.toronto <- as.owin(shp.ottawa1)
xy <- coordinates(spdf.case)
pp.case <- ppp(xy[, 1], xy[, 2], window = w.toronto, marks = as.factor(spdf.case$case_control))
ds.case <- density(pp.case)
spp.case <- split(pp.case)
pp.access <- spp.case$"1"
pp.noaccess <- spp.case$"0"
hpilot.access <- BOOT.density(pp.access)
hpilot.noaccess <- BOOT.density(pp.noaccess)
hglobal <- OS(pp.case, nstar = "geometric")

rho.hat <- risk(f = pp.access, g = pp.noaccess, h0 = hglobal, adapt = c(hpilot.access, hpilot.noaccess), tolerate = TRUE) 


pdf("/home/rjaffe/ottawa_forest_driving.pdf", width = 10, height = 8)
plot(rho.hat, tol.args = list(levels = c(0.01, 0.05), lty = 1:2), xlab = "Easting", ylab = "Northing")
dev.off()


##Walking
#Park
tt_2019_park <- tt_log_2019 %>% filter(year == 2019 & mode == "walking" & green_type == "park")
lonlat <- cbind(tt_2019_park$longitude, tt_2019_park$latitude)
sp.case <- SpatialPoints(lonlat,CRS("+proj=longlat +datum=WGS84"))
newcrs <- proj4string(shp.ottawa)
sp.case<-spTransform(sp.case, CRS(newcrs))
spdf.case <- SpatialPointsDataFrame(sp.case,	data=tt_2019_park)
regions <- slot(shp.ottawa, "polygons")
regions <- lapply(regions, function(x){ SpatialPolygons(list(x))})
windows <- lapply(regions, as.owin)
shp.ottawa1 <-tess(tiles=windows)
w.toronto <- as.owin(shp.ottawa1)
xy <- coordinates(spdf.case)
pp.case <- ppp(xy[, 1], xy[, 2], window = w.toronto, marks = as.factor(spdf.case$case_control))
ds.case <- density(pp.case)
spp.case <- split(pp.case)
pp.access <- spp.case$"1"
pp.noaccess <- spp.case$"0"
hpilot.access <- BOOT.density(pp.access)
hpilot.noaccess <- BOOT.density(pp.noaccess)
hglobal <- OS(pp.case, nstar = "geometric")

rho.hat <- risk(f = pp.access, g = pp.noaccess, h0 = hglobal, adapt = c(hpilot.access, hpilot.noaccess), tolerate = TRUE) 


pdf("/home/rjaffe/ottawa_park_walking.pdf", width = 10, height = 8)
plot(rho.hat, tol.args = list(levels = c(0.01, 0.05), lty = 1:2), xlab = "Easting", ylab = "Northing")
dev.off()


## Forest
tt_2019_forest <- tt_log_2019 %>% filter(year == 2019 & mode == "walking" & green_type == "forest")
lonlat <- cbind(tt_2019_forest$longitude, tt_2019_forest$latitude)
sp.case <- SpatialPoints(lonlat,CRS("+proj=longlat +datum=WGS84"))
newcrs <- proj4string(shp.ottawa)
sp.case<-spTransform(sp.case, CRS(newcrs))
spdf.case <- SpatialPointsDataFrame(sp.case,	data=tt_2019_forest)
regions <- slot(shp.ottawa, "polygons")
regions <- lapply(regions, function(x){ SpatialPolygons(list(x))})
windows <- lapply(regions, as.owin)
shp.ottawa1 <-tess(tiles=windows)
w.toronto <- as.owin(shp.ottawa1)
xy <- coordinates(spdf.case)
pp.case <- ppp(xy[, 1], xy[, 2], window = w.toronto, marks = as.factor(spdf.case$case_control))
ds.case <- density(pp.case)
spp.case <- split(pp.case)
pp.access <- spp.case$"1"
pp.noaccess <- spp.case$"0"
hpilot.access <- BOOT.density(pp.access)
hpilot.noaccess <- BOOT.density(pp.noaccess)
hglobal <- OS(pp.case, nstar = "geometric")

rho.hat <- risk(f = pp.access, g = pp.noaccess, tolerate = TRUE) 


pdf("/home/rjaffe/ottawa_forest_walking.pdf", width = 10, height = 8)
plot(rho.hat, tol.args = list(levels = c(0.01, 0.05), lty = 1:2), xlab = "Easting", ylab = "Northing")
dev.off()

