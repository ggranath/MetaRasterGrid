############################################################################
# Script to make polygons of raster files spatial coverage 
# using meta files (here tif.xml). No need to download all raster
# files to get an overview of coverage and easier to locate the raster
# file needed.
# Code extracts the raster file extents from 'GeoBndBox' (should be WGS84)
# Put all extents into one object and saves it as KML for easy viewing in
# Google maps/earth (or eg mapview/plotKML package)
# tested on ubuntu 16.04, R 3.3.1 
############################################################################

library(xml2) # xml stuff
library(magrittr) # pipe stuff
library(raster) # for bind function
files <- list.files(pattern="*.tif.xml") # list files
myfiles = lapply(files, read_xml) # read all files into a list

lis.name <- substr(unlist(files), 1, nchar(unlist(files))-8) # name of the file w/o extension
names(myfiles)<- lis.name # name list with file names
f = function (x) {
  #point <- x %>% xml_find_all("//nativeExtBox") #  sometimes a local projection is defined
  #matrix(xml_double(xml_children(point))[1:4], ncol=2, byrow = TRUE)
  pol <- x %>% xml_find_all("//GeoBndBox") # wgs84 bbox
  extent(matrix(xml_double(xml_children(pol))[2:5], ncol=2, byrow = TRUE))
}

extents <- lapply(myfiles, f) # retrieve extents from all files and save as a list


# make polygon objects from extent
# (lapply not used as 'bind' doesnt work with that list....dont know why)
library(spex)
poly.lis = list()
for (i in 1:length(extents)) {
  poly.lis[[i]] <- spex(extents[[i]], crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", .id = lis.name[i])
}

# merge polygons into one object
m <- do.call(bind, poly.lis) 

# match up with the correct IDs (ie file names)
library(maptools)
m2 <- spChFIDs(m, lis.name)

# check
getSpPPolygonsIDSlots(m2) 

# To display file names in Google maps a NAME column
# has to be added 
m2$NAME <- lis.name 
# remove the other columns (there is one per file name....dont know why)
namecol <- which(!(colnames(m2@data) == "NAME"))
m2 <- m2[,-namecol]

# plot on google map
# write kml
plot(m2) # quick check
library(rgdal)
writeOGR(m2, dsn="raster_overview.kml", layer = "NAME",  driver="KML") 

## Which raster file covers your location(s)?

# if there is a data frame with coordinates of locations, first make a point df
pts <- data.frame(lon = c(13.1623, 13.13827), 
                  lat = c(55.42784 ,55.48858), loc = c("A","B"))
loc <- SpatialPoints(pts[, c("lon","lat")])

# assign projection
proj4string(loc) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") 

# do an intersection (points in polygon)
# NAME gives the the file name for each point (NA if not covered)
cbind(pts, over(loc, m2))
