#-----------------------------------------------------------------------------------------
library(RCurl)
url <- "http://upload.wikimedia.org/wikibooks/en/a/a8/XML_example_polygon.svg"
svg <- getURL(url)
# Parse the file
library(XML)
x <- SVGMapping::loadSVG("map_2_.svg")
doc <- htmlParse(x)
# Extract the coordinates, as strings
p <- xpathSApply(doc, "//polygon", xmlGetAttr, "points")
# Convert them to numbers
p <- lapply( strsplit(p, " "), function(u) 
        matrix(as.numeric(unlist(strsplit(u, ","))),ncol=2,byrow=TRUE) )

#-----------------------------------------------------------------------------------------
#Region map
require(sp)
require(ggplot2)
require(maptools)
require(rgeos)
#Unit Russia with Chukotka and Crimea 
gadm <- readRDS("RUS_adm1.rds")
adm <- readRDS("UKR_adm1.rds")
for(j in 1:length(gadm@polygons[[12]]@Polygons)){
        gadm@polygons[[12]]@Polygons[[j]]@coords[,1] <-
                sapply(gadm@polygons[[12]]@Polygons[[j]]@coords[,1], function(x) if(x<0) x<-359.999+x else x) }
gadm2 <- rbind(gadm[-c(66,67),], adm[c(4,20),])
#and also remove Sverdlovsk
gadm2@polygons[[66]]@Polygons[[3]] <- NULL
gadm2@polygons[[66]]@plotOrder <- c(2L, 1L)

#Remove small polygins from the map (see maps.R scrypt)
gadm3 <- getSmallPolys(gadm2)
#Generalize boundaries
gadm3@polygons <- lapply(gadm3@polygons, "comment<-", NULL)
sr <- rgeos::gSimplify(spgeom = gadm3, tol = 0.05, topologyPreserve = FALSE)
sr59 <- rgeos::gSimplify(spgeom = gadm3[59,], tol = 0.05, topologyPreserve = TRUE)
gadm4 <- SpatialPolygonsDataFrame(sr, gadm3@data, match.ID = TRUE)
#Make Moscow, Sevastopol, St.Pb and Ryazan visible
gadm4@polygons[[59]]@Polygons[[1]]@coords <- sr59@polygons[[1]]@Polygons[[1]]@coords
gadm4@polygons[[85]]@Polygons[[1]]@coords <- gadm4@polygons[[85]]@Polygons[[1]]@coords[c(1,7:9,12),]
gadm4@polygons[[84]]@Polygons[[1]]@coords <- gadm4@polygons[[84]]@Polygons[[1]]@coords[-c(32:36),]
gadm4@polygons[[14]]@Polygons[[1]]@coords <- gadm4@polygons[[14]]@Polygons[[1]]@coords[-c(15,16),]
gadm4@polygons[[43]]@Polygons[[1]]@coords[4,] <- gadm4@polygons[[44]]@Polygons[[1]]@coords[49,]
gadm4@polygons[[43]]@Polygons[[1]]@coords[5,] <- gadm4@polygons[[44]]@Polygons[[1]]@coords[50,]
gadm4@polygons[[44]]@Polygons[[1]]@coords <- rbind(gadm4@polygons[[44]]@Polygons[[1]]@coords, gadm4@polygons[[44]]@Polygons[[1]]@coords[69:80,])
gadm4@polygons[[44]]@Polygons[[1]]@coords[62:80,] <- gadm4@polygons[[44]]@Polygons[[1]]@coords[50:68,]
gadm4@polygons[[44]]@Polygons[[1]]@coords[50:61,] <- gadm4@polygons[[43]]@Polygons[[1]]@coords[c(3,2,15:6),]
saveRDS(gadm4, "gadm4.Rds")
east <- sapply(lapply(gadm4@polygons, function(x) sapply(x@Polygons, function(y) max(y@coords[,"x"]))), max)
eastern_rus <- gadm4[order(east, decreasing = TRUE),]
for (i in 1:length(gadm4@polygons)) eastern_rus@polygons[[i]]@ID <- as.character(eastern_rus@data$ID_1[i])
gadm4@polygons <- lapply(gadm4@polygons, "comment<-", NULL)

rus <- merge(fortify(gadm4, region = "ID_1"), gadm4@data, by.x = "id", by.y = "ID_1", all.x = TRUE)
saveRDS(rus, "rus.Rds")
p <- ggplot(data = rus, aes(x = long, y = lat, group = group)) + 
        geom_polygon(aes(fill = LGF/LG), color = "white") + 
        coord_map(projection = 'azequidist') +
        guides(fill=FALSE) +
        geom_text(aes(x = x, y = y, label = substr(HASC_1,4,5)), colour = "white", size = 3, fontface = 2)
plot(p)

#-----------------------------------------------------------------------------------------
#District map
require(ggplot2)
r <- readRDS("RUS_adm2.rds")
u <- readRDS("UKR_adm2.rds")
for(i in which(r$NAME_1=="Chukot")) {
        for(j in 1:length(r@polygons[[i]]@Polygons)) {
                r@polygons[[i]]@Polygons[[j]]@coords[,1] <-
                        sapply(r@polygons[[i]]@Polygons[[j]]@coords[,1],function(x) ifelse(x<0,359.999+x,x))
        }}
r@bbox[1,1] <- 0
cr <- u[u$NAME_1 %in% c("Sevastopol'","Crimea"),]
for(i in 1:length(cr@polygons)) cr@polygons[[i]]@ID <- paste0("u", cr@polygons[[i]]@ID)
row.names(cr@data) <- sapply(cr@polygons, function(x) x@ID)
r1 <- getSmallPolys(rbind(r, cr))
r1@polygons <- lapply(r1@polygons, "comment<-", NULL)
sr <- rgeos::gSimplify(spgeom = r1, tol = 0.01, topologyPreserve = T)
r1@data$area <- sapply(r1@polygons, function(x) sum(sapply(x@Polygons, function(y) y@area)))
r2 <- SpatialPolygonsDataFrame(sr, r1@data, match.ID = TRUE)
saveRDS(r2, "r2.Rds")
rd <- merge(fortify(r2, region = "ID_2"), r2@data, by.x = "id", by.y = "ID_2", all.x = TRUE)
saveRDS(rd, "rd.Rds")
ggplot(data = rd, aes(x = long, y = lat, group = group)) + 
        geom_polygon(aes(color = factor(ID_1)), fill = "white") + 
        coord_map(projection = 'azequidist') +
        guides(fill=FALSE)

f <- read.csv2("reliefs.csv", stringsAsFactors = F)
df <- cbind(df[,1:9], f)
rus <- merge(fortify(gadm4, region = "ID_1"), df, by.x = "id", by.y = "ID_1", all.x = TRUE)



getSmallPolys <- function(poly, minarea=0.01) {
        # Get the areas
        areas <- lapply(poly@polygons, function(x) sapply(x@Polygons, function(y) y@area))
        
        # Quick summary of the areas
        print(quantile(unlist(areas)))
        
        # Which are the big polygons?
        bigpolys <- lapply(areas, function(x) which(x>minarea | x==max(x)))
        #length(unlist(bigpolys))
        
        # Get only the big polygons
        for(i in 1:length(bigpolys)) {
                poly@polygons[[i]]@Polygons <- poly@polygons[[i]]@Polygons[bigpolys[[i]]]
                poly@polygons[[i]]@plotOrder <- 1:length(poly@polygons[[i]]@Polygons)
        }
        return(poly)
}
