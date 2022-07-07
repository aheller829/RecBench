# This script will load monitoring data from terradat (and eventually the landscape 
# data commons), shapefiles defining L3 ecoregions, and will join monitoring plots
# and ecoregions

# Set working directory
setwd("C:\\Users\\aheller\\Documents\\Analysis\\RecBench")

# Load packages
library(devtools)
library(tidyverse)
library(sf)
library(terradactyl)
library(rgdal)
library(raster)


# Point to the source geodatabase 
source.dsn <-"C:\\Users\\aheller\\Documents\\Raw Data\\AIMTerrestrialEdtBackup1-25-22.gdb"

# Load L3 shapefile
study.area.dsn <- "C:\\Users\\aheller\\Documents\\GIS\\OandGBenchmarks\\L3_intersect_dissolved.shp"
study.area <- sf::st_read(study.area.dsn)

# Visualize ecoregions
plot(study.area)

# Read in AIM plots from TerrADat
tdat.plots <- sf::st_read(dsn = source.dsn, layer = "tblPlots")
# Check and reproject the plots
tdat.plots <- sf::st_transform(tdat.plots, sf::st_crs(study.area))
# Clip the TerraDat plots to the ecoregions
tdat.study.plots <- sf::st_intersection(study.area, tdat.plots)
# Map the clipped plots
plot(tdat.study.plots, max.plot = 1)
# Check that the number of plots in subset matches what is expected
length(unique(tdat.study.plots$PrimaryKey))

# Repeat for LMF plots
# Gather plots from LMF
lmf.plots <- sf::st_read(dsn = source.dsn, layer = "POINTCOORDINATES")
# Check and fix projections against study area
lmf.plots <- sf::st_transform(lmf.plots, sf::st_crs(study.area))
# Clip the LMF plots to the study area (ignore warning)
lmf.study.plots <- sf::st_intersection(study.area, lmf.plots)
# Map study.plots
plot(lmf.study.plots, max.plot=1)
# Check that the number of plots subsetted matches what is expected
length(unique(lmf.study.plots$PrimaryKey))

# Rbind AIM and LMF 
# Columns need to be modified to match
tdat.study.plots <- tdat.study.plots[, c(1, 2, 98)]
lmf.study.plots <- lmf.study.plots[, c(1, 2, 14)]
study.plots <- rbind(tdat.study.plots, lmf.study.plots)


# Plot points and polygons
plot(study.plots$geometry, cex = 0.5)
plot(study.area, col = alpha("gray", 0.5), lwd = 1.5, add = TRUE)

# Count plots within ecoregions
table(tdat.study.plots$US_L3CODE)
table(lmf.study.plots$US_L3CODE)



# Build environmental variables tables
header.lmf <- terradactyl::gather_header_lmf(source.dsn)
header.aim <- terradactyl::gather_header_terradat(source.dsn)
# Subset header tables
header.lmf <- header.lmf[, c(1, 4, 5, 8, 9, 11, 13, 12)]
header.aim <- header.aim[, c(1, 10, 9, 7, 8, 11, 6)]
# AIM header data doesn't include elevation - extract this variable from tdat.plots
# and join to header
aim.elev <- dplyr::select(tdat.plots, PrimaryKey, Elevation)
aim.elev$SHAPE <- NULL
header.aim <- dplyr::left_join(header.aim, aim.elev, by = "PrimaryKey")
# Rename columns to rbind
names(header.aim)
names(header.lmf)
header.lmf <- dplyr::rename(header.lmf, DateEstablished = DateVisited, Elevation = ELEVATION)
# Rbind tables
header <- rbind(header.aim, header.lmf)


## Define ecoregion of interest
ecoregion <- "20"


# Subset plots to ecoregion
er.plots <- dplyr::filter(study.plots, US_L3CODE == paste(ecoregion))
er.header <- subset(header, header$PrimaryKey %in% er.plots$PrimaryKey)


# Read sdat in from asc file
library(raster)
r <- raster::raster("C:\\Users\\aheller\\Documents\\Benchmarks\\Spatial\\sdat.asc")
# Reproject to match study.plots
raster::crs(r) <- crs("EPSG:5070")
r <- raster::projectRaster(r, crs = crs(study.plots))
# Set -9999 to NA (described in sdat metadata)
r[r == -9999] <- NA
# Look at histogram of raster values
hist(r)


# Extract sdat values to points
study.plots.sdat <- extract(r, study.plots)


population_26 <- extract(raster26,
                         PM25_p26,
                         buffer=500, # 500m radius
                         fun=mean,na.rm=T,
                         sp = TRUE,
                         method='simple')



















###### READ IN SITE DATA
# Read in AIM site data
tdat.plotinformation.tbl <- sf::st_read(dsn = source.dsn, layer = "tblPlots")
# Subset to study area
tdat.plotinfotbl.study <- subset(tdat.plotinformation.tbl,
                                 tdat.plotinformation.tbl$PrimaryKey %in% tdat.study.PK$PrimaryKey)
# Read in LMF site data
lmf.plotinformation.tbl <- sf::st_read (dsn = source.dsn, layer = "POINT")
# Subset to study area
lmf.plotinfotbl.study <- subset(lmf.plotinformation.tbl, 
                                lmf.plotinformation.tbl$PrimaryKey %in% lmf.study.PK$PrimaryKey)

# Customize AIM site data
# Keep only desired variables
names(tdat.plotinfotbl.study)
tdat.plotinfotbl.study <- select(tdat.plotinfotbl.study,
                                 PrimaryKey, PlotID, EstablishDate, State, County, AvgPrecip, AvgPrecipUOM,
                                 Slope, Aspect,
                                 Slope_Shape = ESD_SlopeShape, Elevation, Longitude, Latitude)
# Convert to dataframe and remove geometry variable (spatial reference)
tdat.plotinfotbl.study <- as.data.frame(tdat.plotinfotbl.study)
tdat.plotinfotbl.study$geometry <- NULL
tdat.plotinfotbl.study <- select(tdat.plotinfotbl.study, -SHAPE)
# Transform AvgPrecip units of measurement to mm when recorded in inches 
tdat.plotinfotbl.study$AvgPrecip <- ifelse(tdat.plotinfotbl.study$AvgPrecipUOM %in% c("in"), tdat.plotinfotbl.study$AvgPrecip*25.4, tdat.plotinfotbl.study$AvgPrecip)
tdat.plotinfotbl.study <- select(tdat.plotinfotbl.study, -"AvgPrecipUOM")

# Customize LMF site data
# Keep only desired variables
names(lmf.plotinfotbl.study)
lmf.plotinfotbl.study <- select(lmf.plotinfotbl.study, "PrimaryKey", "STATE", "COUNTY", "MLRA", "VERTICAL_SLOPE_SHAPE",
                                "HORIZONTAL_SLOPE_SHAPE", "SLOPE_PERCENT", "SLOPE_ASPECT")
# Transform states and counties from code to standard abbreviation or name
# Cross reference with a spatial layer in GIS map to determine what codes LMF is using for states and counties
# Change state codes
unique(lmf.plotinfotbl.study$STATE, incomparables=FALSE)
lmf.plotinfotbl.study$STATE <- ifelse(lmf.plotinfotbl.study$STATE == 35, "NM", "CO")
# Change county codes
unique(lmf.plotinfotbl.study$COUNTY, incomparables=FALSE)
lmf.plotinfotbl.study$COUNTY <- ifelse(lmf.plotinfotbl.study$COUNTY == 39, "Rio Arriba",
                                       ifelse(lmf.plotinfotbl.study$COUNTY == 21, "Conejos", "Taos"))
# Transform aspect to numeric value
lmf.plotinfotbl.study <- mutate(lmf.plotinfotbl.study, Aspect = ifelse(SLOPE_ASPECT=="N", 0,
                                                                       ifelse(SLOPE_ASPECT=="NE", 45,
                                                                              ifelse(SLOPE_ASPECT=="E", 90,
                                                                                     ifelse(SLOPE_ASPECT=="SE", 135,
                                                                                            ifelse(SLOPE_ASPECT=="S", 180,
                                                                                                   ifelse(SLOPE_ASPECT=="SW", 225, 270)))))))
# Transform slope shapes to slope shape codes used by AIM
lmf.plotinfotbl.study <- mutate(lmf.plotinfotbl.study, Slope_Shape = ifelse(VERTICAL_SLOPE_SHAPE=="concave" & HORIZONTAL_SLOPE_SHAPE=="concave", "CC",
                                                                            ifelse(VERTICAL_SLOPE_SHAPE=="concave" & HORIZONTAL_SLOPE_SHAPE=="linear", "CL",
                                                                                   ifelse(VERTICAL_SLOPE_SHAPE=="concave" & HORIZONTAL_SLOPE_SHAPE=="convex", "CV",
                                                                                          ifelse(VERTICAL_SLOPE_SHAPE=="linear" & HORIZONTAL_SLOPE_SHAPE=="linear", "LL",
                                                                                                 ifelse(VERTICAL_SLOPE_SHAPE=="linear" & HORIZONTAL_SLOPE_SHAPE=="concave", "LC",
                                                                                                        ifelse(VERTICAL_SLOPE_SHAPE=="linear"& HORIZONTAL_SLOPE_SHAPE=="convex", "LV", 
                                                                                                               ifelse(VERTICAL_SLOPE_SHAPE=="convex" & HORIZONTAL_SLOPE_SHAPE=="convex", "VV",
                                                                                                                      ifelse(VERTICAL_SLOPE_SHAPE=="convex" & HORIZONTAL_SLOPE_SHAPE=="linear", "VL", "VC")))))))))
# Generate PlotID for LMF plots (use PrimaryKey)
lmf.plotinfotbl.study$PlotID <- lmf.plotinfotbl.study$PrimaryKey
# Add additional data to LMF.plotinfotbl.study to create a dataframe compatible with the one housing AIM site data
# Get latitude & longitude from POINTCOORDINATES
lmf.pointcoordinates <- sf::st_read(dsn = source.dsn, layer = "POINTCOORDINATES")
# Subset to study area
lmf.pointcoordinates <- subset(lmf.pointcoordinates, 
                               lmf.pointcoordinates$PrimaryKey %in% lmf.study.PK$PrimaryKey)
# Keep only desired variables
lmf.pointcoordinates <- select(lmf.pointcoordinates, "PrimaryKey", "FIELD_LONGITUDE", "FIELD_LATITUDE")
# Remove geometry and convert to dataframe
lmf.pointcoordinates$geometry <- NULL
lmf.pointcoordinates <- as.data.frame(lmf.pointcoordinates)
# Get elevation and date from LMF GPS table
lmf.gps <- sf::st_read(dsn = source.dsn, layer = "GPS")
# Subset to study area
lmf.gps <- subset(lmf.gps, lmf.gps$PrimaryKey %in% lmf.study.PK$PrimaryKey)
# Keep desired variables
lmf.gps <- select(lmf.gps, "PrimaryKey", "ELEVATION", "SURVEY")
# Merge coordinates, date, and elevation with lmf.plotinfotbl.study
lmf.plotinfotbl.study <- merge(lmf.plotinfotbl.study, lmf.pointcoordinates) 
lmf.plotinfotbl.study <- merge(lmf.plotinfotbl.study, lmf.gps)
# Convert LMF elevation from feet to meters
lmf.plotinfotbl.study$Elevation <- lmf.plotinfotbl.study$ELEVATION * 0.3048
# Rename variables in lmf.plotinfotbl.study to match tdat.plotinfotbl.study so tables can be joined
lmf.plotinfotbl.study <- lmf.plotinfotbl.study %>%
  select(PrimaryKey,
         State = STATE,
         County = COUNTY,
         MLRA,
         Slope = SLOPE_PERCENT,
         Aspect,
         Slope_Shape,
         PlotID,
         Latitude = FIELD_LATITUDE,
         Longitude = FIELD_LONGITUDE,
         Year = SURVEY,
         Elevation)
# Add precipitation data (csv calculated in arcGIS from PRISM raster)
lmf.precip <- read.csv("lmf_precip.csv")
lmf.plotinfotbl.study <- merge(lmf.plotinfotbl.study, lmf.precip)


# Extract year sampled from EstablishDate variable
tdat.plotinfotbl.study$Year <- format(as.Date(tdat.plotinfotbl.study$EstablishDate, format = "%Y-%m-%d"), "%Y")
tdat.plotinfotbl.study <- select(tdat.plotinfotbl.study, -EstablishDate)
# Add MLRAs to AIM plots (file created via join in ArcMap)
mlras <- read.csv("mlras.csv")
# Change 48A to 48
mlras <- mlras %>%
  mutate(MLRA = ifelse(MLRARSYM == "48A", "48",
                       ifelse(MLRARSYM == "51", "51",
                              ifelse(MLRARSYM == "36", "36", NA)))) %>%
  select(-MLRARSYM)
# Join with plot data
tdat.plotinfotbl.study <- left_join(tdat.plotinfotbl.study, mlras, by = "PrimaryKey")

# Join LMF and AIM tables vertically 
names(tdat.plotinfotbl.study)
names(lmf.plotinfotbl.study)
site.data.table <- rbind(tdat.plotinfotbl.study, lmf.plotinfotbl.study)
# Find missing data and revise as needed
missingdatacheck <- site.data.table[rowSums(is.na(site.data.table)) > 0, ]
# Manually add missing values where possible (e.g. calculated aspect in ArcMap to enter for plot that was missing aspect)
site.data.table[205, "Aspect"] <- 194
site.data.table[205, "Slope_Shape"] <- "LL"

# Join any other variables that were derived external to TerrADat (e.g. in ArcMap)
# To simplify, combine into a single csv file so one join adds all variables
# Load in PDIR (csv calculated in ArcMap)
pdir.study <- read.csv("PDIR.csv")
site.data.table <- left_join(site.data.table, pdir.study)
# Load in min temps (calculated in arcmap from USGS Geospatial Gateway raster)
mintemps <- read.csv("mintemps.csv")
site.data.table <- left_join(site.data.table, mintemps)
# Load in treatment data (generated in ArcMap from treatment polygon)
# Read in treatment data
library(lubridate)
treats <- read.csv("treats.csv")
treats$EstablishD <- format(as.Date(treats$EstablishD, format = "%m/%d/%Y"), "%Y")
treats$EstablishD <- as.numeric(treats$EstablishD)
treats$TreatType <- as.character(treats$TreatType)
treats <- treats %>%
  select(-FID) %>%
  mutate(TimeSince = EstablishD - TreatDate) %>%
  select(-EstablishD, -LinkedId) 
# Join to table
site.data.table <- left_join(site.data.table, treats)
# Add broader treatment type categories
unique(site.data.table$TreatType)
site.data.table <- mutate(site.data.table, TreatCat = ifelse(TreatType == "ShaveSeed" | TreatType == "DiscSeed" | TreatType == "DiscSeed " | TreatType == "Disc Seed" |
                                             TreatType == "DiscSeed Brushhog", "MS",
                                           ifelse(TreatType == "Brushhog " | TreatType == "Disc Brushhog" | TreatType == "Disc", "M",
                                                  ifelse(TreatType == "ThinBurn" | TreatType == "DrillSeed Burn", "F",
                                                         ifelse(TreatType == "Teb", "C", "N")))))
# Replace NA with "N" if no treatment
site.data.table <- site.data.table %>%
  mutate(TreatType = ifelse(is.na(TreatType), "N", TreatType),
         TreatDate = ifelse(is.na(TreatDate), "N", TreatDate),
         TimeSince = ifelse(is.na(TimeSince), "N", TimeSince),
         TreatCat = ifelse(is.na(TreatCat), "N", TreatCat))
# Convert aspects to northness/eastness
# Make sure aspect is a numeric data type
str(site.data.table$Aspect)
site.data.table$Aspect <- as.numeric(site.data.table$Aspect)
site.data.table <- site.data.table %>%
  mutate(Northness = cos(Aspect), Eastness = sin(Aspect)) 

# Find missing data and revise as needed
missingdatacheck <- site.data.table[rowSums(is.na(site.data.table)) > 0, ]
# Another plot missing Aspect. This didn't show in the first data check because the NA was entered as 
# character type "N/A" and switched to NA when the Aspect column was converted to numeric data type
site.data.table[188, "Aspect"] <- 174

# Write to csv
write.csv (site.data.table, "site.data.table.csv")

