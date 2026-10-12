###
# R spatial data training
###

library(sf)
library(tmap)
library(terra)
library(tidyverse)

help(package = 'sf')

# First Stuff ####

protectedLands <- terra::rast(x = 'beginnerGeospatialR/proAreas.tif')

terra::plot(protectedLands)

## Coordinate reference systems are the basis of geospatial mapping.
## Learn to read and understand them. Make sure all your fiels use the same.

## epsg.io - Search for specific coordinate reference systems
## EPSG:4326 - Most popular, not projected.

# Moving On ####

observationData <- read.csv('beginnerGeospatialR/cucurbitaData.csv')

str(observationData)

unique(observationData$taxon)

# Starting Workflow Evaluating One ####

## Test workflow on smaller sample set as datasets can be slow to process.

uniqueSpecies <- unique(observationData$taxon)

species1 <- uniqueSpecies[1]

# speciesData <- observationData[observationData$taxon == species1, ]

speciesData <- observationData |> filter(taxon == species1)

## Convert to spatial object ====

# we say lat-long, but most programming languages accept long-lat

spatialData1 <- sf::st_as_sf(
  x = speciesData,
  coords = c('longitude', 'latitude'),
  crs = terra::crs(protectedLands),
  remove = FALSE
)

spatialData1 |> terra::plot()

tmap::qtm(spatialData1)
tmap::tmap_mode(mode = 'view')

# Verify the datasets utilize the same CRS.
terra::same.crs(x = protectedLands, y = spatialData1)

# Check to see if the datasets overlap the same area
sf::st_bbox(spatialData1)
terra::ext(protectedLands)

# HUGE compared to Baja Peninsula
qtm(protectedLands)


## Cropping Raster to extent of point data ====

# sf object and terra object
# sf supports dplyr
# terra does not (limits tabular data functionality)

spatialData1_terra <- terra::vect(spatialData1)

spatialData1_terra

# terra objects work better with

cropped_protectedLands <- terra::crop(
  x = protectedLands,
  y = spatialData1_terra
)

qtm(cropped_protectedLands)

## Extract from raster to points ====

extractedValues <- terra::extract(
  x = cropped_protectedLands,
  y = spatialData1_terra
)

# add back to the spatial feature

spatialData1_terra$inProtected <- terra::extract(
  x = cropped_protectedLands,
  y = spatialData1_terra
)[, 2]

spatialData1_terra

# Generating conservation metrics

totalProtected <- nrow(extractedValues[!is.na(extractedValues$proAreas), ])

conservationMetrics <- round(100 * (totalProtected / nrow(extractedValues)))

# Make a good looking map ####

spatialData1_terra

map <- tmap::tm_shape(shp = spatialData1_terra) +
  tmap::tm_dots(
    fill = 'inProtected',
    size = 0.3,
    fill.legend = "Occurrence in protected lands"
  )

map
