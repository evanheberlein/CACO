library(tidyverse)
library(shapefiles)

HRE_shp <- shapefiles::read.shapefile("/Users/evanheberlein/Library/CloudStorage/Box-Box/Cornell/CACO/Undergrad_Thesis/CACO_GIS/HR_water_area")

map <- ggplot() + 
  geom_polygon(data = HRE_shp)#, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
