# OSM Data interface example script ----
# Package details: https://cran.r-project.org/web/packages/osmdata/index.html

if (!require(osmdata)) { install.packages('osmdata') }; require(osmdata)
if (!require(tidyverse)) { install.packages('tidyverse') }; require(tidyverse)
if (!require(here)) { install.packages('here') }; require(here)
if (!require(sf)) { install.packages('sf') }; require(sf)

bb <- osmdata::getbb('Arkansas')

# List of all available feature types (key-value pairs) from OSM
available_features()

# Example using the healthcare tag
query_healthcare_buildings <- opq(bbox = bb) %>%
  add_osm_feature(key = 'healthcare') %>%
  osmdata_sf()

# Quick plot on returned polygon features
ggplot(query_healthcare_buildings$osm_polygons) +
  geom_sf()

# Output a geojson on the polygon data
st_write(query_healthcare_buildings$osm_polygons,
         here('Outs', 'healthcare_polygon_example.geojson'))


