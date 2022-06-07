# META ----
# Title:  Default setup for fainjj
# First Created: Thu Jun 18 12:05:48 2020
#
# NOTES ----
#'
#

# Setup ----
if (!require(tidyverse)) { install.packages('tidyverse') }; require(tidyverse)
if (!require(here)) { install.packages('here') }; require(here)
if (!require(sf)) { install.packages('sf') }; require(sf)
if (!require(devtools)) { install.packages('devtools') }; require(devtools)
install_github('https://github.com/rich-iannone/splitr')
require(splitr)
#

options(stringsAsFactors = T)
outputOnRun = FALSE

# Create the `dispersion_model` object, add
# a grid of starting locations, add run
# parameters, and then execute the model run
dispersion_model <-
  create_dispersion_model() %>%
  add_source(
    name = "particle",
    lat = 35.7959486, lon = -90.0169288, height = 1,
    rate = 500, pdiam = 0.25, density = 1, shape_factor = 1,
    release_start = lubridate::ymd_hm("2020-10-02 14:24"),
    release_end = lubridate::ymd_hm("2020-10-02 15:24")
  ) %>%
  add_dispersion_params(
    start_time = lubridate::ymd_hm("2020-10-02 14:00"),
    end_time = lubridate::ymd_hm("2020-10-02 15:00"),
    direction = "forward",
    met_type = "reanalysis"
  ) %>%
  run_model()

dispersion_plot(dispersion_model)

hysplit_dispersion(lat = 35.7959486, lon = -90.0169288, height = 1,
                   start_day = "2020-10-02", start_hour = 14, duration = 1,
                   direction = "forward", met_type = "reanalysis", vert_motion = 0,
                   model_height = 20000, particle_num = 2500, particle_max = 10000, disp_name = NULL, binary_path = NULL, exec_dir = NULL,
                   species = list(pdiam = 0.25, density = 1.5, shape_factor = 0.8),
                   met_dir = NULL, clean_up = TRUE)

disp_sf <- st_as_sf(dispersion_model$disp_df, coords = c('lon', 'lat'), crs = st_crs(4326))

outputSwitch(st_write(disp_sf, here('Outs', 'dispersion_mississippi_co_ark_burn1.gpkg')))


burn_point <- st_as_sf(data.frame(lat = 35.7959486, lon = -90.0169288),
                       coords = c('lon', 'lat'), crs = 4326)

disp_dist <- mutate(disp_sf, Distance = st_distance(burn_point, disp_sf))

ggplot(disp_dist,
       aes(
         as.numeric(
           diag(
             udunits2::ud.convert(
               Distance,'m', 'km')
           )),
         height)) +
  geom_point(size = 0.1, alpha = 1) +
  geom_density_2d_filled(alpha = 0.7) +
  geom_point(size = 0.1, alpha = 0.3) +
  # geom_density_2d(aes(color = ..level..)) +
  scale_fill_viridis_d(option='A', begin = 0.1, direction = -1) +
  scale_color_viridis_c(option='A', begin = 0.1, direction = -1) +
  theme_linedraw() +
  scale_y_continuous(limits = c(0, 400)) +
  scale_x_continuous(limits = c(0.01, 9)) +
  guides(fill = 'none', color = 'none') +
  labs(x='Distance from Source [km]',
       y='Height Above Surface [m]') +
  coord_cartesian(expand = c(0)) +
  ggtitle('Simulated Smoke Particle Dispersion')

outputSwitch(ggsave(here('Outs', 'arkansas_smoke_height.png'), dpi = 600))



