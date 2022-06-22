# Load required packages
require(tidyverse)
require(splitr)
require(lubridate)
require(here)
#

# Check for met and exec directories. Create them if they aren't already there.
# if(!dir.exists(here('met'))){system('mkdir met')}
# if(!dir.exists(here('exec'))){system('mkdir exec')}

# Begin model ----
# Single dispersion model run. Change default assumptions/time steps here.
dispersion_run_alias <- function(latitude, longitude, date_time,
                                 model_period_s = 3600,
                                 release_offset_s = 600,
                                 height = 1,
                                 rate = 500,
                                 pdiam = 0.25,
                                 dsty = 1,
                                 shape_factor = 1,
                                 hmax = 1000) {

  # These are the only *required* arguments to be provided via API
  burn <- data.frame(latitude, longitude, burn_dt = date_time)

  #' NOTE: The date_time field must be provided in a canonical format that
  #' can be ingested by lubridate functions such as ymd_hms. I've set
  #' truncated=3 to handle incomplete dates for most cases.

  # Begin single-source dispersion model
  create_dispersion_model() %>%
    # Input all defaults of the alias function as well as single-obs burn info
    add_source(
      name = "particle",
      lat = burn$latitude, lon = burn$longitude,
      height = height,
      rate = rate,
      pdiam = pdiam,
      density = dsty,
      shape_factor = shape_factor,
      release_start = ymd_hms(burn$burn_dt, truncated = 3) + release_offset_s,
      release_end = ymd_hms(burn$burn_dt, truncated = 3) + release_offset_s + model_period_s
    ) %>%
    # Set simulation time and met info
    add_dispersion_params(
      start_time = ymd_hms(burn$burn_dt, truncated = 3),
      end_time = ymd_hms(burn$burn_dt, truncated = 3) + model_period_s,
      direction = "forward",
      met_type = "reanalysis",
      met_dir = here('met'),
      exec_dir = here('exec'),
      clean_up = T,
      model_height = hmax
    ) %>%
    run_model()
}
#


# Provided for testing only ----
firepoints_test <- data.frame(lat=c(35.8, 35.8, 35.8),
                              lon=c(-91.7, -91.7, -91.7),
                              date=c("2020-01-01 12:00",
                                     "2020-01-01 14:00",
                                     "2020-01-01 16:00"))

# Using pmap to run multiple fires. Arguments are positional.
test_runs <- firepoints_test %>%
  pmap(dispersion_run_alias)

# check the plots
test_runs[[1]] %>% dispersion_plot()
test_runs[[2]] %>% dispersion_plot()
test_runs[[3]] %>% dispersion_plot()

# All plots together to prove alignment
test_runs %>%
  lapply(. %>%  `$`('disp_df')) %>%
  bind_rows() %>%
  ggplot(aes(x=lon, y=lat)) +
  geom_point()
#









