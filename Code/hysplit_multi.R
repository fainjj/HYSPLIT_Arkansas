library(magrittr)
library(tidyverse)
library(splitr)
#
# hdisp <- function(lon, lat, strd, strh){
#   hysplit_dispersion(lat = lat, lon = lon, height = 1,
#                      start_day = Date, start_hour = strh, duration = 1,
#                      direction = "forward", met_type = "reanalysis", vert_motion = 0,
#                      model_height = 20000, particle_num = 2500, particle_max = 10000, disp_name = NULL, binary_path = NULL, exec_dir = NULL,
#                      species = list(pdiam = 0.25, density = 1.5, shape_factor = 0.8),
#                      met_dir = NULL, clean_up = TRUE)}
#
# hdisp_wrap <- function(firepoints_list) {
#   firepoints_list %>%
#     mutate(Date = strptime(date)) %$%
#     {hdisp(lat = .$lat, lon = .$lon, start_day = Date, start_hour = strh)} %$%
#   hysplit_dispersion(lat = lat, lon = lon, height = 1,
#                      start_day = Date, start_hour = strh, duration = 1,
#                      direction = "forward", met_type = "reanalysis", vert_motion = 0,
#                      model_height = 20000, particle_num = 2500, particle_max = 10000, disp_name = NULL, binary_path = NULL, exec_dir = NULL,
#                      species = list(pdiam = 0.25, density = 1.5, shape_factor = 0.8),
#                      met_dir = NULL, clean_up = TRUE)}
#
# disp_list = vector(mode = 'list', nrow(firepoints))
#
# for(i in seq(1, 10)) {
#   init_list[[i]] <- firepoints[i, ] %>%
#     hdisp_wrap()
# }


hdist_apply <- function(firepoints_list) {
  firepoints_list %>%
    mutate(st_dt = as.Date(date, format = '%m/%d/%Y %H:%M', tz="US/Central")) %>%
    apply(1,
          function(x){
            hysplit_dispersion(lat = x['lat'], lon = x['lon'],
                               height = 1,
                               start_day = x['st_dt'],
                               start_hour = 12,
                               duration = 1,
                               direction = "forward",
                               met_type = "reanalysis",
                               vert_motion = 0,
                               model_height = 20000,
                               particle_num = 2500,
                               particle_max = 10000,
                               disp_name = NULL,
                               binary_path = NULL,
                               exec_dir = NULL,
                               species = list(pdiam = 0.25,
                                              density = 1.5,
                                              shape_factor = 0.8),
                               met_dir = here('met'),
                               clean_up = TRUE)
            }
          )
  }

hdist_apply(data.frame(lat=c(36), lon=c(-90), date=c("10/02/2020 00:00")))

hysplit_dispersion(lat = 35.7959486, lon = -90.0169288,
                   height = 1,
                   start_day = '2020-10-02',
                   start_hour = 14,
                   duration = 1,
                   direction = "forward",
                   met_type = "reanalysis",
                   vert_motion = 0,
                   model_height = 20000,
                   particle_num = 2500,
                   particle_max = 10000,
                   disp_name = NULL,
                   binary_path = NULL,
                   exec_dir = NULL,
                   species = list(pdiam = 0.25,
                                  density = 1.5,
                                  shape_factor = 0.8),
                   met_dir = NULL,
                   clean_up = TRUE)

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



hdist_apply <- function(firepoints_list) {
  firepoints_list %>%
    mutate(st_rl = (mdy_hm(date, tz="US/Central") + hours(14)) %>% as.character(),
           ed_rl = (mdy_hm(date, tz="US/Central") + hours(15)) %>% as.character(),
           st_tm = (ymd_hms(st_rl)+minutes(24)) %>% as.character(),
           ed_tm = (ymd_hms(ed_rl)+minutes(24)) %>% as.character()
           ) %T>%
    view() %>%
    {.mapply(data.frame, ., NULL)} %>%
    lapply(
        function(x){
          x <- x[[1]]
                  create_dispersion_model() %>%
                    add_source(
                      name = "particle",
                      lat = as.numeric(x['lat']),
                      lon = as.numeric(x['lon']),
                      height = 1,
                      rate = 500, pdiam = 0.25, density = 1, shape_factor = 1,
                      release_start = x['st_rl'],
                      release_end = x['ed_rl']
                    ) %>%
                    add_dispersion_params(
                      start_time = x['st_tm'],
                      end_time = x['ed_tm'],
                      direction = "forward",
                      met_type = "reanalysis"
                    ) %>%
                    run_model()
                }
    )
    #
    # mutate(model = create_dispersion_model() %>%
    #          add_source(
    #            name = "particle",
    #            lat = lat,
    #            lon = lon,
    #            height = 1,
    #            rate = 500, pdiam = 0.25, density = 1, shape_factor = 1,
    #            release_start = st_rl,
    #            release_end = ed_rl
    #          ) %>%
    #          add_dispersion_params(
    #            start_time = st_tm,
    #            end_time = ed_tm,
    #            direction = "forward",
    #            met_type = "reanalysis"
    #          ) %>%
    #          run_model() %>%
    #          list())

    # {.mapply(data.frame, ., NULL)} %>%
    # map(
    #   function(x){
    #             create_dispersion_model() %>%
    #               add_source(
    #                 name = "particle",
    #                 lat = as.numeric(x['lat']),
    #                 lon = as.numeric(x['lon']),
    #                 height = 1,
    #                 rate = 500, pdiam = 0.25, density = 1, shape_factor = 1,
    #                 release_start = x['st_rl'],
    #                 release_end = x['ed_rl']
    #               ) %>%
    #               add_dispersion_params(
    #                 start_time = x['st_tm'],
    #                 end_time = x['ed_tm'],
    #                 direction = "forward",
    #                 met_type = "reanalysis"
    #               ) %>%
    #               run_model()
    #           }
    # )


}

hdist_apply(firepoints[1:10, ])

hdist_apply(data.frame(lat = 35.7959486, lon = -90.0169288, date=c("10/02/2020 00:00")))

firepoints %>%
  mutate(st_dt = as.character(lubridate::mdy_hm(date, tz="US/Central") + hours(12)),
         ed_dt = as.character(lubridate::mdy_hm(date, tz="US/Central") + hours(13))
  )






# apply(1,
#       function(x){
#         create_dispersion_model() %>%
#           add_source(
#             name = "particle",
#             lat = as.numeric(x['lat']),
#             lon = as.numeric(x['lon']),
#             height = 1,
#             rate = 500, pdiam = 0.25, density = 1, shape_factor = 1,
#             release_start = x['st_rl'],
#             release_end = x['ed_rl']
#           ) %>%
#           add_dispersion_params(
#             start_time = x['st_tm'],
#             end_time = x['ed_tm'],
#             direction = "forward",
#             met_type = "reanalysis"
#           ) %>%
#           run_model()
#       }
# )
