##################################################################################
### Clean start
##################################################################################

rm(list = ls())
gc()


##################################################################################
### Libraries etc
##################################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, sf, mapview, httr, tidytransit, devtools)


# avoid scientific notation
options(scipen=999)

# source functions from github
source_url("https://raw.githubusercontent.com/bjornsh/funktioner/main/func_gtfs.R")


dir.create(paste0(getwd(), "/data_input/"))
folder_input = paste0(getwd(), "/data_input/")


## Trafiklab key (https://developer.trafiklab.se/api/gtfs-regional-static-data)
trafiklab_key = rstudioapi::askForPassword()



##################################################################################
### Load data
##################################################################################

# define RKM
rkm = "ul"

# download GTFS
url <- paste0("https://opendata.samtrafiken.se/gtfs/", rkm, "/", rkm, ".zip?key=", trafiklab_key)
GET(url, write_disk(paste0(folder_input, "trafiklab_", rkm, ".zip"), overwrite=TRUE))

# load GTFS and remove all data except for today (ie working dat)
gtfs <- read_gtfs(paste0(folder_input, "trafiklab_", rkm, ".zip")) %>% 
  filter_feed_by_date(., Sys.Date())


### Fix spelling
# hpl name
gtfs$stops = gtfs$stops %>% 
  mutate(stop_name = str_replace_all(stop_name, c("Ã„" = "Ä", 
                                                  "Ã¤" = "ä",
                                                  "Ã–" = "Ö",
                                                  "Ã¶" = "ö",
                                                  "Ã…" = "Å",
                                                  "Ã¥" = "å",
                                                  "Ã©" = "é",
                                                  "Ã¼" = "ü")))

# linje type
gtfs$routes = gtfs$routes %>% 
  mutate(route_desc = str_replace_all(route_desc, c("Ã„" = "Ä", 
                                                    "Ã¤" = "ä",
                                                    "Ã–" = "Ö",
                                                    "Ã¶" = "ö",
                                                    "Ã…" = "Å",
                                                    "Ã¥" = "å",
                                                    "Ã©" = "é",
                                                    "Ã¼" = "ü")))





##################################################################################
### Use data
##################################################################################

antal_departure_hpl = funk_antal_departure_hpl(gtfs)

antal_linjer_hpl = funk_antal_linjer_hpl(gtfs)

hpl_id_namn = funk_hpl_id_namn(gtfs)

hpl_sf = funk_hpl_koordinat(gtfs)
mapview(hpl_sf)

network_sf = funk_linje_network(gtfs)
mapview(network_sf)

