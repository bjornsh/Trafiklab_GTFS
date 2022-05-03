#---------------------------------------------------------------------------------------------------
# Syfte
#---------------------------------------------------------------------------------------------------

# Download GTFS, filter todays date, filter per kommun, save as .shp




#---------------------------------------------------------------------------------------------------
# set up
#---------------------------------------------------------------------------------------------------

# clean
rm(list = ls())
invisible(gc())

options(dplyr.summarise.inform = FALSE)

# libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, sf, sp, httr, mapview, leaflet)


# avoid scientific notation
options(scipen=999)


# create directory
dir.create("data_input")
dir.create("data_output")
dir.create("output")

dir.create("rapport")


wd = getwd()

data_input = paste0(wd,"/data_input")
data_output = paste0(wd,"/data_output")
output = paste0(wd,"/output")

# path to github for shapefiles
folder_github = "https://github.com/bjornsh/gis_data/raw/main/"


### url for GTFS
# !!!!!! Specify RKM.
rkm = "ul"  


kommun_namn = "Uppsala"


# !!!!!! Specify län kod
lan_kod = "03" 

# lan_kod Region                rkm
# 01      Stockholms län        (sl)
# 03      Uppsala län           (ul)
# 04      Södermanlands län     (sormland)
# 05      Östergötlands län     (otraf)
# 06      Jönköpings län        **SAKNAS**
# 07      Kronobergs län        (krono)
# 08      Kalmar län            (klt)
# 09      Gotlands län          (gotland)
# 10      Blekinge län          (blekinge)
# 12      Skåne län             (skane)
# 13      Hallands län          (halland)
# 14      Västra Götalands län  (vt)
# 17      Värmlands län         (varm)
# 18      Örebro län            (orebro)
# 19      Västmanlands län      (vl)
# 20      Dalarnas län          (dt)
# 21      Gävleborgs län        (xt)
# 22      Västernorrlands län   (dintur)
# 23      Jämtlands län         **SAKNAS**
# 24      Västerbottens län     **SAKNAS**
# 25      Norrbottens län       **SAKNAS**
#         SJ                    (sj)


# todays date, used as filter
today = str_remove_all(Sys.Date(), "-")

## Trafiklab key
# api_fil <- read_file(paste0("Z:/api"))
# trafiklab_key = gsub('^.*trafiklab_gtfsstatik: \\s*|\\s*\r.*$', "", api_fil)
trafiklab_key = rstudioapi::askForPassword()


#---------------------------------------------------------------------------------------------------
# Fetch data 
#---------------------------------------------------------------------------------------------------

###### static GTFS timetable data from Trafiklab
url <- paste0("https://opendata.samtrafiken.se/gtfs/", rkm, "/", rkm, ".zip?key=", trafiklab_key)

GET(url, write_disk(paste0(data_input, "/trafiklab_", rkm, "_", today, ".zip"), overwrite=TRUE))

unzip(paste0(data_input, "/trafiklab_", rkm, "_", today, ".zip"), exdir = paste0(data_input, "/trafiklab_", rkm))


##### DeSO
url_shp = paste0(folder_github, "/deso_2018_v2.zip")
download.file(url_shp, destfile = paste0(data_input, "/deso_2018_v2.zip"))
unzip(paste0(data_input, "/deso_2018_v2.zip"), exdir = data_input)

deso = st_read(paste0(data_input, "/DeSO_2018_v2.gpkg")) %>% 
  st_filter(lan == lan_kod)

# create kommun boundaries based on DeSO
kommun = deso %>% 
  filter(substr(kommun, 1, 2) == lan_kod) %>% 
  group_by(kommun, kommunnamn) %>% 
  summarize(sp_geometry = st_union(sp_geometry)) %>% 
  ungroup()


#---------------------------------------------------------------------------------------------------
# load data
#---------------------------------------------------------------------------------------------------

routes = read.csv2(paste0(data_input, "/trafiklab_", rkm, "/routes.txt"), 
                   sep = ",", encoding="UTF-8", stringsAsFactors=FALSE)

stops = read.csv2(paste0(data_input, "/trafiklab_", rkm, "/stops.txt"), 
                  sep = ",", encoding="UTF-8", stringsAsFactors=FALSE)

stop_times = read.csv2(paste0(data_input, "/trafiklab_", rkm, "/stop_times.txt"), 
                       sep = ",", encoding="UTF-8", stringsAsFactors=FALSE)

trips = read.csv2(paste0(data_input, "/trafiklab_", rkm, "/trips.txt"), 
                  sep = ",", encoding="UTF-8", stringsAsFactors=FALSE)

calendar_dates = read.csv2(paste0(data_input, "/trafiklab_", rkm, "/calendar_dates.txt"), 
                           sep = ",", encoding="UTF-8", stringsAsFactors=FALSE)

# linjenät koordinater
shapes = read.csv2(paste0(data_input, "/trafiklab_", rkm, "/shapes.txt"), 
                   sep = ",", encoding="UTF-8", stringsAsFactors=FALSE)

### Create filter variables

# service_id för rätt datum
service_id_inklud = calendar_dates %>% filter(date == today) %>% select(service_id) %>% pull()

# trips för rätt datum
trips_inklud = trips %>% 
  filter(service_id %in% service_id_inklud) %>% 
  select(trip_id) %>% pull()



kat = read.csv2(paste0(data_input, "/Linje_operator_final.csv")) %>% 
  mutate(Linje = as.character((Linje)))



#---------------------------------------------------------------------------------------------------
# Merge gtfs tables
#---------------------------------------------------------------------------------------------------

gtfs = stop_times %>%  
  left_join(., trips, by = "trip_id") %>%
  left_join(., stops, by = "stop_id") %>%
  left_join(., routes, by = "route_id") %>%
  mutate(hpl_id = substr(stop_id, 8, 13)) %>% 
  filter(trip_id %in% trips_inklud) %>%  # remove all rows referring to other dates
  distinct(arrival_time, departure_time, stop_id, .keep_all= TRUE) # remove duplicates

#---------------------------------------------------------------------------------------------------
# Datahantering hållplatser
#---------------------------------------------------------------------------------------------------


## Tidtabelldata är på hållplatslägenivå. Ta medel för att skapa en koordinat per hållplats
hpl_koord = gtfs %>% 
  group_by(hpl_id, stop_name) %>% 
  summarise(lat = round(mean(as.numeric(stop_lat)), 5), lon = round(mean(as.numeric(stop_lon)), 5)) 

# create SF object
spdf = hpl_koord[,c("lon", "lat")] 

# create spatial points
spdf <- SpatialPointsDataFrame(coords = xy_gtfs, data = hpl_koord) 

spdf1 = st_as_sf(spdf) %>% # convert to sf object
  st_set_crs(4326) %>% # set WGS84 as CRS
  st_transform(3006) %>%  # convert to SWEREF99 for intersect with shapefiles
  st_join(., kommun) %>% # intersect with kommun
  select(-kommun) %>% 
  filter(!is.na(kommunnamn) & # remove hållplatser outside länet
           kommunnamn %in% kommun_namn) # filter all hpl inside target kommun


#---------------------------------------------------------------------------------------------------
# Datahantering linjenät
#---------------------------------------------------------------------------------------------------

#### En linje kan följa olika vägsträckor. Här identifieras den vanligaste vägsträckan per linje

line_shapeid = gtfs %>% 
  group_by(route_short_name, shape_id) %>% 
  summarise(n = n()) %>% 
  filter(n == max(n)) %>% # vanligaste vägsträcka
  ungroup() %>% 
  select(-n)

shapeid_inklud = line_shapeid %>% select(shape_id) %>% pull()

# create SF object
xy_shapes = shapes %>%
  filter(shape_id %in% shapeid_inklud) %>% 
  select("shape_pt_lon", "shape_pt_lat") %>% 
  mutate_if(is.character,as.numeric)

# create spatial points
sp_shapes <- SpatialPointsDataFrame(coords = xy_shapes, 
                                    data = filter(shapes,shape_id %in% shapeid_inklud)) # must be same nrow as xy_shapes


# convert to sf object
sp_shapes1 = st_as_sf(sp_shapes) %>% 
  st_set_crs(4326) %>% # set WGS84 as CRS
  st_transform(3006) # convert to SWEREF99 for intersect with shapefiles

# skapa linjer från punktkoordinater
all_lines = sp_shapes1 %>% 
  group_by(shape_id) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") %>%
  # add linje number
  left_join(., line_shapeid, by = "shape_id") %>% 
  # add metadata per linje
  left_join(., kat, by = c("route_short_name" = "Linje"))



#---------------------------------------------------------------------------------------------------
# save file
#---------------------------------------------------------------------------------------------------

# Shapefil
st_write(all_lines, paste0(output, "/", "alla_linjer_", 
                           rkm, "_", Sys.Date(), ".shp"))

st_write(spdf1, paste0(output, "/", "hpl_", 
                           rkm, "_", Sys.Date(), ".shp"))










