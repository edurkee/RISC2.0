#install.packages("sf")
#install.packages("leaflet")
#install.packages("dbscan")
#install.packages("lwgeom")
#install.packages("terra")
#install.packages("readr")
#install.packages("dplyr")
#install.packages("purrr")
#install.packages("osmdata")
#install.packages("ggplot2")
#install.packages("lwgeom")
#install.packages("tidyr")

# # install.packages("mapview")
library(readr)
library(dplyr)
library(sf)
library(leaflet)
library(purrr)
library(dbscan)
library(osmdata)
library(lwgeom)
library(tidyr)
library(rsconnect)

# Load the data
download_fars_year <- function(year, dest_dir = "fars_raw") {
  dir.create(dest_dir, showWarnings = FALSE)

  url <- paste0(
    "https://static.nhtsa.gov/nhtsa/downloads/FARS/",
    year,
    "/National/FARS", year, "NationalCSV.zip"
  )

  zip_file <- file.path(dest_dir, paste0("fars_", year, ".zip"))
  year_dir <- file.path(dest_dir, as.character(year))

  if (!dir.exists(year_dir)) {
    message("Downloading FARS ", year)
    download.file(url, zip_file, mode = "wb")
    unzip(zip_file, exdir = year_dir)
  } else {
    message("FARS ", year, " already downloaded")
  }
}

years <- 2004:2023

purrr::walk(years, download_fars_year)

# Exploratory 2023
accident2023 <- read_csv(
  "fars_raw/2023/FARS2023NationalCSV/accident.csv",
  col_types = cols(.default = col_guess())
)

accident_il_2023 <- accident2023 %>%
  filter(STATE == 17)

accident_il_2023 <- accident_il_2023 %>%
  mutate(
    LATITUDE = as.numeric(LATITUDE),
    LONGITUD = as.numeric(LONGITUD)
  ) %>%
  filter(
    !is.na(LATITUDE),
    !is.na(LONGITUD)
  )

## filter out bad values
accident_il_2023 <- accident_il_2023 %>%
  filter(
    LATITUDE >= 36 & LATITUDE <= 43,
    LONGITUD >= -92 & LONGITUD <= -87
  )

## Make it Spatial
accident_sf_2023 <- st_as_sf(
  accident_il_2023,
  coords = c("LONGITUD", "LATITUDE"),
  crs = 4326,  # WGS84 (GPS coordinates)
  remove = FALSE
)

leaflet(accident_sf_2023) %>%
  addTiles() %>%
  addCircleMarkers(
    radius = 2,
    color = "blue",
    stroke = FALSE,
    fillOpacity = 0.8
  )

## Read in all traffic data for exposure calculations
aadt_url <- "https://gis1.dot.illinois.gov/arcgis/rest/services/AdministrativeData/AADT_Historical/FeatureServer/2023/query?where=1=1&outFields=*&f=geojson"

## read in once 2023 aadt data one time
roads_aadt_2023 <- st_read(aadt_url) 
saveRDS(roads_aadt_2023, "2023roads_aadt.rds")
roads_aadt_2023 <- readRDS("2023roads_aadt.rds")

# Give everything the same CRS for distance calculations
accident_2023_m <- st_transform(accident_sf_2023, 26916)
roads_aadt_2023_m <- st_transform(roads_aadt_2023, 26916)

# prepare each road segment with traffic vol, length, and total annual exposure
roads_aadt_2023_m <- roads_aadt_2023_m %>%
  filter(!is.na(AADT)) %>%
  mutate(
    segment_id = row_number(),
    length_m = as.numeric(st_length(geometry)),
    annual_vmt = AADT * length_m * 365
  )

  nearest_seg <- st_nearest_feature(accident_2023_m, roads_aadt_2023_m)

accident_2023_m$segment_id <- roads_aadt_2023_m$segment_id[nearest_seg]

# aggregate fatalities by segment
crashes_by_segment_2023 <- accident_2023_m %>%
  group_by(segment_id) %>%
  summarize(
    fatalities = sum(FATALS),
    geometry = st_union(geometry)
  )

# Join crash counts onto road segments
roads_with_crashes <- roads_aadt_2023_m %>%
  left_join(
    crashes_by_segment_2023 %>% st_drop_geometry(),
    by = "segment_id"
  ) %>%
  mutate(
    fatalities = replace_na(fatalities, 0),
    fatality_rate = (fatalities / annual_vmt) * 100000
  )

hotspots <- roads_with_crashes %>%
  filter(fatalities > 0, AADT > 0, is.finite(fatality_rate)) %>%
  select(fatalities, AADT, fatality_rate, geometry)

# Drop Z/M coords (ArcGIS includes NA elevation/measure values that break leaflet)
hotspots <- st_zm(hotspots, drop = TRUE, what = "ZM")

# Transform back to WGS84 for leaflet
hotspots_wgs <- st_transform(hotspots, 4326)

# Precompute colors and popups as plain vectors
rate_range <- range(hotspots_wgs$fatality_rate, na.rm = TRUE)
pal <- colorNumeric("YlOrRd", domain = rate_range)

line_colors <- pal(hotspots_wgs$fatality_rate)
line_popups <- paste0(
  "Fatalities: ", hotspots_wgs$fatalities, "<br>",
  "AADT: ", hotspots_wgs$AADT, "<br>",
  "Rate per 100k: ", round(hotspots_wgs$fatality_rate, 4)
)

leaflet() %>%
  addTiles() %>%
  addPolylines(
    data = hotspots_wgs,
    color = line_colors,
    weight = 3,
    opacity = 0.8,
    popup = line_popups
  ) %>%
  addLegend("bottomright", pal = pal, values = rate_range,
            title = "Fatality Rate<br>(per 100k vehicles)")

### Load all years of FARS data for Illinois

load_fars_il <- function(year) {
  # Find the accident CSV regardless of path/casing differences
  acc_file <- list.files(
    file.path("fars_raw", year),
    pattern = "^accident\\.csv$",
    recursive = TRUE,
    ignore.case = TRUE,
    full.names = TRUE
  )
  if (length(acc_file) == 0) {
    message("No accident file found for ", year)
    return(NULL)
  }
  df <- read_csv(acc_file[1], col_types = cols(.default = col_guess()))

  # Normalize column names to uppercase (2004-2005 use lowercase)
  names(df) <- toupper(names(df))
  df %>%
    filter(STATE == 17) %>%
    mutate(
      LATITUDE = as.numeric(LATITUDE),
      LONGITUD = as.numeric(LONGITUD),
      year = as.integer(year)
    ) %>%
    filter(
      !is.na(LATITUDE), !is.na(LONGITUD),
      LATITUDE >= 36, LATITUDE <= 43,
      LONGITUD >= -92, LONGITUD <= -87
    ) %>%
    mutate(PEDS = if ("PEDS" %in% names(.)) as.integer(PEDS) else 0L) %>%
    select(ST_CASE, STATE, COUNTY, LATITUDE, LONGITUD, FATALS, PEDS, year)
}

load_fars_vehicles <- function(year) {
  veh_file <- list.files(
    file.path("fars_raw", year),
    pattern = "^vehicle\\.csv$",
    recursive = TRUE,
    ignore.case = TRUE,
    full.names = TRUE
  )
  if (length(veh_file) == 0) return(NULL)
  df <- read_csv(veh_file[1], col_types = cols(.default = col_guess()))
  names(df) <- toupper(names(df))
  if (!"BODY_TYP" %in% names(df)) return(NULL)
  df %>%
    filter(STATE == 17) %>%
    mutate(year = as.integer(year), ST_CASE = as.numeric(ST_CASE)) %>%
    select(ST_CASE, year, BODY_TYP)
}

all_accidents_il <- purrr::map_dfr(years, load_fars_il)

# Classify each crash as pedestrian, motorcycle, or car
all_vehicles_il <- purrr::map_dfr(years, load_fars_vehicles)

motorcycle_cases <- all_vehicles_il %>%
  filter(BODY_TYP >= 20, BODY_TYP <= 29) %>%
  select(ST_CASE, year) %>%
  distinct() %>%
  mutate(is_motorcycle = TRUE)

all_accidents_il <- all_accidents_il %>%
  mutate(ST_CASE = as.numeric(ST_CASE)) %>%
  left_join(motorcycle_cases, by = c("ST_CASE", "year")) %>%
  mutate(
    is_motorcycle = replace_na(is_motorcycle, FALSE),
    crash_type = case_when(
      PEDS > 0       ~ "pedestrian",
      is_motorcycle  ~ "motorcycle",
      TRUE           ~ "car"
    )
  )

# Make spatial and transform to meters
all_accidents_sf <- st_as_sf(
  all_accidents_il,
  coords = c("LONGITUD", "LATITUDE"),
  crs = 4326,
  remove = FALSE
) %>%
  st_transform(26916)

# Snap to nearest road segment
all_nearest_seg <- st_nearest_feature(all_accidents_sf, roads_aadt_2023_m)
all_accidents_sf$segment_id <- roads_aadt_2023_m$segment_id[all_nearest_seg]

### DBSCAN clustering on all years

# Snap crash points to nearest road before clustering
all_snapped <- st_nearest_points(all_accidents_sf, roads_aadt_2023_m[all_nearest_seg, ], pairwise = TRUE)
all_snapped_coords <- st_coordinates(st_cast(all_snapped, "POINT"))
all_snap_pts <- all_snapped_coords[seq(2, nrow(all_snapped_coords), by = 2), 1:2]

# eps = 300m radius, minPts = 4 crashes to form a cluster 
db_all <- dbscan(all_snap_pts, eps = 300, minPts = 4)
all_accidents_sf$cluster <- db_all$cluster

clustered_all <- all_accidents_sf %>%
  filter(cluster > 0)

# Get AADT for each crash
clustered_all <- clustered_all %>%
  left_join(
    roads_aadt_2023_m %>% st_drop_geometry() %>% select(segment_id, AADT),
    by = "segment_id"
  )

# Summarize each cluster
cluster_summary_all <- clustered_all %>%
  group_by(cluster) %>%
  summarize(
    fatalities = sum(FATALS),
    n_crashes = n(),
    n_years = n_distinct(year),
    year_span = max(year) - min(year) + 1L,
    year_range = paste(min(year), max(year), sep = "-"),
    avg_aadt = mean(AADT, na.rm = TRUE),
    geometry = st_union(geometry)
  ) %>%
  mutate(
    exposure = avg_aadt * 365 * year_span,
    rate_per_100k = (fatalities / exposure) * 100000,
    centroid = st_centroid(geometry)
  )

# What share of IL fatalities do these clusters account for?
total_il_fatalities <- sum(all_accidents_il$FATALS)
cluster_fatalities <- sum(cluster_summary_all$fatalities)
pct <- round(cluster_fatalities / total_il_fatalities * 100, 1)
message(
  cluster_fatalities, " of ", total_il_fatalities,
  " IL fatalities (", pct, "%) fall within ", nrow(cluster_summary_all), " clusters"
)

cluster_pts_all <- st_set_geometry(cluster_summary_all, "centroid") %>%
  st_transform(4326) %>%
  filter(is.finite(rate_per_100k))

# Map all-years clusters
pal_all <- colorNumeric(c("#ff7676", "#440000"), domain = cluster_pts_all$rate_per_100k)

leaflet(cluster_pts_all) %>%
  addTiles() %>%
  addCircleMarkers(
    radius = ~6,
    color = ~pal_all(rate_per_100k),
    fillOpacity = 0.8,
    stroke = TRUE,
    weight = 1,
    popup = ~paste0(
      "<b>Cluster ", cluster, "</b><br>",
      "Fatalities: ", fatalities, "<br>",
      "Crashes: ", n_crashes, "<br>",
      "Rate per 100k: ", round(rate_per_100k, 4), "<br>",
      "Years active: ", year_range, " (", n_years, " yrs)<br>",
      "Avg AADT: ", round(avg_aadt)
    )
  ) %>%
  addLegend("bottomright", pal = pal_all, values = ~rate_per_100k,
            title = "Fatalities per<br>100k Vehicles<br>(2004-2023)") %>%
  addControl(
    html = paste0(
      "<div style='background:white; padding:8px 12px; border-radius:4px; ",
      "border:1px solid #ccc; font-size:13px;'>",
      "<b>", pct, "%</b> of IL road fatalities (", cluster_fatalities,
      " of ", total_il_fatalities, ")<br>occur in <b>",
      nrow(cluster_summary_all), "</b> clusters",
      "</div>"
    ),
    position = "topleft"
  )

### Save precomputed data for Shiny app
fatalities_by_type <- all_accidents_il %>%
  group_by(crash_type) %>%
  summarize(fatalities = sum(FATALS), .groups = "drop")

saveRDS(
  list(
    snap_pts = all_snap_pts,
    fatals = all_accidents_sf$FATALS,
    years = all_accidents_sf$year,
    segment_ids = all_accidents_sf$segment_id,
    crash_type = all_accidents_sf$crash_type,
    aadt_lookup = roads_aadt_2023_m %>% st_drop_geometry() %>% select(segment_id, AADT),
    total_il_fatalities = total_il_fatalities,
    fatalities_by_type = fatalities_by_type
  ),
  "cluster_app_data.rds"
)
