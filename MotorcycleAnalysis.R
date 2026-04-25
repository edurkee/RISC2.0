library(readr)
library(dplyr)
library(sf)
library(purrr)
library(lwgeom)

years <- 2004:2023

# Load already-cached road data (run Analysis.R at least once first)
roads_aadt_2023_m <- readRDS("2023roads_aadt.rds") %>%
  st_transform(26916) %>%
  filter(!is.na(AADT)) %>%
  mutate(
    segment_id = row_number(),
    length_m   = as.numeric(st_length(geometry))
  )

# Load accident CSVs (already downloaded to fars_raw/)
load_moto_accidents <- function(year) {
  acc_file <- list.files(
    file.path("fars_raw", year),
    pattern = "^accident\\.csv$",
    recursive = TRUE, ignore.case = TRUE, full.names = TRUE
  )
  per_file <- list.files(
    file.path("fars_raw", year),
    pattern = "^person\\.csv$",
    recursive = TRUE, ignore.case = TRUE, full.names = TRUE
  )
  if (length(acc_file) == 0 || length(per_file) == 0) return(NULL)

  acc <- read_csv(acc_file[1], col_types = cols(.default = col_guess()))
  per <- read_csv(per_file[1], col_types = cols(.default = col_guess()))
  names(acc) <- toupper(names(acc))
  names(per) <- toupper(names(per))

  if (!all(c("PER_TYP", "INJ_SEV") %in% names(per))) return(NULL)

  # Crashes where a motorcyclist (PER_TYP 6/7) was fatally injured (INJ_SEV 4)
  moto_cases <- per %>%
    filter(STATE == 17, PER_TYP %in% c(6, 7), INJ_SEV == 4) %>%
    mutate(ST_CASE = as.numeric(ST_CASE)) %>%
    select(ST_CASE) %>%
    distinct()

  acc %>%
    filter(STATE == 17) %>%
    mutate(
      LATITUDE = as.numeric(LATITUDE),
      LONGITUD = as.numeric(LONGITUD),
      ST_CASE  = as.numeric(ST_CASE),
      year     = as.integer(year)
    ) %>%
    filter(
      !is.na(LATITUDE), !is.na(LONGITUD),
      LATITUDE >= 36, LATITUDE <= 43,
      LONGITUD >= -92, LONGITUD <= -87
    ) %>%
    inner_join(moto_cases, by = "ST_CASE") %>%
    select(ST_CASE, LATITUDE, LONGITUD, FATALS, year)
}

message("Loading motorcycle crashes...")
moto_il <- purrr::map_dfr(years, load_moto_accidents)
message(nrow(moto_il), " motorcycle fatal crashes loaded")

# Make spatial and snap to nearest road segment
moto_sf <- st_as_sf(moto_il, coords = c("LONGITUD", "LATITUDE"), crs = 4326) %>%
  st_transform(26916)

nearest_seg <- st_nearest_feature(moto_sf, roads_aadt_2023_m)
moto_sf$segment_id <- roads_aadt_2023_m$segment_id[nearest_seg]

# Aggregate fatalities by road segment
moto_by_segment <- moto_sf %>%
  st_drop_geometry() %>%
  group_by(segment_id) %>%
  summarize(
    fatalities = sum(FATALS),
    n_crashes  = n(),
    n_years    = n_distinct(year),
    year_span  = max(year) - min(year) + 1L,
    year_range = paste(min(year), max(year), sep = "-"),
    .groups    = "drop"
  )

# Join back to road geometries with AADT
moto_roads_sf <- roads_aadt_2023_m %>%
  select(segment_id, AADT, geometry) %>%
  inner_join(moto_by_segment, by = "segment_id") %>%
  mutate(
    exposure      = AADT * 365 * year_span,
    rate_per_100k = (fatalities / exposure) * 100000,
    length_km     = as.numeric(st_length(geometry)) / 1000
  ) %>%
  filter(is.finite(rate_per_100k), rate_per_100k > 0) %>%
  st_transform(4326) %>%
  st_zm(drop = TRUE, what = "ZM")

total_moto_fatalities <- sum(moto_il$FATALS)
message(total_moto_fatalities, " total motorcycle fatalities across ",
        nrow(moto_roads_sf), " road segments")

saveRDS(
  list(roads = moto_roads_sf, total_fatalities = total_moto_fatalities),
  "moto_road_data.rds"
)
message("Saved moto_road_data.rds")
