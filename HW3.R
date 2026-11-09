# Homework 3: Reproducing Plots and Exploring Real Spatial Data

# 0. Setup & Package Loading
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(sf)

# Load full datasets from packages
vehicles <- ggplot2::mpg
gems <- ggplot2::diamonds


# PROBLEM 1: Reproduce and Explain Plots (25 Points)

# Part A (8 points)

# 1. Reproduce scatter plot faceted by drive type (6 points)
p1a <- ggplot(vehicles, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv), alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~ drv) +
  scale_color_discrete(name = "drv") +
  labs(
    x = "Engine displacement (litres)",
    y = "Highway fuel economy (mpg)"
  ) +
  theme_bw()

print(p1a)

# 2. Explanation (2 points):
# Why are there three fitted lines even though drv is mapped to color only in geom_point()?
#
# Explanation:
# The global aesthetic aes(x = displ, y = hwy) defined in ggplot() sets up the 
# baseline axes for every layer in the plot. When facet_wrap(~ drv) is added, 
# ggplot physically splits the underlying dataset into three separate subsets (panels) 
# based on drive type (drv). 
# 
# Because geom_smooth(method = "lm") runs on whatever data exists within each panel, 
# it fits an independent linear regression line for each subset separately. The faceting 
# itself partitions the data, which forces geom_smooth() to generate three individual lines.


# Part B (8 points)

# 1. Reproduce absolute count stacked bar chart (5 points - Plot 1)
p1b_count <- ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar() +
  labs(
    x = "drv",
    y = "count",
    fill = "factor(cyl)"
  ) +
  theme_bw()

print(p1b_count)

# 2. Reproduce proportional stacked bar chart (5 points - Plot 2)
p1b_prop <- ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar(position = "fill") +
  labs(
    x = "drv",
    y = "Proportion within drive type",
    fill = "factor(cyl)"
  ) +
  theme_bw()

print(p1b_prop)

# 3. Explanation (3 points):
# Why equal total heights in the right plot do NOT mean drive types have equal sample sizes.
#
# Explanation:
# Using position = "fill" standardizes the total height of every bar to exactly 1.00 (100%). 
# This shifts the y-axis from raw counts to relative conditional proportions WITHIN each drive type category. 
# Equal bar heights mean each bar represents 100% of its own group, not that the groups are equal in size. 
# As shown in the absolute count plot (p1b_count), front-wheel drive ('f') and 4-wheel drive ('4') 
# have over 100 cars each, whereas rear-wheel drive ('r') has only 25 cars total.


# Part C (9 points)

# Filter dataset to create the smaller working table (carat <= 3.0)
gems_subset <- gems %>% filter(carat <= 3.0)

# 1. Create count histograms with fine (0.01) and coarse (0.5) bin widths (5 points)
p_fine <- ggplot(gems_subset, aes(x = carat)) +
  geom_histogram(
    binwidth = 0.01, 
    boundary = 0, 
    closed = "right", 
    fill = "grey40", 
    color = "black"
  ) +
  scale_x_continuous(limits = c(0, 3.2), breaks = seq(0, 3, by = 0.5)) +
  labs(
    title = "Carat Distribution (Bin Width = 0.01)",
    x = "Carat",
    y = "Count"
  ) +
  theme_bw()

p_coarse <- ggplot(gems_subset, aes(x = carat)) +
  geom_histogram(
    binwidth = 0.5, 
    boundary = 0, 
    closed = "right", 
    fill = "grey40", 
    color = "black"
  ) +
  scale_x_continuous(limits = c(0, 3.2), breaks = seq(0, 3, by = 0.5)) +
  labs(
    title = "Carat Distribution (Bin Width = 0.5)",
    x = "Carat",
    y = "Count"
  ) +
  theme_bw()

print(p_fine)
print(p_coarse)

# Report retained and excluded record counts
total_records <- nrow(gems)
retained_records <- nrow(gems_subset)
excluded_records <- total_records - retained_records

cat("PART C RECORD SUMMARY:
")
cat("Total records in original dataset:", total_records, "
")
cat("Retained records (carat <= 3.0):  ", retained_records, "
")
cat("Excluded records:                ", excluded_records, "

")

# 2. Display exact count of diamonds recorded as 0.99 and 1.00 carat (2 points)
exact_counts <- gems %>%
  filter(carat %in% c(0.99, 1.00)) %>%
  group_by(carat) %>%
  summarise(count = n())

print(exact_counts)

# 3. Description of feature visible with narrower bins (2 points, max 4 sentences):
#
# Description:
# Narrower binning (0.01) reveals distinct spikes at popular round numbers like 0.70, 1.00, 
# and 1.50 carats, reflecting strong commercial incentives for cutters to hit key weight milestones. 
# It also highlights sharp drops immediately below these thresholds—such as only 23 diamonds 
# at 0.99 carats compared to 1,558 at 1.00 carat. Coarse binning (0.5) completely smooths over 
# these human trading preferences and market anomalies.


# PROBLEM 2: Real Spatial Data & Buffers (25 Points)
# Region: Chicago, IL | Facilities: Public Libraries | Boundaries: Community Areas

# Data Provenance & Documentation
data_sources <- data.frame(
  dataset_name = c("Chicago Public Libraries", "Chicago Community Area Boundaries"),
  publisher = c("City of Chicago Open Data Portal", "City of Chicago Open Data Portal"),
  source_url = c(
    "https://data.cityofchicago.org/resource/x8fc-82d6.csv?$limit=5000",
    "https://data.cityofchicago.org/resource/igwz-waq9.geojson?$limit=5000"
  ),
  download_date = Sys.Date(),
  crs = "EPSG:32616 (UTM Zone 16N - meters)",
  description = "Official City of Chicago open data records containing public library locations and community area boundaries."
)

write.csv(data_sources, "data_sources.csv", row.names = FALSE)

# Download & Data Loading from Chicago Open Data Portal
lib_url <- "https://data.cityofchicago.org/resource/x8fc-82d6.csv?$limit=5000"
bnd_url <- "https://data.cityofchicago.org/resource/igwz-waq9.geojson?$limit=5000"

# Fetch raw libraries dataset
if (!file.exists("raw_libraries.csv")) {
  download.file(lib_url, destfile = "raw_libraries.csv", quiet = TRUE)
}
raw_sites <- read_csv("raw_libraries.csv", show_col_types = FALSE)

# Standardize coordinate column names
if ("location_location" %in% names(raw_sites)) {
  # Extract lat/lon from location struct if formatted as string
  raw_sites <- raw_sites %>%
    mutate(
      latitude = as.numeric(sub(".*\(([^,]+),.*", "\1", location_location)),
      longitude = as.numeric(sub(".*,\s*([^)]+)\).*", "\1", location_location))
    )
} else if ("location" %in% names(raw_sites)) {
  raw_sites <- raw_sites %>%
    mutate(
      latitude = as.numeric(sub(".*\(([^,]+),.*", "\1", location)),
      longitude = as.numeric(sub(".*,\s*([^)]+)\).*", "\1", location))
    )
}

# Ensure unique site ID
if (!"id" %in% names(raw_sites)) {
  raw_sites <- raw_sites %>% mutate(id = row_number())
}

n_before <- nrow(raw_sites)

# Clean locations: retain valid coordinates, preserve locations sharing identical coordinates
clean_sites <- raw_sites %>%
  filter(!is.na(longitude) & !is.na(latitude)) %>%
  filter(longitude >= -180 & longitude <= 180 & latitude >= -90 & latitude <= 90) %>%
  distinct(id, .keep_all = TRUE)

n_after <- nrow(clean_sites)

cat("PROBLEM 2 DATA CLEANING SUMMARY:
")
cat("Raw records count:            ", n_before, "
")
cat("Clean records count retained: ", n_after, "
")
cat("Removed (invalid/missing):    ", n_before - n_after, "

")

# Convert to sf point object & project to local metric CRS (EPSG:32616 - UTM Zone 16N, meters)
sites_sf <- st_as_sf(clean_sites, coords = c("longitude", "latitude"), crs = 4326)
sites_m <- st_transform(sites_sf, crs = 32616)

# Buffers & Area Calculations
buf_500m <- st_buffer(sites_m, dist = 500)
buf_1000m <- st_buffer(sites_m, dist = 1000)

# Dissolve overlapping buffers to prevent double-counting area
union_500m <- st_union(buf_500m)
union_1000m <- st_union(buf_1000m)

# Calculate union area in square kilometers
area_500m_km2 <- as.numeric(st_area(union_500m)) / 1e6
area_1000m_km2 <- as.numeric(st_area(union_1000m)) / 1e6

# Output buffer summary table required by assignment
buffer_summary <- data.frame(
  radius_m = c(500, 1000),
  union_area_km2 = round(c(area_500m_km2, area_1000m_km2), 4)
)

cat("BUFFER SUMMARY TABLE:
")
print(buffer_summary)

# Check constraint: 1,000-meter buffer area must be at least as large as 500-meter area
stopifnot(area_1000m_km2 >= area_500m_km2)

# Spatial Maps (p_sites & p_buffers)
p_sites <- ggplot() +
  geom_sf(data = sites_m, color = "navy", size = 2, alpha = 0.8) +
  labs(
    title = "Public Library Locations in Chicago Study Area",
    subtitle = paste("Total Retained Facilities: n =", nrow(sites_m)),
    caption = "Source: City of Chicago Open Data Portal | Projected CRS: EPSG:32616 (meters)"
  ) +
  theme_bw()

p_buffers <- ggplot() +
  geom_sf(data = union_500m, fill = "lightblue", color = "royalblue", alpha = 0.5) +
  geom_sf(data = sites_m, color = "darkred", size = 1.5) +
  labs(
    title = "Public Libraries & Combined 500-Meter Service Buffers",
    subtitle = "Dissolved buffer polygon counting overlapping service area once",
    caption = "Source: City of Chicago Open Data Portal | Projected CRS: EPSG:32616 (meters)"
  ) +
  theme_bw()

print(p_sites)
print(p_buffers)


# PROBLEM 3: Spatial Joins, District Counts & Density Maps (25 Points)

# District Boundaries Setup from Chicago Open Data Portal
if (!file.exists("community_areas.geojson")) {
  download.file(bnd_url, destfile = "community_areas.geojson", quiet = TRUE)
}
districts_raw <- st_read("community_areas.geojson", quiet = TRUE)

districts_sf <- districts_raw %>%
  st_transform(crs = 32616) %>%
  mutate(
    district_id = if ("community" %in% names(.)) community else row_number(),
    district_name = if ("area_numbe" %in% names(.)) area_numbe else paste("Area", row_number()),
    area_km2 = as.numeric(st_area(geometry)) / 1e6
  )

stopifnot(all(st_is_valid(districts_sf)))
stopifnot(all(districts_sf$area_km2 > 0))

# Spatial Join & Match Audit
matches <- st_join(sites_m, districts_sf, join = st_intersects)

site_match_audit <- matches %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  summarise(
    n_matches = sum(!is.na(district_id)),
    matched_district_id = ifelse(n_matches == 1, first(district_id), NA)
  )

write.csv(site_match_audit, "site_match_audit.csv", row.names = FALSE)

n_zero <- sum(site_match_audit$n_matches == 0)
n_single <- sum(site_match_audit$n_matches == 1)
n_multi <- sum(site_match_audit$n_matches > 1)

cat("MATCH AUDIT SUMMARY:
")
cat("Sites matching exactly 0 areas: ", n_zero, "
")
cat("Sites matching exactly 1 area:  ", n_single, "
")
cat("Sites matching >1 areas:       ", n_multi, "

")

# Aggregation & District Counts Output
single_match_sites <- matches %>%
  filter(!is.na(district_id)) %>%
  group_by(id) %>%
  filter(n() == 1) %>%
  ungroup()

district_site_counts <- single_match_sites %>%
  st_drop_geometry() %>%
  group_by(district_id) %>%
  summarise(n_sites = n())

district_counts <- districts_sf %>%
  left_join(district_site_counts, by = "district_id") %>%
  mutate(
    n_sites = ifelse(is.na(n_sites), 0, n_sites),
    sites_per_km2 = round(n_sites / area_km2, 4)
  )

stopifnot(sum(district_counts$n_sites) == n_single)

cat("DISTRICT COUNTS TABLE:
")
print(st_drop_geometry(district_counts))

# Comparison Choropleth Maps (p_count & p_density)
p_count <- ggplot(district_counts) +
  geom_sf(aes(fill = n_sites), color = "white", size = 0.3) +
  scale_fill_viridis_c(
    name = "Facility Count", 
    option = "magma",
    labels = scales::number_format(accuracy = 1)
  ) +
  labs(
    title = "Public Library Count by Community Area",
    subtitle = paste("Unassigned sites outside boundaries:", n_zero),
    caption = "Source: City of Chicago Open Data Portal | Projected CRS: EPSG:32616"
  ) +
  theme_bw()

p_density <- ggplot(district_counts) +
  geom_sf(aes(fill = sites_per_km2), color = "white", size = 0.3) +
  scale_fill_viridis_c(
    name = "Sites / km²", 
    option = "viridis"
  ) +
  labs(
    title = "Public Library Density (Facilities per km²) by Community Area",
    subtitle = "Standardized spatial distribution controlling for area size",
    caption = "Source: City of Chicago Open Data Portal | Projected CRS: EPSG:32616"
  ) +
  theme_bw()

print(p_count)
print(p_density)
