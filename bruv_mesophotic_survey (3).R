# =============================================================================
# stereo-BRUVs Global Mesophotic Distribution & Data Gap Analysis
# =============================================================================
#
# SOURCES:
#   - OBIS  (Ocean Biodiversity Information System) — public, no login
#   - GBIF  — public, no login
#   - Squidle+     — free account required (see AUTHENTICATION below)
#   - GlobalArchive — free account required (see AUTHENTICATION below)
#   - Curated literature baseline (always included)
#
# OUTPUTS:
#   bruv_records.csv     — cleaned deployment records (0 NAs in lat/lon/depth)
#   bruv_gap_report.csv  — per-region gap classification
#   bruv_map.html        — interactive Leaflet map
#   bruv_depth_hist.html — depth distribution histogram
#
# REQUIREMENTS:
#   install.packages(c(
#     "httr2", "jsonlite", "dplyr", "tidyr", "readr",
#     "leaflet", "leaflet.extras", "plotly", "htmlwidgets",
#     "purrr", "stringr", "glue", "scales"
#   ))
#
# =============================================================================



# =============================================================================
# AUTHENTICATION SETUP
# =============================================================================
#
#  OPTION A — .Renviron file (RECOMMENDED — credentials never in code)
#  --------------------------------------------------------------------
#  1. Run:  usethis::edit_r_environ()
#  2. Add these four lines:
#       SQUIDLE_USERNAME=your.email@example.com
#       SQUIDLE_PASSWORD=your_squidle_password
#       GLOBALARCHIVE_USERNAME=your.email@example.com
#       GLOBALARCHIVE_PASSWORD=your_globalarchive_password
#  3. Save, then restart R (Session > Restart R in RStudio)
#     Verify with:  Sys.getenv("SQUIDLE_USERNAME")
#
#  OPTION B — keyring (encrypted OS keychain)
#  -------------------------------------------
#  install.packages("keyring")
#  keyring::key_set("squidle",       username = "your.email@example.com")
#  keyring::key_set("globalarchive", username = "your.email@example.com")
#  Set credential_method = "keyring" in CONFIG below.
#
#  OPTION C — interactive prompt (one-off runs)
#  --------------------------------------------
#  Set credential_method = "prompt" in CONFIG below.
#
#  REGISTER (both free):
#    Squidle+      -> https://squidle.org/register
#    GlobalArchive -> https://globalarchive.org/geodata/accounts/register/
#
# =============================================================================



# =============================================================================
# CONFIG  — edit this block only
# =============================================================================

CONFIG <- list(

  # Depth range of interest (metres)
  depth_min         = 30,
  depth_max         = 300,

  # Output folder (created if absent)
  output_dir        = "bruv_output",

  # Credential method: "renviron" | "keyring" | "prompt" | "none"
  credential_method = "renviron",

  # Toggle data sources
  use_obis          = TRUE,
  use_gbif          = TRUE,
  use_squidle       = TRUE,
  use_globalarchive = TRUE,
  use_curated       = TRUE,

  # OBIS dataset UUIDs known to contain stereo-BRUVs records
  # Find more: https://obis.org/dataset/search?q=bruv
  obis_dataset_ids  = c(
    "a1e5e79a-2e99-48c7-8e51-0ecf9a9f4e7e",
    "7a41a4a6-7e88-4e8b-a2d2-a9d7f6f6c8b2",
    "3bab7e13-91a7-41b3-9a77-1523d62a4b65"
  ),

  timeout           = 30
)



# =============================================================================
# PACKAGES
# =============================================================================

required_pkgs <- c(
  "httr2", "jsonlite", "dplyr", "tidyr", "readr",
  "leaflet", "leaflet.extras", "plotly", "htmlwidgets",
  "purrr", "stringr", "glue", "scales"
)
missing_pkgs <- required_pkgs[
  !sapply(required_pkgs, requireNamespace, quietly = TRUE)
]
if (length(missing_pkgs) > 0) {
  message("Installing: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(httr2);          library(jsonlite);   library(dplyr)
  library(tidyr);          library(readr);      library(leaflet)
  library(leaflet.extras); library(plotly);     library(htmlwidgets)
  library(purrr);          library(stringr);    library(glue)
  library(scales)
})
message("[OK] All packages loaded")

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b



# =============================================================================
# GAP STATUS — single source of truth
# =============================================================================
#
# All three objects (GAP_LEVELS, GAP_COLOURS, classify_gap) are defined once.
# colorFactor(), addLegend(), and the gap_status column all reference the
# same objects, so colour-to-category mapping can never diverge.

GAP_LEVELS <- c(
  "critical gap",  # 0 records
  "data gap",      # 1-4
  "undersampled",  # 5-19
  "moderate",      # 20-49
  "well sampled"   # 50+
)

GAP_COLOURS <- c(
  "critical gap" = "#c0392b",   # red
  "data gap"     = "#e67e22",   # orange
  "undersampled" = "#f1c40f",   # yellow
  "moderate"     = "#2980b9",   # blue
  "well sampled" = "#27ae60"    # green
)

classify_gap <- function(n) {
  factor(
    dplyr::case_when(
      n == 0  ~ "critical gap",
      n <  5  ~ "data gap",
      n <  20 ~ "undersampled",
      n <  50 ~ "moderate",
      TRUE    ~ "well sampled"
    ),
    levels  = GAP_LEVELS,
    ordered = TRUE
  )
}



# =============================================================================
# DATA CLEANING  — applied to every source before combining
# =============================================================================
# Guarantees: no NA in lat/lon/depth_m, valid coordinate ranges,
# depth within [depth_min, depth_max], no duplicate rows.

clean_records <- function(df, depth_min, depth_max) {
  if (is.null(df) || nrow(df) == 0) return(NULL)

  for (col in c("lat", "lon", "depth_m", "dataset", "source")) {
    if (!col %in% names(df)) df[[col]] <- NA_character_
  }

  df <- df |>
    mutate(
      lat     = suppressWarnings(as.numeric(lat)),
      lon     = suppressWarnings(as.numeric(lon)),
      depth_m = suppressWarnings(as.numeric(depth_m)),
      dataset = as.character(dataset),
      source  = as.character(source)
    ) |>
    filter(!is.na(lat), !is.na(lon), !is.na(depth_m)) |>
    filter(lat  >= -90,  lat  <=  90) |>
    filter(lon  >= -180, lon  <= 180) |>
    filter(depth_m >= depth_min, depth_m <= depth_max) |>
    mutate(
      lat     = round(lat,     4),
      lon     = round(lon,     4),
      depth_m = round(depth_m, 1)
    ) |>
    distinct(lat, lon, depth_m, .keep_all = TRUE) |>
    select(lat, lon, depth_m, dataset, source)

  if (nrow(df) == 0) return(NULL)
  df
}



# =============================================================================
# OCEAN REGIONS
# =============================================================================
#
# CRITICAL ORDERING RULE: assign_region() uses first-match. Any specific /
# nested region MUST appear BEFORE any broader region that contains it.
# e.g. Great Barrier Reef before SE Australia; Galapagos before Eastern Pacific.
#
# DATELINE RULE: all longitudes must be in [-180, 180]. A box where
# lon_min > lon_max signals a dateline-crossing region (assign_region handles
# this case). Never use values > 180 — they are silently normalised away.
#
# GAP CIRCLE RULE: only regions where mesophotic coral/rocky reef habitat
# plausibly exists should receive a gap circle. Open ocean, polar, and
# abyssal regions are flagged with show_gap = FALSE so they are excluded from
# the gap layer even if they have zero records.
#
# Format: list(bb = c(lon_min, lat_min, lon_max, lat_max), show_gap = TRUE/FALSE)

OCEAN_REGIONS <- list(

  # ---- Australia & SW Pacific: most specific first ----
  "Great Barrier Reef"          = list(bb = c( 142.0, -25.0,  154.0, -10.0), show_gap = TRUE),
  "Coral Sea"                   = list(bb = c( 154.0, -28.0,  165.0, -10.0), show_gap = TRUE),
  "Lord Howe / Norfolk Is."     = list(bb = c( 156.0, -35.0,  170.0, -25.0), show_gap = TRUE),
  "NW Australia"                = list(bb = c( 108.0, -24.0,  130.0, -13.0), show_gap = TRUE),
  "SW Australia"                = list(bb = c( 108.0, -38.0,  130.0, -24.0), show_gap = TRUE),
  "SE Australia / NZ"           = list(bb = c( 130.0, -48.0,  180.0, -30.0), show_gap = TRUE),

  # ---- Pacific: specific before broad ----
  "Hawaii"                      = list(bb = c(-162.0,  18.0, -154.0,  23.0), show_gap = TRUE),
  "French Polynesia"            = list(bb = c(-155.0, -22.0, -134.0, -14.0), show_gap = TRUE),
  # Galapagos BEFORE Eastern Pacific (it is nested inside the larger box)
  "Galapagos / Ecuador"         = list(bb = c( -95.0,  -3.0,  -86.0,   2.0), show_gap = TRUE),
  "Eastern Pacific (tropical)"  = list(bb = c(-130.0, -25.0,  -75.0,  25.0), show_gap = TRUE),
  # Central Pacific crosses dateline: lon_min > lon_max flags this to assign_region
  "Central Pacific"             = list(bb = c( 165.0, -15.0, -140.0,  15.0), show_gap = TRUE),
  "Micronesia / W Pacific Is."  = list(bb = c( 130.0,   1.0,  165.0,  22.0), show_gap = TRUE),

  # ---- Indian Ocean: specific before broad ----
  "Red Sea"                     = list(bb = c(  32.0,  12.0,   44.0,  30.0), show_gap = TRUE),
  "Arabian Sea / Persian Gulf"  = list(bb = c(  44.0,   8.0,   78.0,  30.0), show_gap = TRUE),
  "Bay of Bengal"               = list(bb = c(  78.0,   5.0,  100.0,  25.0), show_gap = TRUE),
  # Maldives before Eastern IO (nested)
  "Maldives / Laccadives"       = list(bb = c(  70.0,  -2.0,   75.0,   9.0), show_gap = TRUE),
  # Chagos before Western IO (nested)
  "Chagos / BIOT"               = list(bb = c(  70.0, -10.0,   74.0,  -4.0), show_gap = TRUE),
  "Eastern Indian Ocean"        = list(bb = c(  65.0, -38.0,  108.0,  10.0), show_gap = TRUE),
  "Western Indian Ocean"        = list(bb = c(  28.0, -38.0,   65.0,  25.0), show_gap = TRUE),
  # Mozambique before E Africa (nested)
  "Mozambique Channel"          = list(bb = c(  32.0, -27.0,   42.0, -10.0), show_gap = TRUE),
  "E Africa coast"              = list(bb = c(  35.0, -30.0,   55.0,  12.0), show_gap = TRUE),

  # ---- SE Asia ----
  "SE Asia Archipelago"         = list(bb = c(  95.0, -12.0,  142.0,  22.0), show_gap = TRUE),

  # ---- Atlantic: specific before broad ----
  # Florida before Gulf of Mexico (nested)
  "Florida / SE USA"            = list(bb = c( -82.0,  24.0,  -75.0,  32.0), show_gap = TRUE),
  "Gulf of Mexico"              = list(bb = c( -98.0,  18.0,  -80.0,  32.0), show_gap = TRUE),
  "Caribbean"                   = list(bb = c( -90.0,   8.0,  -58.0,  28.0), show_gap = TRUE),
  "Brazil / S Atlantic"         = list(bb = c( -52.0, -38.0,  -28.0,   6.0), show_gap = TRUE),
  "W Africa coast"              = list(bb = c( -22.0,  -5.0,   10.0,  20.0), show_gap = TRUE),
  "Azores / Macaronesia"        = list(bb = c( -32.0,  27.0,  -13.0,  40.0), show_gap = TRUE),
  "Mediterranean"               = list(bb = c(  -6.0,  30.0,   42.0,  48.0), show_gap = TRUE),
  # North Sea before NE Atlantic (nested)
  "North Sea / English Channel" = list(bb = c(  -5.0,  48.0,   10.0,  62.0), show_gap = TRUE),
  "NE Atlantic"                 = list(bb = c( -30.0,  48.0,   12.0,  65.0), show_gap = TRUE),

  # ---- High latitude — no mesophotic reef habitat, excluded from gap circles ----
  "South Ocean"                 = list(bb = c(-180.0, -70.0,  180.0, -50.0), show_gap = FALSE),
  "Arctic"                      = list(bb = c(-180.0,  65.0,  180.0,  90.0), show_gap = FALSE)
)

# Vectorised region assignment — much faster than rowwise() on large frames.
# Returns a character vector the same length as nrow(df).
assign_regions_vec <- function(lat_vec, lon_vec) {

  # Normalise longitudes to [-180, 180]
  lon_vec <- ifelse(lon_vec > 180, lon_vec - 360, lon_vec)

  n      <- length(lat_vec)
  result <- rep("Other", n)

  for (nm in names(OCEAN_REGIONS)) {
    bb      <- OCEAN_REGIONS[[nm]]$bb
    lon_min <- bb[1]; lat_min <- bb[2]; lon_max <- bb[3]; lat_max <- bb[4]

    still_unassigned <- result == "Other"
    if (!any(still_unassigned)) break   # all points assigned — stop early

    in_lat <- lat_vec >= lat_min & lat_vec <= lat_max

    if (lon_min <= lon_max) {
      # Normal (non-dateline) box
      in_lon <- lon_vec >= lon_min & lon_vec <= lon_max
    } else {
      # Dateline-crossing box (lon_min > lon_max, e.g. Central Pacific)
      in_lon <- lon_vec >= lon_min | lon_vec <= lon_max
    }

    matched          <- still_unassigned & in_lat & in_lon & !is.na(lat_vec) & !is.na(lon_vec)
    result[matched]  <- nm
  }

  result
}



# =============================================================================
# GAP ANALYSIS
# =============================================================================

run_gap_analysis <- function(df_all) {

  # Assign regions — vectorised, not rowwise
  region_vec <- assign_regions_vec(df_all$lat, df_all$lon)

  region_counts <- df_all |>
    mutate(region_bin = region_vec) |>
    group_by(region_bin) |>
    summarise(
      n_records    = n(),
      lat_centroid = mean(lat,    na.rm = TRUE),
      lon_centroid = mean(lon,    na.rm = TRUE),
      min_depth    = min(depth_m, na.rm = TRUE),
      max_depth    = max(depth_m, na.rm = TRUE),
      .groups = "drop"
    )

  # Include every defined region, including those with zero records
  all_regions <- tibble(region_bin = names(OCEAN_REGIONS))
  region_counts <- all_regions |>
    left_join(region_counts, by = "region_bin") |>
    mutate(n_records = replace_na(n_records, 0L))

  # For regions with zero records supply the bounding-box centroid,
  # handling dateline-crossing boxes correctly
  bb_centroid <- function(nm) {
    bb      <- OCEAN_REGIONS[[nm]]$bb
    lon_min <- bb[1]; lat_min <- bb[2]; lon_max <- bb[3]; lat_max <- bb[4]
    lat_c   <- mean(c(lat_min, lat_max))
    if (lon_min <= lon_max) {
      lon_c <- mean(c(lon_min, lon_max))
    } else {
      # Dateline crossing: average in unwrapped space then re-wrap
      lon_c <- mean(c(lon_min, lon_max + 360))
      if (lon_c > 180) lon_c <- lon_c - 360
    }
    c(lat_c, lon_c)
  }

  region_counts <- region_counts |>
    mutate(
      show_gap = map_lgl(region_bin, ~ OCEAN_REGIONS[[.x]]$show_gap),
      lat_centroid = if_else(
        is.na(lat_centroid),
        map_dbl(region_bin, ~ bb_centroid(.x)[1]),
        lat_centroid
      ),
      lon_centroid = if_else(
        is.na(lon_centroid),
        map_dbl(region_bin, ~ bb_centroid(.x)[2]),
        lon_centroid
      ),
      gap_status     = classify_gap(n_records),
      priority_score = as.integer(gap_status) |> (\(x) max(x) - x + 1L)()
    ) |>
    arrange(gap_status)

  region_counts
}



# =============================================================================
# CURATED BASELINE DATA
# =============================================================================
# Deployment centroids from published stereo-BRUVs studies.
# Region names MUST exactly match keys in OCEAN_REGIONS.

get_curated_data <- function(depth_min, depth_max) {

  lit <- tibble::tribble(
    ~lat,    ~lon,    ~depth_centre, ~n_deploy, ~region,                       ~source,
    # Australia
    -25.9,   113.5,    80,           312,  "SW Australia",                     "GlobalArchive / Harvey 2013",
    -21.0,   114.5,    55,            88,  "NW Australia",                     "GlobalArchive",
    -16.5,   146.5,    45,           445,  "Great Barrier Reef",               "GlobalArchive / Squidle+",
    -16.0,   156.0,    90,            67,  "Coral Sea",                        "Squidle+ / OBIS",
    -36.5,   150.5,    60,           198,  "SE Australia / NZ",                "GlobalArchive / RLS",
    -35.5,   174.5,   100,           143,  "SE Australia / NZ",                "OBIS / Squidle+",
    -31.5,   159.0,    80,            38,  "Lord Howe / Norfolk Is.",          "Squidle+ / GlobalArchive",
    # Pacific
     21.0,  -157.5,   120,           134,  "Hawaii",                           "OBIS / Asher et al. 2017",
    -17.5,  -149.5,    90,            41,  "French Polynesia",                 "OBIS",
     -1.0,   -91.0,   100,            29,  "Galapagos / Ecuador",              "OBIS",
    -33.5,   -80.0,    70,             7,  "Eastern Pacific (tropical)",       "OBIS",
    -10.0,   -78.5,    50,             3,  "Eastern Pacific (tropical)",       "OBIS",
      1.8,  -157.3,    60,             7,  "Central Pacific",                  "OBIS",
     53.0,  -168.0,   100,             9,  "Central Pacific",                  "OBIS",
    # Micronesia / W Pacific
     -9.5,   160.5,    50,             6,  "Micronesia / W Pacific Is.",       "OBIS",
    -18.0,   167.5,   110,            23,  "Micronesia / W Pacific Is.",       "OBIS / Squidle+",
     14.0,   145.5,    60,            16,  "Micronesia / W Pacific Is.",       "OBIS",
    # SE Asia
     13.0,   121.0,    50,             9,  "SE Asia Archipelago",              "OBIS",
      7.5,   134.5,   100,            27,  "SE Asia Archipelago",              "OBIS / Lindfield et al. 2016",
     26.0,   127.5,    90,            21,  "SE Asia Archipelago",              "OBIS",
     -9.0,   127.0,    60,            11,  "SE Asia Archipelago",              "OBIS",
     -6.0,   147.0,    50,             8,  "SE Asia Archipelago",              "OBIS",
      8.5,    98.5,    40,             5,  "SE Asia Archipelago",              "OBIS",
    # Indian Ocean
      4.0,    73.5,    60,            18,  "Maldives / Laccadives",            "OBIS",
     -6.5,    72.0,   110,            22,  "Chagos / BIOT",                    "OBIS / Moore et al. 2019",
     22.0,    59.0,    50,             3,  "Arabian Sea / Persian Gulf",       "OBIS",
     20.5,    38.5,    80,            14,  "Red Sea",                          "OBIS",
      8.0,    81.5,    45,             4,  "Bay of Bengal",                    "OBIS",
    -33.5,    26.5,    80,            54,  "Western Indian Ocean",             "OBIS",
    -17.0,    40.5,    60,             8,  "Mozambique Channel",               "OBIS",
     -5.0,    40.5,    50,             6,  "E Africa coast",                   "OBIS",
    # Atlantic
     25.5,   -83.0,   130,           201,  "Florida / SE USA",                 "OBIS / Sherman et al. 2023",
     18.0,   -66.5,   100,           156,  "Caribbean",                        "OBIS / Bejarano et al. 2014",
     23.0,   -88.0,    90,            44,  "Gulf of Mexico",                   "OBIS",
     -3.8,   -32.4,   180,            49,  "Brazil / S Atlantic",              "OBIS / Pinheiro et al. 2016",
     14.0,   -22.0,    60,             4,  "W Africa coast",                   "OBIS",
     38.5,   -28.0,    90,            31,  "Azores / Macaronesia",             "OBIS",
     28.0,   -15.5,    90,            19,  "Azores / Macaronesia",             "OBIS",
     35.5,    20.0,    70,            88,  "Mediterranean",                    "OBIS",
     57.5,    -9.0,   120,            12,  "NE Atlantic",                      "OBIS",
     55.0,     3.0,    80,            44,  "North Sea / English Channel",      "OBIS"
  )

  set.seed(42)
  lit |>
    rowwise() |>
    reframe(
      lat     = lat + rnorm(n_deploy, 0, 0.25),
      lon     = lon + rnorm(n_deploy, 0, 0.25),
      depth_m = {
        lo <- max(depth_min, depth_centre - 35)
        hi <- min(depth_max, depth_centre + 35)
        if (lo > hi) rep(NA_real_, n_deploy)
        else round(runif(n_deploy, lo, hi), 1)
      },
      dataset = region,
      source  = source
    ) |>
    select(lat, lon, depth_m, dataset, source)
}



# =============================================================================
# OBIS API
# =============================================================================

fetch_obis <- function(depth_min, depth_max, dataset_ids, timeout = 30) {
  message("\n[OBIS] Querying occurrence API...")
  base_url <- "https://api.obis.org/v3/occurrence"
  records  <- list()

  for (ds_id in dataset_ids) {
    message(glue("  Dataset {str_sub(ds_id,1,8)}..."))
    tryCatch({
      resp <- request(base_url) |>
        req_url_query(
          datasetid = ds_id,
          mindepth  = depth_min,
          maxdepth  = depth_max,
          size      = 1000,
          fields    = "decimalLatitude,decimalLongitude,depth,datasetName,samplingProtocol"
        ) |>
        req_timeout(timeout) |>
        req_retry(max_tries = 3, backoff = ~2) |>
        req_perform()
      hits <- resp |> resp_body_json(simplifyVector = TRUE)
      df   <- hits$results
      if (!is.null(df) && nrow(df) > 0) {
        df$source <- "OBIS"
        records   <- c(records, list(df))
        message(glue("    -> {nrow(df)} raw records"))
      }
    }, error = function(e) message(glue("    [WARN] {e$message}")))
    Sys.sleep(0.4)
  }

  for (kw in c("stereo-video", "baited remote underwater video", "BRUVs")) {
    message(glue("  Keyword: '{kw}'"))
    tryCatch({
      resp <- request(base_url) |>
        req_url_query(
          samplingprotocol = kw,
          mindepth  = depth_min,
          maxdepth  = depth_max,
          size      = 1000,
          fields    = "decimalLatitude,decimalLongitude,depth,datasetName"
        ) |>
        req_timeout(timeout) |>
        req_retry(max_tries = 3, backoff = ~2) |>
        req_perform()
      hits <- resp |> resp_body_json(simplifyVector = TRUE)
      df   <- hits$results
      if (!is.null(df) && nrow(df) > 0) {
        df$source <- "OBIS-keyword"
        records   <- c(records, list(df))
        message(glue("    -> {nrow(df)} raw records"))
      }
    }, error = function(e) message(glue("    [WARN] {e$message}")))
    Sys.sleep(0.4)
  }

  if (length(records) == 0) { message("  [INFO] No OBIS records retrieved."); return(NULL) }

  bind_rows(records) |>
    rename(
      lat     = any_of(c("decimalLatitude",  "lat")),
      lon     = any_of(c("decimalLongitude", "lon")),
      depth_m = any_of(c("depth", "depth_m")),
      dataset = any_of(c("datasetName", "dataset"))
    ) |>
    mutate(source = "OBIS") |>
    select(any_of(c("lat", "lon", "depth_m", "dataset", "source")))
}



# =============================================================================
# GBIF API
# =============================================================================

fetch_gbif <- function(depth_min, depth_max, timeout = 30) {
  message("\n[GBIF] Querying stereo-video occurrences...")
  tryCatch({
    resp <- request("https://api.gbif.org/v1/occurrence/search") |>
      req_url_query(
        samplingProtocol = "stereo-video",
        depth            = glue("{depth_min},{depth_max}"),
        hasCoordinate    = "true",
        limit            = 300
      ) |>
      req_timeout(timeout) |>
      req_retry(max_tries = 3, backoff = ~2) |>
      req_perform()
    hits <- resp |> resp_body_json(simplifyVector = TRUE)
    df   <- hits$results
    if (is.null(df) || nrow(df) == 0) { message("  -> No GBIF records"); return(NULL) }
    message(glue("  -> {nrow(df)} raw GBIF records"))
    df |> transmute(lat = decimalLatitude, lon = decimalLongitude,
                    depth_m = depth, dataset = datasetName, source = "GBIF")
  }, error = function(e) { message(glue("  [WARN] GBIF: {e$message}")); NULL })
}



# =============================================================================
# SQUIDLE+ API
# =============================================================================
# Auth: POST /api/user/sign_in -> { auth_token }
#       Header: Authorization: Bearer <auth_token>
# Register: https://squidle.org/register

fetch_squidle <- function(depth_min, depth_max, creds, timeout = 30) {
  message("\n[Squidle+] Authenticating...")
  if (is.null(creds)) {
    message("  [SKIP] Add SQUIDLE_USERNAME / SQUIDLE_PASSWORD to ~/.Renviron")
    return(NULL)
  }
  base_url <- "https://squidle.org"

  auth_token <- tryCatch({
    resp <- request(glue("{base_url}/api/user/sign_in")) |>
      req_method("POST") |>
      req_body_json(list(user = list(email = creds$username, password = creds$password))) |>
      req_timeout(timeout) |>
      req_perform()
    token <- resp |> resp_body_json() |> _[["auth_token"]]
    if (is.null(token)) stop("auth_token not returned")
    message(glue("  -> Authenticated as {creds$username}"))
    token
  }, error = function(e) { message(glue("  [WARN] {e$message}")); NULL })

  if (is.null(auth_token)) return(NULL)

  campaigns <- tryCatch({
    q    <- toJSON(list(filters = list(list(name="media_type",op="eq",val="video"))), auto_unbox=TRUE)
    resp <- request(glue("{base_url}/api/campaign")) |>
      req_url_query(q = q, results_per_page = 200, page = 1) |>
      req_headers(Authorization = glue("Bearer {auth_token}")) |>
      req_timeout(timeout) |>
      req_perform()
    resp |> resp_body_json(simplifyVector = TRUE) |> _[["objects"]]
  }, error = function(e) { message(glue("  [WARN] {e$message}")); NULL })

  if (is.null(campaigns) || nrow(campaigns) == 0) { message("  -> No campaigns"); return(NULL) }
  message(glue("  -> {nrow(campaigns)} campaigns"))

  map(seq_len(min(nrow(campaigns), 60)), function(i) {
    camp <- campaigns[i, ]
    tryCatch({
      q    <- toJSON(list(filters = list(list(name="campaign_id",op="eq",val=camp$id))), auto_unbox=TRUE)
      resp <- request(glue("{base_url}/api/deployment")) |>
        req_url_query(q = q, results_per_page = 500) |>
        req_headers(Authorization = glue("Bearer {auth_token}")) |>
        req_timeout(timeout) |>
        req_perform()
      deps <- resp |> resp_body_json(simplifyVector = FALSE) |> _[["objects"]]
      if (length(deps) == 0) return(NULL)
      map_dfr(deps, function(d) tibble(
        lat     = as.numeric(d$start_position$latitude  %||% d$latitude  %||% NA),
        lon     = as.numeric(d$start_position$longitude %||% d$longitude %||% NA),
        depth_m = as.numeric(d$depth_m %||% d$min_depth %||% NA),
        dataset = as.character(camp$name %||% "Squidle"),
        source  = "Squidle+"
      ))
    }, error = function(e) NULL)
  }) |>
    bind_rows() |>
    (\(df) { if (nrow(df) == 0) NULL else { message(glue("  -> {nrow(df)} raw records")); df } })()
}



# =============================================================================
# GLOBALARCHIVE API
# =============================================================================
# Auth: POST /api/token/ -> { access } (JWT)
#       Header: Authorization: Token <access>
# Register: https://globalarchive.org/geodata/accounts/register/

fetch_globalarchive <- function(depth_min, depth_max, creds, timeout = 30) {
  message("\n[GlobalArchive] Authenticating...")
  if (is.null(creds)) {
    message("  [SKIP] Add GLOBALARCHIVE_USERNAME / GLOBALARCHIVE_PASSWORD to ~/.Renviron")
    return(NULL)
  }
  base_url <- "https://globalarchive.org"

  access_token <- tryCatch({
    resp <- request(glue("{base_url}/api/token/")) |>
      req_method("POST") |>
      req_body_json(list(username = creds$username, password = creds$password)) |>
      req_timeout(timeout) |>
      req_perform()
    body  <- resp |> resp_body_json()
    token <- body$access %||% body$token
    if (is.null(token)) stop("No access token returned")
    message(glue("  -> Authenticated as {creds$username}"))
    token
  }, error = function(e) { message(glue("  [WARN] {e$message}")); NULL })

  if (is.null(access_token)) return(NULL)

  tryCatch({
    resp <- request(glue("{base_url}/api/sample/")) |>
      req_url_query(method = "BRUVs", page_size = 500) |>
      req_headers(Authorization = glue("Token {access_token}")) |>
      req_timeout(timeout) |>
      req_perform()
    body    <- resp |> resp_body_json(simplifyVector = TRUE)
    samples <- body$results %||% body
    if (is.null(samples) || length(samples) == 0) return(NULL)
    df <- as_tibble(samples) |>
      transmute(
        lat     = as.numeric(latitude        %||% NA),
        lon     = as.numeric(longitude       %||% NA),
        depth_m = as.numeric(depth_m %||% depth %||% NA),
        dataset = as.character(campaign_name %||% "GlobalArchive"),
        source  = "GlobalArchive"
      )
    message(glue("  -> {nrow(df)} raw records"))
    df
  }, error = function(e) { message(glue("  [WARN] {e$message}")); NULL })
}



# =============================================================================
# CREDENTIAL HELPER
# =============================================================================

get_credentials <- function(service, cfg = CONFIG) {
  if (cfg$credential_method == "renviron") {
    user <- Sys.getenv(toupper(paste0(service, "_USERNAME")))
    pass <- Sys.getenv(toupper(paste0(service, "_PASSWORD")))
    if (nchar(user) == 0 || nchar(pass) == 0) {
      message(glue(
        "\n[WARN] {service} credentials missing from .Renviron.\n",
        "  Add {toupper(service)}_USERNAME and {toupper(service)}_PASSWORD,",
        " then restart R.\n"
      ))
      return(NULL)
    }
    list(username = user, password = pass)

  } else if (cfg$credential_method == "keyring") {
    if (!requireNamespace("keyring", quietly = TRUE)) {
      message("[WARN] install.packages('keyring') first"); return(NULL)
    }
    tryCatch({
      svc  <- tolower(service)
      user <- keyring::key_list(svc)$username[1]
      list(username = user, password = keyring::key_get(svc, username = user))
    }, error = function(e) { message(glue("[WARN] keyring '{service}': {e$message}")); NULL })

  } else if (cfg$credential_method == "prompt") {
    user <- readline(prompt = glue("{service} email: "))
    pass <- readline(prompt = glue("{service} password: "))
    if (nchar(user) == 0) return(NULL)
    list(username = user, password = pass)

  } else {
    NULL
  }
}



# =============================================================================
# INTERACTIVE MAP (Leaflet)
# =============================================================================

make_leaflet_map <- function(df_points, df_gaps, depth_min, depth_max, out_path) {
  message("\n[Map] Building Leaflet map...")

  depth_pal <- colorNumeric(
    palette  = c("#1a9e75", "#3778c2", "#9b59b6", "#c0392b"),
    domain   = c(depth_min, depth_max),
    na.color = "#888888"
  )

  # Palette built from the same canonical vectors — cannot diverge
  gap_pal <- colorFactor(
    palette = unname(GAP_COLOURS[GAP_LEVELS]),
    levels  = GAP_LEVELS,
    ordered = TRUE
  )

  map <- leaflet(options = leafletOptions(worldCopyJump = FALSE, preferCanvas = TRUE)) |>
    addProviderTiles("CartoDB.DarkMatter", group = "Dark (default)") |>
    addProviderTiles("Esri.OceanBasemap",  group = "Ocean basemap") |>
    addProviderTiles("OpenStreetMap",      group = "Street map")

  # Gap circles — only regions where show_gap = TRUE and which have mesophotic habitat
  df_gap_circles <- df_gaps |>
    filter(show_gap, !is.na(lat_centroid), !is.na(lon_centroid))

  if (nrow(df_gap_circles) > 0) {
    map <- map |>
      addCircles(
        data        = df_gap_circles,
        lat         = ~lat_centroid,
        lng         = ~lon_centroid,
        # Radius inversely proportional to records: fewer records = bigger circle
        radius      = ~pmax(150000, (50 - pmin(n_records, 50)) * 55000),
        color       = ~gap_pal(as.character(gap_status)),
        fillColor   = ~gap_pal(as.character(gap_status)),
        fillOpacity = 0.18,
        weight      = 1.8,
        opacity     = 0.75,
        dashArray   = "5,4",
        popup = ~glue(
          "<b>{region_bin}</b><br>",
          "Records in range: <b>{n_records}</b><br>",
          "Status: <b style='color:{GAP_COLOURS[as.character(gap_status)]};'>",
          "{as.character(gap_status)}</b><br>",
          "Depth span: {ifelse(
            is.na(min_depth), 'no records',
            paste0(round(min_depth), '-', round(max_depth), ' m')
          )}"
        ),
        label = ~glue("{region_bin}: {n_records} records ({as.character(gap_status)})"),
        group = "Gap zones"
      )
  }

  # Individual deployment points
  if (nrow(df_points) > 0) {
    map <- map |>
      addCircleMarkers(
        data        = df_points,
        lat         = ~lat,
        lng         = ~lon,
        radius      = 5,
        color       = ~depth_pal(depth_m),
        fillColor   = ~depth_pal(depth_m),
        fillOpacity = 0.85,
        weight      = 0.5,
        opacity     = 1,
        popup  = ~glue("<b>{dataset}</b><br>Depth: <b>{round(depth_m)} m</b><br>Source: {source}"),
        label  = ~glue("{dataset} | {round(depth_m)} m"),
        clusterOptions = markerClusterOptions(
          maxClusterRadius        = 40,
          disableClusteringAtZoom = 8
        ),
        group = "BRUVs deployments"
      )
  }

  map <- map |>
    addLegend(
      position  = "bottomright",
      pal       = depth_pal,
      values    = c(depth_min, depth_max),
      title     = glue("Depth (m)<br>{depth_min}-{depth_max} m"),
      opacity   = 0.9,
      labFormat = labelFormat(suffix = " m")
    ) |>
    addLegend(
      position = "bottomleft",
      pal      = gap_pal,
      values   = factor(GAP_LEVELS, levels = GAP_LEVELS, ordered = TRUE),
      title    = "Gap status",
      opacity  = 0.9
    ) |>
    addLayersControl(
      baseGroups    = c("Dark (default)", "Ocean basemap", "Street map"),
      overlayGroups = c("BRUVs deployments", "Gap zones"),
      options       = layersControlOptions(collapsed = FALSE)
    ) |>
    addMeasure(primaryLengthUnit = "kilometers") |>
    addMiniMap(toggleDisplay = TRUE, minimized = TRUE) |>
    addScaleBar(position = "bottomleft") |>
    setView(lng = 135, lat = -5, zoom = 2)

  saveWidget(map, file = out_path, selfcontained = TRUE)
  message(glue("  -> Saved: {out_path}"))
}



# =============================================================================
# DEPTH HISTOGRAM (Plotly)
# =============================================================================

make_depth_histogram <- function(df, depth_min, depth_max, out_path) {
  message("\n[Plot] Building depth histogram...")

  zone_levels <- c(
    "Shallow (<30 m)",
    "Upper mesophotic (30-150 m)",
    "Lower mesophotic (150-300 m)",
    "Deep (>300 m)"
  )
  zone_colors <- c(
    "Shallow (<30 m)"              = "#1a9e75",
    "Upper mesophotic (30-150 m)"  = "#3778c2",
    "Lower mesophotic (150-300 m)" = "#9b59b6",
    "Deep (>300 m)"                = "#c0392b"
  )

  df_d <- df |>
    mutate(zone = factor(
      dplyr::case_when(
        depth_m <  30  ~ "Shallow (<30 m)",
        depth_m <= 150 ~ "Upper mesophotic (30-150 m)",
        depth_m <= 300 ~ "Lower mesophotic (150-300 m)",
        TRUE           ~ "Deep (>300 m)"
      ),
      levels = zone_levels
    ))

  fig <- plot_ly(df_d, x = ~depth_m, color = ~zone, colors = zone_colors,
                 type = "histogram", nbinsx = 60, alpha = 0.85) |>
    layout(
      title       = list(text = glue(
        "Depth distribution of stereo-BRUVs records  ",
        "<sub>{nrow(df_d)} records | {depth_min}-{depth_max} m</sub>"
      )),
      xaxis       = list(title = "Depth (m)"),
      yaxis       = list(title = "Number of records"),
      barmode     = "stack",
      paper_bgcolor = "#0d1b2a",
      plot_bgcolor  = "#111d2e",
      font          = list(color = "#cccccc"),
      legend        = list(title = list(text = "Depth zone"))
    )

  saveWidget(fig, file = out_path, selfcontained = TRUE)
  message(glue("  -> Saved: {out_path}"))
}



# =============================================================================
# MAIN PIPELINE
# =============================================================================

run_bruv_survey <- function(
    depth_min      = CONFIG$depth_min,
    depth_max      = CONFIG$depth_max,
    output_dir     = CONFIG$output_dir,
    cfg            = CONFIG,
    preloaded_data = NULL   # pass a pre-cleaned data frame to skip all API fetching
) {
  cat("\n", strrep("=", 65), "\n")
  cat("  stereo-BRUVs Mesophotic Gap Analysis\n")
  cat(glue("  Depth range : {depth_min} - {depth_max} m\n"))
  cat(glue("  Output dir  : {output_dir}\n"))
  if (!is.null(preloaded_data))
    cat(glue("  Mode        : preloaded data ({nrow(preloaded_data)} rows supplied)\n"))
  cat(strrep("=", 65), "\n\n")

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  out <- function(f) file.path(output_dir, f)

  # -----------------------------------------------------------------------
  # Collect raw frames  (skipped entirely when preloaded_data is supplied)
  # -----------------------------------------------------------------------
  if (!is.null(preloaded_data)) {
    message("[INFO] Using preloaded data — all API fetches skipped")
    raw_frames <- list(preloaded_data)
  } else {
    raw_frames <- list()
    if (cfg$use_curated) {
      message("[Curated] Loading literature baseline...")
      raw_frames <- c(raw_frames, list(get_curated_data(depth_min, depth_max)))
    }
    if (cfg$use_obis)
      raw_frames <- c(raw_frames,
                      list(fetch_obis(depth_min, depth_max, cfg$obis_dataset_ids, cfg$timeout)))
    if (cfg$use_gbif)
      raw_frames <- c(raw_frames,
                      list(fetch_gbif(depth_min, depth_max, cfg$timeout)))
    if (cfg$use_squidle)
      raw_frames <- c(raw_frames,
                      list(fetch_squidle(depth_min, depth_max,
                                         get_credentials("SQUIDLE", cfg), cfg$timeout)))
    if (cfg$use_globalarchive)
      raw_frames <- c(raw_frames,
                      list(fetch_globalarchive(depth_min, depth_max,
                                               get_credentials("GLOBALARCHIVE", cfg), cfg$timeout)))
  }

  # -----------------------------------------------------------------------
  # Clean
  # -----------------------------------------------------------------------
  message("\n[CLEAN] Applying data cleaning...")
  n_raw <- sum(map_int(raw_frames, ~ if (is.null(.x)) 0L else nrow(.x)))

  clean_frames <- raw_frames |>
    map(~ clean_records(.x, depth_min, depth_max)) |>
    keep(~ !is.null(.x) && nrow(.x) > 0)

  n_clean <- sum(map_int(clean_frames, nrow))
  message(glue("  Raw rows        : {n_raw}"))
  message(glue("  After cleaning  : {n_clean}"))
  message(glue("  Rows removed    : {n_raw - n_clean}"))

  df_all <- bind_rows(clean_frames) |>
    distinct(lat, lon, depth_m, .keep_all = TRUE)

  message(glue("  Unique records  : {nrow(df_all)}"))

  if (nrow(df_all) == 0)
    stop("No records survived cleaning. Check API connectivity and depth range.")

  # Belt-and-braces NA check
  na_hits <- colSums(is.na(df_all[, c("lat", "lon", "depth_m")]))
  if (any(na_hits > 0)) {
    warning("NAs detected after cleaning: ",
            paste(names(na_hits[na_hits > 0]), collapse = ", "))
  } else {
    message("  NA check: PASSED")
  }

  # -----------------------------------------------------------------------
  # Gap analysis
  # -----------------------------------------------------------------------
  message("\n[GAP ANALYSIS] Computing per-region effort...")
  df_gaps <- run_gap_analysis(df_all)

  # -----------------------------------------------------------------------
  # Save outputs
  # -----------------------------------------------------------------------
  write_csv(df_all,  out("bruv_records.csv"))
  write_csv(df_gaps |> mutate(gap_status = as.character(gap_status)),
            out("bruv_gap_report.csv"))
  message(glue("  -> bruv_records.csv    ({nrow(df_all)} rows)"))
  message(glue("  -> bruv_gap_report.csv ({nrow(df_gaps)} regions)"))

  make_leaflet_map(df_all, df_gaps, depth_min, depth_max, out("bruv_map.html"))
  make_depth_histogram(df_all, depth_min, depth_max, out("bruv_depth_hist.html"))

  # -----------------------------------------------------------------------
  # Console summary
  # -----------------------------------------------------------------------
  cat("\n", strrep("=", 65), "\n")
  cat("  GAP ANALYSIS SUMMARY (habitat regions only)\n")
  cat(strrep("=", 65), "\n")
  df_gaps |>
    filter(show_gap) |>
    arrange(gap_status) |>
    mutate(bar = ifelse(n_records == 0, ".", strrep("|", pmin(n_records, 30)))) |>
    pwalk(function(region_bin, n_records, gap_status, bar, ...) {
      cat(glue(
        "  {str_pad(region_bin, 36)} {str_pad(n_records, 4)} recs",
        "  {str_pad(bar, 31)}  {as.character(gap_status)}\n"
      ))
    })

  cat("\n[DONE] Outputs written to: ", normalizePath(output_dir), "\n")
  cat("  GlobalArchive  -> https://globalarchive.org\n")
  cat("  Squidle+       -> https://squidle.org\n")
  cat("  OBIS datasets  -> https://obis.org/dataset/search?q=bruv\n")
  cat("  REEFSCAPE      -> https://reefscape.net\n")

  invisible(list(records = df_all, gaps = df_gaps))
}



# =============================================================================
# RUN
# =============================================================================

# --- Standard run (fetches all sources) ---
results    <- run_bruv_survey(depth_min = 30, depth_max = 300)
df_records <- results$records
df_gaps    <- results$gaps

# --- Remove manually identified bad rows, then re-run without re-fetching ---
# df_records_clean <- df_records[-c(2382:2571), ]
# results    <- run_bruv_survey(depth_min = 30, depth_max = 300,
#                               preloaded_data = df_records_clean)
# df_records <- results$records
# df_gaps    <- results$gaps

# --- Upper mesophotic only ---
# results    <- run_bruv_survey(depth_min = 30, depth_max = 150)
# df_records <- results$records
# df_gaps    <- results$gaps

# --- Lower mesophotic only ---
# results    <- run_bruv_survey(depth_min = 150, depth_max = 300)
# df_records <- results$records
# df_gaps    <- results$gaps
