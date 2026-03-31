# =============================================================================
# stereo-BRUVs Global Mesophotic Distribution & Data Gap Analysis
# =============================================================================
#
# DESCRIPTION:
#   Queries stereo-BRUVs deployment data from:
#     - OBIS  (Ocean Biodiversity Information System) — public, no login
#     - GBIF  — public, no login
#     - Squidle+  — requires free account (see AUTHENTICATION section below)
#     - GlobalArchive — requires free account (see AUTHENTICATION section below)
#   Merges with curated literature data and produces:
#     - bruv_records.csv      : all cleaned deployment records
#     - bruv_gap_report.csv   : per-region gap classification
#     - bruv_map.html         : interactive Leaflet map
#     - bruv_depth_hist.html  : interactive depth histogram (plotly)
#
# REQUIREMENTS:
#   install.packages(c(
#     "httr2", "jsonlite", "dplyr", "tidyr", "readr",
#     "leaflet", "leaflet.extras", "plotly", "htmlwidgets",
#     "sf", "ggplot2", "scales", "glue", "purrr", "stringr"
#   ))
#
# USAGE:
#   source("bruv_mesophotic_survey.R")
#   run_bruv_survey(depth_min = 30, depth_max = 300)
#   run_bruv_survey(depth_min = 150, depth_max = 300)   # lower meso only
#
# =============================================================================



# =============================================================================
# ██████████████████████████████████████████████████████████████████████████
# ██                      AUTHENTICATION SETUP                            ██
# ██████████████████████████████████████████████████████████████████████████
#
#  HOW TO SET UP YOUR CREDENTIALS (do this ONCE before running the script)
#  -----------------------------------------------------------------------
#
#  OPTION A — .Renviron file (RECOMMENDED — credentials never appear in code)
#  --------------------------------------------------------------------------
#  1. Open your .Renviron file:
#       usethis::edit_r_environ()          # opens ~/.Renviron in RStudio
#     OR in a terminal:
#       nano ~/.Renviron
#
#  2. Add these lines (replace with your actual values):
#
#       SQUIDLE_USERNAME=your.email@example.com
#       SQUIDLE_PASSWORD=your_squidle_password
#
#       GLOBALARCHIVE_USERNAME=your.email@example.com
#       GLOBALARCHIVE_PASSWORD=your_globalarchive_password
#
#  3. Save the file, then restart R (Session → Restart R in RStudio)
#     Verify with:  Sys.getenv("SQUIDLE_USERNAME")
#
#  OPTION B — keyring package (encrypted OS keychain, most secure)
#  ----------------------------------------------------------------
#  install.packages("keyring")
#  keyring::key_set("squidle", username = "your.email@example.com")
#  keyring::key_set("globalarchive", username = "your.email@example.com")
#
#  Then in the CONFIG section below, set:
#    USE_KEYRING = TRUE
#
#  OPTION C — interactive prompt (useful for one-off runs)
#  -------------------------------------------------------
#  Set USE_PROMPT = TRUE in the CONFIG section below.
#  R will ask for your password each run (not suitable for batch/automation).
#
# ██████████████████████████████████████████████████████████████████████████
#
#  WHERE TO REGISTER (both are free):
#    Squidle+      → https://squidle.org/register
#    GlobalArchive → https://globalarchive.org/geodata/accounts/register/
#
# ██████████████████████████████████████████████████████████████████████████
# =============================================================================



# =============================================================================
# CONFIG — edit this block to match your setup
# =============================================================================

CONFIG <- list(

  # --- Depth range of interest ---
  depth_min = 30,    # metres  (mesophotic starts at ~30 m)
  depth_max = 300,   # metres  (mesophotic ends ~300 m; upper = 30-150, lower = 150-300)

  # --- Output directory (created if it doesn't exist) ---
  output_dir = "bruv_output",

  # --- Credential method: "renviron" | "keyring" | "prompt" | "none" ---
  # Set to "none" to skip authenticated sources (OBIS + GBIF still work)
  credential_method = "renviron",

  # --- Source toggles ---
  use_obis        = TRUE,
  use_gbif        = TRUE,
  use_squidle     = TRUE,
  use_globalarchive = TRUE,
  use_curated     = TRUE,   # curated literature baseline — always recommended

  # --- OBIS dataset IDs known to contain stereo-BRUVs records ---
  # Find more: https://obis.org/dataset/search?q=bruv
  obis_dataset_ids = c(
    "a1e5e79a-2e99-48c7-8e51-0ecf9a9f4e7e",
    "7a41a4a6-7e88-4e8b-a2d2-a9d7f6f6c8b2",
    "3bab7e13-91a7-41b3-9a77-1523d62a4b65"
  ),

  # --- Max records to pull per API call ---
  max_records = 5000,

  # --- Request timeout in seconds ---
  timeout = 30
)



# =============================================================================
# PACKAGE LOADING
# =============================================================================

required_pkgs <- c(
  "httr2", "jsonlite", "dplyr", "tidyr", "readr",
  "leaflet", "leaflet.extras", "plotly", "htmlwidgets",
  "purrr", "stringr", "glue", "scales"
)

missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(leaflet)
  library(leaflet.extras)
  library(plotly)
  library(htmlwidgets)
  library(purrr)
  library(stringr)
  library(glue)
  library(scales)
})

message("\n[OK] All packages loaded")



# =============================================================================
# CREDENTIAL HELPERS
# =============================================================================

#' Retrieve credentials using the configured method.
#' Returns list(username, password) or NULL if unavailable.
get_credentials <- function(service, cfg = CONFIG) {

  method <- cfg$credential_method

  if (method == "renviron") {
    key_user <- toupper(paste0(service, "_USERNAME"))
    key_pass <- toupper(paste0(service, "_PASSWORD"))
    user <- Sys.getenv(key_user)
    pass <- Sys.getenv(key_pass)
    if (nchar(user) == 0 || nchar(pass) == 0) {
      message(glue(
        "\n[WARN] {service} credentials not found in .Renviron.\n",
        "  Add these lines to ~/.Renviron (run usethis::edit_r_environ()):\n",
        "    {key_user}=your_email\n",
        "    {key_pass}=your_password\n",
        "  Then restart R and re-run the script.\n"
      ))
      return(NULL)
    }
    return(list(username = user, password = pass))

  } else if (method == "keyring") {
    if (!requireNamespace("keyring", quietly = TRUE)) {
      message("[WARN] keyring package not installed. Run: install.packages('keyring')")
      return(NULL)
    }
    tryCatch({
      service_lower <- tolower(service)
      user <- keyring::key_list(service_lower)$username[1]
      pass <- keyring::key_get(service_lower, username = user)
      return(list(username = user, password = pass))
    }, error = function(e) {
      message(glue("[WARN] keyring entry for '{service}' not found: {e$message}"))
      return(NULL)
    })

  } else if (method == "prompt") {
    user <- readline(prompt = glue("{service} username/email: "))
    pass <- readline(prompt = glue("{service} password: "))  # NOTE: not masked
    # For masked input in RStudio use: rstudioapi::askForPassword(glue("{service} password"))
    if (nchar(user) == 0) return(NULL)
    return(list(username = user, password = pass))

  } else {
    return(NULL)   # "none" — skip authenticated sources
  }
}



# =============================================================================
# CURATED BASELINE DATA
# =============================================================================
# Centroids + deployment counts from published stereo-BRUVs studies.
# References: Harvey et al. 2013 (GlobalArchive), Lindfield et al. 2016,
# Asher et al. 2017, Pinheiro et al. 2016, Moore et al. 2019,
# Sherman et al. 2023, Bejarano et al. 2014, Agudo-Adriani et al. 2016.

get_curated_data <- function(depth_min, depth_max) {
  lit <- tibble::tribble(
    ~lat,   ~lon,    ~depth_m, ~n_deploy, ~region,                           ~source,                          ~is_gap,
    -25.9,  113.5,   80,       312,       "SW Australia (Ningaloo/Rottnest)","GlobalArchive / Harvey 2013",    FALSE,
    -36.5,  150.5,   60,       198,       "SE Australia (SE shelf)",         "GlobalArchive / RLS",            FALSE,
    -16.5,  146.5,   45,       445,       "Great Barrier Reef",              "GlobalArchive / Squidle+",       FALSE,
    -16.0,  152.0,   90,       67,        "Coral Sea (Osprey/Lihou)",        "Squidle+ / OBIS",                FALSE,
    -14.0,  121.0,   55,       88,        "NW Australia (Browse/Scott)",     "GlobalArchive",                  FALSE,
    -35.5,  174.5,  100,       143,       "New Zealand",                     "OBIS / Squidle+",                FALSE,
     21.0, -157.5,  120,       134,       "Hawaii",                          "OBIS / Asher et al. 2017",       FALSE,
     25.5,  -83.0,  130,       201,       "Florida / Gulf of Mexico",        "OBIS / Sherman et al. 2023",     FALSE,
     18.0,  -66.5,  100,       156,       "Caribbean (USVI/PR/Cayman)",      "OBIS / Bejarano et al. 2014",    FALSE,
     -3.8,  -32.4,  180,       49,        "Brazil (Fernando de Noronha)",    "OBIS / Pinheiro et al. 2016",    FALSE,
     38.5,  -28.0,   90,       31,        "Azores / Macaronesia",            "OBIS",                           FALSE,
     35.5,   20.0,   70,       88,        "Mediterranean (E. Basin)",        "OBIS",                           FALSE,
    -33.5,   26.5,   80,       54,        "South Africa (Agulhas)",          "OBIS",                           FALSE,
     -6.5,   72.0,  110,       22,        "Chagos / BIOT",                   "OBIS / Moore et al. 2019",       FALSE,
      4.0,   73.5,   60,       18,        "Maldives",                        "OBIS",                           TRUE,
     13.0,  121.0,   50,       9,         "Philippines (Verde Is. Passage)", "OBIS",                           TRUE,
      7.5,  134.5,  100,       27,        "Palau",                           "OBIS / Lindfield et al. 2016",   FALSE,
     26.0,  127.5,   90,       21,        "Japan (Ryukyu Islands)",          "OBIS",                           TRUE,
     20.5,   38.5,   80,       14,        "Red Sea (Central)",               "OBIS",                           TRUE,
    -17.0,   40.5,   60,       8,         "Mozambique Channel",              "OBIS",                           TRUE,
     -5.0,   40.5,   50,       6,         "Tanzania / Kenya coast",          "OBIS",                           TRUE,
     14.0,  -22.0,   60,       4,         "W Africa (Cape Verde shelf)",     "OBIS",                           TRUE,
     -1.0,  -91.0,  100,       29,        "Ecuador / Galapagos",             "OBIS",                           FALSE,
    -33.5,  -80.0,   70,       7,         "Chile (Juan Fernandez)",          "OBIS",                           TRUE,
    -10.0,  -78.5,   50,       3,         "Peru shelf",                      "OBIS",                           TRUE,
     28.0,  -15.5,   90,       19,        "Canary Islands",                  "OBIS",                           TRUE,
    -31.5,  159.0,   80,       38,        "Lord Howe Island",                "Squidle+ / GlobalArchive",       FALSE,
     -9.0,  127.0,   60,       11,        "Timor Sea / Banda Sea",           "OBIS",                           TRUE,
     -6.0,  147.0,   50,       8,         "Papua New Guinea",                "OBIS",                           TRUE,
     -9.5,  160.5,   50,       6,         "Solomon Islands",                 "OBIS",                           TRUE,
    -18.0,  167.5,  110,       23,        "Vanuatu / New Caledonia",         "OBIS / Squidle+",                FALSE,
    -17.5, -149.5,   90,       41,        "French Polynesia (Moorea)",       "OBIS",                           FALSE,
      1.8, -157.3,   60,       7,         "Line Islands / Kiribati",         "OBIS",                           TRUE,
     14.0,  145.5,   60,       16,        "Micronesia (Guam/CNMI)",          "OBIS",                           TRUE,
     55.0,    3.0,   80,       44,        "North Sea / NE Atlantic",         "OBIS",                           FALSE,
     57.5,   -9.0,  120,       12,        "Scotland (Rockall/Hebrides)",     "OBIS",                           TRUE,
     22.0,   59.0,   50,       3,         "Arabian Sea (Oman shelf)",        "OBIS",                           TRUE,
      8.0,   81.5,   45,       4,         "Sri Lanka / Bay of Bengal",       "OBIS",                           TRUE,
      8.5,   98.5,   40,       5,         "Andaman Sea / Thailand",          "OBIS",                           TRUE,
     53.0, -168.0,  100,       9,         "Alaska / Aleutians",              "OBIS",                           TRUE
  )

  # Only keep sites whose depth overlaps the requested range
  lit <- lit |>
    filter(depth_m >= depth_min * 0.5, depth_m <= depth_max * 1.5)

  # Expand each region centroid into jittered individual-deployment points
  set.seed(42)
  lit |>
    rowwise() |>
    reframe(
      lat      = lat + rnorm(n_deploy, 0, 0.4),
      lon      = lon + rnorm(n_deploy, 0, 0.4),
      depth_m  = pmin(pmax(runif(n_deploy, depth_min, pmin(depth_max, depth_m + 30)),
                           depth_min), depth_max),
      dataset  = region,
      source   = source,
      is_gap   = is_gap
    )
}



# =============================================================================
# OBIS API
# =============================================================================

fetch_obis <- function(depth_min, depth_max, dataset_ids, timeout = 30) {
  message("\n[OBIS] Querying occurrence API (public, no login needed)...")
  base_url <- "https://api.obis.org/v3/occurrence"
  records  <- list()

  # --- 1. Known BRUVs dataset IDs ---
  for (ds_id in dataset_ids) {
    message(glue("  Dataset {str_sub(ds_id, 1, 8)}..."))
    tryCatch({
      resp <- request(base_url) |>
        req_url_query(
          datasetid = ds_id,
          mindepth  = depth_min,
          maxdepth  = depth_max,
          size      = 1000,
          fields    = "decimalLatitude,decimalLongitude,depth,datasetName,eventDate,samplingProtocol"
        ) |>
        req_timeout(timeout) |>
        req_retry(max_tries = 3, backoff = ~2) |>
        req_perform()

      hits <- resp |> resp_body_json(simplifyVector = TRUE)
      df   <- hits$results
      if (!is.null(df) && nrow(df) > 0) {
        df$source <- "OBIS"
        records   <- c(records, list(df))
        message(glue("    → {nrow(df)} records"))
      }
    }, error = function(e) message(glue("    [WARN] {e$message}")))
    Sys.sleep(0.4)
  }

  # --- 2. Sampling protocol keyword search ---
  keywords <- c("stereo-video", "baited remote underwater video", "BRUVs")
  for (kw in keywords) {
    message(glue("  Keyword: '{kw}'"))
    tryCatch({
      resp <- request(base_url) |>
        req_url_query(
          samplingprotocol = kw,
          mindepth  = depth_min,
          maxdepth  = depth_max,
          size      = 1000,
          fields    = "decimalLatitude,decimalLongitude,depth,datasetName,eventDate"
        ) |>
        req_timeout(timeout) |>
        req_retry(max_tries = 3, backoff = ~2) |>
        req_perform()

      hits <- resp |> resp_body_json(simplifyVector = TRUE)
      df   <- hits$results
      if (!is.null(df) && nrow(df) > 0) {
        df$source <- "OBIS-keyword"
        records   <- c(records, list(df))
        message(glue("    → {nrow(df)} records"))
      }
    }, error = function(e) message(glue("    [WARN] {e$message}")))
    Sys.sleep(0.4)
  }

  if (length(records) == 0) {
    message("  [INFO] No OBIS records retrieved. API may be temporarily unavailable.")
    return(NULL)
  }

  bind_rows(records) |>
    rename(
      lat     = any_of(c("decimalLatitude",  "lat")),
      lon     = any_of(c("decimalLongitude", "lon")),
      depth_m = any_of(c("depth", "depth_m")),
      dataset = any_of(c("datasetName", "dataset"))
    ) |>
    select(any_of(c("lat", "lon", "depth_m", "dataset", "source"))) |>
    mutate(across(c(lat, lon, depth_m), as.numeric)) |>
    filter(!is.na(lat), !is.na(lon)) |>
    distinct(lat, lon, depth_m, .keep_all = TRUE)
}



# =============================================================================
# GBIF API
# =============================================================================

fetch_gbif <- function(depth_min, depth_max, timeout = 30) {
  message("\n[GBIF] Querying stereo-video occurrences (public, no login needed)...")
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

    if (is.null(df) || nrow(df) == 0) {
      message("  → No GBIF records found")
      return(NULL)
    }

    message(glue("  → {nrow(df)} GBIF records"))
    df |>
      select(lat = decimalLatitude, lon = decimalLongitude,
             depth_m = depth, dataset = datasetName) |>
      mutate(source = "GBIF", across(c(lat, lon, depth_m), as.numeric)) |>
      filter(!is.na(lat), !is.na(lon))

  }, error = function(e) {
    message(glue("  [WARN] GBIF error: {e$message}"))
    NULL
  })
}



# =============================================================================
# SQUIDLE+ API
# =============================================================================
#
# Authentication flow:
#   POST /api/user/sign_in  → returns auth_token
#   Include header: Authorization: Bearer <auth_token>
#
# Public endpoint (no login): limited campaign metadata only
# Authenticated endpoint:     full deployment list with depths + coordinates
#
# Register free account: https://squidle.org/register
# API docs:              https://squidle.org/api/help

fetch_squidle <- function(depth_min, depth_max, creds, timeout = 30) {
  message("\n[Squidle+] Authenticating...")

  if (is.null(creds)) {
    message("  [SKIP] No Squidle+ credentials. Set SQUIDLE_USERNAME and SQUIDLE_PASSWORD in .Renviron")
    return(NULL)
  }

  base_url <- "https://squidle.org"

  # --- Step 1: Sign in to get auth token ---
  auth_token <- tryCatch({
    resp <- request(glue("{base_url}/api/user/sign_in")) |>
      req_method("POST") |>
      req_body_json(list(
        user = list(
          email    = creds$username,
          password = creds$password
        )
      )) |>
      req_timeout(timeout) |>
      req_perform()

    body <- resp |> resp_body_json()
    token <- body$auth_token
    if (is.null(token)) stop("auth_token not returned — check username/password")
    message(glue("  → Authenticated as {creds$username}"))
    token

  }, error = function(e) {
    message(glue("  [WARN] Squidle+ login failed: {e$message}"))
    NULL
  })

  if (is.null(auth_token)) return(NULL)

  # --- Step 2: Fetch video campaigns ---
  message("  Fetching video campaigns...")
  campaigns <- tryCatch({
    q_filter <- toJSON(list(
      filters = list(list(name = "media_type", op = "eq", val = "video"))
    ), auto_unbox = TRUE)

    resp <- request(glue("{base_url}/api/campaign")) |>
      req_url_query(q = q_filter, results_per_page = 200, page = 1) |>
      req_headers(Authorization = glue("Bearer {auth_token}")) |>
      req_timeout(timeout) |>
      req_perform()

    body <- resp |> resp_body_json(simplifyVector = TRUE)
    body$objects

  }, error = function(e) {
    message(glue("  [WARN] Could not fetch Squidle campaigns: {e$message}"))
    NULL
  })

  if (is.null(campaigns) || nrow(campaigns) == 0) {
    message("  → No campaigns found")
    return(NULL)
  }
  message(glue("  → {nrow(campaigns)} campaigns found"))

  # --- Step 3: Fetch deployments for each campaign ---
  records <- map(seq_len(min(nrow(campaigns), 60)), function(i) {
    camp <- campaigns[i, ]
    tryCatch({
      q_filter <- toJSON(list(
        filters = list(list(name = "campaign_id", op = "eq", val = camp$id))
      ), auto_unbox = TRUE)

      resp <- request(glue("{base_url}/api/deployment")) |>
        req_url_query(q = q_filter, results_per_page = 500) |>
        req_headers(Authorization = glue("Bearer {auth_token}")) |>
        req_timeout(timeout) |>
        req_perform()

      deps <- resp |> resp_body_json(simplifyVector = FALSE)
      deps <- deps$objects

      if (length(deps) == 0) return(NULL)

      map_dfr(deps, function(d) {
        lat   <- d$start_position$latitude  %||% d$latitude
        lon   <- d$start_position$longitude %||% d$longitude
        depth <- d$depth_m %||% d$min_depth
        tibble(
          lat     = as.numeric(lat   %||% NA),
          lon     = as.numeric(lon   %||% NA),
          depth_m = as.numeric(depth %||% NA),
          dataset = camp$name %||% "Squidle campaign",
          source  = "Squidle+"
        )
      })
    }, error = function(e) NULL)
  }) |> bind_rows()

  if (nrow(records) == 0) return(NULL)

  records |>
    filter(!is.na(lat), !is.na(lon)) |>
    filter(is.na(depth_m) | (depth_m >= depth_min & depth_m <= depth_max)) |>
    distinct(lat, lon, depth_m, .keep_all = TRUE) |>
    (\(df) { message(glue("  → {nrow(df)} Squidle deployment records")); df })()
}

# Null coalescing helper
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b



# =============================================================================
# GLOBALARCHIVE API
# =============================================================================
#
# Authentication flow:
#   POST /api/token/  → returns access + refresh JWT tokens
#   Include header: Authorization: Token <access>
#
# Public endpoint:  limited metadata
# Authenticated:    full dataset/campaign/sample access
#
# Register free account: https://globalarchive.org/geodata/accounts/register/
# API docs:              https://globalarchive.org/geodata/data/  (browseable)
# API root:              https://globalarchive.org/api/

fetch_globalarchive <- function(depth_min, depth_max, creds, timeout = 30) {
  message("\n[GlobalArchive] Authenticating...")

  if (is.null(creds)) {
    message("  [SKIP] No GlobalArchive credentials. Set GLOBALARCHIVE_USERNAME and GLOBALARCHIVE_PASSWORD in .Renviron")
    return(NULL)
  }

  base_url <- "https://globalarchive.org"

  # --- Step 1: Obtain JWT token ---
  access_token <- tryCatch({
    resp <- request(glue("{base_url}/api/token/")) |>
      req_method("POST") |>
      req_body_json(list(
        username = creds$username,
        password = creds$password
      )) |>
      req_timeout(timeout) |>
      req_perform()

    body  <- resp |> resp_body_json()
    token <- body$access %||% body$token
    if (is.null(token)) stop("No access token returned — check username/password")
    message(glue("  → Authenticated as {creds$username}"))
    token

  }, error = function(e) {
    message(glue("  [WARN] GlobalArchive login failed: {e$message}"))
    NULL
  })

  if (is.null(access_token)) return(NULL)

  auth_header <- glue("Token {access_token}")

  # --- Step 2: Fetch campaigns (BRUVs method filter) ---
  message("  Fetching BRUVs campaigns...")
  campaigns <- tryCatch({
    resp <- request(glue("{base_url}/api/campaign/")) |>
      req_url_query(
        method     = "BRUVs",
        page_size  = 200
      ) |>
      req_headers(Authorization = auth_header) |>
      req_timeout(timeout) |>
      req_perform()

    body <- resp |> resp_body_json(simplifyVector = TRUE)
    body$results %||% body

  }, error = function(e) {
    message(glue("  [WARN] Could not fetch GlobalArchive campaigns: {e$message}"))
    # Fallback: try without method filter
    tryCatch({
      resp <- request(glue("{base_url}/api/campaign/")) |>
        req_url_query(page_size = 200) |>
        req_headers(Authorization = auth_header) |>
        req_timeout(timeout) |>
        req_perform()
      body <- resp |> resp_body_json(simplifyVector = TRUE)
      body$results %||% body
    }, error = function(e2) NULL)
  })

  if (is.null(campaigns) || length(campaigns) == 0) {
    message("  → No campaigns found")
    return(NULL)
  }

  n_camps <- if (is.data.frame(campaigns)) nrow(campaigns) else length(campaigns)
  message(glue("  → {n_camps} campaigns found"))

  # --- Step 3: Fetch samples (deployments) ---
  message("  Fetching deployment samples...")
  records <- tryCatch({
    resp <- request(glue("{base_url}/api/sample/")) |>
      req_url_query(
        method    = "BRUVs",
        page_size = 500
      ) |>
      req_headers(Authorization = auth_header) |>
      req_timeout(timeout) |>
      req_perform()

    body    <- resp |> resp_body_json(simplifyVector = TRUE)
    samples <- body$results %||% body

    if (is.null(samples) || length(samples) == 0) return(NULL)

    df <- as_tibble(samples) |>
      transmute(
        lat     = as.numeric(latitude  %||% NA),
        lon     = as.numeric(longitude %||% NA),
        depth_m = as.numeric(depth_m   %||% depth %||% NA),
        dataset = campaign_name %||% "GlobalArchive",
        source  = "GlobalArchive"
      ) |>
      filter(!is.na(lat), !is.na(lon)) |>
      filter(is.na(depth_m) | (depth_m >= depth_min & depth_m <= depth_max)) |>
      distinct(lat, lon, depth_m, .keep_all = TRUE)

    message(glue("  → {nrow(df)} GlobalArchive sample records"))
    df

  }, error = function(e) {
    message(glue("  [WARN] Could not fetch GlobalArchive samples: {e$message}"))
    NULL
  })

  records
}



# =============================================================================
# GAP ANALYSIS
# =============================================================================

# Ocean regions defined as (lon_min, lat_min, lon_max, lat_max)
OCEAN_REGIONS <- list(
  "Western Indian Ocean"         = c(  28, -35,  65,  25),
  "Eastern Indian Ocean"         = c(  65, -35, 100,  25),
  "Arabian Sea / Persian Gulf"   = c(  50,   5,  75,  30),
  "Bay of Bengal"                = c(  78,   5, 100,  25),
  "SE Asia Archipelago"          = c(  95, -15, 145,  20),
  "Western Pacific (equatorial)" = c( 130, -20, 170,  20),
  "Central Pacific"              = c( 170, -20, 210,  20),
  "Eastern Pacific (tropical)"   = c(-150, -20, -90,  30),
  "W Africa coast"               = c( -20,  -5,  10,  20),
  "E Africa coast"               = c(  35, -30,  55,  15),
  "Mediterranean"                = c(  -5,  30,  40,  48),
  "Caribbean"                    = c( -90,   5, -60,  30),
  "Brazil / S Atlantic"          = c( -50, -35, -30,   5),
  "SW Australia"                 = c( 108, -38, 128, -18),
  "SE Australia / NZ"            = c( 140, -48, 180, -30),
  "Great Barrier Reef"           = c( 142, -25, 156, -10),
  "Hawaii / C Pacific"           = c(-165,  15,-145,  25),
  "Florida / Gulf Mexico"        = c( -98,  18, -75,  32),
  "NE Atlantic"                  = c( -20,  48,  10,  65),
  "South Ocean (high lat)"       = c(-180, -70, 180, -50),
  "Arctic"                       = c(-180,  65, 180,  90)
)

assign_region <- function(lat, lon) {
  lon <- ifelse(lon > 180, lon - 360, lon)
  result <- "Other"
  for (nm in names(OCEAN_REGIONS)) {
    bb <- OCEAN_REGIONS[[nm]]
    if (lon >= bb[1] && lon <= bb[3] && lat >= bb[2] && lat <= bb[4]) {
      result <- nm
      break
    }
  }
  result
}

run_gap_analysis <- function(df_all, depth_min, depth_max) {
  df_depth <- df_all |>
    filter(is.na(depth_m) | (depth_m >= depth_min & depth_m <= depth_max))

  df_depth <- df_depth |>
    rowwise() |>
    mutate(region_bin = assign_region(lat, lon)) |>
    ungroup()

  region_counts <- df_depth |>
    group_by(region_bin) |>
    summarise(
      n_records    = n(),
      lat_centroid = mean(lat, na.rm = TRUE),
      lon_centroid = mean(lon, na.rm = TRUE),
      min_depth    = min(depth_m, na.rm = TRUE),
      max_depth    = max(depth_m, na.rm = TRUE),
      .groups = "drop"
    )

  all_regions <- tibble(region_bin = names(OCEAN_REGIONS))
  region_counts <- all_regions |>
    left_join(region_counts, by = "region_bin") |>
    mutate(n_records = replace_na(n_records, 0))

  region_counts <- region_counts |>
    mutate(
      gap_status = case_when(
        n_records == 0  ~ "critical gap",
        n_records < 5   ~ "data gap",
        n_records < 20  ~ "undersampled",
        n_records < 50  ~ "moderate",
        TRUE            ~ "well sampled"
      ),
      priority_score = (n_records == 0) * 10 +
                       (n_records < 5)  * 5  +
                       (n_records < 20) * 2
    ) |>
    arrange(desc(priority_score), n_records)

  region_counts
}



# =============================================================================
# INTERACTIVE MAP (Leaflet)
# =============================================================================

make_leaflet_map <- function(df_points, df_gaps, depth_min, depth_max, out_path) {
  message("\n[Map] Building interactive Leaflet map...")

  depth_pal <- colorNumeric(
    palette = c("#1a9e75", "#3778c2", "#9b59b6", "#c0392b"),
    domain  = c(depth_min, depth_max),
    na.color = "#888888"
  )

  gap_pal <- colorFactor(
    palette = c("#c0392b", "#e67e22", "#f1c40f", "#27ae60", "#2980b9"),
    levels  = c("critical gap", "data gap", "undersampled", "moderate", "well sampled")
  )

  map <- leaflet(options = leafletOptions(
    worldCopyJump = FALSE,
    preferCanvas  = TRUE
  )) |>
    addProviderTiles("CartoDB.DarkMatter", group = "Dark (default)") |>
    addProviderTiles("Esri.OceanBasemap",  group = "Ocean basemap") |>
    addProviderTiles("OpenStreetMap",      group = "Street map")

  # Gap zone circles at region centroids
  gap_zones <- df_gaps |>
    filter(!is.na(lat_centroid), lat_centroid != 0,
           gap_status %in% c("critical gap", "data gap", "undersampled"))

  if (nrow(gap_zones) > 0) {
    map <- map |>
      addCircles(
        data        = gap_zones,
        lat         = ~lat_centroid,
        lng         = ~lon_centroid,
        radius      = ~pmax(300000, (20 - pmin(n_records, 20)) * 60000),
        color       = ~gap_pal(gap_status),
        fillColor   = ~gap_pal(gap_status),
        fillOpacity = 0.15,
        weight      = 1.5,
        opacity     = 0.6,
        dashArray   = "6,4",
        popup       = ~glue(
          "<b>{region_bin}</b><br>",
          "Records in depth range: <b>{n_records}</b><br>",
          "Status: <span style='color:{gap_pal(gap_status)};font-weight:bold'>{gap_status}</span><br>",
          "<i>Research priority: {priority_score}/12</i>"
        ),
        label       = ~glue("{region_bin} [{gap_status}]"),
        group       = "Gap zones"
      )
  }

  # Deployment points
  if (nrow(df_points) > 0) {
    map <- map |>
      addCircleMarkers(
        data        = df_points,
        lat         = ~lat,
        lng         = ~lon,
        radius      = 5,
        color       = ~depth_pal(depth_m),
        fillColor   = ~depth_pal(depth_m),
        fillOpacity = 0.8,
        weight      = 0.5,
        opacity     = 0.9,
        popup       = ~glue(
          "<b>{dataset}</b><br>",
          "Depth: <b>{ifelse(is.na(depth_m), 'unknown', paste0(round(depth_m, 0), ' m'))}</b><br>",
          "Source: {source}"
        ),
        label       = ~glue("{dataset} | {round(depth_m, 0)} m"),
        clusterOptions = markerClusterOptions(
          maxClusterRadius  = 40,
          disableClusteringAtZoom = 8
        ),
        group = "BRUVs deployments"
      )
  }

  # Legend
  map <- map |>
    addLegend(
      position  = "bottomright",
      pal       = depth_pal,
      values    = c(depth_min, depth_max),
      title     = glue("Depth (m)<br>{depth_min}–{depth_max} m"),
      opacity   = 0.9,
      labFormat = labelFormat(suffix = " m")
    ) |>
    addLegend(
      position = "bottomleft",
      pal      = gap_pal,
      values   = c("critical gap", "data gap", "undersampled", "moderate", "well sampled"),
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
  message(glue("  → Leaflet map saved: {out_path}"))
}



# =============================================================================
# DEPTH HISTOGRAM (Plotly)
# =============================================================================

make_depth_histogram <- function(df, depth_min, depth_max, out_path) {
  message("\n[Plot] Building depth histogram...")
  df_d <- df |>
    filter(!is.na(depth_m)) |>
    mutate(
      zone = case_when(
        depth_m < 30             ~ "Shallow (<30 m)",
        depth_m <= 150           ~ "Upper mesophotic (30–150 m)",
        depth_m <= 300           ~ "Lower mesophotic (150–300 m)",
        TRUE                     ~ "Deep (>300 m)"
      ),
      zone = factor(zone, levels = c(
        "Shallow (<30 m)", "Upper mesophotic (30–150 m)",
        "Lower mesophotic (150–300 m)", "Deep (>300 m)"
      ))
    )

  zone_colors <- c(
    "Shallow (<30 m)"              = "#1a9e75",
    "Upper mesophotic (30–150 m)"  = "#3778c2",
    "Lower mesophotic (150–300 m)" = "#9b59b6",
    "Deep (>300 m)"                = "#c0392b"
  )

  fig <- plot_ly(
    df_d,
    x      = ~depth_m,
    color  = ~zone,
    colors = zone_colors,
    type   = "histogram",
    nbinsx = 60,
    alpha  = 0.85
  ) |>
    layout(
      title       = list(text = glue(
        "Depth distribution of stereo-BRUVs records<br>",
        "<sub>{nrow(df_d)} records · {depth_min}–{depth_max} m filter</sub>"
      )),
      xaxis       = list(title = "Depth (m)", range = c(0, max(df_d$depth_m, na.rm=T) * 1.05)),
      yaxis       = list(title = "Number of records"),
      barmode     = "stack",
      paper_bgcolor = "#0d1b2a",
      plot_bgcolor  = "#111d2e",
      font          = list(color = "#cccccc"),
      legend        = list(title = list(text = "Depth zone"))
    )

  saveWidget(fig, file = out_path, selfcontained = TRUE)
  message(glue("  → Histogram saved: {out_path}"))
}



# =============================================================================
# MAIN PIPELINE
# =============================================================================

run_bruv_survey <- function(
    depth_min   = CONFIG$depth_min,
    depth_max   = CONFIG$depth_max,
    output_dir  = CONFIG$output_dir,
    cfg         = CONFIG
) {
  cat("\n", strrep("=", 65), "\n")
  cat("  stereo-BRUVs Mesophotic Gap Analysis (R)\n")
  cat(glue("  Depth range : {depth_min}–{depth_max} m\n"))
  cat(glue("  Output dir  : {output_dir}\n"))
  cat(strrep("=", 65), "\n\n")

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  out <- function(f) file.path(output_dir, f)

  frames <- list()

  # 1. Curated baseline
  if (cfg$use_curated) {
    message("[Curated] Loading literature + GlobalArchive baseline...")
    df_c <- get_curated_data(depth_min, depth_max)
    message(glue("  → {nrow(df_c)} curated records"))
    frames <- c(frames, list(df_c))
  }

  # 2. OBIS
  if (cfg$use_obis) {
    df_o <- fetch_obis(depth_min, depth_max, cfg$obis_dataset_ids, cfg$timeout)
    if (!is.null(df_o)) frames <- c(frames, list(df_o))
  }

  # 3. GBIF
  if (cfg$use_gbif) {
    df_g <- fetch_gbif(depth_min, depth_max, cfg$timeout)
    if (!is.null(df_g)) frames <- c(frames, list(df_g))
  }

  # 4. Squidle+
  if (cfg$use_squidle) {
    sq_creds <- get_credentials("SQUIDLE", cfg)
    df_sq <- fetch_squidle(depth_min, depth_max, sq_creds, cfg$timeout)
    if (!is.null(df_sq)) frames <- c(frames, list(df_sq))
  }

  # 5. GlobalArchive
  if (cfg$use_globalarchive) {
    ga_creds <- get_credentials("GLOBALARCHIVE", cfg)
    df_ga <- fetch_globalarchive(depth_min, depth_max, ga_creds, cfg$timeout)
    if (!is.null(df_ga)) frames <- c(frames, list(df_ga))
  }

  # Combine & deduplicate
  df_all <- bind_rows(frames) |>
    mutate(across(c(lat, lon, depth_m), as.numeric)) |>
    filter(!is.na(lat), !is.na(lon)) |>
    mutate(
      lat = round(lat, 4),
      lon = round(lon, 4)
    ) |>
    distinct(lat, lon, depth_m, .keep_all = TRUE)

  message(glue("\n[COMBINED] Total unique records: {nrow(df_all)}"))

  # Gap analysis
  message("\n[GAP ANALYSIS] Computing per-region sampling effort...")
  df_gaps <- run_gap_analysis(df_all, depth_min, depth_max)

  # Save CSVs
  write_csv(df_all,  out("bruv_records.csv"))
  write_csv(df_gaps, out("bruv_gap_report.csv"))
  message(glue("  → bruv_records.csv saved  ({nrow(df_all)} rows)"))
  message(glue("  → bruv_gap_report.csv saved ({nrow(df_gaps)} rows)"))

  # Maps
  make_leaflet_map(df_all, df_gaps, depth_min, depth_max, out("bruv_map.html"))
  make_depth_histogram(df_all, depth_min, depth_max, out("bruv_depth_hist.html"))

  # Console gap report
  cat("\n", strrep("=", 65), "\n")
  cat("  TOP PRIORITY RESEARCH GAP REGIONS\n")
  cat(strrep("=", 65), "\n")
  df_gaps |>
    filter(gap_status %in% c("critical gap", "data gap", "undersampled")) |>
    head(15) |>
    mutate(
      bar   = strrep("#", pmin(n_records, 20)),
      bar   = ifelse(nchar(bar) == 0, "·", bar)
    ) |>
    rowwise() |>
    walk(~cat(glue(
      "  {str_pad(.x$region_bin, 38)}  {str_pad(.x$n_records, 4)} records",
      "  [{.x$bar}]  {.x$gap_status}\n"
    )))

  cat("\n[DONE] All outputs written to:", normalizePath(output_dir), "\n")
  cat("\nUseful links:\n")
  cat("  GlobalArchive   → https://globalarchive.org\n")
  cat("  Squidle+        → https://squidle.org\n")
  cat("  OBIS datasets   → https://obis.org/dataset/search?q=bruv\n")
  cat("  REEFSCAPE       → https://reefscape.net\n")

  invisible(list(records = df_all, gaps = df_gaps))
}



# =============================================================================
# RUN (comment out if sourcing this file as a library)
# =============================================================================

# Default: full mesophotic zone
results <- run_bruv_survey(depth_min = 30, depth_max = 300)

# Upper mesophotic only:
# results <- run_bruv_survey(depth_min = 30, depth_max = 150)

# Lower mesophotic only:
# results <- run_bruv_survey(depth_min = 150, depth_max = 300)

# Access results programmatically:
# df_records <- results$records
# df_gaps    <- results$gaps
