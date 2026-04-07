# This script can read in the raw data files from both EMS and TOMST dendrometers (Mini32
# and lolly) rename columns by their tree ID and QA/QC the data using treenetproc. You can
# further visualize the data, export graphs using treenetproc. Further down, you can look
# at individual trees, or look at graphs by species, and other interesting groupings
# and finally calculate and plot daily values that have been zeroed at midnight

# This script is specifically for looking at PJ data
# written by: Rachael Auer

# Edited by Derek Kober July 20, 2025.
## removed tower data lines, and EMS lines.
## made daily pattern code at the end faster
## cleaned up some names
## works as of July 20, 2025. Able to run all code at once without issues.

# clears work space
rm(list = ls(all = TRUE))

# read in important libraries
library(treenetproc)
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(myClim)
library(tidyverse)
library(readxl)
library(purrr)
library(cowplot)
library(RColorBrewer)
library(ggpubr)
library(beepr)
library(janitor)
library(testit)
library(here)

########### TOMST file cleaning################
# TOMST data needs to be compiled manually in R studio - SEE BELOW EMS SECTION TO IMPORT TOMST DATA

### for TOMST files that are all in one folder on the Synology drive and are the raw .xlsx files from download #######
##### this section will read in the .xlsx files from the folder and convert them into dataframes that can be used
# by the below program and combine them together and name the columns by their tree ID

# CHANGE HERE- Set the path to your folder containing the Excel files
folder_path <- here("data/raw/tomst/MPJ/2025/03.12.26") # I want to change this so it can just download the latest from the synology drive -DK 02/11/26

# read in serial number and name files - DON'T CHANGE UNLESS YOU HAVE TO
sn_tbl <- read.csv(here("data/raw/tomst/MPJ/tomst_sn_names.csv"), sep = ",", stringsAsFactors = FALSE)

as.list(sn_tbl)

# List all data files in the folder
# there are "command" files that i am unsure of they do so for now, we're only looking at the data files
# REGEX = ^data.*\\.csv$
# -DK
file_list <- list.files(folder_path, pattern = "^data.*\\.csv$", full.names = TRUE)

# create null object to be filled with the data
tomst <- NULL

# Helper: extract SN from filename (same logic you had)
get_sn <- function(path) {
  tools::file_path_sans_ext(basename(path)) |> substr(6, 13)
}

# Helper: SN -> tree id lookup (no more character(0))
get_tree_id <- function(sn, names_tbl) {
  id <- names_tbl |>
    filter(SN == sn) |>
    pull(2) |>
    first()
  if (is.na(id) || length(id) == 0) {
    return(NA_character_)
  }
  as.character(id)
}

# Read + clean one TOMST file -> tibble(ts, <tree>_temp, <tree>_value)
read_one_tomst <- function(path, names_tbl) {
  sn <- get_sn(path)
  tree_id <- get_tree_id(sn, names_tbl)

  # If we can't map SN -> tree ID, skip this file (and warn)
  if (is.na(tree_id)) {
    warning("Skipping file (no SN match in sn_tbl): ", basename(path), "  SN=", sn)
    return(NULL)
  }

  dat <- read.csv(path, header = FALSE, stringsAsFactors = FALSE) |>
    # split the packed first column into fields
    separate(
      col = 1,
      into = c("index", "ts", "tz", "temp", "coor1", "coor2", "value", "coor3", "qf"),
      sep = ";",
      remove = TRUE
    ) |>
    transmute(
      ts,
      temp  = suppressWarnings(as.numeric(temp)),
      value = suppressWarnings(as.numeric(value))
    ) |>
    arrange(ts) |>
    distinct(ts, .keep_all = TRUE) |>
    mutate(
      ts = ymd_hm(ts, tz = "MST"),
      tree_id = tree_id
    )

  dat
}

# Build the full dataset
tomst <- map_dfr(file_list, read_one_tomst, names_tbl = sn_tbl) |>
  compact() |>
  arrange(ts)

tomst_clean0 <- tomst

# Join install dates onto each record
tomst_clean1 <- tomst_clean0 %>%
  left_join(sn_tbl %>% select(tree_id, install_date), by = "tree_id") |>
  mutate(install_date = mdy(install_date)) |>
  rename(series = tree_id)

# Drop trees with missing install_date (or keep and warn)
missing_installs <- tomst_clean1 %>%
  filter(is.na(install_date)) %>%
  distinct(series) %>%
  pull(series)

if (length(missing_installs) > 0) {
  warning("Missing install_date for: ", paste(missing_installs, collapse = ", "))
}

# Keep only data after install_date; zero the "value" series per tree
tomstadj <- tomst_clean1 %>%
  filter(!is.na(install_date), ts >= install_date) %>%
  group_by(series) %>%
  mutate(
    ts = format(ts, "%Y-%m-%d %H:%M:%S"), # because treenetproc hates lubridate and joy
    baseline = first(na.omit(value)),
    value = value - baseline
  ) %>%
  ungroup()

# treenetproc is picky and not designed well, like why do i have to separate temperature data from the value data.
tomst_value <- tomstadj |>
  select(ts, value, series)

tomst_temperature <- tomstadj |>
  select(ts, temp, series) |>
  rename(value = temp) # treenetproc being picky

tomst_value_L1 <- proc_L1(
  data_L0 = tomst_value,
  reso = 15,
  tz = "MST"
)

tomst_temperature_L1 <- proc_L1(
  data_L0 = tomst_temperature,
  reso = 15,
  tz = "MST"
)

series_ids <- unique(tomst_value_L1$series)

multi_proc_dendro_L2 <- function(s) {
  dendro_s <- tomst_value_L1 %>% filter(series == s)
  temp_s <- tomst_temperature_L1 %>% filter(series == s)

  # If something is missing, skip safely
  if (nrow(dendro_s) == 0 || nrow(temp_s) == 0) {
    return(NULL)
  }

  proc_dendro_L2(
    dendro_L1 = dendro_s,
    temp_L1   = temp_s, # now a single series -> no temp_ref length mismatch
    plot      = TRUE, # set TRUE if you really want plots (it'll make one per series)
    plot_name = paste0("output/plots/", s),
    tz        = "MST"
  )
}

dendro_data_L2 <- map_dfr(series_ids, multi_proc_dendro_L2)

pj_dendro_all <- dendro_data_L2

write.csv(pj_dendro_all, file = here("data/processed/pj_dendro.csv"))


for (tree in unique(dendro_data_L2$series)) {
  dat <- dendro_data_L2 |> filter(series == tree)
  p <- ggplot(dat, aes(x = ts, y = twd)) +
    geom_point() +
    ggtitle(tree)

  print(p)
}


#### This section is for daily zeroed values and should be run manually because the
# calculations take around 10 minutes (or more!) - only rerun when you want new updated plots!
# alternatively, you can read in the previous files to make plots with the data (below)
# gs_dendro_day <- read.csv("./Compiled Data/MPJ/PJ_DENDRO_daily_zero_means.csv", sep=',', fill=TRUE, stringsAsFactors=FALSE)
# dendro_day_zero <- read.csv("./Compiled Data/MPJ/PJ_DENDRO_daily_zero_means.csv", sep=',', fill=TRUE, stringsAsFactors=FALSE)

all_dendro_2025 <- all_dendro_2025 %>%
  clean_names()

# Create series list
series_list <- as.list(unique(all_dendro_2025$series))

# Create day-zero adjusted data
dendro_day_zero <- all_dendro_2025 |>
  group_by(series, date) |>
  arrange(series, date, .by_group = TRUE) |>
  mutate(
    day_zero_value = value - first(value),
    day_zero_change = day_zero_value - lag(day_zero_value)
  ) |>
  ungroup()

# Subset to growing season (March–August), add month and hour
gs_dendro_day <- dendro_day_zero |>
  mutate(
    month = month(date),
    hour = hour(ts)
  ) |>
  filter(month %in% 4:8)

ci <- function(x, conf = 0.95, na.rm = TRUE) {
  if (na.rm) x <- na.omit(x)
  n <- length(x)
  se <- sd(x) / sqrt(n)
  mean_x <- mean(x)
  error <- qt(conf / 2 + 0.5, df = n - 1) * se
  c(mean = mean_x, lower = mean_x - error, upper = mean_x + error)
}

gs_dendro_day_sum <- gs_dendro_day |>
  group_by(species, dendro, hour) |>
  summarize(
    species_hour_mean = mean(day_zero_change, na.rm = TRUE),
    species_hour_sd = sd(day_zero_change, na.rm = TRUE),
    species_hour_low_ci = ci(day_zero_change)[2],
    species_hour_high_ci = ci(day_zero_change)[3],
    .groups = "drop"
  )

# Replace NaNs with 0 (for nicer plots)
gs_dendro_day_sum$species_hour_mean[is.nan(gs_dendro_day_sum$species_hour_mean)] <- 0

# Plot: hourly jittered changes
ggplot(gs_dendro_day, aes(x = hour, y = day_zero_change, color = species)) +
  geom_jitter() +
  geom_hline(yintercept = 0, color = "black", size = 1.7) +
  geom_hline(yintercept = 0, color = "white", size = 1) +
  labs(y = "Hourly changes", title = "PJ all species × dendro type facet") +
  facet_wrap(dendro ~ species, scales = "free", ncol = 2) +
  theme_pubr()
ggsave("./output/plots/pj_daily_zero_hour_changes.png")

# Plot: mean ± CI of hourly changes
ggplot(gs_dendro_day_sum, aes(x = hour, y = species_hour_mean, color = species)) +
  geom_point(aes(size = 2)) +
  geom_line(aes(group = species, linewidth = 1.5)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_errorbar(aes(ymin = species_hour_low_ci, ymax = species_hour_high_ci), width = 0.2, linewidth = 1) +
  facet_wrap(~dendro, scales = "free") +
  theme_pubr()
ggsave("./output/plots/pj_daily_zero_means_free_scale.png")

# Save data to CSV
write.csv(gs_dendro_day, file = "./data/processed/pj_dendro_daily_zero_means_1.csv", row.names = FALSE)
write.csv(gs_dendro_day_sum, file = "./data/processed/pj_dendro_daily_zero_means_summary_1.csv", row.names = FALSE)
write.csv(dendro_day_zero, file = "./data/processed/pj_dendro_daily_zero_changes_1.csv", row.names = FALSE)

library(ggplot2)

ggplot(gs_dendro_day_sum, aes(x = hour, y = species_hour_mean, color = species)) +
  geom_line(size = 1) +
  geom_ribbon(
    aes(
      ymin = species_hour_low_ci,
      ymax = species_hour_high_ci,
      fill = species
    ),
    alpha = 0.2, color = NA
  ) +
  facet_wrap(~dendro) +
  labs(
    title = "Hourly Dendrometer Signal",
    x = "Hour of Day",
    y = "Mean TWD"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

ggplot(gs_dendro_day %>% filter(series %in% c("J20.value", "J21.value", "J22.value", "J23.value", "J24.value", "J25.value")), aes(date, day_zero_change, color = series)) +
  geom_point() +
  facet_wrap(~series)
