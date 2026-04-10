#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(readr)
  library(lubridate)
  library(tidyr)
  library(ggplot2)
})

source(here("code/supp/mpj_time_utils.R"))

args <- commandArgs(trailingOnly = TRUE)
input_path <- if (length(args) >= 1) args[[1]] else "data/processed/pj_dendro.csv"
output_path <- if (length(args) >= 2) args[[2]] else "data/processed/pj_dendro_twdnorm_daily.csv"
tz_local <- if (length(args) >= 3) args[[3]] else MPJ_FIXED_TZ
plot_dir <- if (length(args) >= 4) args[[4]] else file.path(dirname(output_path), "plots")

stop_if_missing <- function(path) {
  if (!file.exists(path)) {
    stop("Input file not found: ", path, call. = FALSE)
  }
}

pick_col <- function(nms, candidates, label) {
  hit <- intersect(candidates, nms)
  if (length(hit) == 0) {
    stop("Missing required column for ", label, ". Tried: ", paste(candidates, collapse = ", "), call. = FALSE)
  }
  hit[[1]]
}

parse_ts <- function(x, tz_local) {
  parsed <- parse_mpj_wall_time(x, tz = tz_local)
  idx_na <- is.na(parsed)
  if (any(idx_na)) {
    parsed[idx_na] <- suppressWarnings(ymd(normalize_mpj_time_string(x[idx_na]), tz = tz_local, quiet = TRUE))
  }
  parsed
}

stop_if_missing(input_path)

raw <- read_csv(input_path, show_col_types = FALSE)

# Remove an index column often written by base::write.csv
raw <- raw %>% select(-any_of(c("", "...1", "X1")))

series_col <- pick_col(names(raw), c("series", "tree_id", "tree", "id"), "tree/series")
ts_col <- pick_col(names(raw), c("ts", "timestamp", "datetime", "date_time", "Date", "date"), "timestamp")

# Build TWD if absent using available columns.
if (!"twd" %in% names(raw)) {
  if (all(c("max", "value") %in% names(raw))) {
    raw <- raw %>% mutate(twd = .data$max - .data$value)
  } else if ("value" %in% names(raw)) {
    raw <- raw %>%
      group_by(across(all_of(series_col))) %>%
      arrange(.data[[ts_col]], .by_group = TRUE) %>%
      mutate(twd = cummax(.data$value) - .data$value) %>%
      ungroup()
  } else {
    stop("Could not compute TWD. Need `twd` or enough data to derive it (`max`+`value` or `value`).", call. = FALSE)
  }
}

clean <- raw %>%
  mutate(
    .series = as.character(.data[[series_col]]),
    .ts = parse_ts(.data[[ts_col]], tz_local),
    twd = as.numeric(.data$twd)
  ) %>%
  filter(!is.na(.series), !is.na(.ts), !is.na(twd)) %>%
  mutate(date = as.Date(with_tz(.ts, tz = tz_local)))

if (nrow(clean) == 0) {
  stop("No usable rows after parsing `series`, `timestamp`, and `twd`.", call. = FALSE)
}

# Peters et al. (2025):
# TWDnorm = TWD_predawn / MDSmax
# MDSnorm = MDS / MDSmax
# with MDSmax as 99th percentile of daily MDS over the multiyear record.
daily <- clean %>%
  group_by(.series, date) %>%
  summarize(
    twd_predawn = min(twd, na.rm = TRUE),
    mds = max(twd, na.rm = TRUE),
    n_obs = dplyr::n(),
    .groups = "drop"
  ) %>%
  group_by(.series) %>%
  mutate(
    mds_max = quantile(mds, probs = 0.99, na.rm = TRUE, names = FALSE, type = 7),
    twd_norm = twd_predawn / mds_max,
    mds_norm = mds / mds_max,
    drought_stress_onset = twd_norm >= 1
  ) %>%
  ungroup() %>%
  rename(series = .series)

write_csv(daily, output_path)

dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

plot_daily <- daily %>%
  select(series, date, twd_predawn, twd_norm) %>%
  pivot_longer(
    cols = c(twd_predawn, twd_norm),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(metric, twd_predawn = "TWD (pre-dawn, raw)", twd_norm = "TWDnorm")
  )

timeseries_plot <- ggplot(plot_daily, aes(x = date, y = value, group = series)) +
  geom_line(alpha = 0.7, linewidth = 0.35, color = "#1f2937") +
  facet_grid(metric ~ series, scales = "free_y") +
  labs(
    title = "Daily TWD vs Normalized TWD",
    x = "Date",
    y = "Value"
  ) +
  theme_bw(base_size = 10) +
  theme(
    strip.background = element_rect(fill = "grey95", color = "grey80"),
    panel.grid.minor = element_blank()
  )

scatter_plot <- ggplot(daily, aes(x = twd_predawn, y = twd_norm, color = series)) +
  geom_point(alpha = 0.45, size = 0.9) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.7, color = "black") +
  labs(
    title = "TWD (pre-dawn) vs TWDnorm",
    x = "TWD (pre-dawn, raw units)",
    y = "TWDnorm"
  ) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "none")

timeseries_file <- file.path(plot_dir, "twd_vs_twdnorm_timeseries.png")
scatter_file <- file.path(plot_dir, "twd_vs_twdnorm_scatter.png")

ggsave(timeseries_file, timeseries_plot, width = 16, height = 8, dpi = 220)
ggsave(scatter_file, scatter_plot, width = 8.5, height = 6, dpi = 220)

message("Wrote normalized TWD daily table: ", output_path)
message("Wrote plot: ", timeseries_file)
message("Wrote plot: ", scatter_file)
message("Rows: ", nrow(daily), " | Trees: ", dplyr::n_distinct(daily$series))
