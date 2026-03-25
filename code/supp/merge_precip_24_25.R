# this is to combine the precip NMEG climate data from 2024 and 2025 and save in processed.
# You shouldn't have to run this often, currently both 2024 and 2025 are gapfilled and QCed

library(tidyverse)
library(here)

climate24_dat <- read_csv(here("data/raw/US-Mpj_2024_gapfilled.txt")) |>
  clean_names() |>
  slice(-1) |>
  mutate(
    timestamp_start = ymd_hm(timestamp_start),
    timestamp_end = ymd_hm(timestamp_end),
    across(-c(timestamp_start, timestamp_end), parse_number),
    across(where(is.numeric), ~ na_if(., -9999)),
    date = as_date(timestamp_start),
    hour = hour(timestamp_start),
    doy = yday(timestamp_start)
  ) |>
  select(
    # columns we want: timestamp_end, ta_f, p_f, netrad_f, vpd_f, date, hour, doy
    c(
      "timestamp_end",
      "ta_f",
      "p_f",
      "netrad_f",
      "vpd_f",
      "date",
      "hour",
      "doy"
    )
  ) |>
  rename(
    ts = timestamp_end,
    precip = p_f
  )

climate25_dat <- read_csv(here("data/raw/US-Mpj_2025_gapfilled.txt")) |>
  clean_names() |>
  slice(-1) |>
  mutate(
    timestamp_start = ymd_hm(timestamp_start),
    timestamp_end = ymd_hm(timestamp_end),
    across(-c(timestamp_start, timestamp_end), parse_number),
    across(where(is.numeric), ~ na_if(., -9999)),
    date = as_date(timestamp_start),
    hour = hour(timestamp_start),
    doy = yday(timestamp_start)
  ) |>
  select(
    # columns we want: timestamp_end, ta_f, p_f, netrad_f, vpd_f, date, hour, doy
    c(
      "timestamp_end",
      "ta_f",
      "p_f",
      "netrad_f",
      "vpd_f",
      "date",
      "hour",
      "doy"
    )
  ) |>
  rename(
    ts = timestamp_end,
    precip = p_f
  )


# for now i am just going to keep precip to see when there are dry periods, verifying twd

precip24_dat <- climate24_dat |>
  select(ts, precip)

precip25_dat <- climate25_dat |>
  select(ts, precip)

precip_24_25 <- bind_rows(precip24_dat, precip25_dat) |>
  arrange(ts)

summary(precip_24_25)

precip_24_25_daily <- precip_24_25 |>
  mutate(date = as.Date(ts)) |>
  group_by(date) |>
  summarize(precip_daily = sum(precip, na.rm = TRUE), .groups = "drop")

# 30min resolution
saveRDS(precip_24_25, file = here("data/processed/precip_24_25_30min.rds"))

# daily total
saveRDS(precip_24_25_daily, file = here("data/processed/precip_24_25_daily.rds"))
