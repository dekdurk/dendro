#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(lubridate)
})

MPJ_FIXED_TZ <- "Etc/GMT+7"
MPJ_SOLAR_TZ <- "America/Phoenix"
MPJ_TOMST_DEFAULT_OFFSET_QUARTERS <- -28

normalize_mpj_time_string <- function(x) {
  x_chr <- as.character(x)
  x_chr <- trimws(x_chr)
  x_chr[x_chr %in% c("", "NA")] <- NA_character_
  x_chr <- gsub("T", " ", x_chr, fixed = TRUE)
  sub("(\\.\\d+)?(Z|[+-][0-9]{2}:?[0-9]{2})$", "", x_chr, perl = TRUE)
}

parse_mpj_wall_time <- function(x, tz = MPJ_FIXED_TZ) {
  x_chr <- normalize_mpj_time_string(x)

  parsed <- suppressWarnings(ymd_hms(x_chr, tz = tz, quiet = TRUE))

  idx_na <- is.na(parsed)
  if (any(idx_na)) {
    parsed[idx_na] <- suppressWarnings(ymd_hm(x_chr[idx_na], tz = tz, quiet = TRUE))
  }

  idx_na <- is.na(parsed)
  if (any(idx_na)) {
    parsed[idx_na] <- suppressWarnings(dmy_hms(x_chr[idx_na], tz = tz, quiet = TRUE))
  }

  idx_na <- is.na(parsed)
  if (any(idx_na)) {
    parsed[idx_na] <- suppressWarnings(dmy_hm(x_chr[idx_na], tz = tz, quiet = TRUE))
  }

  idx_na <- is.na(parsed)
  if (any(idx_na)) {
    parsed[idx_na] <- suppressWarnings(mdy_hms(x_chr[idx_na], tz = tz, quiet = TRUE))
  }

  idx_na <- is.na(parsed)
  if (any(idx_na)) {
    parsed[idx_na] <- suppressWarnings(mdy_hm(x_chr[idx_na], tz = tz, quiet = TRUE))
  }

  parsed
}

parse_tomst_utc_timestamp <- function(
  ts_utc,
  tz_quarters = NULL,
  tz_local = MPJ_FIXED_TZ,
  default_tz_quarters = MPJ_TOMST_DEFAULT_OFFSET_QUARTERS
) {
  parsed_utc <- parse_mpj_wall_time(ts_utc, tz = "UTC")

  if (is.null(tz_quarters)) {
    return(with_tz(parsed_utc, tzone = tz_local))
  }

  tz_quarters_num <- suppressWarnings(as.numeric(tz_quarters))
  tz_quarters_num[is.na(tz_quarters_num)] <- default_tz_quarters

  out <- as.POSIXct(rep(NA_real_, length(parsed_utc)), origin = "1970-01-01", tz = tz_local)

  keep <- !is.na(parsed_utc)
  out[keep] <- force_tz(
    parsed_utc[keep] + minutes(tz_quarters_num[keep] * 15),
    tzone = tz_local
  )

  out
}

relabel_mpj_wall_time <- function(x, tz = MPJ_FIXED_TZ) {
  if (inherits(x, "POSIXt")) {
    return(force_tz(x, tzone = tz))
  }

  parse_mpj_wall_time(x, tz = tz)
}

to_mpj_local_time <- function(x, tz = MPJ_FIXED_TZ) {
  if (!inherits(x, "POSIXt")) {
    stop("to_mpj_local_time() requires a POSIXct/POSIXlt input.", call. = FALSE)
  }

  with_tz(x, tzone = tz)
}

format_mpj_wall_time <- function(x, tz = MPJ_FIXED_TZ) {
  local_x <- if (inherits(x, "POSIXt")) {
    with_tz(x, tzone = tz)
  } else {
    parse_mpj_wall_time(x, tz = tz)
  }

  format(local_x, "%Y-%m-%d %H:%M:%S", tz = tz)
}

format_mpj_datetime_cols <- function(data, tz = MPJ_FIXED_TZ) {
  out <- data
  datetime_cols <- names(out)[vapply(out, function(x) inherits(x, "POSIXt"), logical(1))]

  if (length(datetime_cols) == 0) {
    return(out)
  }

  for (nm in datetime_cols) {
    out[[nm]] <- format_mpj_wall_time(out[[nm]], tz = tz)
  }

  out
}
