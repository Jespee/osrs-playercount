#!/usr/bin/env Rscript

suppressPackageStartupMessages({
    library(readr)
    library(dplyr)
    library(lubridate)
})

infile <- "data/playercount.csv"
outdir <- "data/rollups"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

df <- read_csv(infile, show_col_types = FALSE) %>%
    mutate(timestamp = ymd_hms(timestamp, tz = "UTC"),
           players = as.numeric(players)) %>%
    filter(!is.na(timestamp), !is.na(players)) %>%
    arrange(timestamp)

rollup <- function(data, unit) {
    data %>%
        mutate(bucket = floor_date(timestamp, unit, week_start = 1)) %>%
        group_by(bucket) %>%
        summarize(
            avg_players = round(mean(players), 0),
            min_players = min(players),
            max_players = max(players),
            n_samples = n(),
            .groups = "drop"
        ) %>%
        mutate(bucket_utc = format(bucket, "%Y-%m-%dT%H:%M:%SZ")) %>%
        select(bucket_utc, avg_players, min_players, max_players, n_samples)
}

daily   <- rollup(df, "day")
weekly  <- rollup(df, "week")
monthly <- rollup(df, "month")
yearly  <- rollup(df, "year")

write_csv(daily, file.path(outdir, "daily.csv"))
write_csv(weekly, file.path(outdir, "weekly.csv"))
write_csv(monthly, file.path(outdir, "monthly.csv"))
write_csv(yearly, file.path(outdir, "yearly.csv"))

recent_5min <- df %>%
    filter(timestamp >= max(timestamp) - days(14)) %>%
    transmute(timestamp_utc = format(timestamp, "%Y-%m-%dT%H:%M:%SZ"),
              players = players)

write_csv(recent_5min, file.path(outdir, "recent_5min.csv"))
