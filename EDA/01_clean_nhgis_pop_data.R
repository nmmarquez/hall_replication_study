rm(list=ls())
library(tidyverse)
library(ipumsr)



DF <- bind_rows(
    read_nhgis(
        "data/nhgis0020_csv.zip",
        data_layer = "nhgis0020_csv/nhgis0020_ds151_2000_tract.csv") %>%
        set_ipums_var_attributes(
            read_ipums_codebook(
                "data/nhgis0020_csv.zip",
                "nhgis0020_csv/nhgis0020_ds151_2000_tract_codebook.txt")) %>%
        mutate(Mexico = GWA068, White = G0S001, Black = G0S003, Year = 2000) %>%
        select(STATE, STATEA, COUNTYA, TRACTA, GISJOIN, Mexico, Black, White, Year),
    read_nhgis(
        "data/nhgis0020_csv.zip",
        data_layer = "nhgis0020_csv/nhgis0020_ds196_20095_2009_tract.csv") %>%
        set_ipums_var_attributes(
            read_ipums_codebook(
                "data/nhgis0020_csv.zip",
                "nhgis0020_csv/nhgis0020_ds196_20095_2009_tract_codebook.txt")) %>%
        mutate(Mexico = RVDE138, Black = RVRE002 + RVRE003 + RVRE004) %>%
        mutate(White = RVPE002 + RVPE003 + RVPE004, Year = 2009) %>%
        select(STATE, STATEA, COUNTYA, TRACTA, GISJOIN, Mexico, Black, White, Year),
    read_nhgis(
        "data/nhgis0020_csv.zip",
        data_layer = "nhgis0020_csv/nhgis0020_ds234_20175_2017_tract.csv") %>%
        set_ipums_var_attributes(
            read_ipums_codebook(
                "data/nhgis0020_csv.zip",
                "nhgis0020_csv/nhgis0020_ds234_20175_2017_tract_codebook.txt")) %>%
        mutate(Mexico = AH9BE139, Black = AH9UE002 + AH9UE003 + AH9UE004) %>%
        mutate(White = AH9SE002 + AH9SE003 + AH9SE004, Year = 2017) %>%
        select(STATE, STATEA, COUNTYA, TRACTA, GISJOIN, Mexico, Black, White, Year))

DF00 <- read_nhgis(
    "data/nhgis0020_csv.zip",
    data_layer = "nhgis0020_csv/nhgis0020_ds151_2000_tract.csv") %>%
    set_ipums_var_attributes(
        read_ipums_codebook(
            "data/nhgis0020_csv.zip",
            "nhgis0020_csv/nhgis0020_ds151_2000_tract_codebook.txt")) %>%
    mutate(Mexico = GWA068, White = G0S001, Black = G0S003) %>%
    select(-tidyr::starts_with("GWA"), -tidyr::starts_with("G0S"))

DF07 <- read_nhgis(
    "data/nhgis0020_csv.zip",
    data_layer = "nhgis0020_csv/nhgis0020_ds196_20095_2009_tract.csv") %>%
    set_ipums_var_attributes(
        read_ipums_codebook(
            "data/nhgis0020_csv.zip",
            "nhgis0020_csv/nhgis0020_ds196_20095_2009_tract_codebook.txt"))

DF15 <- read_nhgis(
    "data/nhgis0020_csv.zip",
    data_layer = "nhgis0020_csv/nhgis0020_ds234_20175_2017_tract.csv") %>%
    set_ipums_var_attributes(
        read_ipums_codebook(
            "data/nhgis0020_csv.zip",
            "nhgis0020_csv/nhgis0020_ds234_20175_2017_tract_codebook.txt"))

DF00 %>%
    mutate(Mexico = GWA068, White = G0S001, Black = G0S003) %>%
    select(STATE, STATEA, COUNTYA, TRACTA, GISJOIN, Mexico, Black, White)

DF07 %>%
    mutate(Mexico = RVDE138, Black = RVRE002 + RVRE003 + RVRE004) %>%
    mutate(White = RVPE002 + RVPE003 + RVPE004) %>%
    select(STATE, STATEA, COUNTYA, TRACTA, GISJOIN, Mexico, Black, White)

DF15 %>%
    mutate(Mexico = AH9BE139, Black = AH9UE002 + AH9UE003 + AH9UE004) %>%
    mutate(White = AH9SE002 + AH9SE003 + AH9SE004) %>%
    select(STATE, STATEA, COUNTYA, TRACTA, GISJOIN, Mexico, Black, White)
