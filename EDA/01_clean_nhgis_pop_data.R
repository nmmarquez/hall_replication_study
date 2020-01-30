rm(list=ls())
library(tidyverse)
library(ipumsr)
library(tidycensus)
library(haven)

msaCountyDF <- read_csv(
    "https://data.nber.org/cbsa-csa-fips-county-crosswalk/cbsa2fipsxw.csv") %>%
    filter(!is.na(fipscountycode) & !is.na(cbsacode)) %>%
    rename(countyname = countycountyequivalent) %>%
    select(cbsacode, cbsatitle, fipsstatecode, fipscountycode, countyname) %>%
    rename(STATEFIPS = fipsstatecode, COUNTYFIPS = fipscountycode)

historicalDF <- bind_rows(

    left_join(
        read_nhgis(
            "data/nhgis0031_csv.zip",
            data_layer = "nhgis0031_csv/nhgis0031_ds233_20175_2017_county.csv") %>%
            set_ipums_var_attributes(
                read_ipums_codebook(
                    "data/nhgis0031_csv.zip",
                    "nhgis0031_csv/nhgis0031_ds233_20175_2017_county_codebook.txt")
                ) %>%
            rename(
                Population = AHY1E001,
                STATEFIPS = STATEA,
                COUNTYFIPS = COUNTYA) %>%
            select(Population, STATEFIPS, COUNTYFIPS, YEAR),
        read_nhgis(
            "data/nhgis0031_csv.zip",
            data_layer = "nhgis0031_csv/nhgis0031_ds234_20175_2017_county.csv") %>%
            set_ipums_var_attributes(
                read_ipums_codebook(
                    "data/nhgis0031_csv.zip",
                    "nhgis0031_csv/nhgis0031_ds234_20175_2017_county_codebook.txt")
            ) %>%
            rename(MEXPOP = AH9BE139, STATEFIPS = STATEA, COUNTYFIPS = COUNTYA) %>%
            select(MEXPOP, STATEFIPS, COUNTYFIPS, YEAR)) %>%
        mutate(YEAR = "2017"),

    left_join(
        read_nhgis(
            "data/nhgis0031_csv.zip",
            data_layer = "nhgis0031_csv/nhgis0031_ds196_20095_2009_county.csv") %>%
            set_ipums_var_attributes(
                read_ipums_codebook(
                    "data/nhgis0031_csv.zip",
                    "nhgis0031_csv/nhgis0031_ds196_20095_2009_county_codebook.txt")
                ) %>%
            rename(MEXPOP = RVDE138, STATEFIPS = STATEA, COUNTYFIPS = COUNTYA) %>%
            select(MEXPOP, STATEFIPS, COUNTYFIPS, YEAR)
            ,
        read_nhgis(
            "data/nhgis0031_csv.zip",
            data_layer = "nhgis0031_csv/nhgis0031_ds195_20095_2009_county.csv") %>%
            set_ipums_var_attributes(
                read_ipums_codebook(
                    "data/nhgis0031_csv.zip",
                    "nhgis0031_csv/nhgis0031_ds195_20095_2009_county_codebook.txt")
                ) %>%
            rename(
                Population = RK9E001,
                STATEFIPS = STATEA,
                COUNTYFIPS = COUNTYA) %>%
            select(Population, STATEFIPS, COUNTYFIPS, YEAR)) %>%
        mutate(YEAR = "2009"),

    read_nhgis(
        "data/nhgis0031_csv.zip",
        data_layer = "nhgis0031_csv/nhgis0031_ts_nominal_county.csv") %>%
        set_ipums_var_attributes(
            read_ipums_codebook(
                "data/nhgis0031_csv.zip",
                "nhgis0031_csv/nhgis0031_ts_nominal_county_codebook.txt")) %>%
        rename(Population = B78AA, MEXPOP = AB9AV) %>%
        rename(STATEFIPS = STATEFP, COUNTYFIPS = COUNTYFP) %>%
        select(Population, STATEFIPS, COUNTYFIPS, YEAR, MEXPOP) %>%
        filter(YEAR %in% c("1980", "1990", "2000"))) %>%
    mutate(YEAR = as.numeric(YEAR)) %>%
    left_join(msaCountyDF)



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

# we define Established Mexican destinations as metropolitan areas in which the 
# Mexican immigrant percent of the total population surpassed the national 
# average in every census year 1980–2009.

avgYearlyDF <- historicalDF %>%
    group_by(YEAR) %>%
    summarize(
        Population = sum(Population),
        MEXPOP = sum(MEXPOP, na.rm = TRUE)) %>%
    mutate(pMEX = MEXPOP / Population) %>%
    select(YEAR, pMEX)

establishedDF <- historicalDF %>%
    filter(YEAR != 2017) %>%
    group_by(cbsacode, cbsatitle, YEAR) %>%
    summarize(
        Population = sum(Population),
        MEXPOP = sum(MEXPOP, na.rm = TRUE)) %>%
    mutate(pMEXMetro = MEXPOP / Population) %>%
    left_join(avgYearlyDF, by = "YEAR") %>%
    summarize(
        Established = all(pMEXMetro >= pMEX)) %>%
    left_join(
        historicalDF %>%
            group_by(cbsacode, cbsatitle, YEAR) %>%
            summarize(
                Population = sum(Population),
                MEXPOP = sum(MEXPOP, na.rm = TRUE)) %>%
            mutate(pMEXMetro = MEXPOP / Population) %>%
            left_join(avgYearlyDF, by = "YEAR") %>%
            summarize(
                Established2017 = all(pMEXMetro >= pMEX)), 
        by = c("cbsacode", "cbsatitle")) %>%
    left_join(
        historicalDF %>%
            filter(YEAR != 1980) %>%
            group_by(cbsacode, cbsatitle, YEAR) %>%
            summarize(
                Population = sum(Population),
                MEXPOP = sum(MEXPOP, na.rm = TRUE)) %>%
            mutate(pMEXMetro = MEXPOP / Population) %>%
            left_join(avgYearlyDF, by = "YEAR") %>%
            summarize(
                newEstablished = all(pMEXMetro >= pMEX)),
        by = c("cbsacode", "cbsatitle"))

# did any locations fall out of established status?
establishedDF %>%
    filter(Established2017 != Established)

# did any locations gain established status?
establishedDF %>%
    filter(newEstablished != Established)

# New destinations are metropolitan areas with a smaller percent Mexican
# immigrant than the national average in 1980 and that met one of the following
# two sets of criteria:
# (1) contained at least 10,000 Mexican immigrants,
# whose share of the total population exceeded the national average in 05–09, 
# and growth of the Mexican-immigrant population during the 80s, 90s, or 00s 
# that was at least 1.5 times the national average; or 
# (2) had at least 5000 Mexican immigrants in 2005–2009 and 
# Mexican immigrant growth rates during the 1980s, 1990, or 2000s 
# that were at least 3 times the national average