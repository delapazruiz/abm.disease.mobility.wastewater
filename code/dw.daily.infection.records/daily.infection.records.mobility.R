#Load libraries
library(dplyr)
library(tidyr)
library(readr)
library(tibble)
library(ggplot2)
library(lubridate)
library(xts)

#Load SMS-ABM Netlogo outcomes ---- 
hourly.house.mobility <- read_csv(
  "results/infectionmobilityathome.csv",
  col_names = c(
    "date.time",
    "people.athome",
    "seed",
    "run",
    "exp"))

# Plotting infection: Stool -----
hourly.house.mobility$date.time <- parse_date_time(
  hourly.house.mobility$date.time, orders = c("%Y-%m-%d"))

hourly.house.mobility <- hourly.house.mobility|>
  filter(
    date.time <= as.POSIXct("2022-04-05", tz='UTC') &
      date.time >= as.POSIXct("2022-03-21", tz='UTC'))

hourly.house.mobility$date.time |> unique()

hourly.house.mobility
