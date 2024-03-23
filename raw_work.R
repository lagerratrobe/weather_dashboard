# raw_work.R

library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(data.table)
source("utils.R")

df <- getData()

dallas_station <- "KTXDALLA724"

df |> filter(stationID == dallas_station) -> df

df |> arrange(Time) -> df

df |> tail(100)

df |> group_by( stationID,
                Date = date(Time),
                Hour = hour(Time)
                ) |> 
  summarise(Temperature = mean(Temperature, na.rm = TRUE),
            Precip = mean(Precip, na.rm = TRUE),
            SolarWatts = mean(SolarWatts, na.rm = TRUE),
            Humidity = mean(Humidity, na.rm = TRUE),
            Pressure = mean(Pressure, na.rm = TRUE),
            .groups = "drop"
            ) |> 
  mutate(Time = ymd_hm( sprintf("%s %s:00", Date, Hour) )
         ) |> 
  arrange(Time) |>
  select(-`Date`, -`Hour`) 
