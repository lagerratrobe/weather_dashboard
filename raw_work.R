# raw_work.R
library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(data.table)
source("utils.R")


# Test 1 - get just the data for 1 station and compare dupe vs deduped times

#system.time({
  df <- getData()
  #df <- cleanDupeTimes(df)
  data <- filter(df, stationID == "KCAALAME63") |>
    arrange(desc(Time)) |> 
    head(n=48)
  getPlot( data, "Temperature", "Alameda" )
  DT::datatable(data)
#})


# Test 2 - Time the entire operation, data fetch plus plot and table generation
library(profvis)
profvis({
  df <- getData()
  df <- cleanDupeTimes(df)
  dallas_station <- "KTXDALLA724"
  data <- filter(df, stationID == dallas_station) |>
    arrange(desc(Time)) |> 
    head(n=48)
 getPlot( data, "Temperature", "Dallas" )
 DT::datatable(data)
})
