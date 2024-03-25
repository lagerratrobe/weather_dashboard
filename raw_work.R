# raw_work.R
library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(data.table)
source("utils.R")


station_vars <- list(
  "Alameda, CA" = "KCAALAME63",
  "Albany, NY" = "KNYALBAN124",
  "Antioch, TN" = "KTNANTIO26",
  "Dallas, TX" = "KTXDALLA872",
  "Houston, TX" = "KTXHOUST3329",
  "Roanoke, VA" = "KVAROANO253",
  "Seattle, WA" = "KWASEATT2743", 
  "Sequim, WA" = "KWASEQUI431", 
  "Montreal, CAN" = "IWESTMOU2"
)
#system.time({
  df <- getData()
  #df <- cleanDupeTimes(df)
  data <- filter(df, stationID == station_vars["Seattle, WA"]) |>
    arrange(desc(Time)) |> 
    head(n=48)
  getPlot( data, "Temperature", "Seattle" )
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
