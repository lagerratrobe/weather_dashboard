library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(stringr, warn.conflicts = FALSE)

# Read in the entire data and convert obsTimeLocal to POSIXct data type
getData <- function() {
  df <- readRDS(
    url("https://github.com/lagerratrobe/weather_station/raw/main/Data/station_obs.RDS")
           ) |>
    mutate(stationID,
           "Time" = lubridate::parse_date_time(
             obsTimeLocal,
             "Ymd HMS",
             tz = "UTC",
             truncated = 0,
             quiet = FALSE,
             exact = FALSE) ,
           "Temperature" = imperial.temp,
           "Precip" = imperial.precipTotal,
           "SolarWatts" = solarRadiation,
           "Humidity" = humidity,
           "Pressure" = imperial.pressure,
           .keep = "none")
  return(df)
}

cleanDupeTimes <- function(df) {
  # Groups the data by station, date and hour
  # Removes extra midnight entry in Dallas data
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
    select(-`Date`, -`Hour`) -> df
  return(df)
}


getPlot <- function(weather_data, weather_variable, city_name) {
    city_name <- str_split_1(city_name, ",")[1]
    if (nrow(weather_data) < 48) {
      return(NULL)
    } else if (weather_variable == "Temperature") {
    title_string <- sprintf("Last 48 Hours of %s Temperature", city_name)
    # Useful vars to use in plotting
    max_temp = max(weather_data[[weather_variable]])
    min_temp = min(weather_data[[weather_variable]])
    max_temp_time = weather_data$Time[24]
    min_temp_time = weather_data$Time[44] # hard-code to lower left
    time_now = weather_data$Time[2] # Back up one to move label left
    current_temp = weather_data[[weather_variable]][1]
    midnight = getMidnight(weather_data)
    
    plot = ggplot(weather_data, mapping = aes(x=Time, y=.data[[weather_variable]])) + 
      geom_line() +
      # Max temp line
      geom_hline(yintercept=max(max_temp),color="red") +
      annotate("text",
               x=max_temp_time ,
               y=max_temp + .5,
               label=sprintf("High = %s deg F", max_temp),
               color="red") +
      # Min temp line
      geom_hline(yintercept=max(min_temp),color="blue") +
      annotate("text",
               x=min_temp_time,
               y=min_temp + .5,
               label=sprintf("Low = %s deg F", min_temp),
               color="blue") +
      # Current Temp 
      annotate("text",
               x=time_now,
               y=current_temp + .5,
               label=sprintf("Now = %d deg", current_temp),
               color="darkgreen") +
      # Midnight times
      geom_vline(xintercept=midnight,color="grey35") +
      annotate("text",
               x=midnight ,
               y=max_temp * .98,
               label="Midnight",
               color="grey35") +
      # Plot title and axis labels
      ggtitle(title_string) +
      theme(plot.title = element_text(hjust = 0.5, size = 28)) +
      labs(x = "Time",
           y = "Temperature")
  } else if (weather_variable == "Precip") {
    title_string <- sprintf("Last 48 Hours of %s Precip", city_name)
    total_precip = getTotalPrecip(weather_data)[[1]]
    time_midpoint = weather_data$Time[24]
    midnight = getMidnight(weather_data)
    plot = ggplot(weather_data, mapping = aes(x=Time, y=.data[[weather_variable]])) +
      geom_line() +
      # Plot Title
      ggtitle(title_string) +
      theme(plot.title = element_text(hjust = 0.5, size = 28)) +
      # Total Precip label
      annotate("text",
               x=time_midpoint,
               y=total_precip * .9,
               label= sprintf("%.2f\" precip total", total_precip),
               color="blue") +
      # Midnight labels
      geom_vline(xintercept=midnight,color="grey35") +
      annotate("text",
               x=midnight ,
               y=total_precip,
               label="Midnight",
               color="grey35")
  }
  
  return(plot)
}

getTotalPrecip <- function(df) {
  df |> 
    group_by(day(Time)) |> 
    summarise(`max_precip` = max(Precip)) |> 
    summarise(total_precip = sum(max_precip)) -> total_precip
  
  return(total_precip)
}

getMidnight <- function(weather_data) {
  midnights = weather_data$Time[hour(weather_data$Time) == 0]
  return(midnights)
}
