library(shiny)
library(openmeteo)
library(httr)
library(jsonlite)
library(tidyverse)
library(leaflet)
library(DT)
library(tidygeocoder)
library(lubridate)
library(rsconnect)


# Define UI
ui <- fluidPage(
  titlePanel("Historical Weather Data Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Location input
      textInput("location_name", "Enter Location:", "Olneyville, RI, USA"),
      actionButton("geocode_btn", "Find Location", class = "btn-primary"),
      
      # Display coordinates
      verbatimTextOutput("coord_display"),
      
      hr(),
      
      # Date range input
      dateRangeInput("date_range", "Date Range:",
                     start = "2000-01-01",
                     end = Sys.Date(),
                     min = "1940-01-29",
                     max = Sys.Date()),
      
     
      
      
      
    # Weather variable selection
      # Daily variable selection (from the raw API data)
      selectInput("daily_var", "Daily Variable:",
                  choices = c("Temperature (Mean)" = "daily_temperature_2m_mean_f",
                              "Temperature (Max)" = "daily_temperature_2m_max_f",
                              "Temperature (Min)" = "daily_temperature_2m_min_f",
                              "Apparent Temperature (Mean)" = "daily_apparent_temperature_mean_f",
                              "Apparent Temperature (Max)" = "daily_apparent_temperature_max_f",
                              "Apparent Temperature (Min)" = "daily_apparent_temperature_min_f",
                              "Wind Speed (Max)" = "daily_wind_speed_10m_max",
                              "Precipitation" = "daily_precipitation_sum_in",
                              "Rainfall" = "daily_rain_sum_in",
                              "Snowfall" = "daily_snowfall_sum_in")),
      
      hr(),
      
      # Analysis type selection
      radioButtons("analysis_type", "Analysis Type:",
                   choices = c("Monthly Average" = "mean",
                               "Monthly Maximum" = "max",
                               "Monthly Minimum" = "min",
                               "Monthly Total" = "sum",
                               "Frequency (Count days meeting condition)" = "frequency"),
                   selected = "mean"),
      
      # Conditional panel for frequency options
      conditionalPanel(
        condition = "input.analysis_type == 'frequency'",
        selectInput("freq_operator", "Condition:",
                    choices = c("Greater than (>)" = "greater",
                                "Greater than or equal to (>=)" = "greater_equal",
                                "Less than (<)" = "less",
                                "Less than or equal to (<=)" = "less_equal",
                                "Equal to (=)" = "equal")),
        numericInput("freq_threshold", "Threshold Value:", value = 1, step = 0.1),
        helpText("Counts the number of days in each month that meet the condition")
      ),
      
      hr(),

      
      conditionalPanel(
        condition = "input.use_frequency == true",
        selectInput("freq_operator", "Condition:",
                    choices = c("Greater than (>)" = "greater",
                                "Greater than or equal to (>=)" = "greater_equal",
                                "Less than (<)" = "less",
                                "Less than or equal to (<=)" = "less_equal",
                                "Equal to (=)" = "equal")),
        numericInput("freq_threshold", "Threshold Value:", value = 1, step = 0.1),
        helpText("Counts the number of days in each month that meet the condition")
      ),
      
      hr(),
      
      # Yarn or bead color palette selection
      selectInput("color_palette", "Color Palette:",
                  choices = c("Yarn Colors (9 levels)" = "yarn",
                              "Bead Colors (reversed yarn)" = "bead",
                              "Six colors"="six",
                              "Six Colors (reversed)"="six_reverse",
                              "Blues" = "Blues",
                              "Reds" = "Reds",
                              "Greens" = "Greens",
                              "Purples" = "PuBu",
                              "Oranges" = "YlOrRd",
                              "Viridis" = "viridis")),
      
      # Year sequence interval
      numericInput("year_interval", "Year Interval for X-axis:",
                   value = 1, min = 1, max = 10, step = 1),
      
      # Plot title
      textInput("plot_title", "Plot Title:", "Weather Data Visualization"),
      
      actionButton("fetch_weather", "Generate Plot", class = "btn-success"),
      
      hr(),
      helpText("Data from OpenMeteo Historical Weather API")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        type = "tabs",
        tabPanel("Heatmap Plot", plotOutput("heatmap_plot", height = "600px")),
        tabPanel("Data Table", DTOutput("weather_table")),
        tabPanel("Summary Statistics", verbatimTextOutput("summary_stats")),
        tabPanel("Raw Data", DTOutput("raw_data"))
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Reactive values
  coords <- reactiveVal(NULL)
  weather_data <- reactiveVal(NULL)
  processed_data <- reactiveVal(NULL)
  
  # Pre-defined yarn and bead colors
  
  
  yarn_colors <- c("#313695FF", "#4575B4FF", 
                    "#74ADD1FF",  "#02818AFF", 
                     "#FFFFFFFF",  "#FEE090FF", 
                 "#FDAE61FF",  "#F46D43FF", 
                   "#A50026FF")
  
  bead_colors <- c( "#A50026FF",  "#F46D43FF", 
                    "#FDAE61FF",  "#FEE090FF", 
                     "#FFFFFFFF",  "#02818AFF", 
                  "#74ADD1FF", "#4575B4FF", 
                    "#313695FF")
  
  six_colors <- c(
    "#313695FF",
    "#02818AFF",
    "#FFFFFFFF",
    "#FEE090FF",
    "#FDAE61FF",
    "#A50026FF"
  )
  
  six_colors_reverse <- c("#A50026FF","#FDAE61FF","#FEE090FF", "#FFFFFFFF","#02818AFF","#313695FF" )
  
  
  custom_palettes <- list(
    yarn = yarn_colors,
    bead = bead_colors,
    six = six_colors,
    six_reverse = six_colors_reverse
  )
  
  
  # Geocoding function
  geocode_location <- function(address) {
    loc_table <- geo(address = address, method = "osm")
    if (!is.null(loc_table) && nrow(loc_table) > 0) {
      return(list(
        lat = loc_table$lat[1],
        lon = loc_table$long[1],
        display_name = address
      ))
    }
    return(NULL)
  }
  
  # Geocode when button is clicked
  observeEvent(input$geocode_btn, {
    showNotification("Geocoding location...", type = "message")
    
    result <- geocode_location(input$location_name)
    
    if (!is.null(result)) {
      coords(result)
      showNotification(paste("Found:", result$display_name), type = "message")
    } else {
      coords(NULL)
      showNotification("Location not found. Try a different search.", type = "error")
    }
  })
  
  # Display coordinates
  output$coord_display <- renderText({
    if (!is.null(coords())) {
      paste("Latitude:", round(coords()$lat, 4), "\n",
            "Longitude:", round(coords()$lon, 4), "\n",
            "Location:", coords()$display_name)
    } else {
      "No coordinates found. Click 'Find Location' first."
    }
  })
  
  # Fetch and process weather data
  # Fetch and process weather data
  observeEvent(input$fetch_weather, {
    if (is.null(coords())) {
      showNotification("Please find coordinates first!", type = "warning")
      return()
    }
    
    showNotification("Fetching weather data...", type = "message")
    
    tryCatch({
      # Fetch weather data from OpenMeteo with expanded variables
      weather_raw <- weather_history(
        location = c(coords()$lat, coords()$lon),
        start = input$date_range[1],
        end = input$date_range[2],
        hourly = NULL,
        daily = c("temperature_2m_mean", "temperature_2m_max", "temperature_2m_min", 
                  "apparent_temperature_max", "apparent_temperature_mean",
                  "apparent_temperature_min", "wind_speed_10m_max", 
                  "wind_direction_10m_dominant", "precipitation_hours", 
                  "precipitation_sum", "rain_sum", "snowfall_sum", "weather_code"),
        response_units = NULL,
        model = NULL,
        timezone = "auto"
      )
      
      # Process data with all derived variables
      weather_processed <- weather_raw %>%
        mutate(
          year = year(date),
          month = month(date),
          day = day(date),
          # Convert to imperial units
          #snowfall is in cm in openmeteo, precipitation and rainfall in mm
          daily_temperature_2m_mean_f = 9/5 * daily_temperature_2m_mean + 32,
          daily_temperature_2m_max_f = 9/5 * daily_temperature_2m_max + 32,
          daily_temperature_2m_min_f = 9/5 * daily_temperature_2m_min + 32,
          daily_apparent_temperature_mean_f = 9/5 * daily_apparent_temperature_mean + 32,
          daily_apparent_temperature_max_f = 9/5 * daily_apparent_temperature_max + 32,
          daily_apparent_temperature_min_f = 9/5 * daily_apparent_temperature_min + 32,
          daily_precipitation_sum_in = daily_precipitation_sum / 25.4,
          daily_rain_sum_in = daily_rain_sum / 25.4,
          daily_snowfall_sum_in = daily_snowfall_sum / 2.54
        )
      
      # Create monthly aggregated data
      weather_monthly <- weather_processed %>%
        group_by(year, month) %>%
        summarise(
          # Temperature variables
          avg_temp = mean(daily_temperature_2m_mean_f, na.rm = TRUE),
          max_temp = max(daily_temperature_2m_max_f, na.rm = TRUE),
          min_temp = min(daily_temperature_2m_min_f, na.rm = TRUE),
          
          # Apparent temperature
          avg_temp_apparent = mean(daily_apparent_temperature_mean_f, na.rm = TRUE),
          max_temp_apparent = max(daily_apparent_temperature_max_f, na.rm = TRUE),
          min_temp_apparent = min(daily_apparent_temperature_min_f, na.rm = TRUE),
          
          # Wind
          avg_wind = mean(daily_wind_speed_10m_max, na.rm = TRUE),
          
          # Precipitation
          avg_precipitation_in = mean(daily_precipitation_sum_in, na.rm = TRUE),
          total_precipitation_in = sum(daily_precipitation_sum_in, na.rm = TRUE),
          
          # Rainfall
          avg_rain_in = mean(daily_rain_sum_in, na.rm = TRUE),
          total_rainfall_in = sum(daily_rain_sum_in, na.rm = TRUE),
          
          # Snowfall
          avg_snow_in = mean(daily_snowfall_sum_in, na.rm = TRUE),
          total_snow_in = sum(daily_snowfall_sum_in, na.rm = TRUE),
          
          # Frequency variables (using >1 as default, but will be overridden in plotting)
          freq_precipitation_in = sum(daily_precipitation_sum_in > 1, na.rm = TRUE),
          freq_rainfall_in = sum(daily_rain_sum_in > 1, na.rm = TRUE),
          freq_snow_in = sum(daily_snowfall_sum_in > 1, na.rm = TRUE),
          freq_snow_3in = sum(daily_snowfall_sum_in > 3, na.rm = TRUE),
          
          .groups = 'drop'
        )
      
      weather_data(weather_raw)
      processed_data(weather_monthly)
      showNotification("Data fetched successfully!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error fetching data:", e$message), type = "error")
    })
  })
  

  # Generate aggregated data for plotting based on user choices
  aggregated_data <- reactive({
    req(weather_data())  # Need the daily data
    
    daily_data <- weather_data()
    
    # First, ensure we have the converted imperial units in the daily data
    daily_processed <- daily_data %>%
      mutate(
        year = year(date),
        month = month(date),
        # Convert to imperial units if not already done
        daily_temperature_2m_mean_f = 9/5 * daily_temperature_2m_mean + 32,
        daily_temperature_2m_max_f = 9/5 * daily_temperature_2m_max + 32,
        daily_temperature_2m_min_f = 9/5 * daily_temperature_2m_min + 32,
        daily_apparent_temperature_mean_f = 9/5 * daily_apparent_temperature_mean + 32,
        daily_apparent_temperature_max_f = 9/5 * daily_apparent_temperature_max + 32,
        daily_apparent_temperature_min_f = 9/5 * daily_apparent_temperature_min + 32,
        daily_precipitation_sum_in = daily_precipitation_sum / 25.4,
        daily_rain_sum_in = daily_rain_sum / 25.4,
        daily_snowfall_sum_in = daily_snowfall_sum / 2.54
      )
    
    # Get the selected daily variable
    selected_var <- input$daily_var
    
    # Apply the chosen analysis type
    if (input$analysis_type == "frequency") {
      # Frequency analysis: count days meeting condition
      threshold <- input$freq_threshold
      
      daily_processed <- daily_processed %>%
        mutate(
          meets_condition = case_when(
            input$freq_operator == "greater" ~ .data[[selected_var]] > threshold,
            input$freq_operator == "greater_equal" ~ .data[[selected_var]] >= threshold,
            input$freq_operator == "less" ~ .data[[selected_var]] < threshold,
            input$freq_operator == "less_equal" ~ .data[[selected_var]] <= threshold,
            input$freq_operator == "equal" ~ .data[[selected_var]] == threshold,
            TRUE ~ .data[[selected_var]] > threshold
          )
        )
      
      # Aggregate by month - count days meeting condition
      agg_data <- daily_processed %>%
        group_by(year, month) %>%
        summarise(
          aggregated_value = sum(meets_condition, na.rm = TRUE),
          .groups = 'drop'
        )
      
    } else {
      # Regular aggregation (mean, max, min, sum)
      agg_data <- daily_processed %>%
        group_by(year, month) %>%
        summarise(
          aggregated_value = case_when(
            input$analysis_type == "mean" ~ mean(.data[[selected_var]], na.rm = TRUE),
            input$analysis_type == "max" ~ max(.data[[selected_var]], na.rm = TRUE),
            input$analysis_type == "min" ~ min(.data[[selected_var]], na.rm = TRUE),
            input$analysis_type == "sum" ~ sum(.data[[selected_var]], na.rm = TRUE),
            TRUE ~ mean(.data[[selected_var]], na.rm = TRUE)
          ),
          .groups = 'drop'
        )
    }
    
    return(agg_data)
  })
  
  
  # Create heatmap plot
  output$heatmap_plot <- renderPlot({
    req(aggregated_data())
    
    data <- aggregated_data()
    
    # Create breaks based on data range
    data_range <- range(data$aggregated_value, na.rm = TRUE)
    
  
    
    # Handle case where all values are the same or range is very small
    if (diff(data_range) == 0 || is.infinite(diff(data_range))) {
      # If all values are the same, create breaks around that value
      val <- data_range[1]
      breaks <- seq(val - 1, val + 1, length.out = 10)
    } else {
      breaks <- seq(floor(data_range[1]), ceiling(data_range[2]), length.out = 10)
    }
    
    
    # Create the plot
    p <- data %>%
      mutate(month = factor(month, levels = 1:12, labels = month.name)) %>%
      ggplot(aes(x = year, y = month, fill = aggregated_value)) +
      geom_tile() +
      scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = input$year_interval)) +
      labs(
        title = input$plot_title,
        x = "Year",
        y = "Month",
        fill = ifelse(input$analysis_type == "frequency",
                      paste("Days with", 
                            gsub("daily_", "", input$daily_var),
                            input$freq_operator, 
                            input$freq_threshold),
                      paste(input$analysis_type, 
                            gsub("daily_", "", input$daily_var),
                            ifelse(grepl("temp", input$daily_var), "(°F)",
                                   ifelse(grepl("precip|rain|snow", input$daily_var), "(in)",
                                          ifelse(grepl("wind", input$daily_var), "(m/s)", "")))))
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Apply color scale based on selection
    custom_palettes <- list(
      yarn = yarn_colors,
      bead = bead_colors,
      six = six_colors,
      six_reverse = six_colors_reverse
    )
    
    legend_name <- ifelse(
      input$analysis_type == "frequency",
      paste(
        "Days with",
        gsub("daily_", "", input$daily_var),
        input$freq_operator,
        input$freq_threshold
      ),
      paste(
        input$analysis_type,
        gsub("daily_", "", input$daily_var),
        ifelse(
          grepl("temp", input$daily_var), "(°F)",
          ifelse(
            grepl("precip|rain|snow", input$daily_var), "(in)",
            ifelse(grepl("wind", input$daily_var), "(m/s)", "")
          )
        )
      )
    )
    
    if (input$color_palette %in% names(custom_palettes)) {
      
      selected_colors <- custom_palettes[[input$color_palette]]
      
      n_colors <- length(selected_colors)
      
      breaks <- seq(
        floor(data_range[1]),
        ceiling(data_range[2]),
        length.out = n_colors + 1
      )
      
      p <- p + scale_fill_stepsn(
        colors = unname(selected_colors),
        breaks = breaks,
        name = legend_name
      )
      
    } else {
      
      p <- p + scale_fill_distiller(
        palette = input$color_palette,
        direction = 1,
        name = legend_name
      )
      
    }
    
    # Return the plot
    p
  })
  
  # Display processed data table
  output$weather_table <- renderDT({
    req(aggregated_data())
    datatable(aggregated_data(), options = list(pageLength = 10))
  })
  
  # Display raw data
  output$raw_data <- renderDT({
    req(processed_data())
    datatable(processed_data(), options = list(pageLength = 10))
  })
  
  # Summary statistics
  output$summary_stats <- renderPrint({
    req(aggregated_data())
    data <- aggregated_data()
    
    var_label <- ifelse(input$use_frequency,
                        paste("Frequency of", input$weather_var, 
                              input$freq_operator, input$freq_threshold),
                        input$weather_var)
    
    cat("Summary Statistics for", var_label, "\n")
    cat("Location:", input$location_name, "\n")
    cat("Date Range:", input$date_range[1], "to", input$date_range[2], "\n")
    
    if(input$use_frequency) {
      cat("Analysis Type: Frequency (days per month)\n")
      cat("Condition:", input$freq_operator, input$freq_threshold, "\n")
    } else {
      cat("Aggregation Method:", input$agg_method, "\n")
    }
    
    cat("\n")
    summary(data$aggregated_value)
  })
}

# Run the application
shinyApp(ui = ui, server = server)