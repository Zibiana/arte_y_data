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


# =====================================================================
# TRANSLATIONS (English / Spanish)
# To fix or change a Spanish phrase, edit it in the `es` list below.
# The values on the right-hand side of the choice vectors (e.g.
# "daily_temperature_2m_mean_f") are the internal codes the app uses
# and must match between `en` and `es` -- only change the names (left side).
# =====================================================================
i18n <- list(
  en = list(
    toggle_btn   = "Espa\u00f1ol",   # button shows the language you will switch TO
    title        = "Historical Weather Data Explorer",
    location     = "Enter Location:",
    find         = "Find Location",
    lat          = "Latitude:",
    lon          = "Longitude:",
    loc          = "Location:",
    no_coords    = "No coordinates found. Click 'Find Location' first.",
    date_range   = "Date Range:",
    date_sep     = "to",
    daily_var    = "Daily Variable:",
    analysis     = "Analysis Type:",
    condition    = "Condition:",
    threshold    = "Threshold Value:",
    freq_help    = "Counts the number of days in each month that meet the condition",
    palette      = "Color Palette:",
    year_int     = "Year Interval for X-axis:",
    plot_title   = "Plot Title:",
    plot_title_default = "Weather Data Visualization",
    generate     = "Generate Plot",
    data_source  = "Data from OpenMeteo Historical Weather API",
    tab_heatmap  = "Heatmap Plot",
    tab_table    = "Data Table",
    tab_summary  = "Summary Statistics",
    tab_raw      = "Raw Data",
    axis_year    = "Year",
    axis_month   = "Month",
    months       = month.name,
    sum_title    = "Summary Statistics for",
    sum_freq_of  = "Frequency of",
    legend_days  = "Days with",
    legend_units = c(temp = "(\u00b0F)", precip = "(in)", wind = "(m/s)"),
    stat_names   = c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "NA's"),
    n_geocoding  = "Geocoding location...",
    n_found      = "Found:",
    n_not_found  = "Location not found. Try a different search.",
    n_need_coords = "Please find coordinates first!",
    n_fetching   = "Fetching weather data...",
    n_success    = "Data fetched successfully!",
    n_error      = "Error fetching data:",
    daily_choices = c("Temperature (Mean)" = "daily_temperature_2m_mean_f",
                      "Temperature (Max)" = "daily_temperature_2m_max_f",
                      "Temperature (Min)" = "daily_temperature_2m_min_f",
                      "Apparent Temperature (Mean)" = "daily_apparent_temperature_mean_f",
                      "Apparent Temperature (Max)" = "daily_apparent_temperature_max_f",
                      "Apparent Temperature (Min)" = "daily_apparent_temperature_min_f",
                      "Wind Speed (Max)" = "daily_wind_speed_10m_max",
                      "Precipitation" = "daily_precipitation_sum_in",
                      "Rainfall" = "daily_rain_sum_in",
                      "Snowfall" = "daily_snowfall_sum_in"),
    analysis_choices = c("Monthly Average" = "mean",
                         "Monthly Maximum" = "max",
                         "Monthly Minimum" = "min",
                         "Monthly Total" = "sum",
                         "Frequency (Count days meeting condition)" = "frequency"),
    operator_choices = c("Greater than (>)" = "greater",
                         "Greater than or equal to (>=)" = "greater_equal",
                         "Less than (<)" = "less",
                         "Less than or equal to (<=)" = "less_equal",
                         "Equal to (=)" = "equal"),
    palette_choices = c("Yarn Colors (9 levels)" = "yarn",
                        "Bead Colors (reversed yarn)" = "bead",
                        "Six colors" = "six",
                        "Six Colors (reversed)" = "six_reverse",
                        "Blues" = "Blues",
                        "Reds" = "Reds",
                        "Greens" = "Greens",
                        "Purples" = "PuBu",
                        "Oranges" = "YlOrRd",
                        "Viridis" = "viridis")
  ),
  es = list(
    toggle_btn   = "English",
    title        = "Explorador de datos meteorol\u00f3gicos hist\u00f3ricos",
    location     = "Introduce la ubicaci\u00f3n:",
    find         = "Buscar ubicaci\u00f3n",
    lat          = "Latitud:",
    lon          = "Longitud:",
    loc          = "Ubicaci\u00f3n:",
    no_coords    = "No se encontraron coordenadas. Haz clic primero en 'Buscar ubicaci\u00f3n'.",
    date_range   = "Intervalo de fechas:",
    date_sep     = "a",
    daily_var    = "Variable diaria:",
    analysis     = "Tipo de an\u00e1lisis:",
    condition    = "Condici\u00f3n:",
    threshold    = "Valor umbral:",
    freq_help    = "Cuenta el n\u00famero de d\u00edas de cada mes que cumplen la condici\u00f3n",
    palette      = "Paleta de colores:",
    year_int     = "Intervalo anual para el eje X:",
    plot_title   = "T\u00edtulo de la trama:",
    plot_title_default = "Visualizaci\u00f3n de datos meteorol\u00f3gicos",
    generate     = "Generar gr\u00e1fico",
    data_source  = "Datos de la API de datos meteorol\u00f3gicos hist\u00f3ricos de OpenMeteo",
    tab_heatmap  = "Gr\u00e1fico de mapa de calor",
    tab_table    = "Tabla de datos",
    tab_summary  = "Estad\u00edsticas de resumen",
    tab_raw      = "Datos brutos",
    axis_year    = "A\u00f1o",
    axis_month   = "Mes",
    months       = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio",
                     "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
    sum_title    = "Estad\u00edsticas de resumen de",
    sum_freq_of  = "Frecuencia de",
    legend_days  = "D\u00edas con",
    legend_units = c(temp = "(\u00b0F)", precip = "(pulg)", wind = "(m/s)"),
    stat_names   = c("M\u00edn.", "1.er cuartil", "Mediana", "Media", "3.er cuartil", "M\u00e1x.", "NA"),
    n_geocoding  = "Geocodificando la ubicaci\u00f3n...",
    n_found      = "Encontrado:",
    n_not_found  = "No se encontr\u00f3 la ubicaci\u00f3n. Prueba con otra b\u00fasqueda.",
    n_need_coords = "\u00a1Primero busca las coordenadas!",
    n_fetching   = "Obteniendo datos meteorol\u00f3gicos...",
    n_success    = "\u00a1Datos obtenidos correctamente!",
    n_error      = "Error al obtener los datos:",
    daily_choices = c("Temperatura (media)" = "daily_temperature_2m_mean_f",
                      "Temperatura (m\u00e1xima)" = "daily_temperature_2m_max_f",
                      "Temperatura (m\u00ednima)" = "daily_temperature_2m_min_f",
                      "Temperatura aparente (media)" = "daily_apparent_temperature_mean_f",
                      "Temperatura aparente (m\u00e1xima)" = "daily_apparent_temperature_max_f",
                      "Temperatura aparente (m\u00ednima)" = "daily_apparent_temperature_min_f",
                      "Velocidad del viento (m\u00e1xima)" = "daily_wind_speed_10m_max",
                      "Precipitaciones" = "daily_precipitation_sum_in",
                      "Lluvias" = "daily_rain_sum_in",
                      "Nevadas" = "daily_snowfall_sum_in"),
    analysis_choices = c("Media mensual" = "mean",
                         "M\u00e1ximo mensual" = "max",
                         "M\u00ednimo mensual" = "min",
                         "Total mensual" = "sum",
                         "Frecuencia (n\u00famero de d\u00edas que cumplen la condici\u00f3n)" = "frequency"),
    operator_choices = c("Mayor que (>)" = "greater",
                         "Mayor o igual que (>=)" = "greater_equal",
                         "Menor que (<)" = "less",
                         "Menor o igual que (<=)" = "less_equal",
                         "Igual a (=)" = "equal"),
    palette_choices = c("Colores del hilo (9 niveles)" = "yarn",
                        "Colores de las cuentas (hilo invertido)" = "bead",
                        "Seis colores" = "six",
                        "Seis colores (invertidos)" = "six_reverse",
                        "Azules" = "Blues",
                        "Rojos" = "Reds",
                        "Verdes" = "Greens",
                        "Morados" = "PuBu",
                        "Naranjas" = "YlOrRd",
                        "Viridis" = "viridis")
  )
)

# Tabs are found by their (unchanged) English tabPanel titles
tab_keys <- c("Heatmap Plot"       = "tab_heatmap",
              "Data Table"         = "tab_table",
              "Summary Statistics" = "tab_summary",
              "Raw Data"           = "tab_raw")

# Small JavaScript helpers so the server can change text that has no update*() function
# (page title, tab titles, help text, the "to" between the dates)
lang_js <- "
Shiny.addCustomMessageHandler('set_text', function(m) {
  var $e = $(m.selector);
  if (m.nth !== undefined && m.nth !== null) { $e = $e.eq(m.nth); }
  $e.text(m.text);
});
Shiny.addCustomMessageHandler('set_date_sep', function(m) {
  $('#date_range .input-group-addon').each(function() {
    var $inner = $(this).find('.input-group-text');
    if ($inner.length) { $inner.text(m.text); } else { $(this).text(m.text); }
  });
});
"

# Define UI
ui <- fluidPage(
  tags$head(tags$script(HTML(lang_js))),
  
  # Language toggle button (top-right corner)
  div(style = "position: absolute; top: 12px; right: 20px; z-index: 1000;",
      actionButton("lang_toggle", "Espa\u00f1ol", class = "btn-default btn-sm")),
  
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
  
  # ---- Language handling ----
  lang <- reactiveVal("en")                 # app starts in English
  L <- function(key) i18n[[lang()]][[key]]  # look up a phrase in the current language
  
  observeEvent(input$lang_toggle, {
    old <- lang()
    new <- if (old == "en") "es" else "en"
    lang(new)
    txt <- i18n[[new]]
    
    # Button itself
    updateActionButton(session, "lang_toggle", label = txt$toggle_btn)
    
    # Inputs: labels (and choices, keeping whatever the user had selected)
    updateTextInput(session, "location_name", label = txt$location)
    updateActionButton(session, "geocode_btn", label = txt$find)
    updateDateRangeInput(session, "date_range", label = txt$date_range)
    updateSelectInput(session, "daily_var", label = txt$daily_var,
                      choices = txt$daily_choices, selected = input$daily_var)
    updateRadioButtons(session, "analysis_type", label = txt$analysis,
                       choices = txt$analysis_choices, selected = input$analysis_type)
    updateSelectInput(session, "freq_operator", label = txt$condition,
                      choices = txt$operator_choices, selected = input$freq_operator)
    updateNumericInput(session, "freq_threshold", label = txt$threshold)
    updateSelectInput(session, "color_palette", label = txt$palette,
                      choices = txt$palette_choices, selected = input$color_palette)
    updateNumericInput(session, "year_interval", label = txt$year_int)
    
    # Plot title: only swap the default text if the user hasn't typed their own
    new_title_value <- if (identical(input$plot_title, i18n[[old]]$plot_title_default)) {
      txt$plot_title_default
    } else {
      input$plot_title
    }
    updateTextInput(session, "plot_title", label = txt$plot_title, value = new_title_value)
    updateActionButton(session, "fetch_weather", label = txt$generate)
    
    # Text with no update*() function: page title, tabs, help text, date separator
    session$sendCustomMessage("set_text", list(selector = "h2", nth = 0, text = txt$title))
    for (tab_name in names(tab_keys)) {
      session$sendCustomMessage("set_text", list(
        selector = sprintf('a[data-value="%s"]', tab_name),
        text = txt[[ tab_keys[[tab_name]] ]]))
    }
    # the "Counts the number of days..." help text (first help text in the sidebar)
    session$sendCustomMessage("set_text", list(selector = ".well .help-block", nth = 0, text = txt$freq_help))
    # the "Data from OpenMeteo..." help text (last one in the sidebar)
    session$sendCustomMessage("set_text", list(selector = ".well .help-block", nth = -1, text = txt$data_source))
    session$sendCustomMessage("set_date_sep", list(text = txt$date_sep))
  }, ignoreInit = TRUE)
  
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
    showNotification(L("n_geocoding"), type = "message")
    
    result <- geocode_location(input$location_name)
    
    if (!is.null(result)) {
      coords(result)
      showNotification(paste(L("n_found"), result$display_name), type = "message")
    } else {
      coords(NULL)
      showNotification(L("n_not_found"), type = "error")
    }
  })
  
  # Display coordinates
  output$coord_display <- renderText({
    if (!is.null(coords())) {
      # Spanish uses a decimal comma (41,8169); English keeps the decimal point
      fmt <- function(x) if (lang() == "es") format(round(x, 4), decimal.mark = ",") else round(x, 4)
      paste(L("lat"), fmt(coords()$lat), "\n",
            L("lon"), fmt(coords()$lon), "\n",
            L("loc"), coords()$display_name)
    } else {
      L("no_coords")
    }
  })
  
  # Fetch and process weather data
  # Fetch and process weather data
  observeEvent(input$fetch_weather, {
    if (is.null(coords())) {
      showNotification(L("n_need_coords"), type = "warning")
      return()
    }
    
    showNotification(L("n_fetching"), type = "message")
    
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
      showNotification(L("n_success"), type = "message")
      
    }, error = function(e) {
      showNotification(paste(L("n_error"), e$message), type = "error")
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
      mutate(month = factor(month, levels = 1:12, labels = L("months"))) %>%
      ggplot(aes(x = year, y = month, fill = aggregated_value)) +
      geom_tile() +
      scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = input$year_interval)) +
      labs(
        title = input$plot_title,
        x = L("axis_year"),
        y = L("axis_month"),
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
    
    legend_name_en <- ifelse(
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
    
    # English keeps the legend text exactly as it was; Spanish builds a translated one
    legend_name <- if (lang() == "es") {
      var_label <- names(L("daily_choices"))[match(input$daily_var, L("daily_choices"))]
      if (input$analysis_type == "frequency") {
        op_symbol <- c(greater = ">", greater_equal = ">=", less = "<",
                       less_equal = "<=", equal = "=")[[input$freq_operator]]
        paste(L("legend_days"), var_label, op_symbol, input$freq_threshold)
      } else {
        analysis_label <- names(L("analysis_choices"))[match(input$analysis_type, L("analysis_choices"))]
        unit <- if (grepl("temp", input$daily_var)) L("legend_units")[["temp"]]
        else if (grepl("precip|rain|snow", input$daily_var)) L("legend_units")[["precip"]]
        else if (grepl("wind", input$daily_var)) L("legend_units")[["wind"]]
        else ""
        paste0(analysis_label, ": ", var_label, " ", unit)
      }
    } else {
      legend_name_en
    }
    
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
    
    var_label <- names(L("daily_choices"))[match(input$daily_var, L("daily_choices"))]
    analysis_label <- names(L("analysis_choices"))[match(input$analysis_type, L("analysis_choices"))]
    op_symbol <- c(greater = ">", greater_equal = ">=", less = "<",
                   less_equal = "<=", equal = "=")[[input$freq_operator]]
    is_freq <- input$analysis_type == "frequency"
    
    title_var <- if (is_freq) {
      paste(L("sum_freq_of"), var_label, op_symbol, input$freq_threshold)
    } else {
      var_label
    }
    
    cat(L("sum_title"), title_var, "\n")
    cat(L("loc"), input$location_name, "\n")
    cat(L("date_range"), format(input$date_range[1]), L("date_sep"), format(input$date_range[2]), "\n")
    cat(L("analysis"), analysis_label, "\n")
    if (is_freq) {
      cat(L("condition"), op_symbol, input$freq_threshold, "\n")
    }
    cat("\n")
    
    stats <- summary(data$aggregated_value)
    names(stats) <- L("stat_names")[match(names(stats), i18n$en$stat_names)]
    stats
  })
}

# Run the application
shinyApp(ui = ui, server = server)