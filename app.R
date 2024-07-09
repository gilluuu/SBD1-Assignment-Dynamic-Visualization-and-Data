library(shiny)
library(leaflet)
library(shinydashboard)
library(shinyWidgets)
library(rvest)
library(readr)
library(leaflet.extras)
library(DT)

# Function for Scraping Precipitation Data
scrape_precipitation_data <- function() {
  url1 <- "https://www.theglobaleconomy.com/rankings/precipitation/"
  page1 <- read_html(url1)
  
  # Scrape the Table Containing Precipitation Data
  precipitation_data <- page1 %>%
    html_nodes("table") %>%
    html_table(fill = TRUE) %>%
    .[[1]]
  
  # Keep only the Necessary Columns and Rename them
  precipitation_data <- precipitation_data[, c(1, 2)]
  colnames(precipitation_data) <- c("country", "Precipitation (in mm)")
  
  # Convert the Precipitation Column to Numeric
  precipitation_data$`Precipitation (in mm)` <- as.numeric(gsub(",", "", precipitation_data$`Precipitation (in mm)`))
  
  # Return Data
  precipitation_data
}

# Function for Scraping Temperature Data
scrape_temperature_data <- function() {
  url2 <- "https://en.wikipedia.org/wiki/List_of_countries_by_average_yearly_temperature"
  page2 <- read_html(url2)
  
  # Scrape the Table Containing Temperature Data
  temperature_data <- page2 %>%
    html_nodes(".wikitable") %>%
    html_table(fill = TRUE) %>%
    .[[1]]
  
  # Keep only the Tables with Country and Temperature
  temperature_data <- temperature_data[, c("Country", "Temperature(°C)")]
  colnames(temperature_data) <- c("country", "Temperature (in °C)")
  
  # Clean up Temperature-Column and keep only Celsius
  temperature_data$`Temperature (in °C)` <- gsub(" .*", "", temperature_data$`Temperature (in °C)`)
  
  # Remove non-numeric Values
  temperature_data$`Temperature (in °C)` <- as.numeric(gsub("[^0-9.-]", "", temperature_data$`Temperature (in °C)`))
  
  # Remove NA-Values
  temperature_data <- temperature_data[!is.na(temperature_data$`Temperature (in °C)`),]
  
  # Return Data
  temperature_data
}

# Geodata creation
getgeodata <- function() {
  
  ## Countries can be found here: https://www.kaggle.com/datasets/paultimothymooney/latitude-and-longitude-for-every-country-and-state
  
  # Read the CSV
  geo_data <- read_csv("world_country_and_usa_states_latitude_and_longitude_values.csv")
  geo_data <- geo_data[, c("country", "latitude", "longitude")]
  
  # Return Data
  geo_data
}

# Shiny Interface
ui <- dashboardPage(
  dashboardHeader(title = "Weather Data Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "map_tab"),
      menuItem("Proportional Map", tabName = "proportional_map_tab")
    )
  ),
  dashboardBody(
    tabItems(
      # Map Tab
      tabItem(
        tabName = "map_tab",
        fluidRow(
          box(
            title = "Country Observation",
            width = 5,
            pickerInput(
              inputId = "selected_countries",
              label = "Select Countries",
              choices = NULL,
              options = list(`actions-box` = TRUE),
              multiple = TRUE
            )
          ),
          box(
            title = "Observational Variable",
            width = 5,
            selectInput(
              inputId = "selected_column",
              label = "Select Variable",
              choices = c("Precipitation (in mm)", "Temperature (in °C)")
            )
          )
        ),
        fluidRow(
          box(
            title = "Map",
            width = 10,
            leafletOutput("map")
          )
        ),
        fluidRow(
          box(
            title = "Number of countries",
            width = 5,
            valueBoxOutput("selected_countries_count", width = 12)
          ),
          box(
            title = "Mean value",
            width = 5,
            valueBoxOutput("average_value", width = 12)
          ),
          box(
            title = "Maximum value",
            width = 5,
            valueBoxOutput("max_value", width = 12)
          ),
          box(
            title = "Minimum value",
            width = 5,
            valueBoxOutput("min_value", width = 12)
          )
        )
      ),
      # Proportional Map Tab
      tabItem(
        tabName = "proportional_map_tab",
        fluidRow(
          box(
            title = "Selected value",
            width = 5,
            selectInput(
              inputId = "selected_column_proportional_map",
              label = "Select column",
              choices = c("Precipitation (in mm)", "Temperature (in °C)")
            )
          )
        ),
        fluidRow(
          box(
            title = "Proportional Map",
            width = 10,
            leafletOutput("proportional_map")
          )
        ),
        fluidRow(
          box(
            title = "Top 10 Werte",
            width = 10,
            dataTableOutput("top_values_table")
          )
        )
      )
    )
  ),
  skin = "blue"
)

# Server
server <- function(input, output, session) {
  # Reactive function for scraping data
  data <- reactive({
    precipitation_data <- scrape_precipitation_data()
    temperature_data <- scrape_temperature_data()
    geodata <- getgeodata()
    
    # Merge the tables based on the "country" column
    merged_data <- merge(precipitation_data, temperature_data, by = "country", all = TRUE)
    merged_data <- merge(merged_data, geodata, by = "country", all.x = TRUE)
    
    # Remove rows with NA values
    merged_data <- na.omit(merged_data)
    
    # Return the merged data
    merged_data
  })
  
  # Update dropdown menus based on the data
  observe({
    merged_data <- data()
    
    # Dropdown menu for selected countries (Map Tab)
    updatePickerInput(
      session = session,
      inputId = "selected_countries",
      choices = unique(merged_data$country),
      selected = unique(merged_data$country)
    )
  })
  
  # Leaflet map (Map Tab)
  output$map <- renderLeaflet({
    merged_data <- data()
    
    # Filter the data based on the selected countries
    selected_data <- merged_data[merged_data$country %in% input$selected_countries, ]
    
    # Create Leaflet map
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
        data = selected_data,
        lat = ~latitude,
        lng = ~longitude,
        label = ~paste0(input$selected_column, ": ", get(input$selected_column)),
        popup = ~paste0("<b>Country:</b> ", country, "<br>",
                        "<b>", input$selected_column, ":</b> ", get(input$selected_column)),
        color = "royalblue",
        fillOpacity = 0.7
      )
  })
  
  # Show the number of selected countries (Statistics Tab)
  output$selected_countries_count <- renderValueBox({
    merged_data <- data()
    selected_data <- merged_data[merged_data$country %in% input$selected_countries, ]
    
    valueBox(
      value = length(input$selected_countries),
      subtitle = "Selected countries",
      color = "teal"
    )
  })
  
  # Show the average value of the selected countries (Statistics Tab)
  output$average_value <- renderValueBox({
    merged_data <- data()
    selected_data <- merged_data[merged_data$country %in% input$selected_countries, ]
    
    valueBox(
      value = round(mean(selected_data[[input$selected_column]], na.rm = TRUE), 2),
      subtitle = "Mean value",
      color = "yellow"
    )
  })
  
  # Show the maximum value and the country name (Statistics Tab)
  output$max_value <- renderValueBox({
    merged_data <- data()
    selected_data <- merged_data[merged_data$country %in% input$selected_countries, ]
    
    max_value <- max(selected_data[[input$selected_column]], na.rm = TRUE)
    country_with_max_value <- selected_data$country[selected_data[[input$selected_column]] == max_value]
    
    valueBox(
      value = max_value,
      subtitle = paste("Maximum value (", country_with_max_value, ")"),
      color = "blue"
    )
  })
  
  # Show the minimum value and the country name (Statistics Tab)
  output$min_value <- renderValueBox({
    merged_data <- data()
    selected_data <- merged_data[merged_data$country %in% input$selected_countries, ]
    
    min_value <- min(selected_data[[input$selected_column]], na.rm = TRUE)
    country_with_min_value <- selected_data$country[selected_data[[input$selected_column]] == min_value]
    
    valueBox(
      value = min_value,
      subtitle = paste("Minimum value (", country_with_min_value, ")"),
      color = "green"
    )
  })
  
  # Create circles with variable color and size (Proportional Map Tab)
  output$proportional_map <- renderLeaflet({
    merged_data <- data()
    
    # Filter the data based on the selected column
    selected_data <- merged_data[merged_data$country %in% input$selected_countries, ]
    
    # Normalize the values for color and size
    values <- selected_data[[input$selected_column_proportional_map]]
    values <- na.omit(values)  # Remove missing values
    normalized_values <- scales::rescale(values, to = c(0, 1))
    
    # Manually define the value range for the color palette
    min_value <- min(values, na.rm = TRUE)
    max_value <- max(values, na.rm = TRUE)
    
    # Define the color palette
    color_palette <- colorNumeric(
      palette = c("blue", "red"),
      domain = c(min_value, max_value)
    )
    
    # Create Leaflet map
    leaflet(data = selected_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lat = ~latitude,
        lng = ~longitude,
        radius = ~sqrt(normalized_values) * 12,  # Adjust circle size
        fillColor = ~color_palette(values),  # Adjust circle color
        color = "red",
        fillOpacity = 0.8,
        stroke = FALSE,
        label = ~paste0(input$selected_column_proportional_map, ": ", get(input$selected_column_proportional_map))
      )
  })
  
  # Create a table with the top 10 values (Proportional Map Tab)
  output$top_values_table <- DT::renderDataTable({
    merged_data <- data()
    
    # Sort the data based on the selected column in descending order
    sorted_data <- merged_data[order(merged_data[[input$selected_column_proportional_map]], decreasing = TRUE), ]
    
    # Limit to the top 10 values
    top_values <- head(sorted_data, 10)
    
    # Create DataTable object
    datatable(top_values, options = list(pageLength = 10))
  })
}

# Start the app
shinyApp(ui, server)
