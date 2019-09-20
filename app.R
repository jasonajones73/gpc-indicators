library(shiny)
library(shinymaterial)
library(shinycssloaders)
library(tidyverse)
library(leaflet)
library(sf)
library(highcharter)
library(viridis)

# Highchart format
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)


# Wrap shinymaterial apps in material_page
ui <- material_page(
  # Defining Material Page Options
  title = "GPC Indicators",
  nav_bar_fixed = TRUE,
  include_icons = TRUE,
  font_color = "white",
  nav_bar_color = "blue-grey darken-4",
  
  # Adding some HTML elements to head
  tags$head(HTML("<link href='https://fonts.googleapis.com/css?family=Roboto+Mono' rel='stylesheet'>")),
  tags$head(HTML("<style>* {font-size: 100%; font-family: 'Roboto Mono', monospace;}</style>")),
  tags$head(HTML("<style>* .leaflet-top, .leaflet-bottom { z-index: 888;} </style>")),
  
  # Defining the side navigation bar
  material_side_nav(
    # Defining side navigation bar options
    fixed = FALSE,
    image_source = "https://storage.googleapis.com/proudcity/elgl/uploads/2019/07/elgl-logo-189x64.png",
    
    # Place side-nav tabs within side-nav
    material_side_nav_tabs(
      side_nav_tabs = c(
        "Main Page" = "main_page"
      ),
      icons = c("insert_chart")
    )
  ),
  
  # Define side-nav tab content
  material_side_nav_tab_content(
    side_nav_tab_id = "main_page",
    
    material_row(
      material_column(width = 4,
                      material_card(title = "About the Data", divider = TRUE, depth = 5,
                                    uiOutput("about") %>% withSpinner(type = 1, color = "#605ca8",
                                                                      size = 1)
                                    ) # Closing material card
                      ) # Closing material column
      ), # Closing material row
    
    material_row(
      material_column(width = 8,
                      material_card(title = "Indicator Selection", divider = TRUE, depth = 5,
                                    selectizeInput(inputId =  "select", width = "100%",
                                                   label = "Please select an indicator:",
                                                   choices = list(
                                                     "ECONOMY & WORKFORCE" = c("Per Capita Income in the Past 12 Months, Inflation Adjusted" = "pci.rds",
                                                                               "Unemployment Rate, Population 16 Years and Over" = "unemployment.rds",
                                                                               "Median Value (dollars), Owner Occupied Housing Units" = "med_home.rds",
                                                                               "Income in the Past 12 Months Below Poverty Level, Related children under 18 years" = "poverty.rds"),
                                                     "People & Community" = c("Industry by Occupation for Civilian Employed Population 16 and Over, Arts, Entertainment, and Recreation, and Accommodation and Food Services" = "arts.rds",
                                                                              "Geographic Mobility, Same House 1 Year Ago, Pop. 1 Year and Over" = "house.rds",
                                                                              "Gini Index, Income Inequality" = "gini.rds",
                                                                              "Median Real Estate Taxes Paid" = "re_taxes.rds",
                                                                              "Dependency Ratio" = "depend_ratio.rds"),
                                                     "Mobility" = c("Means of Transportation to Work, % Using Public Transportation" = "pub_trans.rds",
                                                                    "Aggregate Travel Time to Work Divided by Workers" = "trav_time.rds"))
                                                ) # Closing select input
                                    ) # Closing indicator selection card
                      ), # Closing row 1, column 1
      
      material_column(width = 4,
                      material_card(title = "Year Focus Selection", divider = TRUE, depth = 5,
                                    material_slider(input_id = "slider", label = "Select Year:",
                                                    min_value = 2012, max_value = 2017, step_size = 1,
                                                    initial_value = 2017) # Closing slider
                                    ) # Closing slider card
                      ) # Closing row 1, column 2
      ), # Closing row 1
    
    material_row(
      material_column(
        material_card(title = "Main Map", depth = 5, divider = TRUE,
                      leafletOutput("map") %>%
                        withSpinner(type = 6, color = "#605ca8")
                      ) # Closing main map card
        ), # Closing row 2, column 1
      
      material_column(
        material_card(title = "Secondary Map", depth = 5, divider = TRUE,
                      leafletOutput("map2") %>%
                        withSpinner(type = 6, color = "#605ca8")
                      ) # Closing secondary map card
      ) # Closing row 2, column 2
    ), # Closing row 2
    
    material_row(
      material_column(
        material_card(title = "Data by Year", depth = 5, divider = TRUE,
                      highchartOutput("year_chart") %>%
                        withSpinner(type = 1, color = "#605ca8")
        ) # Closing data by year card
      ), # Closing row 3, column 1
      
      material_column(
        material_card(title = "Data Distribution", depth = 5, divider = TRUE,
                      highchartOutput("dist_chart") %>%
                        withSpinner(type = 1, color = "#605ca8")
                      ) # Closing data distribution card
      ) # Closing row 3, column 2
    ) # Closing row 3
    
    
  ) # Closing Main Page tab_content
) # Closing Material Page

# Read data ----
nc <- read_rds("nc.rds")
def <- read_rds("data/definitions.rds")
nc_tracts <- read_rds("nc_tracts.rds")

# Define server ----
server <- function(input, output, session) {
  
  # Reading in map data for main map
  map_dat <- reactive({
    read_rds(path = paste0("data/", input$select))
  })
  
  nc_filt <- reactive({
    nc %>%
      merge(map_dat()) %>%
      filter(Year == as.character(input$slider))
  })
  
  output$year_chart <- renderHighchart({
    map_dat() %>%
      group_by(Year) %>%
      summarise(Estimate = mean(Estimate, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(Year = as.integer(Year)) %>%
      hchart(type = "line",
             hcaes(x = Year, y = Estimate),
             name = "Data by Year") %>%
      hc_add_theme(hc_theme_google())
  })
  
  output$dist_chart <- renderHighchart({
    map_dat() %>%
      filter(Year == as.character(input$slider)) %>%
      .$Estimate %>%
      hchart() %>%
      hc_add_theme(hc_theme_google())
  })
  
  output$map <- renderLeaflet({
    
    pal <- colorBin(viridis(n = 4, direction = -1), nc_filt()$Estimate)
    
    leaflet() %>%
      addTiles(urlTemplate = "https://api.maptiler.com/maps/streets/{z}/{x}/{y}@2x.png?key=TxvhrAmR6qR1BMLNZjOj",
               attribution = HTML("<a href='https://www.maptiler.com/copyright/' target='_blank'>© MapTiler</a> <a href='https://www.openstreetmap.org/copyright' target='_blank'>© OpenStreetMap contributors</a>")) %>%
      addPolygons(data = nc_filt(),
                  color = "#000000",
                  weight = 1,
                  fillColor = ~pal(Estimate),
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(color = "#ffffff",
                                                      weight = 2,
                                                      bringToFront = TRUE)) %>%
      addLegend(data = nc_filt(),
                position = "topright",
                pal = pal,
                values = ~Estimate,
                opacity = 1)
  })
  
  
  intersection <- reactive({
    req(input$map_shape_click)
    
    tibble(long = input$map_shape_click$lng,
           lat = input$map_shape_click$lat) %>%
      st_as_sf(coords = c("long", "lat"), crs = st_crs(nc))
  })
  
  observe({
    dat <- intersection()
    
    leafletProxy("map", data = dat) %>%
      clearMarkers() %>%
      addMarkers()
  })
  
  tract_filt <- reactive({
    req(input$map_shape_click)
    
    county_filt <- st_join(nc, intersection(), left = FALSE) %>%
      pull(COUNTYFP)
    
    tract_dat <- str_split(input$select, "\\.", simplify = TRUE) %>%
      as.data.frame() %>%
      rename(file = 1, ext = 2) %>%
      mutate(path = paste0("data/", file, "_tracts.", ext)) %>%
      .$path %>%
      read_rds(path = .) %>%
      mutate(Estimate = ifelse(Estimate == "NaN", 0, Estimate)) %>%
      mutate(Estimate = ifelse(Estimate < 0, 0, Estimate))
    
    nc_tracts %>%
      filter(COUNTYFP == county_filt) %>%
      merge(tract_dat) %>%
      filter(Year == as.character(input$slider))
  })
  
  
  output$map2 <- renderLeaflet({
    req(input$map_shape_click)
    
    pal <- colorBin(viridis(n = 4, direction = -1), tract_filt()$Estimate)
    
    leaflet() %>%
      addTiles(urlTemplate = "https://api.maptiler.com/maps/streets/{z}/{x}/{y}@2x.png?key=TxvhrAmR6qR1BMLNZjOj",
               attribution = HTML("<a href='https://www.maptiler.com/copyright/' target='_blank'>© MapTiler</a> <a href='https://www.openstreetmap.org/copyright' target='_blank'>© OpenStreetMap contributors</a>")) %>%
      addPolygons(data = tract_filt(),
                  color = "#000000",
                  weight = 1,
                  fillColor = ~pal(Estimate),
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(color = "#ffffff",
                                                      weight = 2,
                                                      bringToFront = TRUE)) %>%
      addLegend(data = tract_filt(),
                position = "topright",
                pal = pal,
                values = ~Estimate,
                opacity = 1)
  })
  
  output$about <- renderUI({
    HTML(paste("<p><b>Indicator Name: </b>", unique(pull(map_dat(), Description)), "</p>"))
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)