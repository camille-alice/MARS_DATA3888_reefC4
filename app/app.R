library(shinydashboard)
library(ggplot2)
library(viridis)
library(maps)
library(dplyr)
library(sf)
library(tidyverse)

# R code to read and set up the data
# Only run once at the start of the program
world_map = map_data("world")

reef_geomorphic = st_read("../Data/reef_geomorphic_joined_all.gpkg")

reef_geomorphic = reef_geomorphic %>% 
  as.data.frame() %>%
  mutate(Date = as.Date(Date, "%d-%b-%y")) %>%
  mutate(Date = as.numeric(format(Date, "%Y"))) %>%
  mutate(Depth = as.numeric(Depth)) %>%
  filter(Depth <= 15) # as per Allen Coral Atlas specs

reef_recent = reef_geomorphic %>%
  select(Reef.ID, Date, SSTA_Frequency_Standard_Deviation, Depth, Diversity, class, Average_bleaching, Latitude.Degrees, Longitude.Degrees) %>% 
  mutate(SSTA_Frequency_Standard_Deviation = as.numeric(SSTA_Frequency_Standard_Deviation),
         Average_bleaching = as.numeric(Average_bleaching),
         Depth = as.numeric(Depth), 
         Diversity = as.numeric(Diversity)) %>%
  group_by(Reef.ID)

reef_recent["rugosity"] = ifelse(reef_recent$class == "Reef Slope" | reef_recent$class == "Outer Reef Flat","High",
                                 ifelse(reef_recent$class == "Sheltered Reef Slope" | reef_recent$class == "Reef Crest","Medium","Low"))

reef_recent["bleached"] = ifelse(reef_recent$Average_bleaching > 0,1,0)

reef_final = reef_recent %>% drop_na()

# Function to plot the map, with variations based on the variables selected
plot_map = function(var, start_date, end_date) {
  
  reef_temp = reef_final %>%
    filter(Date >= start_date) %>%
    filter(Date <= end_date)
  
  if (var == "SSTA") {
    ggplot() + 
      geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.3) +
      geom_point(data = reef_temp, alpha = 0.2, aes(y = Latitude.Degrees, x = Longitude.Degrees, size = SSTA_Frequency_Standard_Deviation, color = SSTA_Frequency_Standard_Deviation)) + 
      labs(title = "SSTA Frequency Standard Deviation of Coral Reefs", x = "", y = "", colour = "SSTA Frequency Standard Deviation", size = "SSTA Frequency Standard Deviation") +
      scale_colour_viridis() + 
      theme_void()
  } else if (var == "Diversity") {
    ggplot() + 
      geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.3) +
      geom_point(data = reef_temp, alpha = 0.2, aes(y = Latitude.Degrees, x = Longitude.Degrees, size = Diversity, color = Diversity)) + 
      labs(title = "Diversity of Coral Reefs", x = "", y = "", colour = "Diversity", size = "Diversity") +
      scale_colour_viridis() + 
      theme_void()
  } else if (var == "Bleaching") {
    ggplot() + 
      geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.3) +
      geom_point(data = reef_temp, alpha = 0.2, aes(y = Latitude.Degrees, x = Longitude.Degrees, size = Average_bleaching, color = Average_bleaching)) + 
      labs(title = "Bleaching of Coral Reefs", x = "", y = "", colour = "Bleaching", size = "Bleaching") +
      scale_colour_viridis() + 
      theme_void()
  }
  
}

# SHINY component
ui = dashboardPage(
  dashboardHeader(title = "Reef C4"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "Map"),
      sliderInput("map_year", "Year Range", min = 1998, max = 2017, value = c(1998, 2017), sep = ""),
      radioButtons("map_var", "Variable", choices = list("SSTA Frequency Standard Deviation" = "SSTA",
                                                         "Diversity" = "Diversity",
                                                         "Bleaching" = "Bleaching"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Map",
        fluidRow(
          box(
            plotOutput("map")
          )
        )
      )
    )
  )
)

server = function(input, output) {
  
  # Initial map
  output$map = renderPlot({
    plot_map(input$map_var, input$map_year[1], input$map_year[2])
  })
  
}

shinyApp(ui, server)