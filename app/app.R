##########################################################

# R and Data Setup

##########################################################

library(shinydashboard)
library(ggplot2)
library(viridis)
library(maps)
library(dplyr)
library(sf)
library(tidyverse)

# R code to read and set up the data
# Only run once at the start of the program
# Code taken from Pat and Camille
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

reef_recent["rugosity"] = ifelse(reef_recent$class == "Reef Slope" | reef_recent$class == "Outer Reef Flat","High", ifelse(reef_recent$class == "Sheltered Reef Slope" | reef_recent$class == "Reef Crest" | reef_recent$class == "Back Reef Slope","Medium","Low"))
reef_recent["rugosity_num"] = ifelse(reef_recent$class == "Reef Slope" | reef_recent$class == "Outer Reef Flat",3, ifelse(reef_recent$class == "Sheltered Reef Slope" | reef_recent$class == "Reef Crest" | reef_recent$class == "Back Reef Slope",2,1))

reef_recent["bleached"] = ifelse(reef_recent$Average_bleaching > 0,1,0)

reef_final = reef_recent %>% 
  drop_na() %>% 
  mutate(class = as.factor(class)) %>%
  mutate(rugosity = factor(rugosity, levels = c("Low", "Medium", "High")))

##########################################################

# Plotting/Modelling Functions

##########################################################

# Function to plot the map, with variations based on the variables selected
plot_map = function(var, start_date, end_date) {
  
  reef_temp = reef_final %>%
    filter(Date >= start_date) %>%
    filter(Date <= end_date)
  
  title_string = paste(var, "of Coral Reefs from", start_date, "to", end_date)
  
  ggplot() + 
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.3) +
    geom_point(data = reef_temp, alpha = 0.2, aes(y = Latitude.Degrees, x = Longitude.Degrees, size = unlist(reef_temp[var]), color = unlist(reef_temp[var]))) + 
    labs(title = title_string, x = "", y = "", colour = var, size = var) +
    scale_colour_viridis() + 
    theme_void()
  
}

# Function to plot linear regression, with variations based on the variables selected
plot_regression = function(x, y, start_date, end_date) {
  
  reef_temp = reef_final %>%
    filter(Date >= start_date) %>%
    filter(Date <= end_date)
  
  title_string = paste(x, "vs", y, "from", start_date, "to", end_date)
  
  ggplot(reef_temp, aes(unlist(reef_temp[x]), unlist(reef_temp[y]))) +
    geom_point() +
    geom_smooth(method = "lm") +
    labs(title = title_string, x = x, y = y)
}

# Function to plot boxplots of rugosity against different variables
plot_rugosity = function(var, start_date, end_date) {
  
  reef_temp = reef_final %>%
    filter(Date >= start_date) %>%
    filter(Date <= end_date)
  
  title_string = paste("Rugosity compared against", var, "from", start_date, "to", end_date)
  
  ggplot(reef_temp, aes(x = rugosity, y = unlist(reef_temp[var]))) +
    geom_boxplot() +
    labs(title = title_string, x = "Rugosity", y = var)
  
}

##########################################################

# Shiny Component

##########################################################

ui = dashboardPage(
  dashboardHeader(title = "Reef C4"),
  dashboardSidebar(
    sidebarMenu(id = "menu",
      
      # Tabs
      menuItem("Home", tabName = "Home", icon = icon("home")),
      menuItem("Map", tabName = "Map", icon = icon("globe")),
      menuItem("Plots", tabName = "Plots", icon = icon("line-chart")),
      menuItem("Rugosity", tabName = "Rugosity", icon = icon("bar-chart")),
      menuItem("Model", tabName = "Model", icon = icon("superpowers")),
      
      # Conditional panels only appear when their respective tabs are active
      conditionalPanel(
        condition = 'input.menu == "Map"',
        sliderInput("map_year", "Year Range", min = 1998, max = 2017, value = c(1998, 2017), sep = ""),
        radioButtons("map_var", "Variable", choices = list("SSTA Frequency Standard Deviation" = "SSTA_Frequency_Standard_Deviation",
                                                           "Diversity" = "Diversity",
                                                           "Bleaching" = "Average_bleaching"))
      ),
      conditionalPanel(
        condition = 'input.menu == "Plots"',
        sliderInput("reg_year", "Year Range", min = 1998, max = 2017, value = c(1998, 2017), sep = ""),
        radioButtons("reg_x", "X variable", choices = list("SSTA Frequency Standard Deviation" = "SSTA_Frequency_Standard_Deviation",
                                                           "Diversity" = "Diversity",
                                                           "Bleaching" = "Average_bleaching",
                                                           "Depth" = "Depth"), selected = "SSTA_Frequency_Standard_Deviation"),
        radioButtons("reg_y", "Y variable", choices = list("SSTA Frequency Standard Deviation" = "SSTA_Frequency_Standard_Deviation",
                                                             "Diversity" = "Diversity",
                                                             "Bleaching" = "Average_bleaching",
                                                             "Depth" = "Depth"), selected = "Diversity")
      ),
      conditionalPanel(
        condition = 'input.menu == "Rugosity"',
        sliderInput("rug_year", "Year Range", min = 1998, max = 2017, value = c(1998, 2017), sep = ""),
        radioButtons("rug_var", "Variable", choices = list("SSTA Frequency Standard Deviation" = "SSTA_Frequency_Standard_Deviation",
                                                           "Diversity" = "Diversity",
                                                           "Bleaching" = "Average_bleaching",
                                                           "Depth" = "Depth"), selected = "SSTA_Frequency_Standard_Deviation")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Home",
        fluidRow(
          box(img(src = "ReefC4_banner.png", height = 360, width = 640))
        ),
        fluidRow(
          box(h1("intro stuff"))
        )
      ),
      tabItem(tabName = "Map",
        fluidRow(
          box(plotOutput("map"))
        )
      ),
      tabItem(tabName = "Plots",
        fluidRow(
          box(plotOutput("regression"))
        )
      ),
      tabItem(tabName = "Rugosity",
        fluidRow(
          box(plotOutput("rugosity"))
        ),
        fluidRow(
          box(h1("What is rugosity?"), 
              "idk i dont do mars")
        )
      ),
      tabItem(tabName = "Model",
        fluidRow(
          box(h1("model stuff"))
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
  
  # Regression plots
  output$regression = renderPlot({
    plot_regression(input$reg_x, input$reg_y, input$reg_year[1], input$reg_year[2])
  })
  
  # Rugosity plots
  output$rugosity = renderPlot({
    plot_rugosity(input$rug_var, input$rug_year[1], input$rug_year[2])
  })
  
}

shinyApp(ui, server)