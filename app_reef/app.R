##########################################################

# R and Data Setup

##########################################################

library(shiny)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(viridis)
library(maps)
library(dplyr)
library(sf)
library(tidyverse)

# R code to read and set up the data
# Only run once at the start of the program
# Code taken from Pat and Camille
world_map = map_data("world2")

#cleaned file to use for app - no warnings and less computationally expensive
reef_geomorphic = st_read("Data/reef_final.gpkg")
reef_final = reef_geomorphic %>% 
  as.data.frame()

reef_final = reef_final %>% 
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
    filter(Date <= end_date) %>% 
    arrange(Average_bleaching) #re ordering so data appears on map with most bleached on top as ggplot plots from row=1
  
  #cleaning var name for titles 
  clean_var = str_replace_all(var, "_", " ")
  title_string = paste(clean_var, "of Coral Reefs from", start_date, "to", end_date)
  
  ggplot() + 
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.3) +
    geom_point(data = reef_temp, alpha = 0.7, aes(y = Latitude.Degrees, x = wrapLongitute, size = unlist(reef_temp[var]), color = unlist(reef_temp[var]))) + 
    labs(title = title_string, x = "", y = "", colour = var, size = var) +
    scale_colour_viridis(option = "inferno") + 
    theme_minimal() + 
    theme(legend.position="bottom") +
    guides(color= guide_legend(title = clean_var),
           size=guide_legend(title = clean_var))
  
}

## Function to plot the map, with variations based on the variables selected
#plot_map_inter = function(var, start_date, end_date) {
#  
#  reef_temp = reef_final %>%
#    filter(Date >= start_date) %>%
#    filter(Date <= end_date) %>% 
#    arrange(Average_bleaching) #re ordering so data appears on map with most bleached on top as ggplot plots from row=1
#  
#  #cleaning var name for titles 
#  clean_var = str_replace_all(var, "_", " ")
#  title_string = paste(clean_var, "of Coral Reefs from", start_date, "to", end_date)
#  
#  base_map = ggplot() + 
#    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.3) +
#    geom_point(data = reef_temp, alpha = 0.7, aes(y = Latitude.Degrees, x = wrapLongitute, 
#                                                  size = unlist(reef_temp[var]), 
#                                                  color = unlist(reef_temp[var]),
#                                                  text = paste('Reef Name: ', reef_temp$Reef.Name, 
#                                                    '<br>Country: ', reef_temp$Country, 
#                                                    paste(sprintf('<br> %s: ', unlist(reef_temp$reef_temp[var]))), unlist(reef_temp$reef_temp[var])))) + 
#    labs(title = title_string, x = "", y = "", colour = var, size = var) +
#    scale_colour_viridis(option="magma") + 
#    theme_minimal() + 
#    theme(legend.position="bottom") +
#    guides(color= guide_legend(title = clean_var),
#           size=guide_legend(title = clean_var))
#  
#  #interactive map
#  plotly(base_map, tooltip = "text") %>% 
#    layout(base_map, legend=list(orientation = "h"))
#  
#}
#c('Reef Name: ', reef_temp$Reef.Name, 
#  '<br>Country: ', reef_temp$Country, 
#  sprintf('<br> %s: ', unlist(reef_temp$reef_temp[var])), unlist(reef_temp$reef_temp[var]))

# Function to plot linear regression, with variations based on the variables selected
plot_regression = function(x, y, start_date, end_date) {
  
  reef_temp = reef_final %>%
    filter(Date >= start_date) %>%
    filter(Date <= end_date) 
  
  clean_x = str_replace_all(x, "_", " ")
  clean_y = str_replace_all(y, "_", " ")
  title_string = paste(clean_x, "vs", clean_y, "from", start_date, "to", end_date)
  
  ggplot(reef_temp, aes(unlist(reef_temp[x]), unlist(reef_temp[y]))) +
    geom_point(color="#e0812d", alpha = 0.3) +
    geom_smooth(method = "lm", color = I("#2ebaae"), fill = "#bf8058") +
    labs(title = title_string, x = clean_x, y = clean_y)
}

# Function to plot qqplots for the variables selected in regression
plot_qq = function(var, start_date, end_date) {
  
  reef_temp = reef_final %>%
    filter(Date >= start_date) %>%
    filter(Date <= end_date) 
  
  clean_var = str_replace_all(var, "_", " ")
  title_string = paste("QQPlot of", clean_var, "from", start_date, "to", end_date)
  
  ggplot(reef_temp, aes(sample = unlist(reef_temp[var]))) +
    stat_qq(color="#e0812d", alpha = 0.3) +
    stat_qq_line(color = I("#2ebaae")) + 
    labs(title = title_string, x = "Theoretical", y = "Sample") +
    theme_minimal() 
  
}

# Function to plot boxplots of rugosity against different variables
plot_rugosity = function(var, start_date, end_date) {
  
  reef_rug = reef_final %>%
    filter(Date >= start_date) %>%
    filter(Date <= end_date) %>% 
    arrange(rugosity)
  
  clean_var = str_replace_all(var, "_", " ")
  title_string = paste("Rugosity compared against", clean_var, "from", start_date, "to", end_date)
  
  reef_rug %>%
    ggplot(aes(x=rugosity, y= unlist(reef_rug[var]), fill=rugosity)) + 
    geom_violin(alpha=0.6,trim=FALSE, position = position_dodge(width = 0.75),size=1,color=NA) +
    geom_boxplot(width=0.4, color="black", alpha=0.5,
                 outlier.colour="red",
                 outlier.fill="red",
                 outlier.size=3, 
                 show.legend = F) +
    labs(title = title_string, x = "Rugosity", y = clean_var) +
    scale_fill_manual(values = c("#2ebaae", "#0892c3", "#e0812d")) + theme_minimal() +
                        theme(legend.position="none") 
  
}

##########################################################

# Shiny Component

##########################################################

ui = htmlTemplate("www/index.html",
                  slider_skin = chooseSliderSkin("Flat", color = "#2ebaae"),
                  #"Shiny", "Flat","Nice", "Simple", "HTML5")
                  map_slider = sliderInput("map_year", "", min = 1998, max = 2017, value = c(1998, 2017), sep = ""),
                  reg_slider = sliderInput("reg_year", "", min = 1998, max = 2017, value = c(1998, 2017), sep = ""),
                  rug_slider = sliderInput("rug_year", "", min = 1998, max = 2017, value = c(1998, 2017), sep = ""),
                  
                  map_title = verbatimTextOutput("map_title"),
                  map = plotOutput("map"),
                  #map_inter = plotlyOutput("map_inter"),
                  rugosity_plot = plotOutput("rugosity_plot"),
                  regression = plotOutput("regression"),
                  qq_1 = plotOutput("qq_1"),
                  qq_2 = plotOutput("qq_2")

                  
)

server = function(input, output) {
  
  # Initial map
  output$map = renderPlot({
    plot_map(input$map_var, input$map_year[1], input$map_year[2])
  })
  
#  # Interactive map
#  output$map_inter = renderPlotly({
#    plot_map_inter(input$map_var, input$map_year[1], input$map_year[2])
#  })

  # Regression plots
  output$regression = renderPlot({
    plot_regression(input$reg_x, input$reg_y, input$reg_year[1], input$reg_year[2])
  })
  
  # QQ plot for var 1
  output$qq_1 = renderPlot({
    plot_qq(input$reg_x, input$reg_year[1], input$reg_year[2])
  })
  
  # QQ plot for var 2
  output$qq_2 = renderPlot({
    plot_qq(input$reg_y, input$reg_year[1], input$reg_year[2])
  })
  
  # Rugosity plots
  output$rugosity_plot = renderPlot({
    plot_rugosity(input$rug_var, input$rug_year[1], input$rug_year[2])
  })
  
  # Map titles
  output$map_title = renderPrint({ 
    input$map_var 
  })
  
}

shinyApp(ui, server)