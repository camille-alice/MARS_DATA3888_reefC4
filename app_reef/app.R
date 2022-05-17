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

# Assumes that the current working directory is just /MARS_DATA3888_reefC4/app_reef
# Set wd to app_reef if not otherwise

# R code to read and set up the data
# Only run once at the start of the program
# Code taken from Pat and Camille
world_map = map_data("world2")

# cleaned file to use for app - no warnings and less computationally expensive
reef_geomorphic = st_read("../Data/reef_final.gpkg")
reef_final = reef_geomorphic %>% 
  as.data.frame()

reef_final = reef_final %>% 
  drop_na() %>% 
  mutate(class = as.factor(class)) %>%
  mutate(rugosity = factor(rugosity, levels = c("Low", "Medium", "High")))

# File with all the results from model testing (so we don't have to run the models here)
model_results = load("../Data/model_results.rds")


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

# Function to plot f1 results of the models
plot_f1 = function() {
  
  data = cbind(bin_cf$byClass['F1'], bin_sel_cf$byClass['F1'], nb_cf$byClass['F1'], 
               knn_cf$byClass['F1'], rf_cf$byClass['F1'], svm_cf$byClass['F1']) %>%
    as.data.frame() %>%
    gather()
  data$key = c("Initial Binomial Model", "Final Binomial Model", "Naive Bayes",
               "K-Nearest Neighbours", "Random Forest", "Support Vector Machine")
  
  data %>% 
    ggplot(aes(x = key, y = value, fill = key)) +
    geom_bar(stat = "identity") +
    labs(title = "F1 Results", x = "Model", y = "F1") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
}

# Function to plot the results of the models
plot_model_results = function() {
  
  data = cbind(cv_50acc5_bin, cv_50acc5_sel, cv_acc50_NB, cv_acc50_knn, cv_acc50_rf, cv_acc50_svm, cv_50acc5_beta) %>%
    as.data.frame() %>%
    rename("Initial Binomial Regression" = cv_50acc5_bin,
            "Final Binomial Regression" = cv_50acc5_sel,
            "Naive Bayes" = cv_acc50_NB,
            "K-Nearest Neighbours" = cv_acc50_knn,
            "Random Forest" = cv_acc50_rf,
            "Support Vector Machine" = cv_acc50_svm,
            "Beta Regression" = cv_50acc5_beta) %>%
    gather()
  
  data %>%
    ggplot(aes(x=key, y=value, fill=key)) + 
    geom_violin(alpha=0.6,trim=FALSE, position = position_dodge(width = 0.75),size=1,color=NA) +
    geom_boxplot(width=0.4, color="black", alpha=0.5,
                 outlier.colour="red",
                 outlier.fill="red",
                 outlier.size=3, 
                 show.legend = F) +
    labs(title = "Model Accuracy", x = "Models", y = "Accuracy") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
}

# Function to plot the results of the models except for beta regression
plot_model_results_no_beta = function() {
  
  data = cbind(cv_50acc5_bin, cv_50acc5_sel, cv_acc50_NB, cv_acc50_knn, cv_acc50_rf, cv_acc50_svm) %>%
    as.data.frame() %>%
    rename("Initial Binomial Regression" = cv_50acc5_bin,
           "Final Binomial Regression" = cv_50acc5_sel,
           "Naive Bayes" = cv_acc50_NB,
           "K-Nearest Neighbours" = cv_acc50_knn,
           "Random Forest" = cv_acc50_rf,
           "Support Vector Machine" = cv_acc50_svm) %>%
    gather()
  
  data %>%
    ggplot(aes(x=key, y=value, fill=key)) + 
    geom_violin(alpha=0.6,trim=FALSE, position = position_dodge(width = 0.75),size=1,color=NA) +
    geom_boxplot(width=0.4, color="black", alpha=0.5,
                 outlier.colour="red",
                 outlier.fill="red",
                 outlier.size=3, 
                 show.legend = F) +
    labs(title = "Model Accuracy", x = "Models", y = "Accuracy") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
}

# Alternative function to plotting each model result individually
# plot_model_results = function(data, title) { # maybe add colour code to params?
#   
#   data_df = data %>%
#     as.data.frame()
#   
#   ggplot(data = data_df, aes(x = "", y = .)) +
#     geom_violin(alpha=0.6,trim=FALSE, position = position_dodge(width = 0.75),size=1,color=NA) +
#     geom_boxplot(width=0.4, color="black", alpha=0.5,
#                  outlier.colour="red",
#                  outlier.fill="red",
#                  outlier.size=3, 
#                  show.legend = F) +
#     labs(title = title, x = "", y = "Accuracy")
#   
# }

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
                  qq_2 = plotOutput("qq_2"),
                  # binom_initial = plotOutput("binom_initial"),
                  # binom_selected = plotOutput("binom_selected"),
                  # nb = plotOutput("nb"),
                  # knn = plotOutput("knn"),
                  # rf = plotOutput("rf"),
                  # svm = plotOutput("svm"),
                  # beta = plotOutput("beta"),
                  model_f1 = plotOutput("model_f1"),
                  model_results = plotOutput("model_results"),
                  model_results_no_beta = plotOutput("model_results_no_beta")

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
  
  # F1 results
  output$model_f1 = renderPlot({
    plot_f1()
  })
  
  # Model accuracy results
  output$model_results = renderPlot({
    plot_model_results()
  })
  
  # Model accuracy results with no beta
  output$model_results_no_beta = renderPlot({
    plot_model_results_no_beta()
  })
  
  # output$binom_initial = renderPlot({
  #   plot_model_results(cv_50acc5_bin, "Initial Binomial Regression Model Accuracy") # maybe add colour code to params?
  # })
  # 
  # output$binom_selected = renderPlot({
  #   plot_model_results(cv_50acc5_sel, "Final Binomial Regression Model Accuracy")
  # })
  # 
  # output$nb = renderPlot({
  #   plot_model_results(cv_acc50_NB, "Naive-Bayes Model Accuracy")
  # })
  # 
  # output$knn = renderPlot({
  #   plot_model_results(cv_acc50_knn, "K-Nearest Neighbours Model Accuracy")
  # })
  # 
  # output$rf = renderPlot({
  #   plot_model_results(cv_acc50_rf, "Random Forest Model Accuracy")
  # })
  # 
  # output$svm = renderPlot({
  #   plot_model_results(cv_acc50_svm, "Support Vector Machine Model Accuracy")
  # })
  # 
  # output$beta = renderPlot({
  #   plot_model_results(cv_50acc5_beta, "Beta Regression Model Accuracy")
  # })
  
}

shinyApp(ui, server)