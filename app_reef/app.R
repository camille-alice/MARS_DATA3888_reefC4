##########################################################

# R and Data Setup

##########################################################

library(shiny)
library(ggplot2)
library(viridis)
library(maps)
library(dplyr)
library(sf)
library(tidyverse)
library(car)
library(cvTools)
library(randomForest)
library(leaflet)
library(leaflet.extras)
library(shinyWidgets)
library("scales")
library(leaflegend)

# Assumes that the current working directory is just /MARS_DATA3888_reefC4/app_reef
# Set wd to app_reef if not otherwise

# R code to read and set up the data
# Only run once at the start of the program
# Code authors: 
# Camille - app design and explanations, 
# Nathan - plots and base app, 
# Patrick - modeling and evaluation

# hex colour palette 
#015d6f, #1c858b, #2ebaae, #82d8ac, #c5ffb6, #c37c0b, #bf4402

world_map = map_data("world2")

# cleaned file to use for app - no warnings and less computationally expensive
reef_geomorphic = st_read("Data/reef_final.gpkg")
reef_final = reef_geomorphic %>% 
  as.data.frame()

reef_final = reef_final %>% 
  drop_na() %>% 
  mutate(class = as.factor(class)) %>%
  mutate(rugosity = factor(rugosity, levels = c("Low", "Medium", "High")))

# File with all the results from model testing (so we don't have to run the models here)
model_results = load("Data/model_results.rds")


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
    #scale_colour_viridis(option="magma") + 
    scale_colour_gradientn(colours=c("#023e7d", "#0892c3", "#e79f52", "#e0812d", "#bf4402")) + 
    theme_minimal() + 
    theme(legend.position="bottom") +
    guides(color= guide_legend(title = clean_var),
           size=guide_legend(title = clean_var))
  
}

## Function to plot the map, with variations based on the variables selected
plot_map_leaf = function(var, start_date, end_date) {
  
  reef_temp = reef_final %>%
    filter(Date >= start_date) %>%
    filter(Date <= end_date) %>% 
    arrange(Average_bleaching) #re ordering so data appears on map with most bleached on top as ggplot plots from row=1
  
  #cleaning var name for titles 
  clean_var = str_replace_all(var, "_", " ")
  title_string = paste(clean_var, "of Coral Reefs from", start_date, "to", end_date)
  pal = colorNumeric(c("#023e7d", "#0892c3", "#e79f52", "#e0812d", "#bf4402"), domain = reef_temp$var)
  circle_size = unname(rescale(unlist(reef_temp[var]), to = c(3, 10)))
  labels = sprintf("Reef Name: %s <br/> Country: %s <br/> %s: %.2f",
                   reef_temp$Reef.Name, reef_temp$Country, clean_var, unlist(reef_temp[var])) %>% 
    lapply(htmltools::HTML)
  
  # create leaflet map
  leaf_map = leaflet(reef_temp) %>%
    addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Grey Map") %>% #grey
    #addProviderTiles(providers$GeoportailFrance.orthos, group = "Terrain Map") %>% #coloured map
    addCircleMarkers(
      data = reef_temp,
      color = ~pal(unlist(reef_temp[var])),
      radius = ~circle_size,
      lng = ~wrapLongitute,
      lat = ~Latitude.Degrees,
      label = labels, 
      weight = 1) %>%
    addLegendNumeric(pal = pal,
                     values = unlist(reef_temp[var]),
                     title = clean_var,
                     orientation = 'horizontal',
                     width = 200,
                     height = 5,
                     position = 'bottomleft') 
  #%>%addLayersControl(overlayGroups = c("Map Labels"),
  #    options = layersControlOptions(collapsed = TRUE)) 
  
  leaf_map
  
}

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
    labs(title = title_string, x = clean_x, y = clean_y) + 
    theme_minimal()
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
    scale_fill_manual(values = c("#0892c3", "#2ebaae", "#e0812d")) + theme_minimal() +
    theme(legend.position="none") 
  
}

# Function to plot f1 results of the models
plot_f1 = function() {
  
  data = cbind(bin_cf$byClass['F1'], bin_sel_cf$byClass['F1'], rnd_cf$byClass['F1'], svm_cf$byClass['F1'], 
               nb_cf$byClass['F1'], knn_cf$byClass['F1'], rf_cf$byClass['F1']) %>%
    as.data.frame() %>%
    gather()
  data$key = c("Initial Binomial Model", "Final Binomial Model", "Binomial Model with random effect", "Support Vector Machine",
               "Naive Bayes","K-Nearest Neighbours", "Random Forest")
  
  data %>%
    ggplot(aes(x=reorder(key,value,na.rm = TRUE), y = value, fill = reorder(key,value,na.rm = TRUE))) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values=c("#023e7d", "#0892c3", "#4dcde4", "#80cdc4", "#e79f52", "#e0812d", "#bf4402")) +
    labs(title = "F1 Results", x = "Models", y = "F1") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), legend.position="none") 
  
}

# Function to plot the results of the models
plot_model_results = function() {
  
  data = cbind(cv_50acc5_bin, cv_50acc5_sel, cv_50acc_rnd, cv_acc50_svm, 
               cv_acc50_NB, cv_acc50_knn, cv_acc50_rf) %>%
    as.data.frame() %>%
    rename("Initial Binomial Regression" = cv_50acc5_bin,
           "Final Binomial Regression" = cv_50acc5_sel,
           "Binomial Regression with random effect" = cv_50acc_rnd,
           "Support Vector Machine" = cv_acc50_svm,
           "Naive Bayes" = cv_acc50_NB,
           "K-Nearest Neighbours" = cv_acc50_knn,
           "Random Forest" = cv_acc50_rf) %>%
    gather()
  
  data %>%
    ggplot(aes(x=reorder(key,value,na.rm = TRUE), y=value, fill=reorder(key,value,na.rm = TRUE))) + 
    geom_violin(alpha=0.6,trim=FALSE, position = position_dodge(width = 0.75),size=1,color=NA) +
    geom_boxplot(width=0.4, color="black", alpha=0.5,
                 outlier.colour="red",
                 outlier.fill="red",
                 outlier.size=3, 
                 show.legend = F) +
    scale_fill_manual(name = "Models", values=c("#023e7d", "#0892c3", "#4dcde4", "#80cdc4", "#e79f52", "#e0812d", "#bf4402")) +
    labs(title = "Model Accuracy", x = "Models", y = "Accuracy") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  
}

plot_mean = function() {
  
  data = cbind(bin_mean, sel_mean, rnd_mean, svm_mean, 
               nb_mean, knn_mean, rf_mean) %>%
    as.data.frame() %>%
    gather()
  data$key = c("Initial Binomial Model", "Final Binomial Model", "Binomial Model with random effect", "Support Vector Machine",
               "Naive Bayes","K-Nearest Neighbours", "Random Forest")
  
  data %>%
    ggplot(aes(x=reorder(key,value,na.rm = TRUE), y = value, fill = reorder(key,value,na.rm = TRUE))) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values=c("#023e7d", "#0892c3", "#4dcde4", "#80cdc4", "#e79f52", "#e0812d", "#bf4402")) +
    labs(title = "Mean Accuracy", x = "Models", y = "Accuracy") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), legend.position="none") 
  
}

plot_sd = function() {
  
  data = cbind(bin_sd, sel_sd, rnd_sd, svm_sd, 
               nb_sd, knn_sd, rf_sd) %>%
    as.data.frame() %>%
    gather()
  data$key = c("Initial Binomial Model", "Final Binomial Model", "Binomial Model with random effect", "Support Vector Machine",
               "Naive Bayes","K-Nearest Neighbours", "Random Forest")
  
  data %>%
    ggplot(aes(x=reorder(key,value,na.rm = TRUE), y = value, fill = reorder(key,value,na.rm = TRUE))) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values=c("#023e7d", "#0892c3", "#4dcde4", "#80cdc4", "#e79f52", "#e0812d", "#bf4402")) +
    labs(title = "SD Accuracy", x = "Models", y = "SD") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), legend.position="none") 
  
}

plot_time = function() {
  
  i = 10
  num_record = c()
  while (i < 1000){
    num_record = append(num_record,i)
    i = i + 10
  }
  
  data = cbind(time_bin, time_svm, time_nb, 
               time_knn, time_rf) %>%
    as.data.frame() %>%
    gather() %>%
    mutate(key = case_when(
      key == "time_bin" ~ "Binomial Model",
      key == "time_svm" ~ "Support Vector Machine",
      key == "time_nb" ~ "Naive Bayes",
      key == "time_knn" ~ "K-Nearest Neighbours",
      key == "time_rf" ~ "Random Forest"
    ))
  data$time = num_record
  
  data %>%
    ggplot(aes(x = time, y = value, group = key)) +
    geom_line(aes(colour = reorder(key,value,na.rm = TRUE)), size=1) +
    scale_colour_manual(values=c("#023e7d", "#0892c3", "#4dcde4", "#80cdc4", "#e79f52")) +
    labs(title = "Time Performance", x = "Size of test data", y = "Time (ms)", colour = "Models") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  
}

predict_rf = function(SSTA_Frequency_Standard_Deviation, Depth, Diversity, num_rugosity) {
  
  set.seed(3888)
  
  cvSets = cvTools::cvFolds(nrow(save_X), 10)
  test_id = cvSets$subset[cvSets$which == sample.int(10, 1)]
  X_test = save_X[test_id, ] %>% scale()
  X_train = save_X[-test_id, ] %>% scale()
  y_test = save_y[test_id]
  y_train = save_y[-test_id]
  
  rf_res = randomForest::randomForest(x = X_train, y = as.factor(y_train))
  
  num_rugosity = as.numeric(num_rugosity)
  data = matrix(c(SSTA_Frequency_Standard_Deviation, Depth, Diversity, num_rugosity), nrow = 1, ncol = 4)
  colnames(data) = c("SSTA_Frequency_Standard_Deviation", "Depth", "Diversity", "num_rugosity")
  data = rbind(data, save_X[test_id,]) %>% scale()
  # data = rbind(data[1,], X_test)
  
  fit = predict(rf_res, data)
  
  if (fit[[1]] == 0) {
    print('<a class="image featured"><img src="images/non_bleached.jpg" alt=""/></a>')
  } else {
    print('<a class="image featured"><img src="images/bleached.jpg" alt=""/></a>')
  }
  
}

reg_words = function(reg_x, reg_y) {
  
  clean_x = str_replace_all(reg_x, "_", " ")
  if (reg_x == reg_y){
    print(paste("The plot above is a straight line as you are comparing", clean_x, "with", clean_x,"!",
                "\nPlease select differing variables to gain a deeper insight."))
  }
  
  else if (reg_x == "Average_bleaching" & reg_y == "SSTA_Frequency_Standard_Deviation"){
    print("As you can see from the plot above, there is a negative correlation with average bleaching and sea surface temperature anomalies 
(SSTA) frequency standard deviation (SD). What this means is that the more variance (SD) in frequency of SSTAs experienced by a reef, the less 
bleaching it has. As per Darwin’s theory of evolution, we believe this is due to higher variance in the frequency of SSTAs making the reef more adaptable, 
therefore more robust to changes in temperature. Thus, the coral is able to ‘keep their cool’ (and their algae) and be less susceptible to bleaching.")
  }
  
  else if (reg_x == "SSTA_Frequency_Standard_Deviation" & reg_y == "Average_bleaching"){
    print("As you can see from the plot above, there is a negative correlation with sea surface temperature anomalies (SSTA) frequency standard deviation (SD)
and average bleaching. What this means is that the more variance (SD) in frequency of SSTAs experienced by a reef, the less 
bleaching it has. As per Darwin’s theory of evolution, we believe this is due to higher variance in the frequency of SSTAs making the reef more adaptable, 
therefore more robust to changes in temperature. Thus, the coral is able to ‘keep their cool’ (and their algae) and be less susceptible to bleaching.
          Hint: Try swapping the X and Y variable for a clearer correlation.")
  }
  
  else if (reg_x == "Average_bleaching" & reg_y == "Diversity"){
    print("As you can see from the plot above, there is a negative correlation with bleaching and diversity. What this means is that the more diverse a reef 
is (i.e. the more coral species live within that reef), the less percentage of bleaching it has. The causation of this correlation is not so simple as diversity
and coral bleaching have a complex feedback relationship where coral bleaching impacts diversity of reefs, and the diversity of coral reefs impacts the chances 
of a reef to bleach. We expect more diverse reefs to experience less bleaching since the higher genetic diversity results in higher general resilience against 
not only increases in sea surface temperature, but also other causes of bleaching such as: pollution, disease, sedimentation, freshwater flooding, and changes
in light (West, et. al. 2003).")
  }
  
  else if (reg_x == "Diversity" & reg_y == "Average_bleaching"){
    print("As you can see from the plot above, there is a negative correlation with bleaching and diversity. What this means is that the more diverse a reef 
is (i.e. the more coral species live within that reef), the less percentage of bleaching it has. The causation of this correlation is not so simple as diversity
and coral bleaching have a complex feedback relationship where coral bleaching impacts diversity of reefs, and the diversity of coral reefs impacts the chances 
of a reef to bleach. We expect more diverse reefs to experience less bleaching since the higher genetic diversity results in higher general resilience against 
not only increases in sea surface temperature, but also other causes of bleaching such as: pollution, disease, sedimentation, freshwater flooding, and changes
in light (West, et. al. 2003).
          Hint: Try swapping the X and Y variable for a clearer correlation.")
  }
  
  else if (reg_x == "Average_bleaching" & reg_y == "Depth"){
    print("As you can see from the plot above, there is a positive correlation with bleaching and depth. What this means is that the deeper a reef is,
the more percentage of bleaching it has. While this may be a significant correlation, more research is needed in this area as deeper environments are 
generally less impacted by people, so are healthier and more resilient. Deep water corals usually prefer cooler waters so they might be more vulnerable 
to changes in temperature and therefore bleach easier. The deep-water currents could also have an impact on the corals.")
  }
  
  else if (reg_x == "Depth" & reg_y == "Average_bleaching"){
    print("As you can see from the plot above, there is a positive correlation with bleaching and depth. What this means is that the deeper a reef is,
the more percentage of bleaching it has. While this may be a significant correlation, more research is needed in this area as deeper environments are 
generally less impacted by people, so are healthier and more resilient. Deep water corals usually prefer cooler waters so they might be more vulnerable 
to changes in temperature and therefore bleach easier. The deep-water currents could also have an impact on the corals.
          Hint: Try swapping the X and Y variable for a clearer correlation.")
  }
  
  else if (reg_x == "SSTA_Frequency_Standard_Deviation" & reg_y == "Diversity"){
    print("As you can see from the plot above, there is a positive correlation with sea surface temperature anomalies (SSTA) frequency standard deviation (SD) 
and diversity. What this means is that the higher variance in SSTAs frequency a reef experience, the more diverse a reef is.")
  }
  
  else if (reg_x == "Diversity" & reg_y == "SSTA_Frequency_Standard_Deviation"){
    print("As you can see from the plot above, there is a positive correlation with sea surface temperature anomalies (SSTA) frequency standard deviation (SD) 
and diversity. What this means is that the higher variance in SSTAs frequency a reef experience, the more diverse a reef is.
          Hint: Try swapping the X and Y variable for a clearer correlation.")
  }
  
  else if (reg_x == "SSTA_Frequency_Standard_Deviation" & reg_y == "Depth" | reg_x == "Depth" & reg_y == "SSTA_Frequency_Standard_Deviation"){
    print("While it is interesting to see the spread of depth and SSTA frequency SD, our studies focus is on bleaching. 
Try swapping a variable for bleaching and see the results!")
  }
  
  else if (reg_x == "Diversity" & reg_y == "Depth" | reg_x == "Depth" & reg_y == "Diversity"){
    print("While it is interesting to see the spread of depth and diversity, our studies focus is on bleaching. 
Try swapping a variable for bleaching and see the results!")
  }
  
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
                  ssta_slider = sliderInput("ssta_slider", "", min = 0, max = 12, value = 0, sep = ""),
                  depth_slider = sliderInput("depth_slider", "", min = 0, max = 15, value = 0, sep = ""),
                  diversity_slider = sliderInput("diversity_slider", "", min = 0, max = 600, value = 0, sep = ""),
                  map_title = verbatimTextOutput("map_title"),
                  map = plotOutput("map"),
                  map_leaf = leafletOutput("map_leaf"),
                  rugosity_plot = plotOutput("rugosity_plot"),
                  regression = plotOutput("regression"),
                  qq_1 = plotOutput("qq_1"),
                  qq_2 = plotOutput("qq_2"),
                  model_f1 = plotOutput("model_f1"),
                  model_results = plotOutput("model_results"),
                  model_mean = plotOutput("model_mean"),
                  model_sd = plotOutput("model_sd"),
                  model_time = plotOutput("model_time"),
                  predict_results = htmlOutput("predict_results"),
                  regres_words = textOutput("regres_words")
                  
)

server = function(input, output) {
  
  # Initial map
  output$map = renderPlot({
    plot_map(input$map_var, input$map_year[1], input$map_year[2])
  })
  
  # Interactive map
  output$map_leaf = renderLeaflet({
    plot_map_leaf(input$map_var, input$map_year[1], input$map_year[2]) 
  })
  
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
  
  # F1 results
  output$model_f1 = renderPlot({
    plot_f1()
  })
  
  # Model accuracy results
  output$model_results = renderPlot({
    plot_model_results()
  })
  
  # Model mean results
  output$model_mean = renderPlot({
    plot_mean()
  })
  
  # Model sd results
  output$model_sd = renderPlot({
    plot_sd()
  })
  
  # Model time results
  output$model_time = renderPlot({
    plot_time()
  })
  
  # Prediction results
  output$predict_results = renderText({
    predict_rf(input$ssta_slider, input$depth_slider, input$diversity_slider, input$rugosity_val)
  })
  
  # Regression words 
  output$regres_words = renderText({ 
    reg_words(input$reg_x, input$reg_y) 
  })
  
}

shinyApp(ui, server)