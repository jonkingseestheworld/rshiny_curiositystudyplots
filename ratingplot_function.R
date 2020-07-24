
#The RainCloudPlot function used here was created by Allen et al (2019).  
#DOI: 10.12688/wellcomeopenres.15191.1  
#It can be downloaded here (https://github.com/RainCloudPlots/RainCloudPlots)



plot_rcp <- function(df, color1, color2) {

  ## Data wrangling  #Re-structuring data for plotting

  # Using 'aggregate' function, split data into subsets computing summary statistics for each
  # raw_rate: raw rating of curiosity and desirability for food
  # category: food or magic (or trivia in one of the fMRI experiments)
  # choice/decision:  1 for accepted; 0 for rejected

  agg_ppt_data <- aggregate(raw_rate~participant+category+choice, data=df, FUN=mean)
  
  # re-label choice/decision options
  agg_ppt_data["Decision"] <-NA
  agg_ppt_data$Decision[agg_ppt_data$choice==0] <- "Reject"
  agg_ppt_data$Decision[agg_ppt_data$choice==1] <- "Accept"
  

  # compile Mean, SD, & SE of ratings across participants into a DataFrame
  avg_data_m <- aggregate(raw_rate~category+choice, data=df, FUN=mean)
  names(avg_data_m)[3]<- "group_mean_rate"
  
  avg_data_sd <-  aggregate(raw_rate~category+choice, data=df, FUN=sd)  
  names(avg_data_sd)[3]<- "sd_rate"
  avg_data <- merge(avg_data_m, avg_data_sd, by = c("category", "choice"))
  
  
  avg_data["se_rate"] <- avg_data$sd_rate/sqrt(length(unique(df$participant)))
  
  # re-label choice/decision options
  avg_data["Decision"] <-NA
  avg_data$Decision[avg_data$choice==0] <- "Reject"
  avg_data$Decision[avg_data$choice==1] <- "Accept"
  
  # also compute the ymin and ymax of the error bars using SE of ratings
  avg_data$ymin = with(avg_data, group_mean_rate - se_rate)
  avg_data$ymax = with(avg_data, group_mean_rate + se_rate)
  


  ## RainCloudPlot 
  ### Preparing a theme for the plot 
  
  raincloud_theme <- theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_text(size=15, face="bold", color="black", margin = margin(t = 0, r = 4)), 
    axis.text.x = element_text(size = 13, face="bold", margin = margin(t = 10, r = 0, b = 0, l = 15), colour = 'black'),
    axis.text.y = element_text(size = 13,  margin = margin(t = 0, r = 5, b = 0, l = 0), colour = 'black'),
    legend.title=element_text(size=13, face="bold"),
    legend.text=element_text(size=13),
    legend.position = 'top', 
    #plot.title = element_text(lineheight=.8, face="bold", size = 16),
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(), 
    axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.ticks.length = unit(0, "cm")) 
  
  
  ### Plotting

  #use ggplot and R_rainclouds.script (esp 'geom_flat_violin' function) for   plotting
  rcp <- ggplot() + 
    geom_flat_violin(data = agg_ppt_data, aes(y = raw_rate, x = category,   fill=Decision), position = position_nudge(x = .2, y = 0), alpha = .6,   color=FALSE,  show.legend = FALSE) +
    # add data-points (average ppt rating of each participant)
    geom_point(data= agg_ppt_data, aes(y = raw_rate, x = category, color =   Decision), position = position_jitter(width=.1),size = 1.5, alpha = 0.8,   show.legend = FALSE, shape=16) +
    # add the summary scores (average rating across participants for each condition   )
    geom_pointrange (data=avg_data, aes(y = group_mean_rate, x = category, ymin=   ymin, ymax= ymax, color=Decision), shape=16, size=1.2, position =   position_dodge(width=0.1)) + 
    
    # adjust other plot features
    scale_color_manual(name="Decision", values= c(Accept = color1, Reject =   color2)) +
    scale_fill_manual(name="Decision", values= c(Accept = color1, Reject =   color2)) +
    
    raincloud_theme + 
    ggtitle("Relationship between Different Gamble Decisions \n and Level of Curiosity/Food Desirability") +
    theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5)) +
          
    scale_x_discrete(labels=c("Curiosity","Food Desirability")) +
    ylim(0,7) +
    labs(fill = "Decision") +
    scale_y_continuous(name="Level of Rating")
 
  rcp
  
}


