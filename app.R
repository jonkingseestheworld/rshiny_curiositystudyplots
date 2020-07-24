library(ggplot2)
library(cowplot)
#library(dplyr)
#library(lme4)


#rf <- read.csv('data/data1.csv', header=TRUE, sep=",")
#saveRDS(rf, file = "data/data1.rds")

rf <- readRDS("data/data1.rds")
glmm_param <- readRDS("data/glmm_param.rds")


# Read R code from another file
source("R_rainclouds.R")  #this R_rainclouds plot function was downloaded from https://github.com/RainCloudPlots/RainCloudPlots


source("ratingplot_function.R")  
source("sigmoidplot_function.R")  



#Define UI ----
ui <- fluidPage(
  titlePanel("Curiosity/Reward-driven Decision"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Choose an experiment to see how different cohorts perform."),
      selectInput("selectVar", label="Pick an experiment to display",
                  choices = list('Initial Pilot', 'fMRI',
                                 'Follow-up'), 
                  selected = "Initial Pilot"),
      br(),
      p("These plots here illustrating what factors predict decision-making in a risky gamble task is based on a series of experiments from this paper,", em("'Shared striatal activity in decisions to satisfy curiosity and hunger at the risk of electric shocks',"), "published in", a("Nature Human Behaviour", href="https://www.nature.com/articles/s41562-020-0848-3"), "(Mar, 2020)"),
      br(),
      p("In this study, research volunteers were shown videos of magic tricks (and food images). The volunteers were then offered the chance to see how the trick was performed (and the chance to get the food item) - 
        but before they could learn the secret (and get the food), the viewed a spinning wheel that gave them their odds of learning the solution versus their odds of receiving an electric shock. 
        Volunteers then had to decide on whether it was worth taking the gamble to satisfy their curiosity."),
      br(),
      p("Analysis of volunteers' responses shows that increasing expectation of prospective shock scales inversely with an individual's tendency to accept to gamble (i.e. decreasing acceptance rate; see the graph on the top), which is perhaps not too surprising -", em("when you expect more risk, you tend to take less risk.")),
      br(),
      p("What may be more interesting is that, on top of the prospective shock probability, individuals who took part in the study were willing to take more risk to gamble when they were more curious about learning the secret to the magic trick that was shown to them. 
        The graph at the bottom shows that the 'accept to gamble' decisions are related to higher levels of curiosity/food desirability compared to the 'reject' decisions)."),
      br(),
      p("These results have been replicated across a number of experiments, as you can see by choosing a different option from the drop-down list above."),
      br(),
      p("The study also identified the potential neural mechanisms that support motivated or curiosity-driven decisions. Refer to the original paper for the details.")

    ),
    mainPanel(
      plotOutput("sigplot"),
      br(),
      br(),
      br(),
      plotOutput("rcplot"),
      br(),
      br()
      
    )
  )
)
  
  

#Define server logic
server <- function(input, output){
  
  output$rcplot <- renderPlot({
    args <- switch(input$selectVar,
                           
                       'Initial Pilot' = list(subset(rf, expt=='behpilot'), "red", "blue"),
                       'fMRI' = list(subset(rf, expt=='mag_fmri' | expt=='triv_fmri'), "darkgreen","darkorange"),
                       'Follow-up' = list(subset(rf, expt=='fubeh'), "blue", "deeppink")
                   )
    #args$min <- input$sliderrange[1]
    #args$max <- input$sliderrange[2]
    
    do.call(plot_rcp, args) 
  }, height = 400, width = 600)

  output$sigplot <- renderPlot({
    args2 <- switch(input$selectVar,
                   
                   'Initial Pilot' = list(subset(rf, expt=='behpilot'), glmm_param, 'behpilot', "darkgreen", "darkorange"),
                   'fMRI' = list(subset(rf, expt=='mag_fmri' | expt=='triv_fmri'), glmm_param, 'fmri', "blue", "deeppink"),
                   'Follow-up' = list(subset(rf, expt=='fubeh'), glmm_param, 'fubeh', "red", "blue")
                   )

    do.call(plot_sig, args2) 
  }, height = 400, width = 600)
}
  
  

#Run the app ---
shinyApp(ui = ui, server=server)


