
## Plotting Logistic function (Sigmoid curve)

plot_sig <- function(df, glmm_param, exptname, color1, color2){
  
  ## Some data recoding and GLMM model fitting
  # Using 'glmer' function to fit raw data into a Generalised Linear Mixed-effects Model
  
  # ctrd_rate: centered values of curiosity and food desirability ratings
  # category: food or magic (or trivia in one of the fMRI experiments)
  # choice/decision:  1 for accepted; 0 for rejected
  # re_prob: also centered values of percentage of shock probability


  #modelling the curiosity trials separately

  #modelling the food trials separately

  
  
  ## Data wrangling for plotting
  #Preparing modelled data for plotting logistic function

  #Generate a series of continuous x-variable (for plotting later)
  x <- seq(-5,5,length.out=100)
    
  ##To retrieve Estimates (beta coefficients) of various variables ouputted from GLMM   for data modelling
  #Curiosity trials
  cintercept <- with(glmm_param, b_intercept[expt==exptname & category=="curiosity"])
  cbeta_prob <- with(glmm_param, b_ctrd_prob[expt==exptname & category=="curiosity"])
  cbeta_rate <- with(glmm_param, b_ctrd_rate[expt==exptname & category=="curiosity"])


  #the modelled y-cooridinate data based on logsitic function
  y_cmean <- 1/(1+exp(-1*(cintercept + cbeta_prob*x)))
    
  #Food trials
  fintercept <- with(glmm_param, b_intercept[expt==exptname & category=="food"])
  fbeta_prob <- with(glmm_param, b_ctrd_prob[expt==exptname & category=="food"])
  fbeta_rate <- with(glmm_param, b_ctrd_rate[expt==exptname & category=="food"])

  #the modelled y-cooridinate data based on logsitic function
  y_fmean <- 1/(1+exp(-1*(fintercept + fbeta_prob*x)))
    
    
  #compile all (modelled) datapoints into a DataFrame for plotting
  df_plotdata <- data.frame(x_var=x, y_cmean=y_cmean, y_fmean=y_fmean)


  ##additional data wrangling: for plotting distribution of participants' datapoints in the same graph
  #to summarise data so that (for each subject) the average acceptance rate is computed at each (shock) probability level
  
  df_curi <- subset(df, category=="curiosity")
  
  cur_selcols <- data.frame(df_curi$participant, df_curi$re_prob, df_curi$choice) 
  names(cur_selcols)[c(1,2,3)] <- c("participant","re_prob","choice")
 
  ##recoding reward outcome probability to shock probability 
  cur_selcols["ShockProb"] <- NA
  cur_selcols$ShockProb[cur_selcols$re_prob== -2] <- 4
  cur_selcols$ShockProb[cur_selcols$re_prob== -1] <- 2
  cur_selcols$ShockProb[cur_selcols$re_prob== 0] <- 0
  cur_selcols$ShockProb[cur_selcols$re_prob== 1] <- -2
  cur_selcols$ShockProb[cur_selcols$re_prob== 2] <- -4
  abc<- aggregate(choice~participant+ShockProb, data=cur_selcols, FUN=function(x) avg_accept=mean(x))
  names(abc)[3]<-"avg_ppt_choice"
  
  
  #to summarise data so that (for each subject) the average acceptance rate is computed at each (shock) probability level
  df_food <- subset(df, category=="food")
  
  food_selcols <- data.frame(df_food$participant, df_food$re_prob, df_food$choice) 
  names(food_selcols)[c(1,2,3)] <- c("participant","re_prob","choice")
  
  ##recoding reward outcome probability to shock probability 
  food_selcols["ShockProb"] <- NA
  food_selcols$ShockProb[food_selcols$re_prob== -2] <- 4
  food_selcols$ShockProb[food_selcols$re_prob== -1] <- 2
  food_selcols$ShockProb[food_selcols$re_prob== 0] <- 0
  food_selcols$ShockProb[food_selcols$re_prob== 1] <- -2
  food_selcols$ShockProb[food_selcols$re_prob== 2] <- -4
  
  efg<- aggregate(choice~participant+ShockProb, data=food_selcols, FUN=function(x) avg_accept=mean(x))
  names(efg)[3]<-"avg_ppt_choice"
  
  
 
  ### Plotting the modelled curves (separately for curiosity condition and food   condition)
  sigplot<- ggplot(data=df_plotdata, aes(x=x_var)) + 
    geom_line(aes(y=y_cmean, color='curiosity'), size=1, linetype="solid")  +
    geom_line(aes(y=y_fmean, color='food'), size=1, linetype="solid") +
    # also plot the raw data-points (from each participant at each probabilty level)   on the graph
    geom_point(data = abc, mapping = aes(x = ShockProb, y =   avg_ppt_choice), shape=1, position=position_jitter(h=0.03,w=0.18), color=color1) +
    geom_point(data = efg, mapping = aes(x =   ShockProb, y = avg_ppt_choice), shape=1, position=position_jitter(h=0.03,w=0.18),   color=color2) +
    scale_color_manual(name="Category", values= c(curiosity =   color1, food = color2))
    
  # add and adjust graph elements
  axis.title.bold <- element_text(face="bold", size="15", color="black")
  axis.text<- element_text(size="13", color="black")
    
  sigplot <- sigplot + scale_x_continuous(name="Probability of Shock (%)",   breaks=c(-4,-2,0,2,4), labels=c("-4"="16.7%", "-2"="33.3%", "0"="50%",   "2"="66.7%", "4"="83.3%")) + 
      scale_y_continuous(name="Acceptance Rate", breaks=c(0, 0.5, 1)) +
      ggtitle("Effect of Prospective Electric Shock on Gamble Decision") +
      theme_classic() +
      theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5),
            axis.title=axis.title.bold, axis.text =axis.text, 
            legend.title=element_text(size=13, face="bold"),
            legend.text=element_text(size=13),
            legend.position = 'top') 

  sigplot
}

