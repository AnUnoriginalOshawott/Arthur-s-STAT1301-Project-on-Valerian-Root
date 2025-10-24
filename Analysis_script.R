#Install required packages
#install.packages(c("readxl", "dplyr", "tidyr", "broom", "ggplot2"))

#Load Packages
library(readxl)

#Import excel data 
data <- read_excel("Research Project Data File.xlsx", sheet = 3, range = "A1:G63")

####Data processing

#initalise vectors 
ID <- c()
Treatment <-c()
Night <-c()
WakingMinutes <-c()

#fill vectors with collected data
for (i in 1:nrow(data)) {
  id <- data$ID[i] 
  treat <- data$Treatment[i] #store participants ID and treatment code (0 or 1)
  for (n in 1:5) { # walks through each night column for each ID
    value <- data[[as.character(n)]][i] #pulls the data in cell i,n and returns the recorded value
    if (!is.null(value) && !is.na(value) && value != "") { # apend only if the value exists and is numeric (or convertable)
      ID <- c(ID, id)
      Treatment <- c(Treatment, treat)
      Night <- c(Night, n)
      WakingMinutes <- c(WakingMinutes, as.numeric(value)) # creates four vectors for each data type relevant to an individual
    }
  }
}

 #create a data frame (id, treatment, night, wking minutes)
data_long <- data.frame(
  ID = as.integer(ID),
  Treatment = as.integer(Treatment),
  Night = as.integer(Night),
  WakingMinutes = as.integer(WakingMinutes),
  stringsAsFactors = FALSE
)

###Data analysis
# fit - wkaing minutes = beta0 + beta1 x night
#seperate control and valerian takers
data_control <- data_long[data_long$Treatment == 0, ] #all rows where treatment code = 0
data_val <- data_long[data_long$Treatment == 1, ] #all rows where treatment code = 1

#fit a linear model to each data set 
fit_control <- lm(WakingMinutes ~ Night, data = data = data_control)
fit_val <- lm(WakingMinutes ~ Night, data = data = data_val) 

#get the regression summaries (SE's, estimates, t-stats, DF, R^2, ...)
sum_control <- summary(fit_control)
sum_val <- summary(fit_val)

#pull out the slope, standard error and DF of each linear model
#control 
slope_control <- sum_control$coefficents["Night", "Estimate"] #slope of the control model - waking mins per night
se_control <- sum_control$coefficents["Night", "Std. Error"] #standard error of the control 
df_control <- sum_control$df[2] #degrees of freedom (n - 2)

#valerian
slope_control <- sum_val$coefficents["Night", "Estimate"] #slope of the valerian model - waking mins per night
se_control <- sum_val$coefficents["Night", "Std. Error"] #standard error of the valerian model
df_control <- sum_val$df[2] 

#generate a 95% CI for each slope 
ci_control <- confint(fit_control)["Night", ]
ci_val <- confint(fit_val)["Night", ]

###Welch test
#H0: beta1_control = beta1_valerian, alternativley: beta1_control - beta1_valerian = 0
#H1: beta1_control > beta1_valerian, alternativley: beta1_control - beta1_valerian > 0

 


