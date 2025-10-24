###Analysis Script
#This script unpacks data collected in our study on the effectiveness 
#of Valerian root in reducing waking minutes during sleep
#
#The code:
#  - Imports the experimental data
#  - Converts the spreadsheet  into a data frame
#     with columns: ID, Treatment, Night, and Waking minutes
#  - Fits two linear models:
#        Control:  y_C = beta0_C + beta1_C * t
#        Valerian: y_V = beta0_V + beta1_v * t
#     where:
#        - y_C, y_V  = mean waking minutes per night
#        - t = night number (0 -> 4)
#        - beta0 = baseline waking time (Night 0)
#        - beta1 = nightly rate of change in waking minutes
#  - Performs a Welch t-test to check whether the nightly rate of change
#     differs between the control and Valerian groups
#        H0: beta1_C = beta1_V   (valerian has no effect)
#        H1: beta1_C > beta1_V   (Valerian reduces waking time)
#  - Creates diagnostic plots
#  - Exports summary statistics (slopes, SEs, confidence intervals, p-values)
#     and plots the fitted regression lines across both groups
#

#remove the commenting on this install line when running for the first time to ensure the code runs correctly
#install.packages("readxl")

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
  for (n in 0:4) { # walks through each night column for each ID
    value <- data[[as.character(n)]][i] #pulls the data in cell i,n and returns the recorded value
    if (!is.null(value) && !is.na(value) && value != "") { # apend only if the value exists and is numeric (or convertable)
      ID <- c(ID, id)
      Treatment <- c(Treatment, treat)
      Night <- c(Night, n)
      WakingMinutes <- c(WakingMinutes, as.numeric(value)) # creates four vectors for each data type relevant to an individual
    }
  }
}

 #create a data frame (id, treatment, night, waking minutes)
data_long <- data.frame(
  ID = as.integer(ID),
  Treatment = as.integer(Treatment),
  Night = as.integer(Night),
  WakingMinutes = as.integer(WakingMinutes)
)

#recode to make beta0 the baseline (night 0)
data_long$Night0 <- data_long$Night - 1L

###Data analysis

# fit - wakaing minutes = beta0 + beta1 x night
#seperate control and valerian data
data_control <- data_long[data_long$Treatment == 0, ] #all rows where treatment code = 0
data_val <- data_long[data_long$Treatment == 1, ] #all rows where treatment code = 1

#fit a linear model to each data set 
fit_control <- lm(WakingMinutes ~ Night0, data = data_control) #linear regression on control data
fit_val <- lm(WakingMinutes ~ Night0, data = data_val) #linear regression on valerian data 

#get the regression summaries (SE's, estimates, t-stats, DF, R^2 and more)
sum_control <- summary(fit_control)
sum_val <- summary(fit_val)

#pull out the slope, standard error and DF of each linear model
#control 
slope_control <- sum_control$coefficients["Night0", "Estimate"] #slope of the control model - waking mins per night
se_control <- sum_control$coefficients["Night0", "Std. Error"] #standard error of the control 
df_control <- sum_control$df[2] #degrees of freedom (n - 2)

#valerian
slope_val <- sum_val$coefficients["Night0", "Estimate"] #slope of the valerian model - waking mins per night
se_val <- sum_val$coefficients["Night0", "Std. Error"] #standard error of the valerian model
df_val <- sum_val$df[2] 

#generate a 95% CI for each slope 
ci_control <- confint(fit_control, level = 0.95)["Night0", ]
ci_val <- confint(fit_val, level = 0.95)["Night0", ]

###Welch test
#H0: beta1_control = beta1_valerian, alternativley: beta1_control - beta1_valerian = 0
#H1: beta1_control > beta1_valerian, alternativley: beta1_control - beta1_valerian > 0
#the test statistic is formed using the two independent slope estimates:
#   T = (beta1_Control - beta1_Valerian) / sqrt(SE(beta1_control)^2 + SE(beta1_valerian)^2) 
  

slope_diff <- slope_control - slope_val #slope difference
se_diff <- sqrt(se_control^2 + se_val^2) #slope SE's are independent
t_welch <- slope_diff / se_diff #test statistic from welch test

#DF for welch with different variance
df_welch <- (se_control^2 + se_val^2)^2 / ((se_control^4)/df_control + (se_val^4)/df_val)

#p value
p_value <- 1 - pt(t_welch, df = df_welch)

###assumption checks
#residuals vs fitted; plot(model, which = 1)
#qq normal; plot(model, which = 2)

#residuals vs fitted - control
png("residual_fitted_control.png", width = 1200, height = 800)
plot(fit_control, which = 1)
dev.off()

#residuals vs fitted - valerian 
png("residual_fitted_valerian.png", width = 1200, height = 800)
plot(fit_val, which = 1)
dev.off()

#qq - control
png("QQ_control.png", width = 1200, height = 800)
plot(fit_control, which = 2)
dev.off()

#qq - valerian 
png("QQ_valerian.png", width = 1200, height = 800)
plot(fit_val, which = 2)
dev.off()

### plot data sets and their regression lines 

png("linear_reg.png", width = 1200, height = 800)
plot(jitter(data_control$Night0, amount = 0.1), #jitter adds horizontal offset so overlapping points are clearer
  data_control$WakingMinutes,
  xlab = "Night t",
  ylab = "waking minutes per night",
  main = "Sugar pill vs Valerian: Waking minutes per night",
  xlim = c(0,4),
  ylim = range(data_long$WakingMinutes),
  col = "red"
)

#add valerian data
points(jitter(data_val$Night0, amount = 0.1),
       data_val$WakingMinutes,
       col = "blue"
       )

#add regression lines
abline(fit_control, col = "red")
abline(fit_val, col = "blue")

#add legend
legend("topright",
  legend = c("control data & regression", "valerian data & regression"),
  col = c("red", "blue"), pch = 16, bty = "n"
)
dev.off()

#write data to csv files

#slope summary 
slope_summary <- data.frame(
  group = c("control", "valerian"),
  slope = c(slope_control, slope_val),
  se_slope = c(se_control, se_val),
  df = c(df_control, df_val),
  CI_lower = c(ci_control[1], ci_val[1]),
  Ci_upper = c(ci_control[2], ci_val[2])
)
write.csv(slope_summary, "slope_summary.csv", row.names = FALSE)

#welch summary 
welch_summary <- data.frame(
  slope_diff = slope_diff, 
  test_statistic = t_welch,
  df_welch = df_welch, 
  p_value = p_value 
)
write.csv(welch_summary, "welch_summary.csv", row.names = FALSE)




