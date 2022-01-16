################################################################################
#                           FINAL YEAR PROJECT - ELE3001                       #
# Creator: DAFFA ARDANA                                                        #
# Student Number: 40255815                                                     #
# Project Title :                                                              #
# Data Analytics and Machine Learning for forecasting with Economics Data      #
#                                                                              #
################################################################################
#                                                                              #
# SEGMENTS OF THIS CODE:                                                       #
# 1. Boxplot:                                                                  #
#       -> Inflation, Unemployment, Employment, GDP                            #
# 2. correlation using 'Pearson' method                                        #
#       -> Employment - Unemployment, Inflation - Unemployment, Inflation - GDP#
#           Inflation - POPULATION, Unemployment - GDP,                        #
#             Unemployment - Population, Population - GDP                      #
# 3. Regression (linear)                                                       #
# 4. ARIMA MODEL - Forecasting Inflation using historical data                 #
# 5. ARIMA MODEL - Forecasting Inflation using Consumer Price Index (CPI)      #
#                                                                              #
################################################################################
#                                                                              #
# DATA SOURCE:                                                                 #
# 1. ONS.gov.uk : inflation, unemployment, GDP, employment                     #
# 2. FEDERAL RESERVE ECONOMIC DATA : CPI                                       #
#                                                                              #
################################################################################

# Installing packages and accessing library

if(!require(tidyverse)) install_data_month.packages("tidyverse")
if(!require("ggpubr")) install_data_month.packages("ggpubr")
if(!require(fpp2)) install_data_month.packages("fpp2")
if(!require(ggplot2)) install_data_month.packages("ggplot2")
if(!require(quantmod)) install_data_month.packages("quantmod")
if(!require(forecast)) install_data_month.packages("forecast")
if(!require(readxl)) install_data_month.packages("readxl")


library(tidyverse)
library("ggpubr")
library(fpp2)
library(ggplot2)
library(quantmod)
library(forecast)
library(forecast)
library(httr)
library(readr)


################################################################################

#importing data

#change directory to the location of the data being downloaded

################################################################################

#Monthly data

library(readxl)
all_data_month <- read_excel("D:/Apps/RStudio/Project - Inflation/ALL.xlsx", 
                               sheet = "Monthly", col_types = c("date", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric"))


#extracting years from date for boxplot
all_data_month$Year <- format(all_data_month$MONTH, format="%Y")

head(all_data_month[4:length(names(all_data_month))])


View(all_data_month)


#TREND FOR INFLATION
TREND_INFLATION<- ggplot() +
  # blue plot
  geom_line(data=all_data_month, aes(x=MONTH, y=INFLATION)) + 
  geom_smooth(data=all_data_month, aes(x=MONTH, y=INFLATION),method = "lm",fill="blue",colour="darkblue", size=1)

TREND_INFLATION + labs(title="UK Inflation: Linear Model Regression", y="Inflation(%)", 
                      x="Time", subtitle = "1989-2021")





#creating training data with 80% of original data

#importing Training_data.xlxs. blank document for training data
Training_data <- read_excel("D:/Apps/RStudio/Project - Inflation/ALL.xlsx", 
                            sheet = "Training")


#According to original data, there are 392 individual monthly data of inflation, 
#unemployment and employment

# Finding out 80% of data

num_training = as.integer(392*0.8)
print(num_training)

# Inputting 80% of data which is up to 2015-02-01,  or 313 individual montly data
#num_training is number of rows of data in training data

Training_data <- all_data_month[1:num_training,]


View(Training_data)



data <- all_data_month

#boxplot
#Inflation Boxplot
inf<-ggplot(data, aes(x=Year, y=INFLATION)) + 
  labs(y= "Inflation(%)", x = "Year", title = "UK-Inflation Boxplot",
       subtitle = "1989 - 2021") +
  geom_boxplot()+
  scale_y_continuous(breaks = round(seq(min(48), max(113), by = 1),1))
inf

#Unemployment Boxplot
unem<-ggplot(data, aes(x=factor(Year), y=UNEMPLOYMENT)) + 
  labs(y= "Unemployment(%)", x = "Year", title = "UK-Unemployment Boxplot", 
       subtitle = "1989 - 2021") +
  geom_boxplot()
unem



#correlation using training data
#Inflation - Unemployment



inflationXunemployment <- ggscatter(Training_data, x="UNEMPLOYMENT", y="INFLATION", 
                 title="Scatter Plot & Regression Line of Inflation vs Unemployment (Training Data 80%)",
                 subtitle="Year: 1989-2015",add = "reg.line", conf.int = TRUE,
                 cor.coef = TRUE, cor.method = "pearson",
                 xlab = "Unemployment(%)", ylab = "Inflation(%)")
inflationXunemployment




#according to summary, intercept = -0.15566, slope = 0.43036
# regression model: Inflation = Unemployment*slope + intercept


#predicting the rest of 20% of the data
#import the sheet for predicted data

#regression model
model <- lm(INFLATION~UNEMPLOYMENT, data = Training_data) #Create the linear regression
summary(model)


predicted <- read_excel("D:/Apps/RStudio/Project - Inflation/ALL.xlsx", 
                  sheet = "Predicted_20%")

num_predicted = 392 - num_training

print(num_predicted)



predicted <- all_data_month[(392-num_predicted):392,]

predicted$regression_model <- NA



View(predicted)

#plotting the predicted
datt <- predicted

unemp <- ts(datt[,5], start=c(2015,1), end=c(2021,8), frequency = 12)

prediction <- predict(model, newdata = unemp)

pred_data <- data.frame(prediction)

print(pred_data)

abline(model)
library("writexl") 

# sample dataframe

# saves the dataframe at the specified
# path
write_xlsx(pred_data,"D:/Apps/RStudio/Project - Inflation/REGRESSION_MODEL.xlsx")

#MOVING RESULT TO PREDICTED.XLXS

result_regression <- read_excel("D:/Apps/RStudio/Project - Inflation/REGRESSION_MODEL.xlsx", 
                                col_types = c("numeric"))

predicted$regression_model <- result_regression[,1]

View(predicted)

predicted$regression_model <- unlist(predicted$regression_model)


#line plot



line_comp <- ggplot() +
  # blue plot: real data
  geom_line(data=predicted, aes(y=INFLATION, x=MONTH), colour="darkblue") + 
  geom_smooth()+
  # red plot: model
  geom_line(data=predicted, aes(y=regression_model, x=MONTH), colour="red")+
  geom_smooth()+
  scale_y_continuous(breaks = round(seq(min(0), max(3), by = 0.1),1))



line_comp + labs(title="Line Plot: Predicted vs Confirmation Data (Recent 20%)", 
                 x="Time", 
        y="Inflation (%)", subtitle = "Red-Predicted Data || Blue-Confirmation Data")





scatter_comp <- ggplot() +
  # blue plot
  geom_point(data=predicted, aes(x=UNEMPLOYMENT, y=INFLATION)) + 
  geom_smooth(data=predicted, aes(x=UNEMPLOYMENT, y=INFLATION),method = "lm",
              fill="blue",
              colour="darkblue", size=1) +
  # red plot
  geom_point(data=predicted, aes(x=UNEMPLOYMENT, y=regression_model)) + 
  geom_smooth(data=predicted, aes(x=UNEMPLOYMENT, y=regression_model), fill="blue",
              colour="red", size=1)

scatter_comp + labs(title="Scatter + regression Line: Predicted vs Confirmation Data (Recent 20%||2015-2021)", y="Inflation(%)",
         x="Unemployment Rate (%)", subtitle = "Red-Predicted Data || Blue-Confirmation Data")








scatter_confirm <- ggscatter(predicted, x="UNEMPLOYMENT", y="INFLATION",
               add = "reg.line", conf.int = TRUE,
               cor.coef = TRUE, cor.method = "pearson",
               xlab = "Unemployment(%)", ylab = "Inflation(%)", color = "darkblue")


scatter_confirm + labs(title="Scatter+Regression Line of Confirmation Data (The rest 20%||2015-2021)", y="Inflation(%)", 
         x="Unemployment Rate (%)")

model2<-lm(INFLATION~UNEMPLOYMENT, data = predicted)
summary(model2)



