#### Work Place Setup ####
#install.packages("raster")
#install.packages("stringr")
#install.packages("gridExtra")
#install.packages("ggpubr")
library(gridExtra)
library(tidyverse)
library(dplyr)
library(kableExtra)
library(car)
library(stringr)
library(ggpubr)

### Dataset Stimulation ### 

#Load Data
data_orig <- read.csv("/Users/sherrypi/Desktop/STA304 Final Paper/FireIncidentsData.csv")
#Keep coloumns that are useful for the research
data_cate <- data_orig[c(1,2,7,14,15,21,28,31,38,39)]
#Take out row  N/A and missing values
data_de <- na.omit(data_cate)
data_de <- data_de[data_de$Fire_Alarm_System_Operation != "", ]

#Reduce key variables to numbers - taking out text following each category
data_de$Fire_Alarm_System_Operation <- substr(data_de$Fire_Alarm_System_Operation
                                              , 1, 1)
data_de$Fire_Alarm_System_Presence <- substr(data_de$Fire_Alarm_System_Presence
                                             , 1, 1)

data_de$Sprinkler_System_Operation <- substr(data_de$Sprinkler_System_Operation
                                             , 1, 1)
data_de$Sprinkler_System_Presence <- substr(data_de$Sprinkler_System_Presence
                                            , 1, 1)
#Taking out data entries where key explanatory variable takes on values of "undetermined"
data_de <- data_de[!(data_de$Fire_Alarm_System_Operation =="9" | data_de$Fire_Alarm_System_Presence=="9" 
                     | data_de$Sprinkler_System_Operation =="9"), ]
#Reduce dataset to residential fire by fuzzy matching geographical location with keyword "residential"
data_filt <- data_de[str_detect(data_de$Initial_CAD_Event_Type, "Residential"),]

#Create Dummy Variables for Key explanatory variables
data_filt$Fire_Alarm_System_Operation <- ifelse(data_filt$Fire_Alarm_System_Operation == "1", 1, 0)
data_filt$Fire_Alarm_System_Presence <- ifelse(data_filt$Fire_Alarm_System_Presence == "1", 1, 0)
data_filt$Sprinkler_System_Operation <- ifelse(data_filt$Sprinkler_System_Operation == "1", 1, 0)
data_filt$Sprinkler_System_Presence <- ifelse(data_filt$Sprinkler_System_Presence == "1", 1, 0)

fin_d_s <- data_filt[-c(6)] #Final Dataset

###Model building process###
set.seed(2222)
#Sample the observations at 50:50 split
train <- fin_d_s[sample(1:nrow(fin_d_s), 595, replace=F), ]
#Generate Test Data Set, test whether test and train data are of similar characteristics
test <- fin_d_s[which(!(fin_d_s$X_id %in% train$X_id)),]
mtr <- format(apply(train[,-c(1,2,6,7)], 2, mean), scientific = F, digits = 3)
sdtr <- format(apply(train[,-c(1,2,6,7)], 2, sd), scientific = F, digits = 3)
mtest <- format(apply(test[,-c(1,2,6,7)], 2, mean), scientific = F, digits = 3)
sdtest <- format(apply(test[,-c(1,2,6,7)], 2, sd), scientific = F, digits = 3)

#fit MLR FULL MODEL (Training and Testing)
full_model <- lm(Estimated_Dollar_Loss ~ Fire_Alarm_System_Operation+Fire_Alarm_System_Presence+Sprinkler_System_Operation+
                   Sprinkler_System_Presence, data = train)
test_model <- lm(Estimated_Dollar_Loss ~ Fire_Alarm_System_Operation+Fire_Alarm_System_Presence+Sprinkler_System_Operation+
                   Sprinkler_System_Presence, data = test)


#Create SLR model (All Training)
model_1 <- lm(Estimated_Dollar_Loss ~ Fire_Alarm_System_Operation, data = train)
summary(model_1)

model_2 <- lm(Estimated_Dollar_Loss ~ Fire_Alarm_System_Presence, data = train)
summary(model_2)

model_3 <- lm(Estimated_Dollar_Loss ~ Sprinkler_System_Operation, data = train)
summary(model_3)

model_4 <- lm(Estimated_Dollar_Loss ~ Sprinkler_System_Presence, data = train)
summary(model_4)



