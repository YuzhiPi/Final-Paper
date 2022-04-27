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

#Knite Table available for use
kable(fin_d_s[1:15,], caption = "Preview of Cleaned Dataset")%>% 
  kable_styling(latex_options="scale_down")

#Create scatterplot used in article
bar_fire_op <- data_filt %>%
  ggplot(aes(x=Fire_Alarm_System_Operation)) +
  geom_bar(color = 'black', fill = 'steelblue') +
  labs(title = "Fire Alarm System Operation") +
  geom_text(aes(label=..count..), stat='count', position=position_dodge(1),vjust=1)

bar_fire_pre <- data_filt %>%
  ggplot(aes(x=Fire_Alarm_System_Presence)) +
  geom_bar(color = 'black', fill = 'steelblue') +
  labs(title = "Fire Alarm System Presence")+
  geom_text(aes(label=..count..),stat='count',position=position_dodge(1),vjust=1)

bar_sprink_op<- data_filt %>%
  ggplot(aes(x=Sprinkler_System_Operation)) +
  geom_bar(color = 'black', fill = 'steelblue') +
  labs(title = "Sprinkler System Operation")+
  geom_text(aes(label=..count..),stat='count',position=position_dodge(1),vjust=1)

bar_sprink_pre <- data_filt %>%
  ggplot(aes(x=Sprinkler_System_Presence)) +
  geom_bar(color = 'black', fill = 'steelblue') +
  labs(title = "Sprinkler System Presence")+
  geom_text(aes(label=..count..),stat='count',position=position_dodge(1),vjust=1)

figure1 <- ggarrange(bar_fire_pre, bar_fire_op, bar_sprink_pre, bar_sprink_op + font("x.text", size = 10),
                     ncol = 2, nrow = 2)

annotate_figure(figure1,
                top = text_grob("Visualization on Explanatory Variables", color = "black", size = 14),
                fig.lab = "Figure 1"
)

#Create table used in paper
estimated_loss<- data_filt %>%
  ggplot(aes(x=Estimated_Dollar_Loss)) +
  geom_bar(color = 'black', fill = 'steelblue') +
  labs(title = "Figure 2: Estimated Loss Due to Fire (in CAD)")
estimated_loss

#Sort frequency by decending order
frequency <- data.frame(table(fin_d_s$Estimated_Dollar_Loss))
names(frequency)[1] <- 'Estimated_Loss'
names(frequency)[2] <- 'Frequency'
frequency_sorted <- frequency[order(-(frequency$Frequency)),]
kable(frequency_sorted[1:5,], caption = "Most Common Fire Damage (in CAD)")

#Model building process
set.seed(2222)
#Sample the observations at 50:50 split
train <- fin_d_s[sample(1:nrow(fin_d_s), 595, replace=F), ]
#Generate Test Data Set, test whether test and train data are of similar characteristics
test <- fin_d_s[which(!(fin_d_s$X_id %in% train$X_id)),]
mtr <- format(apply(train[,-c(1,2,6,7)], 2, mean), scientific = F, digits = 3)
sdtr <- format(apply(train[,-c(1,2,6,7)], 2, sd), scientific = F, digits = 3)
mtest <- format(apply(test[,-c(1,2,6,7)], 2, mean), scientific = F, digits = 3)
sdtest <- format(apply(test[,-c(1,2,6,7)], 2, sd), scientific = F, digits = 3)

#fit full model 
full_model <- lm(Estimated_Dollar_Loss ~ Fire_Alarm_System_Operation+Fire_Alarm_System_Presence+Sprinkler_System_Operation+
                   Sprinkler_System_Presence, data = train)
test_model <- lm(Estimated_Dollar_Loss ~ Fire_Alarm_System_Operation+Fire_Alarm_System_Presence+Sprinkler_System_Operation+
                   Sprinkler_System_Presence, data = test)
#Check for multicolinearity using VIF test (all VIF < 5, no multicolinearity)
vif(full_model)

#Check scatterplot between yi and y_hat
plot(train$Estimated_Dollar_Loss ~ fitted(full_model), main=
       "Y versus Y-hat", xlab="Y-hat", ylab="Y")
abline(a = 0, b = 1)
lines(lowess(train$Estimated_Dollar_Loss ~ fitted(full_model)), lty=2)

summary(full_model)
summary(test_model)

#residual plot
res <- rstandard(full_model)
y_hat <- fitted(full_model)
plot(y_hat,res)

qqnorm(res)
qqline(res)

#create box plot for outliers
box_plot_1 <- fin_d_s %>% 
  ggplot(aes(y=Estimated_Dollar_Loss)) +
  geom_boxplot(color = 'black', fill = 'steelblue') + 
  labs(title = "Figure 3, Box Plot Analysis on Dependent Variable")

#Create SLR scatterplot and SLR model
scatter1 <- data_filt %>%
  ggplot(aes(x= Fire_Alarm_System_Operation, y = Estimated_Dollar_Loss)) +
  geom_point() + geom_smooth(method="lm")

scatter2 <- data_filt %>%
  ggplot(aes(x= Fire_Alarm_System_Presence, y = Estimated_Dollar_Loss)) +
  geom_point() + geom_smooth(method="lm")

scatter3 <- data_filt %>%
  ggplot(aes(x= Sprinkler_System_Operation, y = Estimated_Dollar_Loss)) +
  geom_point() + geom_smooth(method="lm")

scatter4 <-data_filt %>%
  ggplot(aes(x= Sprinkler_System_Presence, y = Estimated_Dollar_Loss)) +
  geom_point() + geom_smooth(method="lm")

figure4 <- ggarrange(scatter1, scatter2, scatter3, scatter4 + font("x.text", size = 10),
                     ncol = 2, nrow = 2)

annotate_figure(figure4,
                top = text_grob("Visualization on SLR Models", color = "black", size = 14),
                fig.lab = "Figure 4"
)

#Create SLR model
model_1 <- lm(Estimated_Dollar_Loss ~ Fire_Alarm_System_Operation, data = train)
summary(model_1)

model_2 <- lm(Estimated_Dollar_Loss ~ Fire_Alarm_System_Presence, data = train)
summary(model_2)

model_3 <- lm(Estimated_Dollar_Loss ~ Sprinkler_System_Operation, data = train)
summary(model_3)

model_4 <- lm(Estimated_Dollar_Loss ~ Sprinkler_System_Presence, data = train)
summary(model_4)

## Create frequency table available in the paper


#Frequency of Area of Origin
freq_origin <- data.frame(table(fin_d_s$Area_of_Origin))
names(freq_origin)[1] <- 'Area of Origin'
names(freq_origin)[2] <- 'Frequency'
freq_origin_sorted <- freq_origin[order(-(freq_origin$Frequency)),]
kable(freq_origin_sorted[1:5,], caption = "Most Common Fire Origin")

#Frequency of Fire Control
freq_control <- data.frame(table(fin_d_s$Method_Of_Fire_Control))
names(freq_control)[1] <- 'Area of Origin'
names(freq_control)[2] <- 'Frequency'
freq_control_sorted <- freq_control[order(-(freq_control$Frequency)),]
kable(freq_control_sorted[1:5,], caption = "Most Common Fire Control Method")

#Frequency of Possible Cause
freq_cause <- data.frame(table(fin_d_s$Possible_Cause))
names(freq_cause)[1] <- 'Area of Origin'
names(freq_cause)[2] <- 'Frequency'
freq_cause_sorted <- freq_cause[order(-(freq_cause$Frequency)),]
kable(freq_cause_sorted[1:5,], caption = "Most Common Causes of Fire")


