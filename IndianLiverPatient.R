if(!require(dplyr)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(cowplot)) install.packages("cowplot", repos = "http://cran.us.r-project.org")
if(!require(grid)) install.packages("grid", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(ggExtra)) install.packages("ggExtra", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(PerformanceAnalytics)) install.packages("PerformanceAnalytics", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(leaps)) install.packages("leaps", repos = "http://cran.us.r-project.org")

library(ggplot2)
library(dplyr)
library(PerformanceAnalytics)
library(e1071)
library(randomForest)
library(leaps)
library(knitr)

#Download data set "Indian Liver Patient Records""
df_IndianPatient<- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00225/Indian%20Liver%20Patient%20Dataset%20(ILPD).csv", header = TRUE)

colnames(df_IndianPatient)<-c("Age", "Sex", "TB", "DB", "Alkphos", "Alamine", 
                              "Aspartate", "TP", "ALB", "A_G_Ratio", "Disease")

df_IndianPatient<-na.omit(df_IndianPatient)

df_IndianPatient[which(df_IndianPatient$Disease==2),"Disease"]<-0
##################
## Data Analisys##
##################


#we see the first rows of data set:
head(df_IndianPatient)


#Most variabiles are type integer or numerical but only the variable "Sex" is a factor
str(df_IndianPatient)


#We see the summary of data set and their distribution
summary(df_IndianPatient)


#######################
## Data Visualization##
#######################

#The type of Sex in the Indian Patients are more of type Male than Female

df_IndianPatient %>% ggplot(aes(x=Sex,fill= Sex,group=Sex)) + geom_bar()  + 
  labs(title="Bar plot for type of Sex") 



#We see the distribution of Age in the histogram, that is similar at the gaussian.

df_IndianPatient %>% ggplot(aes(x=Age)) +
  geom_histogram(breaks=seq(min(df_IndianPatient$Age), max(df_IndianPatient$Age), by = 3), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram for Age") +
  labs(x="Age", y="Count") + 
  xlim(c(min(df_IndianPatient$Age),max(df_IndianPatient$Age))) + 
  ylim(c(0,(as.numeric(which.max(table(df_IndianPatient$Age)))+5)))


#and the age for type of sex

df_ILPD_Male<-df_IndianPatient %>% filter(Sex=="Male") %>% ggplot(aes(x=Age)) +
  geom_histogram(breaks=seq(min(df_IndianPatient$Age), max(df_IndianPatient$Age), by = 3), 
                 col="Blue", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram for Men Age") +
  labs(x="Age", y="Count") + 
  xlim(c(min(df_IndianPatient$Age),max(df_IndianPatient$Age))) + 
  ylim(c(0,(as.numeric(which.max(table(df_IndianPatient$Age)))+5)))

df_ILPD_Female<-df_IndianPatient %>% filter(Sex=="Female") %>% ggplot(aes(x=Age)) +
  geom_histogram(breaks=seq(min(df_IndianPatient$Age), max(df_IndianPatient$Age), by = 3), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram for Female Age") +
  labs(x="Age", y="Count") + 
  xlim(c(min(df_IndianPatient$Age),max(df_IndianPatient$Age))) + 
  ylim(c(0,(as.numeric(which.max(table(df_IndianPatient$Age)))+5)))

library(cowplot)
cowplot::plot_grid(df_ILPD_Male, df_ILPD_Female, labels = "AUTO")




#The Disease for type of Sex have this distribution:
table(df_IndianPatient$Disease,df_IndianPatient$Sex)

df_IndianPatient$Disease<-as.factor(df_IndianPatient$Disease)
df_IndianPatient %>% ggplot(aes(x=Sex,fill= Disease,group=Disease)) + geom_bar()


library(ggExtra)
##plot TB and DB
plot_center = ggplot(df_IndianPatient, aes(x=TB,y=DB)) + 
geom_point() +
geom_smooth(method="lm")

ggMarginal(plot_center, type="histogram")

df_IndianPatient %>% ggplot(aes(x=Alkphos)) +
geom_histogram(breaks=seq(min(df_IndianPatient$Alkphos), max(df_IndianPatient$Alkphos), by = 3), 
col="red", 
fill="green", 
alpha = .2) + 
labs(title="Histogram for Alkphos") +
labs(x="Alkphos", y="Count") + 
xlim(c(min(df_IndianPatient$Alkphos),max(df_IndianPatient$Alkphos))) + 
ylim(c(0,(as.numeric(which.max(table(df_IndianPatient$Alkphos))))))

#Plot Alamine and Aspartate
plot_center = ggplot(df_IndianPatient, aes(x=Alamine,y=Aspartate)) + 
geom_point() +
geom_smooth(method="lm")

# default: type="density"
ggMarginal(plot_center, type="histogram")

#Now we can see the relation beetwen all variable, in an unique plot

df_IDLRnumeric<-df_IndianPatient[,-2]
df_IDLRnumeric[,1]<-as.numeric(df_IDLRnumeric[,1])
df_IDLRnumeric[,2]<-as.numeric(df_IDLRnumeric[,2])
df_IDLRnumeric[,3]<-as.numeric(df_IDLRnumeric[,3])
df_IDLRnumeric[,4]<-as.numeric(df_IDLRnumeric[,4])
df_IDLRnumeric[,5]<-as.numeric(df_IDLRnumeric[,5])
df_IDLRnumeric[,6]<-as.numeric(df_IDLRnumeric[,6])
df_IDLRnumeric[,7]<-as.numeric(df_IDLRnumeric[,7])
df_IDLRnumeric[,8]<-as.numeric(df_IDLRnumeric[,8])
df_IDLRnumeric[,9]<-as.numeric(df_IDLRnumeric[,9])
df_IDLRnumeric[,10]<-as.numeric(df_IDLRnumeric[,10])
chart.Correlation(as.data.frame(df_IDLRnumeric), histogram=TRUE, pch=19)

## Model of analisys
set.seed(7) # for reproducibility
test_index <- createDataPartition(y = df_IndianPatient$Disease, times = 1, p = 0.7, list = FALSE)
edx <- df_IndianPatient[-test_index,]
temp <- df_IndianPatient[test_index,]
########################
## Logistic Regression##
########################

#We applicate the logistic regression to predict a model, because the output variable assume value 0 and 1

fit <- glm(Disease ~ Age + Sex + TB + DB + Alkphos + Alamine + Aspartate + 
TP + ALB + A_G_Ratio, data = edx, family = binomial(link = "logit"))

summary(fit)

#Calculate the accuracy of model, that is
Edx_Predictions <- data.frame(Probability = predict(fit, edx, type = "response"))
Edx_Predictions$Prediction <- ifelse(Edx_Predictions > 0.5, 1, 0)
Edx_Predictions$Disease <- edx$Disease
accuracy <- mean(Edx_Predictions$Disease == Edx_Predictions$Prediction, na.rm = TRUE)
tot_accuracy<-data.frame(Model="Logistic Regression",Value=accuracy)

accuracy

####################################
## Backward in Logistic Regression##
####################################
#Now we applicate the backward for select the best significative variabiles to rappresent the model.

regfit.full=regsubsets(Disease~.,edx, method ="backward")
summary(regfit.full)


#The most significative variables for model are Age, DB, Alamine, Thart select the m
backward.model <- glm(Disease ~ Age + DB + Alamine, data = edx, family = binomial(link = "logit"))

#we calculate the accuracy
predBw<-predict(backward.model,temp,type = "response")
predicted.BW <- as.numeric(ifelse(predBw > 0.5, 1, 0))
accuracyBackward<-mean(predicted.BW==temp$Disease,na.rm=TRUE)
tot_accuracy<-rbind(tot_accuracy,data.frame(Model="Backward Logistic Regression",Value=accuracyBackward))
accuracyBackward


##################
## Random Forest##
##################
#Predict a Model with approach Random Forest
IDLR.rf=randomForest(Disease ~ ., data = edx,
na.action=na.exclude)
pred<-as.numeric(predict(IDLR.rf,temp))

#we have the accuracy model with this value:

predValid <- as.numeric(as.character(predict(IDLR.rf, temp, type = "class")))
# Checking classification accuracy
predValid<- ifelse(predValid > 0.5, 1, 0)
accuracyRF<-mean(as.numeric(predValid) == temp$Disease,na.rm=TRUE)  
tot_accuracy<-rbind(tot_accuracy,data.frame(Model="Random Forest",Value=accuracyRF))
accuracyRF

############
## Results##
############
#We see the results of accuracy of differents models applicate
tot_accuracy %>% knitr::kable()



