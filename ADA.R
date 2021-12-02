#install.packages("factoextra")
#install.packages('caret')
library(factoextra)
library(tidyverse)
library(ggplot2)
library(caret)
library(dplyr)

library(cluster)
library(factoextra)
library(BSDA)
library("partykit")



data <- read.csv("C:/Users/SREEDATH/Desktop/ADA_Project/diabetes.csv")

colSums(is.na(data))


head(data)
ggplot(data,aes(x = Glucose, y = BloodPressure, col = Outcome ,size =  Age))+geom_point()
                     
str(data)
range(data$Pregnancies)
ggplot(data,aes(x = Age, y = BloodPressure,col = Outcome ,size =  Glucose))+geom_point()


#Check if someone having BMI level > 30 then BP should be > 90
BMI_0_BP_90 = subset(data,BMI>30&BloodPressure>90)
View(BMI_0_BP_90)

#Check if someone having BMI level > 30 then BP should be <= 90

BMI_0_BP_NOT90 = subset(data,BMI>30&BloodPressure<=90)
View(BMI_0_BP_NOT90)


summary(data)
#data$Outcome<-as.factor(data$Outcome)


head(data)
#Preprocessing the data
preProcess_range_modeltr <- preProcess(data, method='range')
trainData <- predict(preProcess_range_modeltr,newdata = data)
levels(trainData$Outcome) <- c("Class0","Class1")

summary(trainData)



p3 <- ggplot(data, aes(x = Age, y=Glucose ,col =Outcome))+geom_point()
p3+geom_smooth(method = "loess")

p4 <- ggplot(data, aes(x = Age, y=BMI ,col =Outcome))+geom_point()
p4+geom_smooth(method = "loess")

p5 <- ggplot(trainData, aes(x = Age, y=Glucose ,col =Outcome))+geom_point()
p5+geom_smooth(method = "loess")

p6 <- ggplot(trainData, aes(x = Age, y=BMI ,col =Outcome))+geom_point()
p6+geom_smooth(method = "loess")



#t-test

#t- test for BMI and Outcome

df1 <- data%>%select(BMI,Outcome)%>%filter(Outcome == '0')
df2 <- data%>%select(BMI,Outcome)%>%filter(Outcome == '1')
dfbmi1<- df1$BMI
dfbmi2<- df2$BMI
t.test(dfbmi1,dfbmi2)


#t test for Glucose and Outcome

df1glu <- data%>%select(Glucose,Outcome)%>%filter(Outcome == '0')
df2glu <- data%>%select(Glucose,Outcome)%>%filter(Outcome == '1')
dfglu1<- df1glu$Glucose
dfglu2<- df2glu$Glucose
t.test(dfglu1,dfglu2)

#t-test for DiabetesPedigreeFunction

df1pf <- data%>%select(DiabetesPedigreeFunction,Outcome)%>%filter(Outcome == '0')
df2pf <- data%>%select(DiabetesPedigreeFunction,Outcome)%>%filter(Outcome == '1')
dfpf1<- df1pf$DiabetesPedigreeFunction
dfpf2<- df2pf$DiabetesPedigreeFunction
t.test(dfpf1,dfpf2)


#install.packages("BSDA")
library(BSDA)
#significant defernce in both the values
#BMI can be a good factor to see the difference

#z-test for BMI and Outcome
x<-dfbmi1
y<-dfbmi2
zbmi<-z.test(x,sigma.x=0.5,y,sigma.y=0.5,conf.level = 0.90)
zbmi
rm(x,y)


str(data)

#======================================================================================================

#Various Correlation measure

corr <- cor(data)
corr
corr<-round(corr, 3)
corr
#install.packages("Hmisc")
library("Hmisc")
corr2 <- rcorr(as.matrix(data))
corr2

# Extract the correlation coefficients
corr2$r
# Extract p-values
corr2$P

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

corr2<-rcorr(as.matrix(data[,1:9]))
corr2
flattenCorrMatrix(corr2$r, corr2$P)


symnum(corr, abbr.colnames = FALSE)


#install.packages("corrplot")

library(corrplot)

# correlogram plot
corrplot(corr, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


#Use chart.Correlation():
#install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
#Let's produce the matrix of scatterplots and histogram
chart.Correlation(data, histogram=TRUE, pch=19)

#Using heatmap
# Get some colors
col<- colorRampPalette(c("red", "orange", "blue"))(20)
heatmap(x = corr, col = col, symm = TRUE)


#we compute the matrix of correlations between the variables using corrplot()
corrplot(cor(data[, -10]), type = "lower", method = "number")


#==================================================================================================
#ANOVA


anovabmi <- aov(data$Outcome ~ data$BMI)
summary(anovabmi)


anovaglu <- aov(data$Outcome ~ data$Glucose)
summary(anovaglu)

anovadia <- aov(data$Outcome ~ data$DiabetesPedigreeFunction)
summary(anovadia)




#====================================================================================================

#5.Time series algorithm

#Print time series analysis basis on Age (40-60) as per glucose value.

age_40_60 = subset(data,Age>40&Age<60)
time_series_age = ts(age_40_60$Age,start=1,end=162,frequency = 1)
time_series_glucose = ts(age_40_60$Glucose,start=1,end=162,frequency = 1)
age_glucose_matrix = matrix(c(time_series_age,time_series_glucose),nrow=162)
time_series_matrix = ts(age_glucose_matrix,start=1,end=162,frequency = 1)
plot(time_series_matrix)


time_series_age
time_series_glucose
time_series_matrix


#=====================================================================================================
#SVM-Support Vector Machine


# Input the dataset for analysis 
library(readr)
library("e1071")
#Using the SVM model defined in the package with all variables considered in building the model
svm_model=svm(Outcome~.,data=data,type='C-classification')
#Summary will list the respective parameters such as cost, gamma, etc.
summary(svm_model)
#Predicting the data with the input to be the dataset itself, we can calculate the accuracy with a confusion matrix
pred=predict(svm_model,newdata=data)
table(pred,data$Outcome)
#The accuracy turns out to be 82.42%-----(463+170/98+37)
#Now let's tune the SVM parameters to get a better accuracy on the training dataset
svm_tune <- tune(svm, train.x=data, train.y=data$Outcome, 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
print(svm_tune)
#Gives an optimal cost to be 10 and a gamma value of 0.5

svm_model_after_tune <- svm(Outcome ~ ., data=data, type='C-classification',kernel="radial", cost=10, gamma=0.5)
summary(svm_model_after_tune)
#The results show us that there is an improved accuracy of about 98%, results are obtained in the form of a confusion matrix
pred <- predict(svm_model_after_tune,data)
system.time(predict(svm_model_after_tune,data))
table(pred,data$Outcome)

#============================================================================================
#Conditional Inference tree
#install.packages("partykit")
library("partykit")
modelct = lm(Outcome~Pregnancies+Glucose+BloodPressure+SkinThickness+Insulin+BMI+DiabetesPedigreeFunction+Age,data=data)
x = c(coef(modelct)[2],coef(modelct)[3],coef(modelct)[4],coef(modelct)[5],coef(modelct)[6],coef(modelct)[7],coef(modelct)[8],coef(modelct)[9])
max_3 = sort(x,decreasing = T)
max_3 = max_3[1:3]
modelct = lm(Outcome~BMI+DiabetesPedigreeFunction+Age,data=data) #check for the values in max using above given characterstics of modelct
tree = ctree(Outcome~BMI+DiabetesPedigreeFunction+Age,data)
plot(tree)
#================================================================================================
#Logistic Regeression


set.seed(123)
n <- nrow(data)
train <- sample(n, trunc(0.70*n))
data_training <- data[train, ]
data_testing <- data[-train, ]

# Training The Model
glm_fm1 <- glm(Outcome ~., data = data_training, family = binomial)
summary(glm_fm1)

#skin thickness,insulin and age are not significant that is >0.1
#so remove them 

glm_fm2 <- update(glm_fm1, ~. - SkinThickness - Insulin - Age )
summary(glm_fm2)


par(mfrow = c(2,2))
plot(glm_fm2)

# Testing the Model
glm_probs <- predict(glm_fm2, newdata = data_testing, type = "response")
glm_pred <- ifelse(glm_probs > 0.5, 1, 0)
glm_pred
glm_probs
#print("Confusion Matrix for logistic regression"); table(Predicted = glm_pred, Actual = pima_testing$Diabetes)
confusionMatrix(glm_pred, data_testing$Outcome ) 
# Confusion Matrix for logistic regression

acc_glm_fit <- confusionMatrix(glm_pred, data_testing$Outcome )$overall['Accuracy']


#==================================================================================================
#Clustering
#k-means clustering
#outcome is 0

dfcl1 <- data%>%filter(Outcome == '0')
  dfclusel1 <- subset(dfcl1,select = -c(Pregnancies,SkinThickness,Outcome,Insulin,DiabetesPedigreeFunction,Glucose))
  
  
kmclu1 <- kmeans(dfclusel1, 4, nstart = 25)
print(kmclu1)

fviz_cluster(kmclu1, data = dfclusel1)


table1 <- table(dfclusel1$Glucose,kmclu1$cluster)
table1

plot(dfclusel1[c("Age","Glucose")], col=kmclu1$cluster)
points(kmclu1$centers[,c("Age","Glucose")], col =1:3,pch =8, cex=3)


#outcome is 1

dfcl2 <- data%>%filter(Outcome == '1')
dfclusel2 <- subset(dfcl2,select = -c(Pregnancies,SkinThickness,Outcome,Insulin,DiabetesPedigreeFunction,Glucose))


kmclu2 <- kmeans(dfclusel2, 3, nstart = 25)
print(kmclu2)

fviz_cluster(kmclu2, data = dfclusel2)


table2 <- table(dfclusel2$Glucose,kmclu2$cluster)
table2

plot(dfclusel2[c("SkinThickness","Glucose")], col=kmclu2$cluster)
points(kmclu2$centers[,c("SkinThickness","Glucose")], col =1:3,pch =8, cex=3)


#we can clearly see there is good quality clustering possible when the outcome is 1
#that means studying these groups act more similar to the medicines and side effects,etc.
#when we dont divide the data

dfclusel = subset(data,select = -c(Pregnancies,SkinThickness,Outcome,Insulin,DiabetesPedigreeFunction,Glucose))

set.seed(123)
kmclu <- kmeans(dfclusel, 3, nstart = 25)
print(kmclu)

fviz_cluster(kmclu, data = dfclusel)

table(dfclusel$Glucose,kmclu$cluster)
table

plot(dfclusel[c("Age","Glucose")], col=kmclu$cluster)
points(kmclu$centers[,c("Age","Glucose")], col =1:3,pch =8, cex=3)

#with this table we can see that all the people with low glucose came under
#cluster 3 almost till 120

#===============================================================================

#medoids clustering
library(cluster)
library(factoextra)

#for normal data
dfmed <- subset(data,select = -c(Pregnancies,SkinThickness,Outcome,Insulin,DiabetesPedigreeFunction,Glucose))
kmed <- pam(x=dfmed,k=3)
kmed
kmed$clustering
kmed$medoids

summary(kmed)
fviz_cluster(kmed)

#for outcome0
dfmed1 <- subset(dfcl1,select = -c(Pregnancies,SkinThickness,Outcome,Insulin,DiabetesPedigreeFunction,Glucose))
kmed1 <- pam(x=dfmed1,k=3)
kmed1
kmed1$clustering
kmed1$medoids

summary(kmed1)
fviz_cluster(kmed1)

#for outcome1
dfmed2 <- subset(dfcl2,select = -c(Pregnancies,SkinThickness,Outcome,Insulin,DiabetesPedigreeFunction,Glucose))
kmed2 <- pam(x=dfmed2,k=3)
kmed2
kmed2$clustering
kmed2$medoids

summary(kmed2)
fviz_cluster(kmed2)


#heirarical clustering


#heiclu=hclust(dfcl1, method="complete")

#plot(heiclu,cex = 0.6, hang = -1,main="The Complete Method")
#==========================================
