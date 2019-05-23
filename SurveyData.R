data<-readxl::read_xls("PHEVSurveyResponses.xls")
data[-8]
data[is.na(data)] <- 1


#https://www.listendata.com/2015/05/converting-multiple-numeric-variables.html

names <- c(2:104)
data[,names] <- lapply(data[,names] , factor)
str(data)

library(ggplot2)
library(RColorBrewer)
library(caret)



levels(data$`PHEV_Comfort 4`)[levels(data$`PHEV_Comfort 4`)=="1"] <- "WN"
levels(data$`PHEV_Comfort 4`)[levels(data$`PHEV_Comfort 4`)=="4"] <- "WN"

levels(data$`PHEV_Comfort 4`)[levels(data$`PHEV_Comfort 4`)=="2"] <- "M"
levels(data$`PHEV_Comfort 4`)[levels(data$`PHEV_Comfort 4`)=="5"] <- "M"

levels(data$`PHEV_Comfort 4`)[levels(data$`PHEV_Comfort 4`)=="3"] <- "WD"
levels(data$`PHEV_Comfort 4`)[levels(data$`PHEV_Comfort 4`)=="6"] <- "WD"


x = nearZeroVar(data, saveMetrics = TRUE)
str(x, vec.len=2)
x[x[,"zeroVar"] > 0, ] 



#Recursive Feature Elimination

set.seed(10)
rfcontrol<-rfeControl(functions=rfFuncs,method="cv",number=10)
imp<-train(training$`PHEV_Comfort 4`~.,data=training[-1],method="lvq",
  trControl=control)

#Recursive 
set.seed(10)
rfemodel<-rfe(training[,1:104],training[,89],sizes=c(1:25),rfeControl=rfcontrol,
              preProcess=c("center","scale"),verbose=T)
rfemodel$optVariables  
max(rfemodel$results$Accuracy)
plot(rfemodel)



model <- glm(data$`PHEV_Comfort 4` ~., data = training, family = binomial)
probabilities <- model %>% predict(testing, type = "response")

predicted.classes <- ifelse(probabilities > 0.7, "WD", "WN")

mean(predicted.classes == testing$`PHEV_Comfort 4`)


set.seed(10)
indxTrain <- createDataPartition(y = data$`PHEV_Comfort 4`,p = 0.80,list = FALSE)




training <- data[indxTrain,]
testing  <- data[-indxTrain,]

set.seed(10)
control<-trainControl(method="cv", number=10,savePredictions = TRUE, allowParallel = TRUE,
                      summaryFunction = multiClassSummary,
                      classProbs = TRUE,returnResamp="all",verboseIter = TRUE
)




#knn
set.seed(10)
knn<-train(`PHEV_Comfort 4`~.,data=training,method="LogitBoost",
           trControl=control,metric="AUC")
#Testing 
knn.pred <- predict(knn, newdata = testing,probability=TRUE)
knn.caret<-confusionMatrix(knn.pred,testing$`PHEV_Comfort 4`)
knn.cv<-confusionMatrix.train(data=knn,metric="ROC")
knn.caret
knn.cv
