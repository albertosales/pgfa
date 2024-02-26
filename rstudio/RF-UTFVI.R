#install.packages("randomForestExplainer")
library(caret)
library(randomForest)
library(party)
library(dplyr)
library(plyr)
library(readr)
library(rpart)
library(rpart.plot)
library(rattle)
library(caTools)
library(e1071)
library(randomForestExplainer)
#library(ElemStatLearn)
set.seed(1234)

#data <- read_csv("era5_2017_2022.csv")#, col_names = FALSE)
#era5 = "era5_2020_cat"
ano = '2020'
era5 = paste('era5',ano,sep="_")
print(era5)
data <- read_csv(paste(era5,"csv",sep="."),show_col_types = FALSE)#, col_names = FALSE)
data <- subset(data, select = -c(windquad_cat) )
data
#data$Temp2m <- as.numeric(data$Temp2m)
data$skinTemp <- as.numeric(data$skinTemp)
data$RH <- as.numeric(data$RH)
data$windspeed <- as.numeric(data$windspeed)
data$windquad <- as.numeric(data$windquad)
data$UTFVI <- as.numeric(data$UTFVI)
data$windquad_cat <- as.factor(data$windquad_cat)
#data$UHIn <- as.numeric(data$UHIn)
#data$UTFVI2m <- as.numeric(data$UTFVI2m)
#data$UHIn2m <- as.numeric(data$UHIn2m)
data$UHIp <- as.factor(data$UHIp)
#data$UHIp2m <- as.factor(data$UHIp2m)
data$windpower <- as.factor(data$windpower)
#data$local <- as.factor(data$local)
#data$timestamp <- as.factor(data$timestamp)
data$surface_pressure <- as.numeric(data$surface_pressure)
#data$NDVI <- as.numeric(data$NDVI)
#data<-rename(data, c("X1"="Temp2m", "X2"="RH", "X3"="windspeed", "X4"="winddir", "X5"="UTFVI", "X6"="windquad", "X7"="UHIp", "X8"="UHIn"))
data
summary(data)
#data$UHIp <- factor(data$UHIp, levels=c('extremo','forte','fraco','medio','muito_forte','nenhum'))
data$UHIp <- factor(data$UHIp, levels=c('extremo','forte','muito_forte'))
#data$UHIp2m <- factor(data$UHIp2m, levels=c('extremo','forte','muito_forte'))
#data$windquad_cat <- factor(data$windquad_cat, levels=c('Primeiro','Segundo','Terceiro','Quarto','Quinto','Sexto','Sétimo','Oitavo'))
data <- subset(data, select = c(RH, windspeed, windquad, UHIp))
data
aux = createDataPartition(data$UHIp, p=0.7, times = 1, list = F)
tr <- trainControl(method = "cv", number = 10) #setting parameters for the model
tuneGrid <- expand.grid(
  mtry = c(2:4)
)
treino = data[aux,]
teste = data[-aux,]
treino[,-4]
teste[,-4]
prop.table(table(treino$UHIp))
treino[,4]
teste[,4]
prop.table(table(teste$UHIp))

rf <- randomForest(x = treino[,-4],
                   y = treino$UHIp,
                   xtest = teste[,-4],
                   ytest = teste$UHIp,
                   ntree = 500,
                   mtry = 3,
                   importance=TRUE,
                   replace = T,
                   nodesize = 10,
                   #maxnode = 15,
                   keep.forest = T)
rf
title = paste('Importâncias das Variáveis Micro-Climáticas do ano',ano,sep=" ")
print(varImpPlot(rf, main=title))
rfStats(rf)
getTree(rf)
plot(rf)
varImp(rf)

#treinamento
prediction <- predict(rf, treino)
confusionMatrix(prediction, treino$UHIp, mode = "everything", positive="1")

#validação
prediction <- predict(rf, teste)
confusionMatrix(prediction, teste$UHIp,mode = "everything", positive="1")

#REGRAS
target = UHIp ~ RH + windspeed + windquad
fit <- rpart(target, data = data)
rpart.rules(fit,cover=TRUE)

#windquad rate was greater for Relative Humidity
rpart.rules(rpart(windquad ~ UHIp + windspeed + RH , data = data))

rpart.rules(rpart(windspeed ~ UHIp + RH + windquad, data = data))
# main indicator of missing data is 3rd class esp. with many children
#obs.with.nas <- rowSums(is.na(data)) > 0
#rpart.rules(rpart(obs.with.nas ~ windquad , data = data, method = "class"))

fit$splits
fit$splits[fit$splits[,'ncat']==1,]

###### FIM REGRAS #######


partition = createDataPartition(data$UHIp, p=0.7, times = 1, list = F)
df_train <- data[partition, ]
df_test <- data[-partition, ]
tr <- trainControl(method = "cv", number = 10) #setting parameters for the model

### FUNCIONANDO PARA TREINO E TESTE

ind = sample(2, nrow(data), replace=TRUE, prob=c(0.7,0.3))
trainData = data[ind==1,]
testData = data[ind==2,]
target = UHIp ~ windspeed + windquad + RH + NDVI
nrow(data)
nrow(trainData)
prop.table(table(trainData$UHIp))
nrow(testData)
prop.table(table(testData$UHIp))

fit <- randomForest(target,   data=trainData)
print(fit) # view results
rfStats(fit)
varImpPlot(fit)
varImp(fit)

#treinamento
prediction <-predict(fit, trainData)
confusionMatrix(prediction, trainData$UHIp, mode = "everything", positive="1")
prop.table(table(trainData$UHIp))
nrow(trainData)

fit <- randomForest(target,   data=testData)
print(fit) # view results
rfStats(fit)
varImpPlot(fit)
varImp(fit)

#validação/teste
prediction <-predict(fit, testData)
confusionMatrix(prediction, testData$UHIp,mode = "everything", positive="1")
prop.table(table(testData$UHIp))
nrow(testData)

nrow(data)
nrow(trainData)
nrow(testData)
prop.table(table(trainData$UHIp))
prop.table(table(testData$UHIp))

########################################

####### USING CARET#######
partition = createDataPartition(data$UHIp, p=0.7, times = 1, list = F)
df_train <- data[partition, ]
df_test <- data[-partition, ]
ind = sample(2, nrow(data), replace=TRUE, prob=c(0.7,0.3))
trainData = data[ind==1,]
testData = data[ind==2,]
trainData
testData

tr <- trainControl(method = "cv", number = 10) #setting parameters for the model

fit.rf <- train(UHIp ~ .,                      #fit model
                data = trainData, 
                method = "rf", #random forest model
                metric = "Accuracy",
                trControl = tr, #model parameters
                allowParallel = TRUE, #use parallel processing (if availible)
                ntree = 500, #number of trees
                keep.forest=TRUE,
                importance=TRUE,
                verbose = FALSE)

fit.rf
varImp(fit.rf)
plot(fit.rf)
str(fit.rf,1)
fit.rf$results
print(fit.rf$finalModel)
print(fit.rf$resample) # view results
#rfStats(fit_rf)
fit.rf$finalModel$importance
fit.rf$finalModel$votes

#treinamento
prediction <- predict(fit.rf, trainData)
confusionMatrix(prediction, trainData$UHIp,mode = "everything", positive="1")

#validação
prediction <- predict(fit.rf, testData)
confusionMatrix(prediction, testData$UHIp,mode = "everything", positive="1")


randomForest::varImpPlot(fit.rf$finalModel,main=title)

#########################################3



##########################

target = UHIp2m ~ windspeed + windquad + RH

ind = sample(2, nrow(data), replace=TRUE, prob=c(0.7,0.3))
trainData = data[ind==1,]
testData = data[ind==2,]
trainData
testData
data_rf = randomForest(UHIp ~ ., data=trainData, ntree=500,mtry=3, proximity=T, importance = TRUE)
table(predict(data_rf), trainData$UHIp)
rfStats(data_rf)
data_rf2 = randomForest(UHIp ~ ., data=testData, ntree=500,mtry=3, proximity=T, importance = TRUE)
table(predict(data_rf2), testData$UHIp)
rfStats(data_rf2)



ind = sample(2, nrow(data), replace=TRUE, prob=c(0.7,0.3))
trainData = data[ind==1,]
testData = data[ind==2,]
nrow(data)
nrow(trainData)
nrow(testData)
prop.table(table(trainData$UHIp2m))
fit <- randomForest(target,   data=trainData)
print(fit) # view results
rfStats(fit)
varImpPlot(fit)
varImp(fit)

#treinamento
prediction <-predict(fit, trainData)
confusionMatrix(prediction, trainData$UHIp2m, mode = "everything", positive="1")
prop.table(table(trainData$UHIp2m))
nrow(trainData)

#validação/teste
prediction <-predict(fit, testData)
confusionMatrix(prediction, testData$UHIp2m,mode = "everything", positive="1")
prop.table(table(testData$UHIp2m))
nrow(testData)



fit_rf = train(UHIp ~ .,
               data,
               method = "rf",
               metric = "rmse",
               tuneGrid = tuneGrid,
               # mtry = 2,
               preProcess="pca",
               trControl = tr,
               importance = TRUE,
               # nodesize = 14,
               ntree = 500,
               # maxnodes = 24
               )
warnings()
#treinamento
prediction <- predict(fit_rf, trainData)
confusionMatrix(prediction, trainData$UHIp)
#validação
prediction <- predict(fit_rf, testData)
confusionMatrix(prediction, testData$UHIp)
varImp(fit_rf)


arvore <- rpart::rpart(target ~ ., data = trainData)
summary(arvore)

rf <- randomForest(
  num ~ .,
  data=trainData
)

pred = predict(rf, newdata=teste[-5])

cm = table(test[,5], pred)



fit.rf = train(UHIp ~ ., 
               trainData, 
               method = "rf",    
               preProcess = c("pca", "BoxCox"), 
               trControl = trainControl(method = "cv")
               )
#treinamento
prediction <- predict(fit.rf, trainData)
confusionMatrix(prediction, trainData$UHIp)
#validação
prediction <- predict(fit.rf, testData)
confusionMatrix(prediction, testData$UHIp)
varImp(fit_rf)
