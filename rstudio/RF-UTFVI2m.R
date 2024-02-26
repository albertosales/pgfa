#install.packages("Metrics")
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
ano = '2000'
era5 = paste('era5_2m',ano,sep="_")
era5
data <- read_csv(paste(era5,"csv",sep="."))#, col_names = FALSE)
data <- subset(data, select = -c(windquad_cat) )
data
data$Temp2m <- as.numeric(data$Temp2m)
data$RH <- as.numeric(data$RH)
data$windspeed <- as.numeric(data$windspeed)
data$windquad <- as.numeric(data$windquad)
data$UHIp2m <- as.factor(data$UHIp2m)
data$UTFVI2m <- as.numeric(data$UTFVI2m)
#data$windquad_cat <- as.factor(data$windquad_cat)
#data$windpower <- as.factor(data$windpower)
#data$local <- as.factor(data$local)
#data$timestamp <- as.factor(data$timestamp)
#data$surface_pressure <- as.numeric(data$surface_pressure)
#data<-rename(data, c("X1"="Temp2m", "X2"="RH", "X3"="windspeed", "X4"="winddir", "X5"="UTFVI", "X6"="windquad", "X7"="UHIp", "X8"="UHIn"))
data
summary(data)
#data$UHIp <- factor(data$UHIp, levels=c('extremo','forte','fraco','medio','muito_forte','nenhum'))
data$UHIp2m <- factor(data$UHIp2m, levels=c('extremo','forte','muito_forte'))
#data$UHIp2m <- factor(data$UHIp2m, levels=c('extremo','forte','muito_forte'))
#data$windquad_cat <- factor(data$windquad_cat, levels=c('Primeiro','Segundo','Terceiro','Quarto','Quinto','Sexto','Sétimo','Oitavo'))
data <- subset(data, select = c(RH, windspeed, windquad,UHIp2m))
data

aux = createDataPartition(data$UHIp2m, p=0.7, times = 1, list = F)
tr <- trainControl(method = "cv", number = 10) #setting parameters for the model
tuneGrid <- expand.grid(
  mtry = c(2:4)
)
treino = data[aux,]
teste = data[-aux,]
treino[,-4]
teste[,-4]
prop.table(table(treino$UHIp2m))
treino[,4]
teste[,4]
prop.table(table(teste$UHIp2m))

rf <- randomForest(x = treino[,-4],
                   y = treino$UHIp2m,
                   xtest = teste[,-4],
                   ytest = teste$UHIp2m,
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
importance(rf)
measure_importance(rf)

#treinamento
prediction <- predict(rf, treino)
confusionMatrix(prediction, treino$UHIp2m, mode = "everything", positive="1")

#validação
prediction <- predict(rf, teste)
confusionMatrix(prediction, teste$UHIp2m,mode = "everything", positive="1")

target = UHIp2m ~ windspeed + windquad + RH
fit <- rpart(target, data = data)
rpart.rules(fit,cover=TRUE)

#UHIP rate was greater for Relative Humidity
rpart.rules(rpart(windquad ~ windspeed + RH + UHIp2m, data = data))
rpart.rules(rpart(windspeed ~ windquad + RH + UHIp2m, data = data))

partition = createDataPartition(data$UHIp2m, p=0.7, times = 1, list = F)
df_train <- data[partition, ]
df_test <- data[-partition, ]
tr <- trainControl(method = "cv", number = 10) #setting parameters for the model

### FUNCIONANDO PARA TREINO E TESTE

ind = sample(2, nrow(data), replace=TRUE, prob=c(0.7,0.3))
trainData = data[ind==1,]
testData = data[ind==2,]
target = UHIp2m ~ windspeed + windquad + RH
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

fit <- randomForest(target,   data=testData)
print(fit) # view results
rfStats(fit)
varImpPlot(fit)
varImp(fit)

#validação/teste
prediction <-predict(fit, testData)
confusionMatrix(prediction, testData$UHIp2m,mode = "everything", positive="1")
prop.table(table(testData$UHIp2m))
nrow(testData)

nrow(data)
nrow(trainData)
nrow(testData)
prop.table(table(trainData$UHIp2m))
prop.table(table(testData$UHIp2m))

########################################
tr <- trainControl(method = "cv", number = 10) #setting parameters for the model
fit.rf <- train(UHIp2m ~ .,                      #fit model
                data = data, 
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
print(fit.rf) # view results

fit.rf$finalModel$importance

#treinamento
prediction <- predict(fit.rf, trainData)
confusionMatrix(prediction, trainData$UHIp2m,mode = "everything", positive="1")

#validação
prediction <- predict(fit.rf, testData)
confusionMatrix(prediction, testData$UHIp2m,mode = "everything", positive="1")


randomForest::varImpPlot(fit.rf$finalModel)

#########################################3





target = UHIp2m ~ windspeed + windquad + RH

# r.forest <- train(UHIp2m ~ .,
#                   data,
#                   method = "ranger",
#                   metric = "rmse")
# 
# r.forest

# indep_var = colnames(data) != "UHIp2m"
# model_rf = train(x = data[indep_var], 
#                  y = data$UHIp2m,
#                  method = 'rf')
# 
# model_rf

data.trn <- data[partition,]
data.tst <- data[-partition,]
ctrl <- trainControl(method = "cv", number = 10)

fit.cv <- train(UHIp2m ~ ., data = data, method="rf", trControl = ctrl, tuneLength = 50)

print(fit.cv)
plot(fit.cv)

pred <- predict(fit.cv, data.trn)
confusionMatrix(pred, data.trn$UHIp2m,mode = "everything", positive="1")

pred <- predict(fit.cv, data.tst)
confusionMatrix(pred, data.tst$UHIp2m,mode = "everything", positive="1")

print(varImp(fit.cv))
plot(varImp(fit.cv))

#Decision Tree
# fit.cv <- train(UHIp2m ~ ., data = data.trn, method="rpart", trControl = ctrl, tuneLength = 20)
# pred <- predict(fit.cv, data.tst)
# confusionMatrix(pred, data.tst$UHIp2m,mode = "everything", positive="1")
# print(fit.cv)
# plot(fit.cv)

# 
# print(varImp(fit.cv))
# plot(varImp(fit.cv))

# rpart.plot(fit.cv$finalModel, fallen.leaves = F)
# print(varImp(fit.cv))
# plot(varImp(fit.cv))


data_rf = randomForest(UHIp2m ~ ., data=trainData, ntree=500,mtry=3, proximity=T, importance = TRUE)
table(predict(data_rf), trainData$UHIp2m)
rfStats(data_rf)
varImp(data_rf)
prediction <- predict(data_rf, trainData)
confusionMatrix(prediction, trainData$UHIp2m)

data_rf2 = randomForest(UHIp2m ~ ., data=testData, ntree=500,mtry=3, proximity=T, importance = TRUE)
table(predict(data_rf2), testData$UHIp2m)
rfStats(data_rf2)
varImp(data_rf2)
prediction <- predict(data_rf, testData)
confusionMatrix(prediction, testData$UHIp2m,mode = "everything", positive="1")
data

aux = createDataPartition(data$UHIp2m, p=0.7, times = 1, list = F)
treino = data[aux,]
teste = data[-aux,]
treino[,-5]
teste[,-5]
treino[,5]
teste[,5]
prop.table(table(treino$UHIp2m))
prop.table(table(teste$UHIp2m))

rf <- randomForest(x = treino[,-4],
                   y = treino$UHIp2m,
                   xtest = teste[,-4],
                   ytest = teste$UHIp2m,
                   ntree = 500,
                   mtry = 3,
                   importance=TRUE,
                   replace = T,
                   #nodesize = 10,
                   #maxnode = 15,
                   keep.forest = T,
                   proximity = TRUE)

rf
varImpPlot(rf, main = "Importância das Variáveis")
rfStats(rf)
getTree(rf)
plot(rf)
prediction <- predict(rf, newdata=treino[-4])
confusionMatrix(prediction, treino$UHIp2m,mode = "everything", positive="1")

prediction <- predict(rf, newdata=teste[-4])
confusionMatrix(prediction, teste$UHIp2m,mode = "everything", positive="1")

 
# target = UHIp2m ~ windspeed + windquad + RH

# fit <- randomForest(target, data=data,
#                     importance=TRUE,)
# print(fit) # view results
# varImpPlot(fit)
# varImp(fit)
# rfStats(fit)
# 
# #treinamento
# 
# prediction <-predict(fit, treino)
# confusionMatrix(prediction, treino$UHIp, mode = "everything", positive="1")
# prop.table(table(treino$UHIp))
# nrow(treino)
# #validação/teste
# prediction <-predict(fit, teste)
# confusionMatrix(prediction, teste$UHIp,mode = "everything", positive="1")
# prop.table(table(testData$UHIp))
# testData
# 

target = UHIp2m ~ windspeed + windquad + RH

ind = sample(2, nrow(data), replace=TRUE, prob=c(0.7,0.3))
trainData = data[ind==1,]
testData = data[ind==2,]
trainData
testData
data_rf = randomForest(UHIp2m ~ ., data=trainData, ntree=500,mtry=4, proximity=T, importance = TRUE)
table(predict(data_rf), trainData$UHIp2m)
rfStats(data_rf)
data_rf2 = randomForest(UHIp2m ~ ., data=testData, ntree=500,mtry=4, proximity=T, importance = TRUE)
table(predict(data_rf2), testData$UHIp2m)
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


fit_rf = train(UHIp2m ~ .,
               data,
               method = "rf",
               metric = "Accuracy",
               tuneGrid = tuneGrid,
               trControl = trControl,
               importance = TRUE,
               nodesize = 14,
               ntree = 500,
               maxnodes = 24
               )
title = paste('Importâncias das Variáveis Micro-Climáticas do ano',ano,sep=" ")
#treinamento
prediction <- predict(fit_rf, trainData)
confusionMatrix(prediction, trainData$UHIp2m)
#validação
prediction <- predict(fit_rf, testData)
confusionMatrix(prediction, testData$UHIp2m)
varImp(fit_rf)
randomForest::varImpPlot(fit_rf$finalModel,main=title)
plot(fit_rf)
# add legend to know which is which
legend("top", colnames(fit_rf$err.rate), fill=1:ncol(fit_rf$err.rate))


arvore <- rpart::rpart(target ~ ., data = trainData)
summary(arvore)

rf <- randomForest(
  num ~ .,
  data=trainData
)

pred = predict(rf, newdata=teste[-5])

cm = table(test[,5], pred)

train_ctrl_rand <- trainControl(method="boot", # type of resampling, in this case bootstrap
                                number = 13, # number of resamplings     
                                search = "random" # we are performing a "random" search
)

model_boot_rand <- train(UHIp2m ~ .,
                         data = treino,
                         method = "rf", # this will use the randomForest::randomForest function
                         metric = "Accuracy", # which metric should be optimized for 
                         trControl = train_ctrl_rand, 
                         # options to be passed to randomForest
                         ntree = 741 )

model_boot_rand   
probs <- predict(model_boot_rand , teste,"prob")
class <- predict(model_boot_rand , teste,"raw")

teste.scored <- cbind(teste , probs, class )
glimpse(teste.scored)

train_ctrl_rand <- trainControl(method="boot", # type of resampling, in this case bootstrap
                                number = 13, # number of resamplings     
                                search = "random" # we are performing a "random" search
)

teste.model_cv_grid.score <- teste.scored %>% 
  mutate(model="cv") 

teste.model_boot_rand.score <- teste.scored.boot %>%
  mutate(model='boot') 

stacked_df <- rbind(teste.model_cv_grid.score, teste.model_boot_rand.score)

colnames(teste.scored)

library('yardstick')

roc_aucs <- stacked_df %>%
  group_by(model) %>%
  roc_auc(truth = UHIp2m, class)

roc_aucs

confusion_matrices <- stacked_df %>%
  group_by(model) %>%
  conf_mat(truth =  UHIp2m, class)

(confusion_matrices %>% filter(model == 'boot'))$conf_mat[[1]]

metrics_boots <- summary((confusion_matrices %>% filter(model == 'boot'))$conf_mat[[1]])  

metrics_cv <- summary((confusion_matrices %>% filter(model == 'cv'))$conf_mat[[1]])  

metrics_compare <- metrics_boots %>%
  left_join(metrics_cv, 
            by=c('.metric','.estimator'),
            suffix = c("_boot","_cv")) %>%
  gather(-.metric,-.estimator,key="model_type",value = Value)

ggplot(metrics_compare, aes(x=.metric, y=Value, fill=model_type)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  coord_flip()

target = UHIp2m ~ windspeed + windquad + RH

model <- randomForest(formula = target,
                      data = treino)
print(model)
teste$pred <- predict(model, treino)
teste$pred <- as.factor(teste$pred)
confusionMatrix(teste$pred, teste$UHIp2m)

pred <- predict(object = model,
                newdata = teste,
                type = "prob")
pred


model <- randomForest(formula = target,
                      data = teste)
print(model)
teste$pred <- predict(model, teste)
teste$pred <- as.factor(teste$pred)
confusionMatrix(teste$pred, teste$UHIp2m)

pred <- predict(object = model,
                newdata = teste,
                type = "prob")
pred

auc(actual = teste$UHIp2m, 
    predicted = pred[,"extremo"])

