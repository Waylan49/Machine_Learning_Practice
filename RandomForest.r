## Random Forest  ------- STHDA -------
library(tidyverse)
library(caret)
library(randomForest)

## Classification
#### Preparing Data
data("PimaIndiansDiabetes2", package = "mlbench")
PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)
set.seed(123)
index<-createDataPartition(PimaIndiansDiabetes2$diabetes, p = 0.8, list=FALSE)
train.data<-PimaIndiansDiabetes2[index, ]
test.data<-PimaIndiansDiabetes2[-index,]

### Building random forest
#### We'll use the caret workflow, which invokes the 
#### randomforest() function [randomForest package]
set.seed(123)
model<-train(diabetes~., data=train.data, method="rf",
             trControl=trainControl("cv", number = 10), importance=TRUE)

### Error message: Error in na.fail.default(list(diabetes = 
### c(2L, 1L, 2L, 1L, 2L, 1L, 2L,  : missing values in object 
### ==> !!! NA values not removed !!!

predicted.class<-predict(model, test.data)
confusionMatrix(test.data$diabetes, predicted.class)

### Variable importance (rf) and plotting it - function "varImpPlot" from rf package
importance(model$finalModel)
varImpPlot(model$finalModel, type=1)
varImpPlot(model$finalModel, type=2)
varImp(model)


## Regression
#### Preparing Data
data("Boston", package = "MASS")
set.seed(123)
index<-createDataPartition(Boston$medv, p = 0.8, list=FALSE)
train.data<-Boston[index,]
test.data<-Boston[-index,]

# Fit the model on the training set
set.seed(123)
model2 <- train(
        medv ~., data = train.data, method = "rf",
        trControl = trainControl("cv", number = 10)
)
# Best tuning parameter mtry
model2$bestTune
# Make predictions on the test data
predictions <- model2 %>% predict(test.data)
# Compute the average prediction error RMSE
RMSE(predictions, test.data$medv)

## Hyperparameters
models<-list()
data("PimaIndiansDiabetes2", package = "mlbench")
PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)
set.seed(123)
index<-createDataPartition(PimaIndiansDiabetes2$diabetes, p = 0.8, list=FALSE)
train.data<-PimaIndiansDiabetes2[index, ]
test.data<-PimaIndiansDiabetes2[-index,]

for(nodesize in c(1, 2, 4, 8)){
        set.seed(123)
        model_test <- train(
                diabetes~., data = train.data, method = "rf",
                trControl = trainControl("cv", number = 10),
                metric="Accuracy", nodesize=nodesize
        )
        model.name<-toString(nodesize)
        models[[model.name]]<-model_test
}

#### Compare results by use function resample from package "caret"
resamples(models) %>% summary(metric="Accuracy")




## Random Forest  ------- UC Business -------
library(randomForest) # basic implementation
library(ranger)       # a faster implementation of randomForest
library(caret)        # an aggregator package for performing many machine 
                      # learning models

library(AmesHousing)
set.seed(123)
data<-make_ames()
index<-createDataPartition(data$Sale_Price, p = 0.7, list=FALSE)
ames_train<-data[index, ]
ames_test<-data[-index, ]

# for reproduciblity
set.seed(123)

# default RF model
m1<-randomForest(Sale_Price~., data=ames_train)

# Plotting the model will illustrate the error rate
# The plotted error rate above is based on the OOB sample error 
# and can be accessed directly at m1$mse.
plot(m1)

# RMSE of this optimal random forest
sqrt(m1$mse[which.min(m1$mse)])


## randomForest also allows us to use a validation set to 
## measure predictive accuracy if we did not want to use the OOB samples.
set.seed(123)
valid_split <- initial_split(ames_train, .8)
# training data
ames_train_v2 <- analysis(valid_split)
# validation data
ames_valid <- assessment(valid_split)
x_test <- ames_valid[setdiff(names(ames_valid), "Sale_Price")]
y_test <- ames_valid$Sale_Price


rf_oob_comp<-randomForest(Sale_Price~., data=ames_train_v2, 
                          xtest=x_test, ytest=y_test)

oob<-sqrt(rf_oob_comp$mse)
validation<-sqrt(rf_oob_comp$test$mse)
oob_valid_rmse<-c(oob, validation)
oob_valid_index<-as.factor(c(rep("oob", times=length(oob)), 
                   rep("valid", times=length(validation))))
trees<-c(seq(1, 500, 1), seq(1, 500, 1))

library(ggplot2)
data.frame(rmse=oob_valid_rmse, index=oob_valid_index, trees=trees) %>%
        ggplot(aes(x=trees ,y=rmse))+geom_line(aes(color=index))

predicted.value<-predict(m1, ames_test)
RMSE(predicted.value, ames_test$Sale_Price)


## Tuning
### 1. ntree
### 2. mtry
### 3. samplesize
### 4. nodesize
### 5. maxnodes

# If we are interested with just starting out and tuning the 
# mtry parameter we can use randomForest::tuneRF for a quick and 
# easy tuning assessment. 
## Note that tuneRF requires a separate x y specification

features <- setdiff(names(ames_train), "Sale_Price")
set.seed(123)

m2<-tuneRF(
        x=ames_train[features],
        y=ames_train$Sale_Price, 
        mtryStart = 5, 
        stepFactor = 1.5, 
        improve = 0.01, 
        ntreeTry = 500,
        trace = FALSE
)

## Full grid search with ranger
hyper_grid <- expand.grid(
        mtry       = seq(20, 30, by = 2),
        node_size  = seq(3, 9, by = 2),
        sampe_size = c(.55, .632, .70, .80),
        OOB_RMSE   = 0
)

for(i in 1:nrow(hyper_grid)){
        model<-ranger(Sale_Price~., data=ames_train, 
                      num.trees=500, 
                      mtry=hyper_grid$mtry[i], 
                      min.node.size = hyper_grid$node_size[i], 
                      sample.fraction =  hyper_grid$sampe_size[i])
        hyper_grid$OOB_RMSE[i]<-sqrt(model$prediction.error)
}

### Create our best rf 
final_rf<-ranger(formula = Sale_Price~., data = ames_train, 
                 num.trees = 500, mtry = 26, min.node.size =5, sample.fraction = 0.8, 
                 importance = "impurity")

predicted.value1<-predict(final_rf, ames_test)
RMSE(predicted.value1$predictions, ames_test$Sale_Price) ### RMSE reduce to 24552.99



