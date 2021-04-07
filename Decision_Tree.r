####### Decision Tree ------- STHDA -------

library(tidyverse)
library(caret)
library(rpart)

## -----Basics and visual representation
model<-rpart(Species~., data = iris)
plot(model)
text(model, digits=3)

newdata <- data.frame(
        Sepal.Length = 6.5, Sepal.Width = 3.0,
        Petal.Length = 5.2, Petal.Width = 2.0
)

predict(model, newdata = newdata, type = "class")


## -----Classification trees
data("PimaIndiansDiabetes2", package = "mlbench")
PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)

set.seed(123)
index<-createDataPartition(PimaIndiansDiabetes2$diabetes, p=0.8, list=FALSE)
train.data<-PimaIndiansDiabetes2[index,]
test.data<-PimaIndiansDiabetes2[-index,]

set.seed(123)
model1<-rpart(diabetes~., data=train.data, method="class")
plot(model1)
text(model1, digits=3)

predict_result<-predict(model1, newdata = test.data, type = "class")
confusionMatrix(test.data$diabetes, predict_result)

## Pruning the tree
### In rpart package, this is controlled by the 
### complexity parameter (cp), which imposes a penalty 
### to the tree for having two many splits. The default 
### value is 0.01. The higher the cp, the smaller the tree.

model2 <- train(
        diabetes ~., data = train.data, method = "rpart",
        trControl = trainControl("cv", number = 10),
        tuneLength = 10
)
### plot cp value and accuracy
plot(model2)
model2$bestTune
### plot decision tree
plot(model2$finalModel)
text(model2$finalModel, digits=3)

predicted_result<-predict(model2, newdata=test.data)
confusionMatrix(test.data$diabetes, predicted_result)


## -----Regression trees
### Load the data
data("Boston", package = "MASS")
index<-createDataPartition(Boston$medv, p=0.8, list=FALSE)
train.data<-Boston[index,]
test.data<-Boston[-index,]

### Create the regression tree
set.seed(123)
model<-train(medv~., data=train.data, method='rpart',
             trControl=trainControl("cv", number=10),
             tuneLength=10)

### check cp value
model$bestTune
plot(model)

### plot the tree
plot(model$finalModel)
text(model$finalModel, digits=3)

### predictions
predictions<-predict(model, test.data)
RMSE(predictions, test.data$medv)


## -----Conditionnal inference tree
# Load the data
data("PimaIndiansDiabetes2", package = "mlbench")
pima.data <- na.omit(PimaIndiansDiabetes2)
# Split the data into training and test set
set.seed(123)
training.samples <- pima.data$diabetes %>%
        createDataPartition(p = 0.8, list = FALSE)
train.data  <- pima.data[training.samples, ]
test.data <- pima.data[-training.samples, ]

library(party)
model<-train(diabetes~., data=train.data, method="ctree2",
             trControl=trainControl("cv", number = 10),
             tuneGrid=expand.grid(maxdepth=3, mincriterion=0.95))

predicted.classes <- model %>% predict(test.data)
confusionMatrix(predicted.classes, test.data$diabetes)



####### Decision Tree ------- UC Business -------

library(rsample)     # data splitting 
library(dplyr)       # data wrangling
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(ipred)       # bagging
library(caret)       # bagging
library(purrr)
library(ggplot2)

### Load data and split it
library(AmesHousing)
set.seed(123234)
index<-createDataPartition(make_ames()$Sale_Price, p=0.7, list=FALSE)
ames_train<-make_ames()[index, ]
ames_test<-make_ames()[-index, ]

### when fitting a regression tree, we need to set method = "anova"
m1<-rpart(Sale_Price~., data=ames_train, method="anova")
rpart.plot(m1)
pred1<-predict(m1, ames_test)
RMSE(pred1, ames_test$Sale_Price) ## 38977.34

### Behind the scenes rpart is automatically applying a range of 
### cost complexity (??) values to prune the tree. 
plotcp(m1)
m1$cptable

### rpart uses a special control argument where we provide a list 
### of hyperparameter values. For example, if we wanted to assess a 
### model with minsplit = 10 and maxdepth = 12, we could execute the following:

m2<-rpart(Sale_Price~., data=ames_train, method="anova", 
          control=list(minsplit=10, maxdepth=12, xval=10))

pred2<-predict(m2, ames_test)
RMSE(pred2, ames_test$Sale_Price) ## 38977.34

### To perform a grid search
hyper_grid <- expand.grid(
        minsplit = seq(4, 25, 1),
        maxdepth = seq(7, 18, 1)
)

head(hyper_grid)

models<-list()

for (i in 1:nrow(hyper_grid)){
        
        # get minsplit, maxdepth values at row i
        minsplit=hyper_grid[i, 1]
        maxdepth=hyper_grid[i, 2]
        
        # train a model and store in the list
        models[[i]]<-rpart(Sale_Price~., data=ames_train,
                           method="anova",
                           control=list(minsplit=minsplit, 
                                        maxdepth=maxdepth))
}

### function to get optimal cp
get_cp <- function(x) {
        min    <- which.min(x$cptable[, "xerror"])
        cp <- x$cptable[min, "CP"] 
}

# function to get minimum error
get_min_error<-function(x){
        min<-which.min(x$cptable[, "xerror"])
        xerror <- x$cptable[min, "xerror"] 
}

library(purrr)

hyper_grid %>% mutate(
        cp=map_dbl(models, get_cp),
        error=map_dbl(models, get_min_error)
) %>% arrange(error) %>% top_n(n = -15, wt = error)

### Based on above result, let's construct the final tree
optimal_tree<-rpart(Sale_Price~., data=ames_train, method="anova", 
                    control=list(minsplit=23, maxdepth=7, cp=0.01))
pred<-predict(optimal_tree, ames_test)
RMSE(pred, ames_test$Sale_Price) ### 38977.34


## Bagging with ipred
### By default bagging performs 25 bootstrap samples and trees
set.seed(123)
bagged_m1<-bagging(Sale_Price~., data=ames_train, coob=TRUE)


ntree=seq(10, 70, 1)
models<-list()
for(i in 1:length(ntree)){
        number_tree<-ntree[i]
        models[[i]]<-bagging(formula = Sale_Price~., 
                             data = ames_train, 
                             coob=TRUE,
                             nbagg=number_tree)
}

get_bagging_err<-function(x){
        x$err
}

tree_err<-data.frame(tree_count=ntree, err=map_dbl(models, get_bagging_err))
tree_err %>% ggplot(aes(x=tree_count, y=err))+geom_line()

bagged_m2<-bagging(Sale_Price~., data=ames_train, coob=TRUE, nbagg=58)
### bagged_m2$err ==> 36230.28
pred_bag<-predict(bagged_m2, ames_test)
RMSE(pred_bag, ames_test$Sale_Price) ### 35013.82

## Bagging with ipred is quite simple; however, there are some 
## additional benefits of bagging with caret.

## Its easier to perform cross-validation. Although we can use 
## the OOB error, performing cross validation will also provide 
## a more robust understanding of the true expected test error.
## We can assess variable importance across the bagged trees.

## Specify 10-fold cross validation
ctrl<-trainControl("cv", number = 10)
bagged_cv<-train(Sale_Price~., data=ames_train, method="treebag",
                 trControl=ctrl, importance=TRUE)  ### RMSE 36005.13

# plot most important variables
plot(varImp(bagged_cv), 20)  

pred_bag_cv<-predict(bagged_cv, ames_test)
RMSE(pred_bag_cv, ames_test$Sale_Price) ### 35879.94

