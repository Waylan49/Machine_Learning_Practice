### Linear Discreminate Analysis ----- STHDA -----
library(tidyverse)
library(caret)
theme_set(theme_classic())

### Discriminant analysis can be affected by the scale/unit in which predictor 
### variables are measured. It's generally recommended to standardize/normalize 
### continuous predictor before the analysis.
set.seed(123)
index<-createDataPartition(iris$Species, p=0.8, list=FALSE)
train.data<-iris[index, ]
test.data<-iris[-index, ]

### Estimate preprocessing parameters
preproc.param<-preProcess(train.data, method = c("center", "scale"))
train.t<-preproc.param %>% predict(train.data)
test.t<-preproc.param %>% predict(test.data)

## ==> Linear discriminant analysis - LDA

#### LDA assumes that predictors are normally distributed 
#### (Gaussian distribution) and that the different classes have 
#### class-specific means and equal variance/covariance.
#### The linear discriminant analysis can be easily computed using 
#### the function lda() [MASS package].
library(MASS)
model<-lda(Species~., data=train.t)
predicted.class<-model %>% predict(test.t)
confusionMatrix(predicted.class$class, test.t$Species)

### Using the function plot() produces plots of the linear 
### discriminants, obtained by computing LD1 and LD2 for each of the training observations.
plot(model)
ld<-predict(model)$x
update.train.t<-cbind(train.t, ld)
update.train.t %>% ggplot(aes(x=LD1, y=LD2))+geom_point(aes(color=Species))


## ==> Quadratic discriminant analysis - QDA
### it does not assumes the equality of variance/covariance. 
### LDA tends to be a better than QDA when you have a small training set.
### In contrast, QDA is recommended if the training set is very large, so 
### that the variance of the classifier is not a major issue, or if the 
### assumption of a common covariance matrix for the K classes is clearly untenable
model<-qda(Species~., data=train.t)
library(klaR)
### Plot QDA
partimat(Species~., data=train.t, method="qda", plot.matrix=TRUE,
         col.correct="green", col.wrong="red")
predicted.class<-predict(model, test.t)
confusionMatrix(predicted.class, test.t$Species)


## ==> Mixture discriminant analysis - MDA
### The LDA classifier assumes that each class comes from a single normal 
### (or Gaussian) distribution. This is too restrictive.
### For MDA, there are classes, and each class is assumed to be a Gaussian 
###mixture of subclasses, where each data point has a probability of 
### belonging to each class. Equality of covariance matrix, among 
### classes, is still assumed.
library(mda)
model<-mda(Species~., data=train.t)
plot(model)


## ==> Flexible discriminant analysis - FDA
### FDA is a flexible extension of LDA that uses non-linear 
### combinations of predictors such as splines. FDA is useful to model 
### multivariate non-normality or non-linear relationships among variables 
### within each group, allowing for a more accurate classification.
model<-fda(Species~., data=train.t)
plot(model)
predicted.class<-predict(model, test.t)
confusionMatrix(predicted.class, test.t$Species)


## ==> Regularized discriminant analysis
### RDA builds a classification rule by regularizing the group covariance 
### matrices (Friedman 1989) allowing a more robust model against 
### multicollinearity in the data. This might be very useful for a large 
### multivariate data set containing highly correlated predictors.
### Regularized discriminant analysis is a kind of a trade-off between LDA and QDA.
model<-rda(Species~., data=train.t)
partimat(Species~., data=train.t, method="rda", plot.matrix=TRUE,
         col.correct="blue", col.wrong="red")
predicted.class<-predict(model, test.t)
confusionMatrix(predicted.class$class, test.t$Species)

### Discriminant analysis is more suitable to multiclass classification 
### problems compared to the logistic regression
### LDA assumes that the different classes has the same variance or 
### covariance matrix. We have described many extensions of LDA in this 
### chapter. The most popular extension of LDA is the quadratic discriminant 
### analysis (QDA), which is more flexible than LDA in the sens that it does 
### not assume the equality of group covariance matrices.
### LDA tends to be better than QDA for small data set. QDA is recommended 
### for large training data set.


### Linear Discriminate Analysis ----- UC Business-----
# Packages
library(tidyverse)  # data manipulation and visualization
library(MASS)       # provides LDA & QDA model functions
library(ISLR)
data("Default")


### 1. Both LDA and QDA assume the the predictor variables X are drawn 
###    from a multivariate Gaussian (aka normal) distribution.
### 2. LDA assumes equality of covariances among the predictor variables X across each all levels of Y. 
###    This assumption is relaxed with the QDA model.
### 3. LDA and QDA require the number of predictor variables (p) to be less then the sample size (n). 
###    Furthermore, its important to keep in mind that performance will severely decline as 
###    p approaches n. A simple rule of thumb is to use LDA & QDA on data sets where n>=5*p
library(caret)
index<-createDataPartition(Default$default, p=0.6, list=FALSE)
train<-Default[index, ]
test<-Default[-index, ]
lda.m1<-lda(default~balance+student, data=train)





