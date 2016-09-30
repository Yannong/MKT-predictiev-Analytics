setwd("~/Google Drive/Advanced MA/HW3")
bp.train <- read.csv("HW3 - Training Data.csv", fileEncoding = 'latin1', stringsAsFactors = F)
bp.test <- read.csv("HW3 - Testing Data For Students.csv", fileEncoding = 'latin1', stringsAsFactors = F)


########clean data###########
bp.train$X.1 <- NULL
bp.train$X <- NULL

bp.test$X.1 <- NULL
bp.test$X <- NULL

bp.train$reviewLength = nchar(paste(bp.train$review.text))
bp.train$titleLength = nchar(paste(bp.train$review.title))

bp.test$reviewLength = nchar(paste(bp.test$review.text))
bp.test$titleLength = nchar(paste(bp.test$review.title))

#Clear cases where number.of.votes = 0 
bp.train = subset(bp.train,bp.train$number.of.votes >0)
bp.train$percentHelpful = bp.train$number.of.helpfulness/bp.train$number.of.votes

#split train data into two sample data
set.seed(432)
train <- sample(1:nrow(bp.train),nrow(bp.train)*0.8)
baby.train <- bp.train[train,]
baby.test <- bp.train[-train,]
########################### Part 1:Model Fitting #################

#######################cart trees########################
library('rpart')

#cross validation=100
cart.fit = rpart(percentHelpful~number.of.votes+reviewLength
                 +star.rating+post.year+number.of.helpfulness,
                 data=baby.train,method='anova',
                 xval=100)
cart.result <- predict(cart.fit,baby.test)
cartMSE <-mean((baby.test$percentHelpful-cart.result)^2)
print(cartMSE) #####0.002231548
cartVec <- predict(cart.fit, data = bp.test)

########################## MARS #########################
library(earth)

mars.fit = earth(percentHelpful~reviewLength, 
                 data=baby.train)
mars.result <- predict(mars.fit,data=baby.test)
marsMSE<- mean((baby.test$percentHelpful-mars.result)^2)
print(marsMSE)   ###0.007419
marsVec <- predict(mars.fit,bp.test)

########################## Bagged Trees ###################
library(ipred)

##xval=10
bagged.fit <- bagging(percentHelpful~reviewLength+titleLength,
                       data=baby.train,xval=10)
bagged.result <- predict(bagged.fit, data=baby.test)
baggedMSE <- mean((baby.test$percentHelpful-bagged.result)^2)
print(baggedMSE)    ###0.0073374476
baggedVec <- predict(bagged.fit, data=bp.test)

########################## Random Forests ######################
library("randomForest")

#cross validation=10
randForest.fit <- randomForest(percentHelpful~reviewLength+titleLength+verified.purchase, 
                               data=baby.train,xval=10)
forest.result <- predict(randForest.fit, data=baby.test)
forestMSE <- mean((baby.test$percentHelpful-forest.result)^2)
print(forestMSE)    ##0.007454594
forestVec <- predict(randForest.fit, data=bp.test)


########################## Support Vector Machine ###################
library("e1071")

##cross validation=10
svm.fit <- svm(percentHelpful~reviewLength+titleLength+verified.purchase+post.year, data=baby.train,xval=10)
svm.result <- predict(svm.fit, data=baby.test)
svmMSE <- mean((baby.test$percentHelpful-svm.result)^2)
print(svmMSE) ##0.008749773
svmVec <- predict(svm.fit, data=bp.test)

########################## Boosting ###############################
library("gbm")

boosting.fit <- gbm(percentHelpful~number.of.votes+star.rating+
                      post.year+number.of.helpfulness,data = baby.train, 
                    distribution = "gaussian")
boost.result <- predict(boosting.fit, data=baby.test,n.trees = boosting.fit$n.trees)
boostMSE <- mean((baby.test$percentHelpful-boost.result)^2)
print(boostMSE)   ##0.007222694
boostVec <- predict(boosting.fit, data=bp.test,n.trees = boosting.fit$n.trees)

######################### Neural networks ######################
library("nnet")

nnet.fit <- nnet(percentHelpful~reviewLength+titleLength+
                   number.of.votes+star.rating+
                   post.year+number.of.helpfulness,
                 data = baby.train,linout=1,size = 2)
nnet.result <- predict(nnet.fit, data=baby.test)
nnetMSE <- mean((baby.test$percentHelpful-nnet.result)^2)
print(nnetMSE)  ##0.007234208
nnetVec <- predict(nnet.fit, data=bp.test)

######################### Knn-regression ######################
library("kknn")
kknn.fit <- kknn(percentHelpful~reviewLength+titleLength+
                 number.of.votes+star.rating+
                 post.year+number.of.helpfulness,
               train=baby.train,test = baby.test,k=10)
kknn.result <- predict(kknn.fit,data=baby.test)
kknnMSE <- mean((baby.test$percentHelpful-kknn.result)^2)
print(kknnMSE) ###0.00659851
kknnVec <- predict(kknn.fit, data=bp.test)
########################### best model###########

bestVec <- kknnVec


########################## Part 2: Model Comparison ##############

table <-as.data.frame(matrix(data = NA,nrow=2,ncol=8))
colnames(table) <- c("CART","MARS","BAGGED","FOREST","SVM","BOOST","NNET","KNN")
rownames(table) <- c("MSE","System.Time")
data.a <- c(cartMSE,marsMSE,baggedMSE,forestMSE,svmMSE,boostMSE,nnetMSE,kknnMSE)
data.b <- c(0.728,0.071,0.541,5.242,2.631,0.101,0.030,0.195)
table[1,] <- data.a
table[2,] <- data.b
as.data.frame(table)

#######sample comment
lm <-lm(percentHelpful~reviewLength+titleLength+
          number.of.votes+star.rating+
          post.year+number.of.helpfulness, data=baby.train)


save.image(file = "hw3_all.Rdata")
