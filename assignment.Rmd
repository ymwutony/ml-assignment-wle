## Load required packages
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(RColorBrewer)

## Set seed
set.seed(414)

## Load data
trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
training <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))
testing <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!",""))

## Divide Training data to "Training" and "Validation"
inTrain <- createDataPartition(y=training$classe, p=0.6, list=FALSE)
myTraining <- training[inTrain, ]; 
myValiation <- training[-inTrain, ]

## Tidy up the training data
myNearZero <- nearZeroVar(myTraining, saveMetrics=TRUE)
myTraining <- myTraining[!myNearZero$nzv]
myTraining <- myTraining[, -(1:6)]

## Remove Column that have more than 60% missing value
rmCol <- sapply(colnames(myTraining), function(x) if(sum(is.na(myTraining[, x])) > 0.50*nrow(myTraining)) { return(TRUE)
}else{
        return(FALSE)
}
)
myTraining <- myTraining[, !rmCol]

## Tidy up the validation and test data
clean1 <- colnames(myTraining)
myValidation <- myTesting[clean1]
testing <- testing[clean1]

## Use Random Forests for prediction
modFit <- randomForest(classe ~. , data=myTraining)
predictions <- predict(modFit, myValidation, type = "class")

## Use confusion Matrix to test results
confusionMatrix(predictions, myValidation$classe)
