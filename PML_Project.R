# set up env
require(caret) #loads ggplot2 also
library(randomForest)

setwd("~/Training/CourseraDataScience/8PML")
set.seed(1234)

train.raw <- read.csv(file="pml-training.csv", header=TRUE, as.is = TRUE, stringsAsFactors = FALSE, na.strings=c("NA",""))
test.raw <- read.csv(file="pml-testing.csv", header=TRUE, as.is = TRUE, stringsAsFactors = FALSE, na.strings=c("NA",""))

# create a cross-validation set?
## Since we have over 19k observations and will be testing on 20 records, cross-validation will help us understand accuracy of model and tendency to overfit


# Pre process
## Remove columns with too many NA values ; don't forget to do this for Test set, too. 
table(colSums(is.na(train.raw))/nrow(train.raw)) # shows 100 columns have .979 NA values

bad.cols <- which(colSums(is.na(train.raw))/nrow(train.raw) > 0 )

train.pre <- train.raw[ ,-bad.cols]

test.pre <- test.raw[ ,-bad.cols]

inTrain <- createDataPartition(y = train.pre$classe, p = 0.75, list = FALSE)

training <- train.pre[inTrain, ]
crossval <- train.pre[-inTrain, ]

## "Basic Preprocessing"

num.cols <- which(lapply(training, class) %in% "numeric")

pre <- preProcess(training[,num.cols],method=c('center', 'scale'))

train2 <- predict(pre, training[ ,num.cols])

near.0 <- nearZeroVar(x = train2, saveMetrics = TRUE) #remove any NZV; looks to be 0 

train.final <- train2[,near.0$nzv==FALSE]
train.final$classe <- training$classe



## RF, Boosting


model.fit <- train(as.factor(classe) ~ . ,data = train.final ,method = "rf")


#cross val

crossval2 <- predict(pre, crossval[ ,num.cols])

near.0 <- nearZeroVar(x = crossval2, saveMetrics = TRUE) #remove any NZV; looks to be 0 

crossval.final <- crossval2[,near.0$nzv==FALSE]
crossval.final$classe <- crossval$classe

crossval.predict <- predict(model.fit, crossval.final)


confusionMatrix(crossval.predict, crossval.final$classe) #shows fantastic accuracy

varImp(model.fit)

# Save model for use

saveRDS(model.fit, file ="modelfit.RDS")

# Apply to test set
test2 <- predict(pre, test.pre[ ,num.cols])
near.0 <- nearZeroVar(x = test2, saveMetrics = TRUE) #remove any NZV; looks to be 0 

test.final <- test2[,near.0$nzv==FALSE]

test.pred <- predict(model.fit, test.final)

test.pred


# submit
pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}

pml_write_files(test.pred)
