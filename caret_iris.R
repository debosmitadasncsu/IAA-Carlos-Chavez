#install.packages("caret", dependencies = TRUE)
#changes
library(caret)
data("iris")
dataset <- iris

validation_index <- createDataPartition(dataset$Species, p=0.8,
                                        list=FALSE)

#this is the beginning




##quedándose con el 20%
validation <- dataset[-validation_index,]

##quedándose con el 80%
dataset <- dataset[validation_index,]


dim(dataset)
sapply(dataset, class)
levels(dataset$Species)
head(dataset)

###################

x <- dataset[,1:4]
y <- dataset[,5]


par(mfrow=c(1,4))





















#####################################################################
#####################################################################

library(caret)
library(datasets)

data("mtcars")

split <- createDataPartition(y=mtcars$mpg, p=0.6,
                             list=FALSE)


dev <- mtcars[split,]
val <- mtcars[-split,]

lmFit <- train(mpg~., data=dev , method='lm')
summary(lmFit)
print(lmFit)


ctrl <- trainControl(method = "cv", number=10)
lmCVFit <- train(mpg~., data=dev , method='lm', trControl=ctrl,
                 metric='Rsquared')


##################################################################

residuals <- resid(lmFit)

predictedValues <- predict(lmFit)

plot(dev$mpg,residuals)
abline(0,0)
plot(dev$mpg, predictedValues)



varImp(lmFit)
varImp(lmCVFit)

plot(varImp(lmFit))


###################################################################################
###################################################################################

library(caret)





# this symbol |
# alt + 124



x <- 5
y <- 7
!(!(x < 4) & !!!(y > 12))






