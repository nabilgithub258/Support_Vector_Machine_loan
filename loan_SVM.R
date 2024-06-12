library(corrgram)
library(corrplot)
library(caTools)
library(Amelia)
library(ggplot2)
library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ISLR)
library(e1071)

#### importing the csv file

loan <- read.csv('loan_data.csv')

View(loan)

str(loan)

##### checking which column we can factor

str(loan)

summary(loan)

table(loan$credit.policy)

####

loan$credit.policy <- factor(loan$credit.policy)
loan$inq.last.6mths <- factor(loan$inq.last.6mths)
loan$delinq.2yrs <- factor(loan$delinq.2yrs)
loan$pub.rec <- factor(loan$pub.rec)
loan$not.fully.paid <- factor(loan$not.fully.paid)

str(loan)

#### EDA time

ggplot(loan,aes(fico)) + geom_histogram(aes(fill=not.fully.paid),color='black',alpha=0.7) + theme_bw()

#### in the column not.fully.paid 0 means False meaning paid and 1 means true meaning not paid
#### in short 0 paid and 1 unpaid

ggplot(loan,aes(factor(purpose))) + geom_bar(aes(fill=not.fully.paid),alpha=0.7) + theme_bw()

ggplot(loan,aes(factor(purpose))) + geom_bar(aes(fill=not.fully.paid),position = 'dodge',alpha=0.7) + theme_bw()

ggplot(loan,aes(fico,inq.last.6mths)) + geom_point(position=position_jitter(w=1, h=0),aes(color=fico),alpha=0.5) + scale_color_gradientn(colours = c('red','yellow','orange','blue','light green','dark green','green')) + theme_bw()

ggplot(loan,aes(fico,inq.last.6mths)) + geom_point(position=position_jitter(w=1, h=0),aes(color=not.fully.paid),alpha=0.5) + theme_bw()

#### train and test sample

sample <- sample.split(loan$not.fully.paid,SplitRatio = 0.7)

train <- subset(loan,sample == TRUE)
test <- subset(loan,sample == FALSE)

#### model building

initial.model <- svm(not.fully.paid ~.,train)

summary(initial.model)

first.predict <- predict(initial.model,test)

table(first.predict,test$not.fully.paid)

#### its not really effective like this because we did not choose the gamma nor the cost so now lets make the final model with those in mind
#### we can achieve better gamma and cost with using tune and then later we can use that cost and gamma inside the final model to make it more optimum

tune.results <- tune(svm,train.x=not.fully.paid~., data=train,kernel='radial',
                     ranges=list(cost=c(1,10), gamma=c(0.1,1)))
summary(tune.result)

#### we could have done more complicated tuning with more variables inside the cost and gamma but it needs a faster computer which i dont have
#### so we will settle for this summary
#### best parameters are cost = 2 and gamma = 0.1

final.model <- svm(not.fully.paid ~.,train,kernal='radial',cost=2,gamma=0.1)

#### predict now

predict.model <- predict(final.model,test)

table(predict.model,test$not.fully.paid)

Acc.model <- (2410+3)/(2410+3+457+3)

print(Acc.model)

#### well it seems like its not the best model but for the better model we will need better computer
#### as we will have to put more values inside the cost and gamma of the tuning procedure