# Take the data
if(!file.exists("training.csv")) download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", "training.csv")
if(!file.exists("testing.csv")) download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", "testing.csv")
training <- read.csv("training.csv")
testing <- read.csv("testing.csv")

# Tidy it up : remove the lines that are almost completely na and the ones that containg #DIV/0! because they are almost completely empty
library(caret)
toKeep <- grepl("user_name|^accel_|^magnet|^gyros|^roll|^pitch|^yaw|classe", names(training))
trainingTK <- training[,toKeep] # TK stands for To Keep
testingTK <- testing[,toKeep]
inTrain <- createDataPartition(y=trainingTK$classe, p=0.75, list=FALSE)
trainTK <- trainingTK[inTrain,]
crossTK <- trainingTK[-inTrain,]

a <- preProcess(trainTK[, 2:49], method=c("center", "scale", "pca"))
trainPC <- predict(a, trainTK)

fitAPC <- train(
        trainPC[, !names(trainPC) %in% c("classe")],
        as.factor(trainPC$classe=="A"),
        method="glm", family = "binomial"
)
fitBPC <- train(
        trainPC[, !names(trainPC) %in% c("classe")],
        as.factor(trainPC$classe=="B"),
        method="glm", family = "binomial"
)
fitCPC <- train(
        trainPC[, !names(trainPC) %in% c("classe")],
        as.factor(trainPC$classe=="C"),
        method="glm", family = "binomial"
)
fitDPC <- train(
        trainPC[, !names(trainPC) %in% c("classe")],
        as.factor(trainPC$classe=="D"),
        method="glm", family = "binomial"
)
fitEPC <- train(
        trainPC[, !names(trainPC) %in% c("classe")],
        as.factor(trainPC$classe=="E"),
        method="glm", family = "binomial"
)

pred <- as.data.frame(cbind(
        predict(fitAPC, type="prob")[2],
        predict(fitBPC, type="prob")[2],
        predict(fitCPC, type="prob")[2],
        predict(fitDPC, type="prob")[2],
        predict(fitEPC, type="prob")[2]))
res <- as.factor(apply(pred, 1, which.max))
levels(res) <- c("A", "B", "C", "D", "E")
confusionMatrix(res, trainPC$classe)

# Random foresting
fitRF <- train(trainPC, trainPC$classe, method="rf")
confusionMatrix(predict(fitRF), trainPC$classe)
library(rattle)
fancyRpartPlot(fitT$finalModel)

# Some plots
library(ggplot2)
ggplot(training, aes(x = user_name, y=sqrt(accel_belt_x^2 + accel_belt_y^2 + accel_belt_z^2), fill=classe)) + 
        geom_violin()
ggplot(trainingScaled, aes(x = user_name, y=sqrt(accel_belt_x^2 + accel_belt_y^2 + accel_belt_z^2), fill=classe)) + 
        geom_violin()
ggplot(training, aes(x = user_name, y=total_accel_belt, fill=classe)) + 
        geom_boxplot()
ggplot(trainingScaled, aes(x = user_name, y=total_accel_belt, fill=classe)) + 
        geom_boxplot()
ggplot(training, aes(x = user_name, y=roll_forearm, fill=classe)) + 
        geom_boxplot()


# preprocessing for each user
library(dplyr)
means <- aggregate(training, list(training$user_name), function (x) if(!is.factor(x)) mean(x))
means[, "user_name"] <- means[, "Group.1"]
means <- means[, -1]
sdevs <- aggregate(training, list(training$user_name), function (x) if(!is.factor(x)) sd(x))
sdevs[, "user_name"] <- sdevs[, "Group.1"]
sdevs <- sdevs[, -1]

scaleUser <- function(df, name){
        user <- which(df$user_name==name)
        m <- filter(means, user_name==name)
        s <- filter(sdevs, user_name==name)
        s[1, which(s[1,1:53]==0)] <- 1
        df[user,2:53] <- (df[user,2:53]-m[rep(1, length(user)),2:53])/s[rep(1, length(user)),2:53]
        df
}
trainingScaled <- training
for (name in unique(training$user_name)) {
        trainingScaled <- scaleUser(trainingScaled, name)
}
mean(trainingScaled==training)

# Create a k-means model
fitK <- train(trainingScaled[, 2:53], trainingScaled$classe, method="knn")
table(predict(fitK), trainingScaled$classe)
# IT DOESN'T WORK ! The distribution is almost random

# Create 5 different classifiers
fitA <- train(trainingScaled[, 2:53], as.factor(trainingScaled$classe=="A"), method="glm", family = "binomial")
fitB <- train(trainingScaled[, 2:53], as.factor(trainingScaled$classe=="B"), method="glm", family = "binomial")
fitC <- train(trainingScaled[, 2:53], as.factor(trainingScaled$classe=="C"), method="glm", family = "binomial")
fitD <- train(trainingScaled[, 2:53], as.factor(trainingScaled$classe=="D"), method="glm", family = "binomial")
fitE <- train(trainingScaled[, 2:53], as.factor(trainingScaled$classe=="E"), method="glm", family = "binomial")

#table(predict(fitA), trainingScaled$classe)
#table(predict(fitB), trainingScaled$classe)
#table(predict(fitC), trainingScaled$classe)
#table(predict(fitD), trainingScaled$classe)
#table(predict(fitE), trainingScaled$classe)

pred <- as.data.frame(cbind(
        predict(fitA, type="prob")[2],
        predict(fitB, type="prob")[2],
        predict(fitC, type="prob")[2],
        predict(fitD, type="prob")[2],
        predict(fitE, type="prob")[2]))
res <- as.factor(apply(pred, 1, which.max))
levels(res) <- c("A", "B", "C", "D", "E")
table(res, trainingScaled$classe)
confusionMatrix(res, trainingScaled$classe)

fitA2 <- train(training[, 2:53], as.factor(training$classe=="A"), method="glm", family = "binomial")
fitB2 <- train(training[, 2:53], as.factor(training$classe=="B"), method="glm", family = "binomial")
fitC2 <- train(training[, 2:53], as.factor(training$classe=="C"), method="glm", family = "binomial")
fitD2 <- train(training[, 2:53], as.factor(training$classe=="D"), method="glm", family = "binomial")
fitE2 <- train(training[, 2:53], as.factor(training$classe=="E"), method="glm", family = "binomial")

pred2 <- as.data.frame(cbind(
        predict(fitA2, type="prob")[2],
        predict(fitB2, type="prob")[2],
        predict(fitC2, type="prob")[2],
        predict(fitD2, type="prob")[2],
        predict(fitE2, type="prob")[2]))
res2 <- as.factor(apply(pred2, 1, which.max))
levels(res2) <- c("A", "B", "C", "D", "E")
table(res2, training$classe)
confusionMatrix(res2, training$classe)



# Apply decision trees
fitT <- train(trainingScaled[2:53], trainingScaled$classe, method="rpart")
confusionMatrix(predict(fitT), trainingScaled$classe)
library(rattle)
fancyRpartPlot(fitT$finalModel)

fitRF <- train(trainingScaled[2:53], trainingScaled$classe, method="rf")
confusionMatrix(predict(fitRF), trainingScaled$classe)
library(rattle)
fancyRpartPlot(fitT$finalModel)


predTest <- as.data.frame(cbind(
        predict(fitA, newdata=testingTK, type="prob")[2],
        predict(fitB, newdata=testingTK, type="prob")[2],
        predict(fitC, newdata=testingTK, type="prob")[2],
        predict(fitD, newdata=testingTK, type="prob")[2],
        predict(fitE, newdata=testingTK, type="prob")[2]))
resTest <- as.factor(apply(predTest, 1, which.max))
levels(resTest) <- c("A", "B", "C", "D", "E")

resTest
