setwd("~/Documents/University/COMP3008/_New Coursework/Data")

library(class)
library(nnet)
library(epibasix)
library(CVThresh)
library(e1071)

df <- read.table("semeion.data.data")
df$digit <- replicate(nrow(df), 42)

df$digit[which(as.logical(df$V266), arr.ind=TRUE)] = 9
df$digit[which(as.logical(df$V265), arr.ind=TRUE)] = 8
df$digit[which(as.logical(df$V264), arr.ind=TRUE)] = 7
df$digit[which(as.logical(df$V263), arr.ind=TRUE)] = 6
df$digit[which(as.logical(df$V262), arr.ind=TRUE)] = 5
df$digit[which(as.logical(df$V261), arr.ind=TRUE)] = 4
df$digit[which(as.logical(df$V260), arr.ind=TRUE)] = 3
df$digit[which(as.logical(df$V259), arr.ind=TRUE)] = 2
df$digit[which(as.logical(df$V258), arr.ind=TRUE)] = 1
df$digit[which(as.logical(df$V257), arr.ind=TRUE)] = 0

data <- df[,c(0:256,267)]

squareTable <- function(x,y) {
    x <- factor(x)
    y <- factor(y)

    commonLevels <- sort(unique(c(levels(x), levels(y))))

    x <- factor(x, levels = commonLevels)
    y <- factor(y, levels = commonLevels)

    table(x,y)

}

kf.knn <- function(data, k, nearest)
{
  N <- nrow(data)
  data <- data[sample(1:N),]
  
  folds.index <- cvtype(N, cv.bsize=1, cv.kfold=k, FALSE)$cv.index
  
  total = 0
  
  for (i in 1:k)
  {
    test <- data[folds.index[i,], 0:(ncol(data)-1)]
    test.labels <- data[as.array(folds.index[i,]), ncol(data)]
    
    rest <- as.array(folds.index[-i,])
    
    train <- data[rest, 0:(ncol(data)-1)]
    train.labels <- data[rest, ncol(data)]
    
    knnpredict <- knn(train, test, train.labels, nearest)
    t <- table(as.factor(test.labels), as.factor(knnpredict))
    
    kap <- epiKappa(t)
    total <- total + kap$kappa
  }

return(total / k)
}

kf.nnet <- function(data, k, hidden)
{
  N <- nrow(data)
  data <- data[sample(1:N),]
  
  folds.index <- cvtype(N, cv.bsize=1, cv.kfold=k, FALSE)$cv.index
  
  total = 0
  
  for (i in 2:k)
  {
    test <- data[folds.index[i,], 0:(ncol(data)-1)]
    test.labels <- data[as.array(folds.index[i,]), ncol(data)]
    
    rest <- as.array(folds.index[-i,])
    
    train <- data[rest, 0:(ncol(data)-1)]
    train.labels <- data[rest, ncol(data)]
    
    train$label <- as.factor(train.labels)
    
    
    nnetwork <- nnet(label ~ ., data=train, size=hidden, MaxNWts = 1000000, maxit=200)
    nnetpredict <- predict(nnetwork, test, "class")
    
    #print(as.factor(test.labels))
    #print(as.factor(nnetpredict))
    table1 <- table(as.factor(test.labels), as.factor(nnetpredict))
    
    table2 <- squareTable(as.factor(test.labels), as.factor(nnetpredict))
    
    print(table2)
    kap <- epiKappa(table2)
    total <- total + kap$kappa
  }

return(total / k)
}

kf.svm <- function(data, k)
{
  N <- nrow(data)
  data <- data[sample(1:N),]
  
  folds.index <- cvtype(N, cv.bsize=1, cv.kfold=k, FALSE)$cv.index
  
  total = 0
  
  for (i in 2:k)
  {
    test <- data[folds.index[i,], 0:256]
    test.labels <- data[as.array(folds.index[i,]), 257]
    
    rest <- as.array(folds.index[-i,])
    
    train <- data[rest, 0:256]
    train.labels <- data[rest, 257]
    
    train$label <- as.factor(train.labels)
     
    svmmodel <- svm(label ~ ., data=train, cost=8, gamma=0.001, cross=2)
    svmpredict <- predict(svmmodel, test)
    
    #print(as.factor(test.labels))
    #print(as.factor(nnetpredict))
    
    t <- table(as.factor(test.labels), as.factor(svmpredict))
    print(t)
    kap <- epiKappa(t)
    total <- total + kap$kappa
  }

return(total / k)
}

result <- vector("numeric", 1)
n <- vector("numeric", 1)

#print(kf.svm(data, 5))


for (i in 25:26)
{
  avg <- kf.nnet(data, 5, i)
  result <- append(result, avg)
  n <- append(n, i)
  cat("!!!!!!!", i, avg, "\n")
}

result <- result[2:length(result)]
n <- n[2:length(n)]

df <- data.frame(kappa = result, n=n)
plot(df$n, df$res)

print(df)

# train$label <- as.factor(truetrain)
# 
# nnetwork <- nnet(label ~ ., data=train, size=5, MaxNWts = 10000)
# nnetpredict <- predict(nnetwork, test, "class")
# t <- table(truetest, nnetpredict)
# print(t)
# print(epiKappa(t))