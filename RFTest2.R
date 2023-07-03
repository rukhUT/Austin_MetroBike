library(ggplot2)
library(cowplot)
library(randomForest)
library(dplyr)

setwd('C:/Users/Rukh/School/UT/Summer/STA 380/Project')
train = read.csv('Training.csv')
test = read.csv('Test.csv')

mlat = 30.2840788
mlong = -97.7376486

train = mutate(train, distanceMcCombs = sqrt((Lat-mlat)^2+(Long-mlong)^2))
train = mutate(train, lat_distance = Lat-mlat)
train = mutate(train, long_distance = Long-mlong)

test = mutate(test, distanceMcCombs = sqrt((Lat-mlat)^2+(Long-mlong)^2))
test = mutate(test, lat_distance = Lat-mlat)
test = mutate(test, long_distance = Long-mlong)

head(test)
str(train)

#--------------------
#Bagging - Create random forest with all variables (m=p=22)
set.seed(42)
n = nrow(train)

#create validation set separated into 2 sets: x inputs for prediction, y values for validation
pred = subset(test, select = -TripCount)
predY = as.double(test$TripCount)

bag_20 = randomForest(TripCount ~., data = train, ntree=20, mtry=22)
print(bag_20)

bag_50 = randomForest(TripCount ~., data = train, ntree=50, mtry=22)
print(bag_50)

bag_200 = randomForest(TripCount ~., data = train, ntree=200, mtry=22)
print(bag_200)

plot(bag_200)

###bagging with validation set

bag_val_150 = randomForest(TripCount ~., data = train, xtest = pred, ytest = predY, ntree=150, mtry=22)
print(bag_val_150)
plot(bag_val_150)


##random forests
rf_50 = randomForest(TripCount ~., data = train, ntree=50)
print(rf_50)

rf_150 = randomForest(TripCount ~., data = train, ntree=150)
print(rf_150)

rf_300 = randomForest(TripCount ~., data = train, ntree=300)
print(rf_300)
plot(rf_300)


#rf validation
rf_val_120 = randomForest(TripCount ~., data = train, xtest = pred, ytest = predY, ntree=120)
print(rf_val_120)
plot(rf_val_120)



###other work
plot(train$Temp, train$TripCount)
plot(train$distance, train$TripCount)
plot(train$lat_distance, train$TripCount)
plot(train$Lat, train$TripCount)

lmdistance = lm(TripCount ~ distance, data = train)
summary(lmdistance)

pred = subset(test, select = -TripCount)
predY = as.double(test$TripCount)
str(pred)
str(predY)

rf2 = randomForest(TripCount ~., data = train, xtest = pred, ytest = predY, ntree=200, mtry=15)
print(rf2)

plot(rf2)

rf3 = randomForest(TripCount ~., data = train, xtest = pred, ytest = predY, ntree=200)
print(rf3)

plot(rf3)

#--------------------------------------------------
#get rf fits for different number of trees
#note: to get this to work I had to use maxnodes parameter of randomForest!!!
set.seed(99)
n = nrow(Boston)
ntreev = c(10,500,5000)
nset = length(ntreev)
fmat = matrix(0,n,nset)
fmat
for(i in 1:nset) {
  cat('doing Boston rf: ',i,'\n')
  rffit = randomForest(medv~lstat,data=Boston,ntree=ntreev[i],maxnodes=15)
  fmat[,i] = predict(rffit)
}
#--------------------------------------------------
#plot oob error using last fitted rffit which has the largest ntree.


par(mfrow=c(1,1))
plot(rffit)

#--------------------------------------------------
#plot fits

par(mfrow=c(1,3))
oo = order(Boston$lstat)
for(i in 1:nset) {
  plot(Boston$lstat,Boston$medv,xlab='lstat',ylab='medv')
  lines(Boston$lstat[oo],fmat[oo,i],col=i,lwd=3)
  title(main=paste('bagging ntrees = ',ntreev[i]))
}
