#Bagging and Random Forests

```{R setup include=FALSE}
library(ggplot2)
library(cowplot)
library(randomForest)
library(dplyr)

train = read.csv('Training.csv')
test = read.csv('Test.csv')

train$Local365 = train$Membership.Type
train$DayWalkUpPass = train$Membership.Type
train$Local30 = train$Membership.Type
train$Explorer = train$Membership.Type
train$WalkUp = train$Membership.Type
train$Local31 = train$Membership.Type
train$Weekender = train$Membership.Type
train$DayKiosk = train$Membership.Type
train$UTStudent = train$Membership.Type
train$Student = train$Membership.Type

train$Local365 = ifelse(train$Local365=='Local365',1,0)
train$DayWalkUpPass = ifelse(train$Local365=='24 Hour Walk Up Pass',1,0)
train$Local30 = ifelse(train$Local365=='Local30',1,0)
train$Explorer = ifelse(train$Local365=='Explorer',1,0)
train$WalkUp = ifelse(train$Local365=='Walk Up',1,0)
train$Local31 = ifelse(train$Local365=='Local 31',1,0)
train$Weekender = ifelse(train$Local365=='Weekender',1,0)
train$DayKiosk = ifelse(train$Local365=='24-Hour Kiosk (Austin B-cycle)',1,0)
train$UTStudent = ifelse(train$Local365=='U.T. Student Membership',1,0)
train$Student = ifelse(train$Local365=='Student Membership',1,0)

train = train[-1]

test$Local365 = test$Membership.Type
test$DayWalkUpPass = test$Membership.Type
test$Local30 = test$Membership.Type
test$Explorer = test$Membership.Type
test$WalkUp = test$Membership.Type
test$Local31 = test$Membership.Type
test$Weekender = test$Membership.Type
test$DayKiosk = test$Membership.Type
test$UTStudent = test$Membership.Type
test$Student = test$Membership.Type

test$Local365 = ifelse(test$Local365=='Local365',1,0)
test$DayWalkUpPass = ifelse(test$Local365=='24 Hour Walk Up Pass',1,0)
test$Local30 = ifelse(test$Local365=='Local30',1,0)
test$Explorer = ifelse(test$Local365=='Explorer',1,0)
test$WalkUp = ifelse(test$Local365=='Walk Up',1,0)
test$Local31 = ifelse(test$Local365=='Local 31',1,0)
test$Weekender = ifelse(test$Local365=='Weekender',1,0)
test$DayKiosk = ifelse(test$Local365=='24-Hour Kiosk (Austin B-cycle)',1,0)
test$UTStudent = ifelse(test$Local365=='U.T. Student Membership',1,0)
test$Student = ifelse(test$Local365=='Student Membership',1,0)

test = test[-1]
```

### Bagging - Create random forest with all variables (m=p=28)
```{R}
print('Creating random forest of 20 trees..')
```
```{R include=FALSE}
set.seed(42)
n = nrow(train)

bag_20 = randomForest(TripCount ~., data = train, ntree=20, mtry=28)
```
```{R echo=FALSE}
print(bag_20)

print('Creating random forest of 50 trees..')
```

```{R include=FALSE}
bag_50 = randomForest(TripCount ~., data = train, ntree=50, mtry=28)\
```
```{R echo=FALSE}
print(bag_50)

print('Creating random forest of 50 trees..')
```
```{R include=FALSE}
bag_200 = randomForest(TripCount ~., data = train, ntree=200, mtry=28)
```
```{R echo=FALSE}
print(bag_200)
plot(bag_200)
```

### Bagging with validation set

```{R echo=FALSE}
#create validation set separated into 2 sets: x inputs for prediction, y values for validation
pred = subset(test, select = -TripCount)
predY = as.double(test$TripCount)

bag_val_150 = randomForest(TripCount ~., data = train, xtest = pred, ytest = predY, ntree=150, mtry=28)
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
rf_val_150 = randomForest(TripCount ~., data = train, xtest = pred, ytest = predY, ntree=150)
print(rf_val_150)
plot(rf_val_150)

importance(rf_val_150)
varImpPlot(rf_val_150)

###other work - distance from mccombs
mlat = 30.2840788
mlong = -97.7376486

train = mutate(train, distanceMcCombs = sqrt((Lat-mlat)^2+(Long-mlong)^2))
test = mutate(test, distanceMcCombs = sqrt((Lat-mlat)^2+(Long-mlong)^2))

print(19)
head(test)
str(test)




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
