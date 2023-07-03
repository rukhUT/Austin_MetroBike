library(ggplot2)
library(cowplot)
library(randomForest)
library(dplyr)

setwd('C:/Users/Rukh/School/UT/Summer/STA 380/Project')
train = read.csv('Training.csv')
test = read.csv('Test.csv')

mlat = 30.2840788
mlong = -97.7376486

train = mutate(train, distance = sqrt((Lat-mlat)^2+(Long-mlong)^2))
train = mutate(train, lat_distance = Lat-mlat)
train = mutate(train, long_distance = Long-mlong)

test = mutate(test, distance = sqrt((Lat-mlat)^2+(Long-mlong)^2))
test = mutate(test, lat_distance = Lat-mlat)
test = mutate(test, long_distance = Long-mlong)

head(train)
str(train)
set.seed(42)
rf = randomForest(TripCount ~., data = train, ntree=200, mtry=15)
print(rf)
plot(rf)
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
