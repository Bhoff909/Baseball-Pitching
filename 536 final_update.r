data = na.omit(read.csv("C:/Users/jbrin/OneDrive/Documents/Spring 2022/Math 536/final/baseball-1.csv"))
#data = na.omit(read.csv("C:/Users/jon/OneDrive/Documents/Spring 2022/Math 536/final/baseball-1.csv"))

#plot balls and strikes
plot(data$plate_x[data$description=="ball"],data$plate_z[data$description=="ball"],col=3,cex=.2)
points(data$plate_x[data$description!="ball"],data$plate_z[data$description!="ball"],col=2,cex=.2)

#frame the fringe zone
#upper range
abline(h = 3.2, col="red")
abline(h = 3.5, col="red")
#lower range
abline(h = 1.6,col="red")
abline(h = 1.3,col="red")
#right range
abline(v=.8,col="red")
abline(v=1.1,col="red")
#left range
abline(v=-0.8,col="red")
abline(v=-1.1,col="red")

#truncate data to only include data in the fringe zone
trunc_data = subset(data, (data$plate_z > 3.2 & data$plate_z < 3.5 & data$plate_x > -1.1  & data$plate_x < 1.1)
                        | (data$plate_z > 1.3 & data$plate_z < 1.6 & data$plate_x > -1.1  & data$plate_x < 1.1)
                        | (data$plate_x > -1.1 & data$plate_x < -.8 & data$plate_z > 1.6  & data$plate_z < 3.2)
                        | (data$plate_x > .8 & data$plate_x < 1.1 & data$plate_z > 1.6  & data$plate_z < 3.2))

#plot truncated balls and strikes
plot(trunc_data$plate_x[trunc_data$description=="ball"],trunc_data$plate_z[trunc_data$description=="ball"],col=3,cex=.2)
points(trunc_data$plate_x[trunc_data$description!="ball"],trunc_data$plate_z[trunc_data$description!="ball"],col=2,cex=.2)

#try a couple models. glm and random forest
attach(trunc_data)
library(randomForest)

trunc_data$description = as.factor(trunc_data$description)#Why is this necessary?

model.a = glm(description~pitch_type+
                release_speed+
                balls+
                strikes+
                plate_x+
                plate_z+
                home_score+
                away_score,
                data=trunc_data,family='binomial')

model.b = randomForest(description~pitch_type+
                release_speed+
                balls+
                strikes+
                plate_x+
                plate_z+
                home_score+
                away_score,
                data=trunc_data,ntree=100,mtry=3,
                control=rpart.control(minsplit=5,cp=.01))

summary(model.a)

model.b #interpretation?

#compute a log likelihood for each model and then compare them.
y = as.numeric(trunc_data$description) - 1
p.a = predict.glm(model.a,newdata=trunc_data,type="response")
p.b = predict(model.b,newdata=trunc_data,type="prob")[,2] #why column 2?
#Probably don't want rounded probabilities to exactly 0 or 1
p.b[p.b==0] = 0.00001
p.b[p.b==1] = .9999

LL.a = sum(y*log(p.a) + (1-y)*log(1-p.a))
LL.b = sum(y*log(p.b) + (1-y)*log(1-p.b))
LL.a
LL.b

#add high_heat indicator to truncated data
trunc_data$high_heat = rep(0,length(trunc_data$description))
trunc_data$high_heat[trunc_data$pitch_type %in% c("FF","FT","FC","FS") & trunc_data$plate_z > 3.3] = 1

#new model removes pitch_type and release speed predictors
#replaces with high_heat predictor
model.c = randomForest(description~balls+
                         strikes+
                         plate_x+
                         plate_z+
                         home_score+
                         away_score+high_heat,
                         data=trunc_data,ntree=100,mtry=3,
                         control=rpart.control(minsplit=5,cp=.01))


trunc_data_a = trunc_data
trunc_data_b = trunc_data
trunc_data_a$high_heat = 1 #all pitches in a are high heat
trunc_data_b$high_heat = 0 #no pitches in b are high heat

avg.prob.hh = mean(predict(model.c,newdata=trunc_data_a,type="prob")[,2])
avg.prob.nhh = mean(predict(model.c,newdata=trunc_data_b,type="prob")[,2])

#bootstrapping
#this code takes time to process, so only run 100 times
bs.avg.prop.hh = rep(0,100)
bs.avg.prop.nhh = rep(0,100)
for(i in 1:100){
  
  row.index= sample(1:dim(trunc_data)[1],dim(trunc_data)[1],replace=T) #sample row indices with replacement
  
  bs.trunc.data = data.frame(trunc_data[row.index,]) #bootstrapped data from sampled indices
  
  bs.trunc_data_a = bs.trunc.data
  bs.trunc_data_b = bs.trunc.data
  bs.trunc_data_a$high_heat = 1 #all pitches in a are high heat
  bs.trunc_data_b$high_heat = 0 #no pitches in b are high heat
  
  bs.avg.prop.hh[i] = mean(predict(model.c,newdata=bs.trunc_data_a,type="prob")[,2])
  bs.avg.prop.nhh[i] = mean(predict(model.c,newdata=bs.trunc_data_b,type="prob")[,2])
}

hh_lower_bound = sort(bs.avg.prop.hh)[2.5]
hh_upper_bound = sort(bs.avg.prop.hh)[97.5]

nhh_lower_bound = sort(bs.avg.prop.nhh)[2.5]
nhh_upper_bound = sort(bs.avg.prop.nhh)[97.5]

#Residual diagnostics for logistic regression
library(car)
residualPlots(model.a)

#random forest appears to be performing better on this dataset
#make predictions from random forest model

#prediction of strikes for high-heat fast ball
#subset data to only include high-heat pitches
trunc_data$high_heat = rep(0,length(trunc_data$description))
trunc_data$high_heat[trunc_data$pitch_type %in% c("FF","FT","FC","FS") & trunc_data$plate_z > 3.3] = 1

########################################my work. Gives similar results to professors
high_heat = subset(trunc_data,trunc_data$pitch_type %in% c("FF","FT","FC","FS") & trunc_data$plate_z > 3.3)

hh_pred = mean(predict(model.b,newdata=high_heat,type="prob")[,2])

#subset data to include all other pitches
other_pitches = subset(trunc_data, trunc_data$plate_z < 3.3)

other_pred = mean(predict(model.b,newdata=other_pitches,type="prob")[,2])




#######################attempt cross-validation
#Adapted from "model comparison ..." code

n = dim(trunc_data)[1]
n

###with 4370 data points, going to have 437 data points in each testing set
###Obviously that means there is 1 data point that never gets partitioned
###as testing data.  But we can pick those up as their own testing group at the end if we want to.

CV.ind.mat = matrix(sample(1:n,4370,replace=F),nrow=10)

#Run same code as before, but each time, we're going to run our models using only train data
#and testing them using only testing data.

CVLL.a = rep(0,10)
CVLL.b = rep(0,10)

for(k in 1:10){
  train.data = trunc_data[(1:n)[-CV.ind.mat[k,]],]
  test.data = trunc_data[CV.ind.mat[k,],]
  
  model.a = glm(description~pitch_type+
                  release_speed+
                  balls+
                  strikes+
                  plate_x+
                  plate_z+
                  home_score+
                  away_score,
                  data=train.data,family='binomial')
  
  model.b = randomForest(description~pitch_type+
                  release_speed+
                  balls+
                  strikes+
                  plate_x+
                  plate_z+
                  home_score+
                  away_score,
                  data=train.data,ntree=100,mtry=3,
                  control=rpart.control(minsplit=5,cp=.01))

  
  
  y = (as.numeric(trunc_data$description) - 1)[CV.ind.mat[k,]]
  p.a = predict.glm(model.a,newdata=test.data,type="response")
  p.b = predict(model.b,newdata=test.data,type="prob")[,2]
  #Probably don't want rounded probabilities to exactly 0 or 1
  p.b[p.b==0] = 0.00001
  p.b[p.b==1] = .9999
  
  LL.a1 = sum(y*log(p.a) + (1-y)*log(1-p.a))
  LL.b1 = sum(y*log(p.b) + (1-y)*log(1-p.b))
  CVLL.a[k] = LL.a1
  CVLL.b[k] = LL.b1
  
  
  
}

CVLL.a
CVLL.b












#I consider high heat balls to be fast balls with plate_z > 3.3
#calculate the probability of these being called strikes
n = length(trunc_data$X)
#pitch_count = n
high_fast_count = 0
high_pitch_count = 0


#count fringe balls
sum(trunc_data$description=="ball")
#fringe strikes
sum(trunc_data$description=="called_strike")

#count all high pitches
for(i in 1:n){
  if(plate_z[i] > 3.3){
    high_pitch_count = high_pitch_count + 1
  }
}

#count high fast balls called strikes (high heat)
for(i in 1:n){
  if(plate_z[i] > 3.3 & (pitch_type[i] == "FF" | pitch_type[i] == "FT" | pitch_type[i] == "FC" | pitch_type[i] == "FS") & description[i] == "called_strike"){
  high_fast_count = high_fast_count + 1
  }
}

high_other_count = 0
#count high pitches other than fast balls called strikes
for(i in 1:n){
  if(plate_z[i] > 3.3 & pitch_type[i] != "FF" & pitch_type[i] != "FT" & pitch_type[i] != "FC" & pitch_type[i] != "FS" & description[i] == "called_strike"){
    high_other_count = high_other_count + 1
  }
}

#proportion of high fast balls called strikes
high_fast_count/high_pitch_count

#proportion of high pitches other than fast balls called strikes
high_other_count/high_pitch_count


#count low pitches called strikes
low_other_count = 0
#count high pitches other than fast balls called strikes
for(i in 1:n){
  if(data$plate_z[i] < 1.85 & data$pitch_type[i] != "FF" & data$pitch_type[i] != "FT" & data$pitch_type[i] != "FC" & data$pitch_type[i] != "FS" & data$description[i] == "called_strike"){
    low_other_count = low_other_count + 1
  }
}

#total low pitches
low_pitch_count = 0
for(i in 1:n){
  if(data$plate_z[i] < 1.85){
    low_pitch_count = low_pitch_count + 1
  }
}

#low fast balls called strikes
low_fast_count = 0
for(i in 1:n){
  if(data$plate_z[i] < 1.85 & (data$pitch_type[i] == "FF" | data$pitch_type[i] == "FT" | data$pitch_type[i] == "FC" | data$pitch_type[i] == "FS") & data$description[i] == "called_strike"){
    low_fast_count = low_fast_count + 1
  }
}
