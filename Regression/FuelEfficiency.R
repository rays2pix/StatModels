
data = "./FuelEfficiency.csv"
FuelEff = read.csv("./FuelEfficiency.csv")
#scatter plots indicate linear relationship
plot(FuelEff$GPM ~ FuelEff$WT)
plot(FuelEff$GPM ~ FuelEff$DIS)
FuelEff =FuelEff[-1]

#the full model explains 93.29 percent variance
m1 = lm(GPM ~.,data=FuelEff)
plot(m1)
summary(m1)
cor(FuelEff)


library(leaps)  
X=FuelEff[,2:7]
y=FuelEff[,1]
out=summary(regsubsets(X,y,nbest=2,nvmax=ncol(X)))
tab=cbind(out$which,out$rsq,out$adjr2,out$cp)
tab

m2=lm(GPM~WT,data=FuelEff)
summary(m2)


## cross-validation (leave one out) for the model on all six regressors
n=length(FuelEff$GPM)
diff=dim(n)
percdiff=dim(n)
for (k in 1:n) {
  train1=c(1:n)
  #leave one out
  train=train1[train1!=k]

  m1=lm(GPM~.,data=FuelEff[train,])
  pred=predict(m1,newdat=FuelEff[-train,])
  obs=FuelEff$GPM[-train]
  diff[k]=obs-pred
  percdiff[k]=abs(diff[k])/obs
}
me=mean(diff)
rmse=sqrt(mean(diff**2))
mape=100*(mean(percdiff))
me   # mean error
rmse # root mean square error
mape # mean absolute percent error 

## cross-validation (leave one out) for the model on weight only
n=length(FuelEff$GPM)
diff=dim(n)
percdiff=dim(n)
for (k in 1:n) {
  train1=c(1:n)
  train=train1[train1!=k]
  m2=lm(GPM~WT,data=FuelEff[train,])
  pred=predict(m2,newdat=FuelEff[-train,])
  obs=FuelEff$GPM[-train]
  diff[k]=obs-pred
  percdiff[k]=abs(diff[k])/obs
}
me=mean(diff)
rmse=sqrt(mean(diff**2))
mape=100*(mean(percdiff))
me   # mean error
rmse # root mean square error
mape # mean absolute percent error



