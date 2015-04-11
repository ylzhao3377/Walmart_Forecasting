# linear model and random forest
setwd("~/Dropbox/2014Fall_425project")
source("Rscripts/helper.R")
#rm(list=objects())
lapply(packages,require, character.only = TRUE)

# load data
load("data/mytrain.Rdata")
load("data/mytest.Rdata")

mypred2= rep(NA, nrow(mytest))
#########################################################
# linear regression
#########################################################
#########################################################
# Dept+store not match
#########################################################
dept_store_train = table(mytrain$dept,mytrain$store);
dept_store_test = table(mytest$dept, mytest$store)
dim(dept_store_train)
dim(dept_store_test)
#[1] 81 45

dept.names = sort(unique(mytrain$dept)) # names for the 81 departments

tmp=(dept_store_train ==0 ) * (dept_store_test>0)
sum(tmp)
#[1] 11, # of combinations that exist in test but no in train

missing_dept_store = which(tmp>0, arr.ind=TRUE, useNames = FALSE)
missing_dept_store[,1] = dept.names[missing_dept_store[,1]]
missing_dept_store = missing_dept_store[order(missing_dept_store[,1], missing_dept_store[,2]),]
# order the missing dept+store by stores and dept
missing_dept_store
#[1,]    29   37  # no historical data for store 37, dept 29. 
#[2,]    30   36
#[3,]    30   42
#[4,]    39   34
#[5,]    39   45
#[6,]    43   18
#[7,]    43   24
#[8,]    99    5
#[9,]    99    9
#[10,]   99   10
#[11,]   99   25


id=NULL
tmptest = as.matrix(mytest[, c("dept", "store")])
for(k in 1:nrow(missing_dept_store)){
  tmp.flag = apply(tmptest, 1, function(x) identical(as.numeric(x), as.numeric(missing_dept_store[k,])))
  id=c(id, which(tmp.flag==TRUE))
}
length(id)
mytest1=mytest[-id,]
#####################################################
data prepare
#####################################################
lm.mytrain=subset(mytrain,select=c(store,dept,sales,temp:holiday))
for (j in c(1:2,8:10)){
  lm.mytrain[,j]=as.factor(lm.mytrain[,j])  
}

lm.mytest=subset(mytest1,select=c(store,dept,temp:holiday))
for (j in c(1:2,7:9)){
  lm.mytest[,j]=as.factor(lm.mytest[,j])  
}

lm.mytest1=lm.mytest[which(lm.mytest$year !="2013"),]
lm.mytest2=lm.mytest[which(lm.mytest$year =="2013"),]
sum(is.na(lm.mytest))
sum(is.na(lm.mytrain))
sum(is.na(lm.mytest1))
sum(is.na(lm.mytest2))

mytest2=mytest[id,]
lm.mytest3=subset(mytest2,select=c(store,dept,temp:holiday))
for (j in c(1:2,7:9,11)){
  lm.mytest3[,j]=as.factor(lm.mytest3[,j])  
}

head(lm.mytest1)
head(lm.mytest2)
head(lm.mytest3)
head(lm.mytrain)
##############################


dept.names = sort(unique(lm.mytrain$dept))
num.stores = length(unique(mytest$store))
num.depts = length(dept.names)


mypred2=rep(NA,nrow(mytest))
r2=rep(0,num.depts)

for (i in 1:num.depts){
  if(i==58)next
  print(paste("dept = ", i, "....")) # keep track of the process
  id.train = lm.mytrain[which(lm.mytrain$dept==dept.names[i]),]
  id.test = lm.mytest1[which(lm.mytest1$dept==dept.names[i]),]
  tmp.test.id=which(mytest$year!="2013"&mytest$dept==dept.names[i])
  fullModel=lm(log(sales+4990)~store+temp+fuel+cpi+unemp+year+as.numeric(yday)+mday+month+wk+holiday,data=id.train)
  dept_pred = try(predict(fullModel,newdata=id.test))
  r2[i]=summary(fullModel)$r.squared
  for (row_id in names(dept_pred)){
    mypred2[as.numeric(row_id)] = dept_pred[row_id][[1]]   
  }
}
sum(is.na(mypred2))

r2=rep(0,num.depts)
for (i in 1:num.depts){
  if(i==58)next
  print(paste("dept = ", i, "....")) # keep track of the process
  id.train = lm.mytrain[which(lm.mytrain$dept==dept.names[i]),]
  id.test = lm.mytest2[which(lm.mytest2$dept==dept.names[i]),]
  fullModel=lm(log(sales+4990)~store+temp+fuel+month+as.numeric(yday)+mday+wk+holiday,data=id.train)
  r2[i]=summary(fullModel)$r.squared
  dept_pred = try(predict(fullModel,newdata=id.test))
  for (row_id in names(dept_pred)){
    mypred2[as.numeric(row_id)] = dept_pred[row_id][[1]]   
  }
}
sum(is.na(mypred2))


id.train = lm.mytrain[which(lm.mytrain$dept==dept.names[58]),]
id.test =lm.mytest1[which(lm.mytest1$dept==dept.names[58]),]
fullModel=lm(log(sales+4990)~temp+fuel+cpi+unemp+year+as.numeric(yday)+month+wk+holiday,data=id.train)
dept_pred = try(predict(fullModel,newdata=id.test))
summary(fullModel)$r.squared
for (row_id in names(dept_pred)){
  mypred2[as.numeric(row_id)] = dept_pred[row_id][[1]]   
}


id.train = lm.mytrain[which(lm.mytrain$dept==dept.names[42]),]
id.test =lm.mytest1[which(lm.mytest$dept==dept.names[42]),]
fullModel=lm(log(sales+revise)~temp+fuel+cpi+unemp+year+as.numeric(yday)+as.numeric(mday)+as.numeric(month)+wk,data=id.train)
dept_pred = try(predict(fullModel,newdata=id.test))
summary(fullModel)$r.squared
for (row_id in names(dept_pred)){
  mypred2[as.numeric(row_id)] = dept_pred[row_id][[1]]   
}
sum(is.na(mypred2))


id.train = lm.mytrain[which(lm.mytrain$dept==dept.names[63]),]
id.test =lm.mytest2[which(lm.mytest2$dept==dept.names[63]),]
fullModel=lm(log(sales+revise)~temp+fuel+cpi+unemp+as.numeric(yday)+as.numeric(mday)+as.numeric(month)+wk+holiday,data=id.train)
dept_pred = try(predict(fullModel,newdata=id.test))
summary(fullModel)$r.squared
for (row_id in names(dept_pred)){
  mypred2[as.numeric(row_id)] = dept_pred[row_id][[1]]   
}
sum(is.na(mypred2))


id.train = lm.mytrain[which(lm.mytrain$store %in% c(19,4)),]
id.test =lm.mytest[which(lm.mytest$dept==dept.names[38]),]
fullModel=lm(log(sales+revise)~store+temp+fuel+cpi+unemp+as.numeric(yday)+mday+as.numeric(month)+as.numeric(wk)+holiday,data=id.train)
dept_pred = try(predict(fullModel,newdata=id.test))
summary(fullModel)$r.squared
for (row_id in names(dept_pred)){
  mypred2[as.numeric(row_id)] = dept_pred[row_id][[1]]   
}
sum(is.na(mypred2))

lm.mytest3[is.na(lm.mytest3)]<-0

for (i in c(37,36,42,34,45,18,24,5,9,10,25)){
  id.train = lm.mytrain[which(lm.mytrain$dept==i),]
  fullModel=lm(log(sales+revise)~temp+fuel+cpi+unemp+as.numeric(yday)+mday+month+as.numeric(wk)+holiday,data=id.train)
  dept_pred = try(predict(fullModel,newdata=lm.mytest3))
  r2[i]=summary(fullModel)$r.squared
  for (row_id in names(dept_pred)){
    mypred2[as.numeric(row_id)] = dept_pred[row_id][[1]]   
  } 
}
sum(is.na(mypred2))


mypred2=exp(as.numeric(mypred2))-revise
mypred2[is.na(mypred2)]<-0
head(mypred2)

mypred = mypred2
na_index = which(is.na(mypred))
tmp_na <- mytest[na_index,]

for (index in na_index) {
  this_store = mytest$store[index]
  this_dept = mytest$dept[index]
  this_wk = mytest$wk[index]
  
  # if dept in train, use median to estimate
  # else 
  # dept not in train, use dept from other stores
  # construct 
  tmp_train = deptInTrain(this_store, this_dept,this_wk, mytrain)
  mypred[index] = median(tmp_train$sales)
}


# write output
output_file = "output/submission_lm"
writeSubmission(mytest, mypred, output_file)


#filepath = "output/submissionlm.csv";
#writeSubmission(mytest,mypred, filepath)
`

