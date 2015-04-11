# tslm and arima
setwd("~/Documents/stat425/425 final project")
install.packages("tslm")
library(tslm)

source("Rscripts/helper.R")

# rpart package may also be used
load("data/myTest.Rdata")
load("data/myTrain.Rdata")

mytrain$month = factor(mytrain$month)
mytest$month = factor(mytest$month)

mytrain$store = factor(mytrain$store)
mytest$store = factor(mytest$store)

mytrain$dept = factor(mytrain$store)
mytest$dept = factor(mytest$store)

dept_store_test = table(mytest$dept, mytest$store)
dept_store_train = table(mytrain$dept, mytrain$store)
# weeks * stores


train_ts

test_ts


# if historical data given, do time series prediction
# otherwise 

i = 1
j = 1
tmp.train = mytrain[which(mytrain$dept==dept.names[i] & mytrain$store ==j),]
tmp.test = mytest[which(mytest$dept==dept.names[i] & mytest$store ==j),]

tmp.train$sales[is.na(tmp.train$sales)]=0
tmp.sales = tmp.train$sales

horizon = nrow(tmp.test)
s = ts(tmp.sales,frequency = 52)

model = tslm(s~trend+season)

fc = forecast(model,h= horizon)

pred = as.numeric(fc$mean)



mypred = rep("NA",nrow(mytest))
count = 0
for(i in 1:num.depts){
    print(paste("dept = ", i, "....")) # keep track of the process
    for(j in 1:num.stores){
        count = count + 1
        print (count)
        if (dept_store_test[i,j]!=39 | dept_store_train[i,j]!=143) 
            {
                tmp.train = mytrain[which(mytrain$dept==dept.names[i] & mytrain$store ==j),]
                tmp.test = mytest[which(mytest$dept==dept.names[i] & mytest$store ==j),]

                tmp.sales = tmp.train$sales
                
                horizon = nrow(tmp.test)
                s = ts(tmp.sales,frequency = 52)
                
                model = try(tslm(s~trend+season))
                
                fc = forecast(model,h= horizon)
                
                mypred[which(mytest$dept==dept.names[i] & mytest$store ==j)] = as.numeric(fc$mean)
            }

    }
}

