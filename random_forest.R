setwd("~/Dropbox/2014Fall_425project")
source("Rscripts/helper.R")
library("randomForest")
library("rpart")
# rpart package may also be used
load("data/myTest.Rdata")
load("data/myTrain.Rdata")

mytrain$month = factor(mytrain$month)
mytest$month = factor(mytest$month)

mytrain$store = factor(mytrain$store)
mytest$store = factor(mytest$store)

mytrain$store = factor(mytrain$store)
mytest$store = factor(mytest$store)

dept.names = sort(unique(mytrain$dept))
num.stores = length(unique(mytest$store))
num.depts = length(dept.names)


dept_store_test = table(mytest$dept, mytest$store)
dept_store_train = table(mytrain$dept, mytrain$store)

mypred = rep(NA, nrow(mytest)) # vector used to store the prediction
for(i in 1:num.depts){
    print(paste("dept = ", i, "....")) # keep track of the process
    for(j in 1:num.stores){
        if (dept_store_test[i,j]==0 | dept_store_train[i,j]==0) next # no store+dept combo info in test or train
        
        

        # for each store+dept combo, filter the train/test data   
        tmp.train.id = which(mytrain$dept==dept.names[i] & mytrain$store==j)
        tmp.train.features = subset(mytrain[tmp.train.id,], select = -c(date,store,dept,sales,type,size))
        tmp.train.sales = mytrain$sales[tmp.train.id]; 
        
        tmp.test.id = which(mytest$dept==dept.names[i] & mytest$store==j)
        
        if (length(tmp.train.sales)>50){

            rf.tmp <- rpart(sales~ temp+fuel+year+month+mday+yday
                           +wk,data = mytrain[tmp.train.id,], method="anova")
            
            tmp.test.features = subset(mytest[tmp.test.id,], select = -c(date,store,dept,missing,type,size))
            mypred[tmp.test.id] = predict(rf.tmp, tmp.test.features,na.action = na.omit)
        }
        else{
            week1 = week("2010-02-05")
            cum.wk.train = (mytrain$year[tmp.train.id] -2010)*53+mytrain$wk[tmp.train.id]
            cum.wk.train = cum.wk.train - week1+1; 
            cum.wk.test = (mytest$year[tmp.test.id] -2010)*53+mytest$wk[tmp.test.id]
            cum.wk.test = cum.wk.test - week1 + 1; 
            
            # there could be some missing weeks in the training data for store+dept
            # Note that tmp.train.sales may have NA values
            tmp.train.sales = rep(NA, max(cum.wk.train))
            tmp.train.sales[cum.wk.train] = mytrain$sales[tmp.train.id];
            
            
            # create the 3 sales for each record in the test data
            hist.data = matrix(, length(tmp.test.id), 3)
            hist.data[,1] = tmp.train.sales[cum.wk.test-53-1]; 
            hist.data[,2] = tmp.train.sales[cum.wk.test-53];
            hist.data[,3] = tmp.train.sales[cum.wk.test-53+1];
            
            med.pred = apply(hist.data, 1, function(x) median(x[!is.na(x)]))
            
            # If no prior year's data available, just predict it to be the median
            # of the other predicted values
            med.pred[is.na(med.pred)] = median(med.pred, na.rm=TRUE)
            
            mypred[tmp.test.id] = med.pred
        }
    }
}
sum(is.na(mypred))


load("data/missing_combo.RData") # get id
for(k in id){
    tmp_train_missing_dept= which(mytrain$dept == mytest$dept[k] & mytrain$wk == mytest$wk[k] 
    )
    mypred[k]=mean(mytrain$sales[tmp_train_missing_dept])
}
writeSubmission(mytest,mypred,"output/randomforest")



table(mytrain$store)
mypred = rep("NA",nrow(mytest))
for (i in 1:num.depts){
    print(paste("Depts", i ,sep=" "))
    tmp.train = mytrain[which(mytrain$dept==dept.names[i]),]
    rf <- rpart(log(sales+4900)~ store + temp + fuel+year+month+mday+yday
                       +wk+holiday, data = tmp.train, method = "anova")
    tmp.test = mytest[which(mytest$dept ==dept.names[i]),]
    pred <- predict(rf, tmp.test[,c("store","type","size","temp","fuel"
                                    ,"year","month","mday","yday","wk","holiday")])
    mypred[which(mytest$dept==dept.names[i])] = exp(pred)-4900
}

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


writeSubmission(mytest,mypred,"output/randomforest")
# rank 305: 3510


rf.tmp = randomForest(tmp.train.sales ~ store + dept+type+size+temp+fuel+year+month+mday+yday
                      +wk,data = tmp.train, ntree = 100, mtry = 6,importance=TRUE,na.action = na.omit)
