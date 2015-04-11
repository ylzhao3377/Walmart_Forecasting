setwd("~/Documents/stat425/425 final project")
source("Rscripts/helper.R")
packages <- c("lubridate", "lattice", "forecast")
lapply(packages, require, character.only = TRUE)

load("data/myTrain.Rdata")
load("data/myTest.Rdata")

table(mytrain$year, mytrain$wk) # 53 weeks per year

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
        tmp.train.sales = mytrain$sales[tmp.train.id]; 
        tmp.test.id = which(mytest$dept==dept.names[i] & mytest$store==j)
        
        # cumulative weeks; set week 1 in training as the reference
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


# replace all NAs with 0s



##################################################################
# WRITE YOUR CODE HERE
# No histrical data available for the 36 obs in the test set
##################################################################

load("data/missing_combo.RData") # get id
for(k in id){
    tmp_train_missing_dept= which(mytrain$dept == mytest$dept[k] & mytrain$wk == mytest$wk[k] 
    )
    mypred[k]=mean(mytrain$sales[tmp_train_missing_dept])
}

sum(is.na(mypred)) # 288

mypred[mytest$missing==1] # the 36 obs in the test data



##################################################################
# WRITE YOUR CODE HERE
# The prediction for holidays need to be adjusted
##################################################################


test_holiday_indices = which(mytest$isholiday==TRUE)
original_pred = mypred[test_holiday_indices] # to compare with adjusted predictions

count = 0
for (index in test_holiday_indices){
    this_holiday = as.character(mytest[index,c("holiday","dept","store")][1,1])
    this_dept = as.character(mytest[index,c("holiday","dept","store")][1,2])
    this_store = as.character(mytest[index,c("holiday","dept","store")][1,3])
    this_wk = mytest[index,c("wk")]
    
    tmp.train.id = which(mytrain$dept==this_dept & mytrain$store==this_store & mytrain$holiday == this_holiday)
    if (sum(tmp.train.id)>0){
        tmp.train.sales = mytrain$sales[tmp.train.id]; 
    }
    else
    {
        tmp.train.sales = mytrain$sales[which(mytrain$dept==this_dept & mytrain$holiday == this_holiday)]
    }
    mypred[index] = median(tmp.train.sales, na.rm = TRUE)
    count = count + 1
    print(count)
    print("\n")
}
print(paste("Number of records updated in predictions: ", count,".",sep = ""))



############################################################
#Further Deal with NA values
##########################################################3

# use median of sales from other stores but same department to predict

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




filepath = "output/submission_naive.csv";
writeSubmission(mytest,mypred, filepath)
# rank 232: 3232.31792
