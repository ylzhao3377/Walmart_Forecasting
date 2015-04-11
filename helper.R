# helper functions needed for this project




# to deal with NAs
# returns: filtered data frame
deptInTrain<- function(store, dept, wk, mytrain){
    # if both dept, store in train
    dept_store_both = mytrain[which(mytrain$dept==dept & mytrain$store==store),]
    if (dim(dept_store_both)[1]>0){
        return (dept_store_both)
    }
    else {
        # use median of all same department across stores
        return (mytrain[which(mytrain$dept == dept), ])
    }
}


##################################################################
# Prepare the submission file
##################################################################

# For each row in the test set (store + department + date triplet), 
# you should predict the weekly sales of that department. The Id column 
# is formed by concatenating the Store, Dept, and Date with underscores 
# (e.g. Store_Dept_2012-11-02).  The file should have a header and looks 
# like the following:
#
# Id,Weekly_Sales
# 1_1_2012-11-02,0
# 1_1_2012-11-09,0
# 1_1_2012-11-16,0

writeSubmission <- function(mytest,mypred, output_path){
    ID=apply(mytest[,c("store", "dept", "date")], 1, function(x)  
        paste(x[1],  x[2],  as.character(x[3]), sep='_'))
    #ID[1:5] 
    # need to trim the space in ID
    ID = gsub("\\s","", ID)
    myout = data.frame(ID=ID, Weekly_Sales = mypred)
    write.csv(myout, file=output_path,  row.names=FALSE, quote = FALSE);
}