run_analysis <-function() {
    
    add_feilds<-c("label","subject")
    add_feilds<-matrix(add_feilds)
    
    
    
    ##Data Extraction
    test_set<-read.table("./UCI HAR Dataset/test/X_test.txt")
    test_label<-read.table("./UCI HAR Dataset/test/y_test.txt")
    test_subject<-read.table("./UCI HAR Dataset/test/subject_test.txt")
    
    
    train_set<-read.table("./UCI HAR Dataset/train/X_train.txt")
    train_label<-read.table("./UCI HAR Dataset/train/y_train.txt")
    train_subject<-read.table("./UCI HAR Dataset/train/subject_train.txt")#     ,nrows=10)
    
    
    activity_label<-read.table("./UCI HAR Dataset/activity_labels.txt")
    features<-read.table("./UCI HAR Dataset/features.txt")
    
    
    #Data Integration
    
    #Getting label and subjects data for test
    test_set<-cbind(test_set,test_label)
    test_set<-cbind(test_set,test_subject)
    
    #Getting label and subjects data for train
    train_set<-cbind(train_set,train_label)
    train_set<-cbind(train_set,train_subject)
    
    
    #Renaming Columns by proper variable names
    features<-matrix(features[,2])
    features<-rbind(features,add_feilds)
    
    colnames(train_set)<-features
    colnames(test_set)<-features
    
    #Merging test and train data
    merge_set<-rbind(train_set,test_set)
    
    #Applying correct label Naming
    merge_set<-merge(merge_set,activity_label,by.x="label",by.y="V1")
    merge_set$label<-merge_set$V2
    
    #Restricting to only std and mean columns
    merge_colname<-colnames(merge_set)
    std_feilds<-grepl('std',merge_colname)
    mean_feilds<-grepl('mean',merge_colname)
    std_mean_feilds<-std_feilds|mean_feilds
    
    label<-merge_set$label
    subject<-merge_set$subject
    
    merge_set<-cbind(label,subject,merge_set[,std_mean_feilds])
    
    
    library(plyr)
    mean_merge<-ddply(merge_set,c("label","subject"),colwise(mean))
    write.table(mean_merge,file="mean_merge.txt",row.name=FALSE)
    write.table(mean_merge,file="mean_merge.csv",row.name=FALSE)
    write.table(mean_merge,file="mean_merge.pdf",row.name=FALSE)
    write.table(mean_merge,file="mean_merge.jpg",row.name=FALSE)
    write.table(mean_merge,file="mean_merge.gif",row.name=FALSE)
}