# Separate->Downsample
library(readr);library(plyr);library(tidyverse);library(openxlsx);library(tidyr);library(immunarch)
path<-'E:/Data/TCR-seq/Project1/PT081/Separate'
fileNames <- dir(path) 
filePath <- sapply(fileNames, function(x){ 
    paste(path,x,sep='/')})  
df <- lapply(filePath, function(x){
    read.table(x,na.strings = c("NA"),header = T,sep=",")})
df <- repSample(df, "sample")
for (i in 1:length(fileNames)){
    names(df)[i]<-substring(names(df[i]),1,6)
    write_csv(df[[i]], file = paste('E:/Data/TCR-seq/Project1/PT081/Downsample/',names(df[i]),".csv",sep=""),na = "")
}
