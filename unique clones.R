# Separate->Top10000 unique clones
library(readr);library(plyr);library(tidyverse);library(openxlsx);library(tidyr)
path<-'E:/Data/TCR-seq/Project1/PT081/Separate'
fileNames <- dir(path) 
filePath <- sapply(fileNames, function(x){ 
    paste(path,x,sep='/')})  
df <- lapply(filePath, function(x){
    read.table(x,na.strings = c("NA"),header = T,sep=",")})
for (i in 1:length(df)){
    temp<-df[[i]]
    temp<-unite(temp,"CloneType",c('CDR3.aa','V.name','J.name'), sep="_", remove = T)
    temp<-temp[,c(1,4,5)]
    temp<-aggregate(.~CloneType,data=temp,sum)
    temp = temp[order(-temp[,2]),]
    temp<-temp[1:10000,]
    temp$Proportion<-prop.table(temp$Clones)
    temp<-separate(data = temp, col = CloneType, into = c('CDR3.aa','V.name','J.name'), sep = "_")
    df[[i]] = temp}
for (i in 1:length(fileNames)){
    names(df)[i]<-substring(names(df[i]),1,6)
    write.table(df[[i]], file = paste('E:/Data/TCR-seq/Project1/PT081/unique/',names(df[i]),'.txt',sep=''), sep ="	", row.names =FALSE, col.names =T, quote =FALSE)}
