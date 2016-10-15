# install.packages("lubridate")
# install.packages("utils")
# install.packages("data.table")
# install.packages("stringi")

rm(list = ls())
setwd("C:/benzione_Drive/Research/Courses/BI/R")

library("data.table")
library("stringi")
library("lubridate")
library("utils")

str <- c("C:/benzione_Drive/Research/Courses/BI/data_mix/mix")
mydata <- as.matrix(fread("C:/benzione_Drive/Research/Courses/BI/data/P1_SHPR_PROD.csv"))
transctions <- data.frame(mydata)
products.number <- unique(transctions$CV_PRODUCT_ITEM_ID)

for (i in 1:length(products.number)){
  # i  <- 83
  dataperproduct <- subset(transctions,CV_PRODUCT_ITEM_ID == products.number[i])
  N <- nrow(dataperproduct)
  median.price <- median(as.numeric(as.character(dataperproduct$PURCHASE_SPENT)))
  first.date <- min(as.vector(dataperproduct$PURCHASE_DATE))
  dataperproduct$Week <- sapply(dataperproduct$PURCHASE_DATE,
                                function(x)(floor(difftime(x,first.date,units = "weeks"))+1))
  dataperproduct$Day <- sapply(dataperproduct$PURCHASE_DATE,
                               function(x)wday(x))
  if (median.price>0){
    dataperproduct$discount <- (as.numeric(as.character(dataperproduct$PURCHASE_SPENT))
                                /median.price)
  } else {
    dataperproduct$discount <- as.numeric(as.character(dataperproduct$PURCHASE_SPENT))
  }
  
  dataperproduct <- subset(dataperproduct,select = c("CV_SHOPPER_ID"
                                                     ,"Week","Day","discount"))
  
  dataperproduct <- dataperproduct[order(dataperproduct$CV_SHOPPER_ID, 
                                         dataperproduct$Week,dataperproduct$Day),]
   
  if (N>1){
    for (j in 2:N){
      if (dataperproduct[j,1]==dataperproduct[j-1,1]){
        if (dataperproduct[j,2]<=dataperproduct[j-1,2]){
          dataperproduct[j,2] <- dataperproduct[j-1,2]+1
        }
      }
    }
  }

  
  allwk <- max(dataperproduct$Week)
  maxID <- max(as.numeric(as.character(dataperproduct$CV_SHOPPER_ID)))+1
  
  fix.bug.df <- data.frame(CV_SHOPPER_ID=as.factor(rep(maxID,allwk)),Week=c(1:allwk),Day=rep(1,allwk)
                           ,discount=rep(0,allwk))
  dataperproduct <- rbind(dataperproduct,fix.bug.df)
  
  firstTh <- 0.6
  secondTh <- 1
  thirdTh <- 4
  output.data <- data.frame(Week = 1:(max(dataperproduct$Week)), 
                             Market = rep(1,max(dataperproduct$Week)))
  output.data$first <- by(dataperproduct, dataperproduct$Week, function(x) 
    if (sum(x$discount<firstTh)>0){1}else{0})
  output.data$second <- by(dataperproduct, dataperproduct$Week, function(x) 
    if (sum(x$discount>=firstTh & x$discount<secondTh)){
      sum(1)}else{0})
  output.data$third <- by(dataperproduct, dataperproduct$Week, function(x) 
    if (sum(x$discount>=secondTh & x$discount<thirdTh)){
      sum(1)}else{0})
  output.data$fourth <- by(dataperproduct, dataperproduct$Week, function(x) 
    if (sum(x$discount>=thirdTh)){1}else{0})
  str.file <- paste(str,as.character(products.number[i]),".csv",sep = "")
  write.csv(output.data, file = str.file, row.names=FALSE)
}

