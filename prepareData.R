# install.packages("lubridate")

rm(list = ls())
setwd("D:/benzione_Drive/Research/Courses/BI/R")

library("data.table")
library("lubridate")

str <- c("D:/benzione_Drive/Research/Courses/BI/data/")
mydata <- as.matrix(fread("D:/benzione_Drive/Research/Courses/BI/data/P1_SHPR_PROD.csv"))
transctions <- data.frame(mydata)
products.number <- unique(transctions$CV_PRODUCT_ITEM_ID)

# for (i in 1:length(products.number)){
  # i<-1
#   i
#   dataperproduct <- subset(transctions,CV_PRODUCT_ITEM_ID == products.number[i])
  dataperproduct <- subset(transctions,CV_PRODUCT_ITEM_ID == 635478)
  N <- nrow(dataperproduct)
  first.date <- min(as.vector(dataperproduct$PURCHASE_DATE))
  dataperproduct$Week <- sapply(dataperproduct$PURCHASE_DATE,
                               function(x)(floor(difftime(x,first.date,units = "weeks"))+1))
  dataperproduct$Day <- sapply(dataperproduct$PURCHASE_DATE,
                                     function(x)wday(x))
  output.data <- data.frame(ID = dataperproduct$CV_SHOPPER_ID, Market = rep(1,N), 
                            Week = dataperproduct$Week, Day = dataperproduct$Day, 
                            Count = dataperproduct$PURCHASE_UNITS)
#   str.file <- paste(str,as.character(products.number[i]),".csv",sep = "")
  str.file <- paste(str,"635478.csv",sep = "")
  write.csv(output.data, file = str.file, row.names=TRUE)
  
}

