# install.packages('reshape2')
# install.packages("hydroGOF")
rm(list = ls())
require(reshape2)
require(stats)
library(hydroGOF)


# p_0 <- 0.0862
# theta_T <- 0.06428
# p_tmp <- 0.36346
# theta_tmp <- 0.46140

setwd("C:/benzione_Drive/Research/Courses/BI/R")

mydata <- read.csv("C:/benzione_Drive/Research/Courses/BI/data/635478.csv", header = TRUE, row.names=1)
market2data <- subset(mydata,Market == 1)
data.sort <- market2data[order(market2data$ID, market2data$Week, market2data$Day),]

N <- 6360
endwk <- 24
allwk <- max(market2data$Week)

maxID <- max(data.sort$ID)+1
fix.bug.df <- data.frame(ID=rep(maxID,allwk),Market=rep(2,allwk),Week=c(1:allwk),Day=rep(1,allwk)
                       ,Count=rep(0,allwk),DoR=rep(-1,allwk))
n <- nrow(data.sort)
DoR <- vector(mode="integer", length=n)
for (i in 2:n){
  if (data.sort[i,1]==data.sort[i-1,1]){
    if (data.sort[i,3]<=data.sort[i-1,3]){
      data.sort[i,3]=data.sort[i-1,3]+1
    }
    DoR[i]=DoR[i-1]+1
  }
}
data.sort$DoR <- DoR
allwk <- max(data.sort[,3])
data.sort <- rbind(data.sort,fix.bug.df)

sum.pivot <- dcast(data.sort, Week ~ DoR, value.var = c("Count"), fun.aggregate = sum)
length.pivot <- dcast(data.sort, Week ~ DoR, value.var = c("ID"), fun.aggregate = length)

length.cumulative <- data.frame(sapply(length.pivot,function(x)cumsum(x)))
length.cumulative$Week <-c(1:allwk)
length.cumulative$X.1 <- NULL

sum.cumulative <- data.frame(sapply(sum.pivot,function(x)cumsum(x)))
sum.cumulative$Week <-c(1:allwk)
sum.cumulative$X.1 <- NULL

num.repeat <- ncol(length.cumulative)-1

p <- rep(0,num.repeat)
theta <- rep(0,num.repeat)
predict <- matrix(0,endwk,num.repeat)
rmse.min <- rep(99999999,num.repeat)
average.product <- rep(0,num.repeat)
average.product[1] <- (sum.cumulative[endwk,2]/length.cumulative[endwk,2])

for (i in c(seq(from = 0.01, to = 0.099, by = 0.001),seq(from = 0.1, to = 1, by = 0.01))){
  for (j in c(seq(from = 0.01, to = 0.099, by = 0.001),seq(from = 0.1, to = 0.99, by = 0.01),seq(from = 1, to = 30, by = 1))){
    p_tmp <- i
    theta_tmp <- j
    predict.tmp <- data.frame(sim = N*p_tmp*(1-exp(-theta_tmp*c(1:endwk))))
    observed.tmp <- data.frame(obs = length.cumulative[1:endwk,2])
    
#     predict.tmp <- data.frame(sim = (N*p_tmp*(1-exp(-theta_tmp*c(1:endwk)))
#                                      *average.product[l]))
#     observed.tmp <- data.frame(obs = sum.cumulative[1:endwk,(l+1)])
    
    rmse.current <- rmse(predict.tmp, observed.tmp)
    if (rmse.min[1] > rmse.current){
      rmse.min[1] <- rmse.current
      p[1] <- i
      theta[1] <- j
      predict[,1] <- predict.tmp$sim
    }
  }
}


for (l in 2:num.repeat){
  # l <- 2
  average.product[l] <- (sum.cumulative[endwk,(l+1)]/length.cumulative[endwk,(l+1)])
  if (!is.nan(average.product[l])){
    eligible <- as.vector(c(predict[1,(l-1)],diff(predict[,(l-1)])))
    
    for (i in c(seq(from = 0.01, to = 0.099, by = 0.001),seq(from = 0.1, to = 1, by = 0.01))){
      for (j in c(seq(from = 0.01, to = 0.099, by = 0.001),seq(from = 0.1, to = 0.99, by = 0.01),seq(from = 1, to = 30, by = 1))){
#         i <- 0.2
#         j <- 0.05
        
        p_tmp <- i
        theta_tmp <- j
        
        prob = data.frame(prob=p_tmp*(1-exp(-theta_tmp*c(1:endwk))))
        probmat <- matrix(data=0,nrow=endwk,ncol=endwk)
        for (k in l:(endwk-1)){
          probmat[,k] = c(rep(0,(k-1)),prob$prob[1:(endwk-(k-1))])
        }
        
        predict.tmp <- data.frame(sim = colSums(t(probmat)*eligible))
        observed.tmp <- data.frame(obs = length.cumulative[1:endwk,(l+1)])
        
#         predict.tmp <- data.frame(sim = (colSums(t(probmat)*eligible)
#                                          *average.product[l]))
#         observed.tmp <- data.frame(obs = sum.cumulative[1:endwk,(l+1)])
        
        rmse.current <- rmse(predict.tmp, observed.tmp)
        if (rmse.min[l] > rmse.current){
          rmse.min[l] <- rmse.current
          p[l] <- i
          theta[l] <- j
          predict[,l] <- predict.tmp$sim
        }
      }
    }
  }
  else{
    rmse.min[l] <- 0
  }
}
# results <- data.frame( P = p, Theta = theta, SSE_proudct = rmse.min, SSE_buyers = sse.buyers.min)
# predict <- data.frame(predict)

# write.csv(results,'expResults635478.csv')
# write.csv(predict,'expPredict635478.csv')

average.product[1] <- (sum.cumulative[endwk,2]/
                         length.cumulative[endwk,2])
predict.result <- matrix(0,(allwk-endwk),num.repeat)
rmse.buyers <- rep(0,num.repeat)
rmse.products <- rep(0,num.repeat)

p_tmp <- p[1]
theta_tmp <- theta[1]

predict.tmp <- data.frame(sim = N*p_tmp*(1-exp(-theta_tmp*c((endwk+1):allwk))))
length.observed.tmp <- data.frame(obs = length.cumulative[(endwk+1):allwk,2])
rmse.buyers[1] <- rmse(predict.tmp, length.observed.tmp)

predict.product.tmp <- data.frame(sim = (predict.tmp$sim*average.product[1]))
sum.observed.tmp <- data.frame(obs = sum.cumulative[(endwk+1):allwk,2])
rmse.products[1] <- rmse(predict.product.tmp, sum.observed.tmp)

predict.result[,1] <- predict.tmp$sim


for (l in 2:num.repeat){
  # l <- 2
  average.product[l] <- (sum.cumulative[endwk,(l+1)]/
                           length.cumulative[endwk,(l+1)])
  if (!is.nan(average.product[l])){
    eligible <- as.vector(c(predict.result[1,(l-1)],diff(predict.result[,(l-1)])))
    p_tmp <- p[l]
    theta_tmp <- theta[l]
    
    prob = data.frame(prob=p_tmp*(1-exp(-theta_tmp*c((endwk+1):allwk))))
    probmat <- matrix(data=0,nrow=(allwk-endwk),ncol=(allwk-endwk))
    for (k in 1:(allwk-endwk)){
      probmat[,k] = c(rep(0,(k-1)),prob$prob[1:(allwk-endwk-(k-1))])
    }
    
    predict.tmp <- data.frame(sim = colSums(t(probmat)*eligible))
    length.observed.tmp <- data.frame(obs = length.cumulative[(endwk+1):allwk,l+1])
    rmse.buyers[l] <- rmse(predict.tmp, length.observed.tmp)
    
    predict.product.tmp <- data.frame(sim = (predict.tmp$sim*average.product[l]))
    sum.observed.tmp <- data.frame(obs = sum.cumulative[(endwk+1):allwk,(l+1)])
    rmse.products[l] <- rmse(predict.product.tmp, sum.observed.tmp)
    
    predict.result[,l] <- predict.tmp$sim
  }
}

write.csv(rmse.products,'exp2RMSEProducts635478.csv')
write.csv(rmse.buyers,'exp2RMSEBuyers635478.csv')


