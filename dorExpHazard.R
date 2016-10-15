# install.packages('reshape2')
rm(list = ls())
require(reshape2)
require(stats)
library(hydroGOF)


# p_0 <- 0.0862
# theta_T <- 0.06428
# p_tmp <- 0.36346
# theta_tmp <- 0.46140

setwd("C:/benzione_Drive/Research/Courses/BI/R")
N <- 6360
endwk <- 24

mydata <- read.csv("C:/benzione_Drive/Research/Courses/BI/data/635478.csv", header = TRUE, row.names=1)
market2data <- subset(mydata,Market == 1)
data.sort <- market2data[order(market2data$ID, market2data$Week, market2data$Day),]

mydata.discount <- read.csv("C:/benzione_Drive/Research/Courses/BI/data_mix/mix635478.csv", header = TRUE)
allwk <- max(mydata.discount$Week)
market2data.discount <- subset(mydata.discount,Market == 1)
x <- data.frame(beta0 = rep(1,endwk),first = mydata.discount$first[1:endwk],
           second = mydata.discount$second[1:endwk],
           third = mydata.discount$third[1:endwk],
            fourth = mydata.discount$fourth[1:endwk])

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

data.for.logit<-x[,2:5]
data.for.logit$y <- as.numeric(length.pivot[1:endwk,3]>0)
logitA <- glm(y~.,data = data.for.logit, family = binomial())
beta <- coef(logitA)

for (i in c(seq(from = 0.01, to = 0.099, by = 0.001),seq(from = 0.1, to = 1, by = 0.01))){
  for (j in c(seq(from = 0.01, to = 0.099, by = 0.001),seq(from = 0.1, to = 0.99, by = 0.01),seq(from = 1, to = 30, by = 1))){
    p_tmp <- i
    theta_tmp <- j
#     predict.tmp <- N*p_tmp*(1-exp(-theta_tmp*cumsum(exp(crossprod(beta,t(x))))))
    prob.logit <- cumsum(1/(1+exp(-crossprod(beta,t(x[l:endwk,])))))
    predict.tmp <- N*p_tmp*(1-exp(-theta_tmp*prob.logit))
    observed.tmp <- data.frame(obs = length.cumulative[1:endwk,2])
    
    #     predict.tmp <- data.frame(sim = (N*p_tmp*(1-exp(-theta_tmp*prob.logit))
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
  average.product[l] <- (sum.cumulative[endwk,(l+1)]/length.cumulative[endwk,(l+1)])
  if (!is.nan(average.product[l])){
    eligible <- as.vector(c(predict[1,(l-1)],diff(predict[,(l-1)])))
    data.for.logit$y <- as.numeric(length.pivot[1:endwk,l+2]>0)
    logitA <- glm(y~.,data=data.for.logit[l:endwk,], family = binomial())
    beta <- coef(logitA)
    
    for (i in c(seq(from = 0.01, to = 0.099, by = 0.001),seq(from = 0.1, to = 1, by = 0.01))){
      for (j in c(seq(from = 0.01, to = 0.099, by = 0.001),seq(from = 0.1, to = 0.99, by = 0.01),seq(from = 1, to = 30, by = 1))){
        p_tmp <- i
        theta_tmp <- j
        prob.logit <- cumsum(c(rep(1,(l-1)),1/(1+exp(-crossprod(beta,t(x[l:endwk,]))))))
        prob = data.frame(prob=p_tmp*(1-exp(-theta_tmp*prob.logit)))
        probmat <- matrix(data=0,nrow=endwk,ncol=endwk)
        for (k in l:(endwk-1)){
          probmat[,k] = c(rep(0,(k-1)),prob$prob[1:(endwk-(k-1))])
        }
        
        predict.tmp <- colSums(t(probmat)*eligible)
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
# results <- data.frame( P = p, Theta = theta, SSE_proudct = sse.product.min, SSE_buyers = sse.buyers.min, logit=logit)
# predict <- data.frame(predict)
# write.csv(results,'expResultsHazard.csv')
# write.csv(predict,'expPredictHazard.csv')

for (i in c(seq(from = 0.01, to = 0.099, by = 0.001),seq(from = 0.1, to = 1, by = 0.01))){
  for (j in c(seq(from = 0.01, to = 0.099, by = 0.001),seq(from = 0.1, to = 0.99, by = 0.01),seq(from = 1, to = 30, by = 1))){
    p_tmp <- i
    theta_tmp <- j
    #     predict.tmp <- N*p_tmp*(1-exp(-theta_tmp*cumsum(exp(crossprod(beta,t(x))))))
    prob.logit <- cumsum(1/(1+exp(-crossprod(beta,t(x)))))
    predict.tmp <- N*p_tmp*(1-exp(-theta_tmp*prob.logit))
    observed.tmp <- data.frame(obs = length.cumulative[1:endwk,2])
    
    #     predict.tmp <- data.frame(sim = (N*p_tmp*(1-exp(-theta_tmp*prob.logit))
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
  average.product[l] <- (sum.cumulative[endwk,(l+1)]/length.cumulative[endwk,(l+1)])
  if (!is.nan(average.product[l])){
    eligible <- as.vector(c(predict[1,(l-1)],diff(predict[,(l-1)])))
    data.for.logit$y <- as.numeric(length.pivot[1:endwk,l+2]>0)
    logitA <- glm(y~.,data=data.for.logit[l:endwk,], family = binomial())
    beta <- coef(logitA)
    
    for (i in c(seq(from = 0.01, to = 0.099, by = 0.001),seq(from = 0.1, to = 1, by = 0.01))){
      for (j in c(seq(from = 0.01, to = 0.099, by = 0.001),seq(from = 0.1, to = 0.99, by = 0.01),seq(from = 1, to = 30, by = 1))){
        p_tmp <- i
        theta_tmp <- j
        prob.logit <- cumsum(c(rep(1,(l-1)),1/(1+exp(-crossprod(beta,t(x[l:endwk,]))))))
        prob = data.frame(prob=p_tmp*(1-exp(-theta_tmp*prob.logit)))
        probmat <- matrix(data=0,nrow=endwk,ncol=endwk)
        for (k in l:(endwk-1)){
          probmat[,k] = c(rep(0,(k-1)),prob$prob[1:(endwk-(k-1))])
        }
        
        predict.tmp <- colSums(t(probmat)*eligible)
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
