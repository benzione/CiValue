# install.packages('reshape2')
require(reshape2)
require(stats)


# p_0 <- 0.0862
# alpha_T <- 0.06428
# p_tmp <- 0.36346
# alpha_tmp <- 0.46140

setwd("D:/benzione_Drive/Research/Courses/BI/R")

N <- 1499
allwk <- 52
endwk <- 24

mydata <- read.table("kiwibubbles_tran.txt", header = TRUE)
market2data <- subset(mydata,Market == 2)
data.sort <- market2data[order(market2data$ID, market2data$Week, market2data$Day),]

mydata.discount <- read.table("kiwibubbles_mktmix.txt", header = TRUE)
market2data.discount <- subset(mydata.discount,Market == 2)
x <- data.frame(beta0=rep(1,endwk),Coupon_Stock=mydata.discount$Coupon_Stock[1:endwk],
                Advertising_Stock=mydata.discount$Advertising_Stock[1:endwk],
                ACV_Any_Promotion=mydata.discount$ACV_Any_Promotion[1:endwk])


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
length.cumulative$Week <-c(1:52)
length.cumulative$X.1 <- NULL

sum.cumulative <- data.frame(sapply(sum.pivot,function(x)cumsum(x)))
sum.cumulative$Week <-c(1:52)
sum.cumulative$X.1 <- NULL

num.repeat <- ncol(length.cumulative)-1

p <- rep(0,num.repeat)
r <- rep(0,num.repeat)
alpha <- rep(0,num.repeat)
logit <- rep(0,num.repeat)
predict <- matrix(0,endwk,num.repeat)
sse.product.min <- rep(99999999,num.repeat)
sse.buyers.min <- rep(99999999,num.repeat)
average.product <- rep(0,num.repeat)
average.product[1] <- (sum.cumulative[endwk,2]/length.cumulative[endwk,2])

data.for.logit<-x[,2:4]
data.for.logit$y <- as.numeric(length.pivot[1:endwk,3]>0)
logitA <- glm(y~.,data=data.for.logit, family = binomial())
beta <- coef(logitA)

for (i in c(seq(from = 0.01, to = 0.099, by = 0.001),seq(from = 0.1, to = 1, by = 0.01))){
  for (j in c(seq(from = 0.01, to = 0.09, by = 0.01),seq(from = 0.1, to = 0.9, by = 0.1),seq(from = 1, to = 100, by = 1))){
    for (m in c(seq(from = 0.01, to = 0.09, by = 0.01),seq(from = 0.1, to = 0.9, by = 0.1),seq(from = 1, to = 100, by = 1))){ 
      p_tmp <- i
      alpha_tmp <- j
      r_tmp <- m
#       predict.tmp <- N*p_tmp*(1-(alpha_tmp/(alpha_tmp+c(1:endwk)))^r_tmp)
      prob.logit <- cumsum(1/(1+exp(-crossprod(beta,t(x)))))
      predict.tmp <- N*p_tmp*(1-(alpha_tmp/(alpha_tmp+prob.logit))^r_tmp)
      sse.buyers.tmp<-sum((predict.tmp-length.cumulative[1:endwk,2])^2)
      sse.product.tmp<-sum((predict.tmp*average.product[1]-sum.cumulative[1:endwk,2])^2)
      if (sse.product.min[1]>sse.product.tmp){
        sse.product.min[1] <- sse.product.tmp
        sse.buyers.min[1] <- sse.buyers.tmp
        p[1] <- i
        alpha[1] <- j
        r[1] <- m
        predict[,1] <- predict.tmp
      }
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
      for (j in c(seq(from = 0.01, to = 0.09, by = 0.01),seq(from = 0.1, to = 0.9, by = 0.1),seq(from = 1, to = 100, by = 1))){
        for (m in c(seq(from = 0.01, to = 0.09, by = 0.01),seq(from = 0.1, to = 0.9, by = 0.1),seq(from = 1, to = 100, by = 1))){ 
          p_tmp <- i
          alpha_tmp <- j
          r_tmp <- m
          prob.logit <- cumsum(c(rep(1,(l-1)),1/(1+exp(-crossprod(beta,t(x[l:endwk,]))))))
          prob = data.frame(prob=p_tmp*(1-(alpha_tmp/(alpha_tmp+prob.logit))^r_tmp))
          probmat <- matrix(data=0,nrow=endwk,ncol=endwk)
          for (k in l:(endwk-1)){
            probmat[,k] = c(rep(0,(k-1)),prob$prob[1:(endwk-(k-1))])
          }
          predict.tmp <- colSums(t(probmat)*eligible)
          sse.buyers.tmp<-sum((predict.tmp-length.cumulative[1:endwk,(l+1)])^2)
          sse.product.tmp<-sum((predict.tmp*average.product[l]-sum.cumulative[1:endwk,(l+1)])^2)
          if (sse.product.min[l]>sse.product.tmp){
            sse.product.min[l] <- sse.product.tmp
            sse.buyers.min[l] <- sse.buyers.tmp
            p[l] <- i
            alpha[l] <- j
            r[l] <- m
            predict[,l] <- predict.tmp
          }
        }
      }
    }
  }
  else{
    sse.product.min[l] <- 0
    sse.buyers.min[l] <- 0
  }
}
results <- data.frame( P = p, Theta = theta, SSE_proudct = sse.product.min, SSE_buyers = sse.buyers.min, logit=logit)
predict <- data.frame(predict)
write.csv(results,'gammaResultsHazard.csv')
write.csv(predict,'gammaPredictHazard.csv')
