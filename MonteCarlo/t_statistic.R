library(downloader)
library(dplyr)
library(rafalib)

#
# Pure Monte Carlo experiment to demonstrate the relationship between:
#
# a set of 1000 (=B) calculated t-statistics based on samples randomly chosen from a normal distribution (ts)
#        and
# a set of 1000 t-statistics taken from the quantile function of the t-distribution (probablilites in ps)
#
# Charts are produced for 6 sample sizes between 5 and 30, which show that the approximation is good for
# all sample sizes.

set.seed(1)

Ns<-seq(5,30,5)
B <- 1000
mypar(3,2)
LIM <- c(-4.5,4.5)
for(N in Ns){
  ts <- replicate(B, {
    X <- rnorm(N)
    sqrt(N)*mean(X)/sd(X)
  })
  ps <- seq(1/(B+1),1-1/(B+1),len=B)
  qqplot(qt(ps,df=N-1),ts,main=N,
         xlab="Theoretical",ylab="Observed",
         xlim=LIM, ylim=LIM)
  abline(0,1)
}
