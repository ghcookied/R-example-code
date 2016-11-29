library(rafalib)

#
# Pure Monte Carlo experiment to demonstrate the relationship between:
#
# a set of 10000 medians of samples, of various sizes,  taken from a normal distribution
#        and
# normal distribution 
#
# note that in all cases sd of medians is greater than 1/sqrt(N)

set.seed(1)
Ns <- seq(5,45,5)
mypar(3,3)

for(N in Ns){
  medians <- replicate(10000, median ( rnorm(N) ) )
  title <- paste("N=",N,", avg=",round( mean(medians), 2) , ", sd*sqrt(N)=", round( sd(medians)*sqrt(N),2) )
  qqnorm(medians, main = title )
  qqline(medians)
}


