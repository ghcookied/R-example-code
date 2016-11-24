library(dplyr)
library(rafalib)

#
# Statistical power is defined as one minus the Type II error rate, or the probability that the null hypothesis
# will be rejected when the alternative hypothesis is true.
#
# That is the probability of being correct
#
# This script calculates the statistical power using a range of significance levels (alpha) and sample sizes for
# a t test on two populations with known significantly different means - baby weights for smokers and non smokers
#

babies <- read.table("babies.txt", header=TRUE)

bwt_nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt_smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

sample_sizes  <- c(5,30,60,90,120)
sig_levels <- c(0.05,0.01)

set.seed(1)

reject <- function(N,alpha,pop1,pop2) {
  samp1 <- sample(pop1,N)
  samp2  <- sample(pop2,N)
  stat <- t.test(samp1, samp2)$p.value
  return(stat < alpha)
}


for (sig_level in sig_levels) {
  for (sample_size in sample_sizes) {
    power <- mean(replicate(10000, reject(sample_size,sig_level,bwt_nonsmoke,bwt_smoke)))
    cat("significance level ",sig_level," sample_size ", sample_size, " power ", power,"\n")
  }
}



