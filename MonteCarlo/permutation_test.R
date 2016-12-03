
library(rafalib)
library(dplyr)

#
# Permutation test
#
# An oberved sample mean from two sets of results (smokers/nonsmokers) is directly 
# compared with 10,000 permutations of the combined results.
#

setwd("C:/Users/David/Documents/Technology/R/edx stats and R")

babies <- read.table("babies.txt", header=TRUE)

# dplyr functionality :

bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

N <- 10
set.seed(1)
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obs <- mean(smokers) - mean(nonsmokers)
obs

# Want to check this observed difference for statistical significance without relying on assumptions about
# distributions so going to use a non-paramtric test - permutations

set.seed(1)
dat <- c(smokers,nonsmokers)              # dat is a vector containing both smoker and non smoker data
replications <- 1000

null_dist <- replicate(replications,{
  shuffle <- sample( dat )                  # shuffled randomly
  smokersstar <- shuffle[1:N]               # first N results
  nonsmokersstar <- shuffle[(N+1):(2*N)]    # second N results
  mean(smokersstar)-mean(nonsmokersstar)    # new observation from combined data
})

hist(null_dist)

( sum( abs(null_dist) >= abs(obs)) + 1 ) / ( length(null_dist)+1 ) 
##we add the 1s to avoid p-values=0 but we also accept:
( sum( abs(null_dist) >= abs(obs)) ) / ( length(null_dist) )
