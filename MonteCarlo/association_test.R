
library(rafalib)
library(dplyr)

#
# Association test
#

setwd("C:/Users/David/Documents/Technology/R/edx stats and R")

d <= read.csv("assoctest.csv")

class(d)                      # d is a data frame
dim(d)                        # with dimension 72 x 2
names(d)                      #column names "allele" and "case"
summary(d)                    # both columns take values 0 or 1 or more sensibly either AA/Aa or aa and case/control


tab <- table(d)

chisq.test(tab)
fisher.test(tab)

