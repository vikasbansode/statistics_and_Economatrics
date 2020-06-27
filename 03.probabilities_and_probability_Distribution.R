# PROBABILITY DISTRIBUTION
# Binomial Distribution - X is Binomially Distributed with n = 20 trials and p = 1/6 prob of success.

# In R dbinom command is used to find values for the probability density function of x, f(x)
# suppose 80%  adults are smoker and out of 10 are caesarean what 
# is the probability that they are male exactly seven
# observations is n = 10
# success or events of male is x = 7
# p=0.8

dbinom(x=7,size = 10,prob = 0.8)

# Probability of having exactly 7 males is 20.13%

# Possion Distribution - is the probability distribution of independent occurances in an interval.

# support there are 12 adults smoking per minute on an average,
# find the probability of having seventeen or more adults smoking in a
# particular minutes

# probability of haveing sixteen or less adults smoking in a particular
# minute is given by the fuction ppois.

ppois(16,lambda = 12) # lower tail

# Here, the probability of having seventeen or more adults smoking in a # minute is in the upper tail of the probability density function

ppois(16,lambda = 12,lower = FALSE) # UPPER TAIL

# if there are twelve adults smoking per minute on an average, the probability of having seventeen ore more adults smoking in a particuler minute is 10.1%


# Normal Distribution
# the mean of Lung capacity is 7, and standard deviation is 2.66.
# What is the percentage of Lung capacity of female which has 9 or more lung cap.

pnorm(9,mean = 7,sd=2.66, lower.tail = FALSE)

# The percentage of female having Lung cap 9 or more is 22.6%


