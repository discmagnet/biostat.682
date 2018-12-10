# HOMEWORK 4

# Problem 1 ---------------------------------------------------------------

# Download the "swim_time" dataset from Canvas. The data file contains
# a data matrix Y on the amount of time, in seconds, it takes each of
# four high school swimmers to swim 50 yards. Each swimmer has six times,
# taken on a biweekly basis.
load("~/WORKING_DIRECTORIES/biostat.682/swim_time.RData")
library(R2jags)
# (a) For each swimmer j (j = 1,2,3,4), fit a Bayesian linear regression
#     model which considers the swimming time as the response variable
#     and week as the explanatory variable. To formulate your prior, use
#     the information that competitive times for this age group generally
#     range from 22 to 24 seconds.

meet <- c(1:7)

JAGS_RE_model = function() {
  # Likelihood
  for (j in 1:N) {
    for (w in 1:6) {
      Y[j, w]    ~ dnorm(meanY[j, w], taue)
      meanY[j, w] <- beta[j, 1] + beta[j, 2] * meet[w]
    }
  }
  
  # Random effects
  for (j in 1:N) {
    beta[j, 1:2] ~ dmnorm(mu[1:2], Omega[1:2, 1:2])
  }
  
  # Priors
  taue  ~ dgamma(0.001, 0.001)
  mu[1] ~ dnorm(23, 4) # prior for intercept using info provided
  mu[2] ~ dnorm(0, 0.001) # uninformative prior
  
  Omega[1:2, 1:2] ~ dwish(R[, ], 2.001)
  R[1, 1] <- 1 / 2.001
  R[1, 2] <- 0
  R[2, 1] <- 0
  R[2, 2] <- 1 / 2.001
  
  # Output the parameters of interest
  for (j in 1:N) {
    pred[j] <- beta[j, 1] + beta[j, 2] * meet[7]
  }
}

fit_JAGS_RE_model = jags(
  data = list(Y = Y, meet = meet, N = 4),
  inits = list(list(taue=1)),
  parameters.to.save = c("pred"),
  n.chains = 1,
  n.iter = 10000,
  n.burnin = 1000,
  model.file = JAGS_RE_model
)

post_mean <- summary(as.mcmc(fit_JAGS_RE_model))$statistics[paste("pred[",1:4,"]",sep=""),1]
Yp        <- Y
Yp[,7]    <- post_mean

par(mfcol=c(2,2))

matplot(t(Y),type="l",xlab="Meet",ylab="Swimming Time",main="Data")
matplot(t(Y),add=TRUE,pch=19)

matplot(t(Yp),type="l",xlab="Meet",ylab="Swimming Time",main="Data with predicted value for meet 7")
matplot(t(Yp[,1:6]),add=TRUE,pch=19)
points(rep(7,4),post_mean,col=1:4)

# (b) For each swimmer j (j = 1,2,3,4), obtain a posterior predictive
#     distribution for Yj*, their time if they were to swim two weeks
#     from the last recorded time.

# (c) The coach of the team has to decide which of the four swimmers will
#     compete in a swimming meet in two weeks. Using your predictive
#     distributions, compute Pr(Yj* = min{Y1*,Y2*,Y3*,Y4*} | Y) for each
#     swimmer j, and based on this make a recommendation to the coach.
swim01 <- rnorm(1000000, mean = 22.614, sd = 0.089)
swim02 <- rnorm(1000000, mean = 23.578, sd = 0.087)
swim03 <- rnorm(1000000, mean = 22.898, sd = 0.100)
swim04 <- rnorm(1000000, mean = 23.378, sd = 0.089)
best <- vector()
best01 <- vector()
best02 <- vector()
best03 <- vector()
best04 <- vector()
for(i in 1:1000000){
  best[i] <- min(swim01[i],swim02[i],swim03[i],swim04[i])
  best01[i] <- 1*(swim01[i] == best[i])
  best02[i] <- 1*(swim02[i] == best[i])
  best03[i] <- 1*(swim03[i] == best[i])
  best04[i] <- 1*(swim04[i] == best[i])
}
prob01 <- mean(best01)
prob02 <- mean(best02)
prob03 <- mean(best03)
prob04 <- mean(best04)

# Problem 2 ---------------------------------------------------------------

# Consider the dataset UScrime which contains crime rates (y) and data
# on 15 explanatory variables for 47 US states. A description of the
# variables can be obtained by typing "?UScrime" in the R console.
library(MASS)

# (a) Fit a Bayesian linear regression model using uninformative priors.
#     Obtain marginal posterior means and 95% credible intervals for
#     coefficients. Describe the relationships between crime and the
#     explanatory variables. Which variables seem strongly predictive
#     of crime rates?
JAGS_BLR_flat = function(){
  # Likelihood
  for(i in 1:47){
    Y[i] ~ dnorm(mu[i],inv_sigma2)
    mu[i] <- beta_0 + inprod(UScrime[i,1:15],beta) 
    # same as beta_0 + X[i,1]*beta[1] + ... + X[i,p]*beta[p]
  }
  # Prior for beta
  for(j in 1:15){
    beta[j] ~ dnorm(0,0.000001)
    #non-informative priors 
  }
  # Prior for intercept
  beta_0 ~ dnorm(0, 0.000001)
  
  # Prior for the inverse variance
  inv_sigma2 ~ dgamma(0.0001, 0.0001)
  sigma2 <- 1.0/inv_sigma2
}

fit_JAGS_flat = jags(data=list(Y = UScrime$y, UScrime = UScrime),
                     inits=list(list(beta = rnorm(15),
                                     beta_0 = 0,
                                     inv_sigma2 = 1)),
                     parameters.to.save = c("beta_0","beta","sigma2"),
                     n.chains=1,
                     n.iter=10000,
                     n.burnin=1000,
                     model.file=JAGS_BLR_flat)

print(fit_JAGS_flat)
fit_flat =as.mcmc(fit_JAGS_flat)

# (b) To test how well regression models can predict crime rates based
#     on the explanatory variables, randomly divide the data roughly
#     in half, into a training set and a test set. Use the training
#     dataset to fit the model and generate the posterior predictive
#     median of the crime rates given the explanatory variables in the
#     test dataset. Compare the posterior predictive median and the
#     actual crime rate in the test dataset.


# (c) Repeat (a) and (b) using spike-and-slab priors for regression
#     coefficients.
JAGS_BLR_SpikeSlab = function(){
  # Likelihood
  for(i in 1:47){
    Y[i] ~ dnorm(mu[i],inv_sigma2)
    mu[i] <- beta_0 + inprod(UScrime[i,1:15],beta) 
  }
  # Prior for beta
  for(j in 1:15){
    beta[j] ~ dnorm(0,inv_tau2[j])
    inv_tau2[j] <- (1-gamma[j])*1000+gamma[j]*0.01
    gamma[j] ~ dbern(0.5)
  }
  # Prior for intercept
  beta_0 ~ dnorm(0, 0.0001)
  
  # Prior for the inverse variance
  inv_sigma2 ~ dgamma(0.0001, 0.0001)
  sigma2 <- 1.0/inv_sigma2
  tau2 <- 1.0/inv_tau2
}

fit_JAGS_SpikeSlab = jags(list(Y = UScrime$y, UScrime = UScrime),
                          inits=list(list(beta = rnorm(15),
                                          beta_0 = 0,
                                          inv_sigma2 = 1,
                                          gamma = rep(1,length=15))),
                          parameters.to.save = c("beta","gamma"),
                          n.chains=1,
                          n.iter=10000,
                          n.burnin=1000,
                          model.file=JAGS_BLR_SpikeSlab)

print(fit_JAGS_SpikeSlab)
fit_SpikeSlab =as.mcmc(fit_JAGS_SpikeSlab)

# Problem 3 ---------------------------------------------------------------

# Consider the dataset gambia which consists of 2,035 children from
# 65 villages from The Gambia. It contains eight different variables.
# A description of the variables can be obtained by typing "?gambia"
# in the R console. Let Yi = {0,1} (pos) indicate the presence (1) or
# absence (0) of malaria in a blood sample taken from the child i
# (i = 1,...,2035). Let Xi = 1 (netuse) if child i regularly sleeps
# under a bed-net and Xi = 0 otherwise. Let Vi = {1,...,65} denote
# the village of child i. Note that the dataset only contains the
# locations of villages instead of the labels. You can use the
# following R code to obtain Vi.
library(geoR)
v_loc <- unique(gambia[,"x"])
v <- match(gambia[,"x"], v_loc)
# Fit the following logistic regression model:
# logit{Pr(Yi = 1)} = alpha_Vi + Xi*beta_Vi,
# where alpha_j and beta_j are intercept and slope for village j
# (j = 1,...,65). The priors are:
# alpha_j ~ N(mu_a,var_a)
# beta_j ~ N(mu_b,var_b)
# Choose uninformative priors for the hyperparameters mu_a, mu_b,
# var_a, and var_b. Based on your model fitting, address the
# following questions:

# (a) Scientifically, why might the effect of bed-net vary by village?

# (b) Do you see evidence that the slopes and/or intercepts vary by
#     village? You may consider alternative model fitting and perform
#     model comparisons.

# (c) Which village has the largest intercept? Slope? Does this agree
#     with the data in these villages?

# (d)) Are the results sensitive to the priors for the hyperparameters?
