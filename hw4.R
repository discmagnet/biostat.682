# HOMEWORK 4

# Problem 1 ---------------------------------------------------------------

# Download the "swim_time" dataset from Canvas. The data file contains
# a data matrix Y on the amount of time, in seconds, it takes each of
# four high school swimmers to swim 50 yards. Each swimmer has six times,
# taken on a biweekly basis.
load("~/WORKING_DIRECTORIES/biostat.682/swim_time.RData")

# (a) For each swimmer j (j = 1,2,3,4), fit a Bayesian linear regression
#     model which considers the swimming time as the response variable
#     and week as the explanatory variable. To formulate your prior, use
#     the information that competitive times for this age group generally
#     range from 22 to 24 seconds.

# (b) For each swimmer j (j = 1,2,3,4), obtain a posterior predictive
#     distribution for Yj*, their time if they were to swim two weeks
#     from the last recorded time.

# (c) The coach of the team has to decide which of the four swimmers will
#     compete in a swimming meet in two weeks. Using your predictive
#     distributions, compute Pr(Yj* = min{Y1*,Y2*,Y3*,Y4*} | Y) for each
#     swimmer j, and based on this make a recommendation to the coach.

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

# (b) To test how well regression models can predict crime rates based
#     on the explanatory variables, randomly divide the data roughly
#     in half, into a training set and a test set. Use the training
#     dataset to fit the model and generate the posterior predictive
#     median of the crime rates given the explanatory variables in the
#     test dataset. Compare the posterior predictive median and the
#     actual crime rate in the test dataset.

# (c) Repeat (a) and (b) usind spike-and-slab priors for regression
#     coefficients.

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
