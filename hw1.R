# Homework 1

# Problem 2b
x <- c(0:10)/10
y <- choose(100,66)*x^66*(1-x)^34
plot(x,y,xlab = "Theta",ylab = "Probability")

# Problem 2c
y2 <- (y/11)/mean(y)
plot(x,y2,xlab = "Theta",ylab = "Posterior Probability")

# Problem 2d
curve(choose(100,66)*x^66*(1-x)^34,from = 0,to = 1,
      xlab = "Theta",ylab = "Posterior Probability")

# Problem 3a
curve(gamma(10)/(gamma(2)*gamma(8))*x*(1-x)^7,
      from = 0, to = 1, xlab = "Theta", ylab = "Prior Distribution")
curve(choose(43,15)*x^15*(1-x)^28,
      from = 0, to = 1, xlab = "Theta", ylab = "Sampling Distribution")
curve(x^16*(1-x)^35,
      from = 0, to = 1, xlab = "Theta", ylab = "Posterior Distribution")
qbeta(0.025,17,36)
qbeta(0.975,17,36)

# Problem 3b
curve(gamma(10)/(gamma(8)*gamma(2))*x^7*(1-x),
      from = 0, to = 1, xlab = "Theta", ylab = "Prior Distribution")
curve(choose(43,15)*x^15*(1-x)^28,
      from = 0, to = 1, xlab = "Theta", ylab = "Sampling Distribution")
curve(x^22*(1-x)^29,
      from = 0, to = 1, xlab = "Theta", ylab = "Posterior Distribution")
qbeta(0.025,23,30)
qbeta(0.975,23,30)

# Problem 3c
curve(gamma(10)/(4*gamma(2)*gamma(8))*(3*x*(1-x)^7 + x^7*(1-x)),
      from = 0, to = 1, xlab = "Theta", ylab = "75-25% Mixture Prior Distribution")

# Problem 3d
curve(choose(43,15)*gamma(10)/(4*gamma(2)*gamma(8))*(3*x^16*(1-x)^35 + x^22*(1-x)^29),
      from = 0, to = 1, xlab = "Theta", ylab = "75-25% Mixture Posterior Distribution")
a <- c(0:1000)/1000
b <- choose(43,15)*gamma(10)/(4*gamma(2)*gamma(8))*(3*a^16*(1-a)^35 + a^22*(1-a)^29)
mode <- a[b == max(b)]