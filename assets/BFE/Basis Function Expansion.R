# Basis Function expansion!

# Silverman's Motorcycle Data
# accel = head acceleration (g)
# times = time after impact (milliseconds)
library(MASS)
head(mcycle)

# plot data (notice the nonlinear effect)
plot(accel~times, data = mcycle)


# polynomial approximation
out1 <- lm(accel ~ times + I(times^2) + I(times^3) + I(times^4) + I(times^5), 
           data = mcycle, 
           x = TRUE)
head(out1$x)


# judge the fit from the plot

x.star <- seq(0,60,length=100)
yhat1 <- predict(out1,newdata=data.frame(times=x.star))
lines(x.star,yhat1,col="red")


# polynomial will never get 0,10 right. 
# There is a discontinuity in the derivative.


# judge the fit from the prediction performance!
# median absolute prediction error (deviation)**.
median(abs(predict(out1)))
# we are within 36.19


# order | MAD
#   5   | 36.92


# VIF
library(car)
vif(out1)
# There is a complete collinearity problem.
# "collinearity plots" **
plot(~ times + I(times^2) + I(times^3) + I(times^4) + I(times^5), 
     data = mcycle)
# maybe in theory, this is a fine way to go about it, but there is nothing special about it





# instead of computing the basis function expansion ourselves,
# use one that is orthonormal with poly()
out2 <- lm(accel ~ poly(times,5), data = mcycle, x=TRUE)
head(out2$x)
plot(~out2$x[,2] + out2$x[,3] + out2$x[,4] + out2$x[,5] + out2$x[,6])
# centering it around the mean and squaring: 
# We have quadratic looking function for first plot
# We have cubic centered around the mean.

# evaluate fit

# judge fit from plot
plot(accel~times, data = mcycle)
x.star <- seq(0,60,length=100)
yhat1 <- predict(out2,newdata=data.frame(times=x.star))
lines(x.star,yhat1,col="red")
median(abs(predict(out2)))


# Try to fit a model while continuing to add polynomials
# 6 
out3 <- lm(accel ~ poly(times,6), data = mcycle, x=TRUE)
plot(accel~times, data = mcycle)
x.star <- seq(0,60,length=100)
yhat1 <- predict(out3,newdata=data.frame(times=x.star))
lines(x.star,yhat1,col="red")
median(abs(predict(out3)))

# 7
out4 <- lm(accel ~ poly(times,7), data = mcycle, x=TRUE)
plot(accel~times, data = mcycle)
x.star <- seq(0,60,length=100)
yhat1 <- predict(out4,newdata=data.frame(times=x.star))
lines(x.star,yhat1,col="red")
median(abs(predict(out4)))

# 10
out5 <- lm(accel ~ poly(times,15), data = mcycle, x=TRUE)
plot(accel~times, data = mcycle)
x.star <- seq(0,60,length=100)
yhat1 <- predict(out5,newdata=data.frame(times=x.star))
lines(x.star,yhat1,col="red")
median(abs(predict(out5)))

# technical "unfitness" of the data is smooth
# technical too wiggly.
# we want just right.
med <- numeric(0)
for (i in 1:23) {
outs <- lm(accel ~ poly(times,i), data = mcycle, x = TRUE)
med[i] <- median(abs(predict(outs)))
}


# plot of MAD
cbind(seq(1:23),med)
plot(cbind(seq(1:23),med))


# GRACE WAHALA
  # She showed that the optimal leave one out prediction

# Let's not fit polynomials, but cubic splines.

# Fit the cubic splines
library(splines)

outs1 <- lm(accel ~ ns(times,5), data = mcycle, x = TRUE)
# ns means natural spline
# force the extrapolation to be linear beyond boundaries

plot(accel~times, data = mcycle)
x.star <- seq(0,60,length=100)
yhat1 <- predict(outs1,newdata=data.frame(times=x.star))
lines(x.star,yhat1,col="red")
median(abs(predict(outs1)))

# uses same number of estimation as the 5th order polynomial.
# except the beginning of the plot is still really wiggly.

# Change the 5 until we get a plot that we like. (9)
outs2 <- lm(accel ~ ns(times,9), data = mcycle, x = TRUE)
plot(accel~times, data = mcycle)
x.star <- seq(0,60,length=100)
yhat1 <- predict(outs2,newdata=data.frame(times=x.star))
lines(x.star,yhat1,col="red")
median(abs(predict(outs2)))







