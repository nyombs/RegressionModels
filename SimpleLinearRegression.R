install.packages("UsingR")
library(UsingR)
data(diamond)
library(ggplot2)
g = ggplot(diamond, aes(x= carat, y = price),)
g = g + xlab("Mass (carats)")
g = g + ylab("Price (SIN $)")
g = g + geom_point(size = 6, colour = "black", alpha= 0.2)
g = g + geom_point(size = 5, colour = "blue", alpha= 0.2)
g = g + geom_smooth(method = "lm", colour = "black")
g

fit <- lm(price ~ carat, data = diamond)
coef(fit)

## Plotting residuals:
data(diamond)
y <- diamond$price; 
x <- diamond$carat;
n <- length(y)
fit <- lm(y ~ x)
#computing residual, all equivalent
e <- resid(fit)
yhat <- predict(fit)
max(abs(e -(y - yhat)))
max(abs(e - (y - coef(fit)[1] - coef(fit)[2] * x)))

# plot1

plot(diamond$carat, diamond$price, 
     xlab = "Mass (carats)",
     ylab = "Price (SIN $)",
     bg = "lightblue",
     col = "black", cex = 1.1, pch = 21, frame = FALSE)

abline(fit, lwd = 2)
# plotting in red the residuals
for (i in 1: n)
      lines(c(x[i], x[i]), c(y[i], yhat[i]), col = "red", lwd = 2)
# plot 2: better
plot(x, e, 
     xlab = "Mass (carats)",
     ylab = "Price (SIN $)",
     bg = "lightblue",
     col = "black", cex = 2, pch = 21, frame = FALSE)

abline(h = 0, lwd = 2)
# plotting in red the residuals
for (i in 1: n)
  lines(c(x[i], x[i]), c(e[i], 0), col = "red", lwd = 2)

# now with ggplot to visualize residuals

g = ggplot(diamond, aes(x= diamond$carat, y = resid(lm(y ~ x))))
g = g + geom_hline(yintercept = 0, size = 2)
g = g + xlab("Mass (carats)")
g = g + ylab("Residual")
g = g + geom_point(size = 7, colour = "black", alpha= 0.4)
g = g + geom_point(size = 5, colour = "red", alpha= 0.4)
g

#

y <- diamond$price; 
x <- diamond$carat;
n <- length(y)
#linear regression model formula: Y = beta0 + beta1X
beta1 <- cor(y, x) * sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
#residual 
e <- y - beta0 - beta1 * x
# estimate of the standard deviation around the regression line
sigma <- sqrt(sum(e^2) / (n-2))
# sum of square x
ssx <- sum(x - mean(x)^2)

seBeta0 <- (1/n + mean(x) ^2 /ssx)^ 0.5 * sigma
seBeta1 <- sigma /sqrt(ssx)
#t statistics
tBeta0 <- beta0 / ssBeta0
tbeta1 <- beta1 /ssBeta1

pBeta0 <- 2 * pt(abs(tBeta0), df = n - 2, lower.tail = FALSE)
pBeta1 <- 2 * pt(abs(tBeta1), df = n - 2, lower.tail = FALSE)
coefTable <- rbind(c(beta0, seBeta0, tBeta0, pBeta0), c(beta1, seBeta1,tBeta1, pBeta1))
colnames(coefTable) <- c("Estimate", "Std. Error", "t value", "P(>|t|)")
rownames(coefTable) <- c("(Intercept)", "x")

fit <- lm(y ~x)
summary(fit)$coefficients

#Quiz 2:
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit <- lm(y ~x)
summary(fit)


data(mtcars)
summary(mtcars)

fit <- lm(mtcars$mpg ~ mtcars$wt)
summary(fit)
#  Get a 95% confidence interval for the expected mpg at the average weight. What is the lower endpoint? 2.5% value
confint(fit)
# Also, equivalent to the following 30 below is the level of freedom (you get it by running summary(fit))
coef=summary(fit)$coefficients[2,1] 
err=summary(fit)$coefficients[2,2] 
coef + c(-1,1)*err*qt(0.975, fit$df)
#prediction
to.lm = lm(mtcars$mpg ~ mtcars$wt)
input = data.frame(X = 3)
predict(to.lm,  input, interval = "predict")

summary(mtcars$wt)

local <- mtcars
y <- local$mpg 
local$wt <- local$wt/2
fit <- lm(mtcars$mpg ~ I(mtcars$wt + mean(mtcars$wt)))
confint(fit)


fit <- lm(mpg ~ wt, data=mtcars)
newdata <- data.frame(wt=3000/1000)
predict(fit, newdata, interval=("prediction"))

fit_num <- lm(mpg ~ wt, data=mtcars)
fit_denom <- lm(mpg ~ 1, data=mtcars)
sum(resid(fit_num)^2) / sum(resid(fit_denom)^2)


#Quiz 3:
# 1 Consider the mtcars data set. Fit a model with mpg as the outcome that includes number of cylinders as a factor variable and weight as confounder. Give the adjusted
# estimate for the expected change in mpg comparing 8 cylinders to 4.
data(mtcars)
summary(mtcars)
str(mtcars)
newCars <- mtcars
newCars$cyl <- as.factor(newCars$cyl)
# reference factor to compare the change to
cyl4 <- relevel(newCars$cyl, "4")
x <- lm(mpg ~ cyl4 + wt, data = newCars)
#x <- lm(mpg ~ I(1 * (cyl == 8)) + I(1 * (cyl == 6)) * wt, data = newCars)

# 2 Consider the mtcars data set. Fit a model with mpg as the outcome that includes number of cylinders as a factor variable 
# and weight as a possible confounding variable. Compare the effect of 8 versus 4 cylinders on mpg for the adjusted and unadjusted by weight models. 
# Here, adjusted means including the weight variable as a term in the regression model and unadjusted means the model without weight included. 
# What can be said about the effect comparing 8 and 4 cylinders after looking at models with and without weight included?.
summary(x)$coef

x <- lm(mpg ~ cyl4 , data = newCars)
summary(x)$coef

# Consider the mtcars data set. Fit a model with mpg as the outcome that considers number of cylinders as a factor variable and weight as confounder. 
# Now fit a second model with mpg as the outcome model that considers the interaction between number of cylinders (as a factor variable) and weight. 
# Give the P-value for the likelihood ratio test comparing the two models and suggest a model using 0.05 as a type I error rate significance benchmark.
x <- lm(mpg ~ factor(cyl) + wt, data = newCars)
y <- lm(mpg ~ factor(cyl) * wt, data = newCars)

anova(x, y)$"Pr(>F)"

x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit <- lm(y ~ x)
hatvalues(fit)
dfbetas(fit)
