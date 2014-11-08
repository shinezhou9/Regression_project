#Q1
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit <- lm(y~x)
summary(fit)$coefficients
summary(fit)$sigma

#Q2
data(mtcars)
x <- mtcars$wt; y <- mtcars$mpg
fit2 <- lm(y ~ I(x - mean(x)))
coef(fit2)
p2 <- predict(fit2, newdata = data.frame(x=0), interval = "confidence")
p2
#or
sumcoef2 <- summary(fit2)$coefficients
CIBeta0 <- sumcoef2[1,1]+c(-1,1)*qt(.975, df = fit2$df)*sumcoef2[1,2]
CIBeta0

#Q5
fit5 <- lm(y ~ x)
newdata <- data.frame(x = 3)
p5 <- predict(fit5, newdata, interval = "prediction")
p5

#Q6
fit6 <- lm(y ~ I(x/2))
sumcoef6 <- summary(fit6)$coefficients
sumcoef6[2,1]+c(-1,1)*qt(.975, df = fit6$df)*sumcoef6[2,2]

#Q9
beta0 <- mean(y)
e1 <- sum((y-beta0)^2)
e2 <- sum(resid(fit5)^2)
ratio <- e2/e1
ratio