library(datasets); data(mtcars); require(stats); require(graphics)
pairs(mtcars, panel = panel.smooth, main = "mtcars data")

cars <- transform(mtcars, am = as.factor(am))
fit <- lm(mpg ~., cars)
fit1 <- lm(mpg ~ am, cars)

makelmcars <- function(){
  # Store the coefficient of linear models with different independent variables
        cf <- c(coef(lm(mpg ~ am, cars))[2], 
                coef(lm(mpg ~ am + cyl, cars))[2],
                coef(lm(mpg ~ am + cyl + disp, cars))[2],
                coef(lm(mpg ~ am + cyl + disp + hp, cars))[2],
                coef(lm(mpg ~ am + cyl + disp + hp + drat, cars))[2],
                coef(lm(mpg ~ am + cyl + disp + hp + drat + wt, cars))[2],
                coef(lm(mpg ~ am + cyl + disp + hp + drat + wt + qsec, cars))[2],
                coef(lm(mpg ~ am + cyl + disp + hp + drat + wt + qsec + vs, cars))[2],
                coef(lm(mpg ~ am + cyl + disp + hp + drat + wt + qsec + vs + gear, cars))[2],
                coef(lm(mpg ~ am + cyl + disp + hp + drat + wt + qsec + vs + gear + carb, cars))[2])
  print(cf)
}