rm(list = ls())
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

library(plyr)
library(ggplot2)


simulate_drones <- function(){
  x <- runif(100, 1, 25)
  y <- x*x * 195 + unlist(llply(x, function(x) {
    rnorm(1, 0, x*x*15)
  }))
  
  data.frame(x, y)
}

analyze_drones <- function(drones){
  lm(formula = y ~ poly(x, 2), data = drones)
}

drones <- simulate_drones()

print(qplot(x, y, data=drones, geom = c("point", "smooth"), xlab = "Radius (m)", ylab = "Number of conflicts"))
ggsave('data.png')

# Lets analyze our model

model <- analyze_drones(drones)
print(summary(model))
print(coef(model))

# Also we can calculate our confidence intervals
print(confint(model, level=0.95))

print(qplot(fitted(model), residuals(model)))
ggsave('residuals.png')


sim_many_drones <- replicate(1000, analyze_drones(simulate_drones()), simplify = FALSE)

coefficients <- ldply(sim_many_drones, function(x){coef(x)})
mean_coef_Intercept <- mean(coefficients$`(Intercept)`)
mean_coef_1 <- mean(coefficients$`poly(x, 2)1`)
mean_coef_2 <- mean(coefficients$`poly(x, 2)2`)

print(mean_coef_Intercept)
print(mean_coef_1)
print(mean_coef_2)