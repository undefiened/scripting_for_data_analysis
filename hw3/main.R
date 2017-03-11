rm(list = ls())
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

library(plyr)
library(ggplot2)


simulate_drones <- function(number_of_drones, error_coefficient=15){
  x <- runif(number_of_drones, 1, 25)
  y <- x*x * 195 + unlist(llply(x, function(x) {
    rnorm(1, 0, x*x*error_coefficient)
  }))
  
  data.frame(x, y)
}

analyze_drones <- function(drones){
  lm(formula = y ~ poly(x, 2, raw=TRUE), data = drones)
}

drones <- simulate_drones(100)

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


sim_many_drones <- replicate(1000, analyze_drones(simulate_drones(100)), simplify = FALSE)

coefficients <- ldply(sim_many_drones, function(x){coef(x)})
mean_coef_Intercept <- mean(coefficients$`(Intercept)`)
mean_coef_1 <- mean(coefficients$`poly(x, 2, raw = TRUE)1`)
mean_coef_2 <- mean(coefficients$`poly(x, 2, raw = TRUE)2`)

print(mean_coef_Intercept)
print(mean_coef_1)
print(mean_coef_2)



# Lets try to reduce sample size
analyze_many_replications <- function(replications, sample_size, error_coefficient=15){
  sim_many_drones <- replicate(10, analyze_drones(simulate_drones(100, error_coefficient=error_coefficient)), simplify = FALSE)
  
  coefficients <- ldply(sim_many_drones, function(x){coef(x)})
  mean_coef_2 <- mean(coefficients$`poly(x, 2, raw = TRUE)2`)
  
  print(paste("Replication=", replications, " sample size=", sample_size, " error=", error_coefficient, sep=""))
  print(mean_coef_2)
}

analyze_many_replications(10, 100)
analyze_many_replications(1000, 10)

# Lets analyze if we can restore our coefficient with greater uncertainty
analyze_many_replications(1000, 100, error_coefficient=200)
analyze_many_replications(1000000000, 100, error_coefficient=200)
analyze_many_replications(100000000000000, 10000000000, error_coefficient=200)


analyze_many_replications(1000, 100, error_coefficient=50)
analyze_many_replications(1000000000, 100, error_coefficient=50)
analyze_many_replications(100000000000000, 10000000000, error_coefficient=50)
