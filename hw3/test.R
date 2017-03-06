simulate_things <- function(n) {
  x <- rep(c("raindrops", "roses", "whiskers"), n)
  y <- 1 + 0.5 * ifelse(x == "raindrops", 1, 0) + ifelse(x == "whiskers", 1, 0) + rnorm(n)
  data.frame(x, y)
}
analyze_things <- function(things) {
  lm(y ~ x, data = things)
}
get_p_value <- function(model) {
  drop <- drop1(model, test = "F")
  drop$"Pr(>F)"[2]
}

sim_data <- ldply(1:6, function(i) transform(simulate_things(10), replicate = i))
qplot(x = x, y = y, data = sim_data, geom = "boxplot") + facet_wrap(~ replicate)

aaa <- ddply(sim_data, c("replicate", "x"), summarise, average = mean(y), stdev = sd(y))