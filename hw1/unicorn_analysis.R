this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
unicorns <- read.csv('../week1/unicorn_data.csv')
library(hashmap)

################################
# Point 3
print('--------------------------------')
print('Point 3')

print(summary(unicorns))

print(table(unicorns$colour, unicorns$diet))
# 9 green unicorns eats candies and 9 green unicorns eats flowers
# 6 pink unicorns eats candies and 6 pink unicorns eats flowers

print(table(unicorns$diet))
# 15 unicorns eats candies and 15 unicorns eats flowers

print(table(unicorns$colour))
# There are 18 green unicorns and 12 pink unicorns

################################
# Point 4
print('--------------------------------')
print('Point 4')

library(ggplot2)

# Plot scatterplot of weight and horn length
unicorns_plot_dots <- qplot(x = horn.length, y = weight, data = unicorns, geom = "point")
unicorns_plot_dots = unicorns_plot_dots + labs(x = "Horn length (Rainbow miles)", y = "Weight (Magical kilograms)", title = "Unicorns")
print(unicorns_plot_dots)
ggsave('unicorns_plot_dots.png')

unicorns_model <- lm(horn.length ~ weight, data = unicorns)
#print(summary(unicorns_model))

unicorns_model_coef <- coef(unicorns_model)
print("Slope: ")
print(unicorns_model_coef[2]) # Slope
# 0.03153332
# weight

print('')
print("Prediction for fat, 1000 Magical kg, unicorns:")
print(unicorns_model_coef[1] + 1000*unicorns_model_coef[2]) # prediction for 1000 Magical kg


################################
# Point 5
print('--------------------------------')
print('Point 5')

unicorns_plot_residuals_vs_fitted <- qplot(x = fitted(unicorns_model), y = residuals(unicorns_model))
unicorns_plot_residuals_vs_fitted <- unicorns_plot_residuals_vs_fitted + labs(title = "Residuals vs fitted")
print(unicorns_plot_residuals_vs_fitted)
ggsave('unicorns_plot_residuals_vs_fitted.png')

unicorns_plot_residuals <- qplot(sample = residuals(unicorns_model))
unicorns_plot_residuals <- unicorns_plot_residuals + labs(title = "Residuals")
print(unicorns_plot_residuals)
ggsave('unicorns_plot_residuals.png')

################################
# Point 5
print('--------------------------------')
print('Point 6')

green_unicorns <- subset( unicorns, colour == "green")
pink_unicorns <- subset( unicorns, colour == "pink")
print("Student's t-Test")
print(t.test(x=pink_unicorns$weight, y=green_unicorns$weight))

# I did't found way how to use aes_string in qplot, so I decided to use ggplot
# Plot boxplot

for (param in c("weight", "horn.length")){
  for (second_param in c("diet", "colour")){
    unicorns_plot <- ggplot(data=unicorns, aes_string(x=second_param, y=param)) + geom_boxplot()
    unicorns_plot <- unicorns_plot + labs(title = paste(param, " vs ", second_param))
    ggsave(paste('unicorns_plot_boxplot_', param, '_', second_param,'.png'))
  }
}

# Plot jitter

for (param in c("weight", "horn.length")){
  for (second_param in c("diet", "colour")){
    unicorns_plot <- ggplot(data=unicorns, aes_string(x=second_param, y=param)) + geom_jitter()
    unicorns_plot <- unicorns_plot + labs(title = paste(param, " vs ", second_param))
    ggsave(paste('unicorns_plot_jitter_', param, '_', second_param,'.png'))
  }
}

# Plot jitter + boxplot

for (param in c("weight", "horn.length")){
  for (second_param in c("diet", "colour")){
    unicorns_plot <- ggplot(data=unicorns, aes_string(x=second_param, y=param)) + geom_boxplot() + geom_jitter()
    unicorns_plot <- unicorns_plot + labs(title = paste(param, " vs ", second_param))
    ggsave(paste('unicorns_plot_jitter_boxplot_', param, '_', second_param,'.png'))
  }
}


unicorns_weight_model <- lm(weight ~ colour+diet, data = unicorns)
print(summary(unicorns_weight_model))
print(coef(unicorns_weight_model))

# Coef for colour+diet
#(Intercept)  colourpink dietflowers 
# 333.570150   45.519804   -8.368133

# Coefficient for colourpink actually reflect the effect size of pink being compared to that of green
# Same for dietflowers and dietcandies

unicorns_weight_model <- lm(weight ~ colour:diet, data = unicorns)
print(summary(unicorns_weight_model))
print(coef(unicorns_weight_model))

# (Intercept)   colourgreen:dietcandy    colourpink:dietcandy colourgreen:dietflowers 
# 360.56212               -34.61175                30.71947               -27.74033 
# colourpink:dietflowers 
# NA

# We can see that coefficient for colourpink:dietflowers is NA. 
# NA as a coefficient in regression means that the coefficient is not estimable.