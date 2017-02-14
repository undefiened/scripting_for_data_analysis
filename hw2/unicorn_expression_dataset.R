rm(list = ls())
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

library(stringr)
library(plyr)
library(ggplot2)
library(reshape2)

###########################################################################
# Exercise 1
###########################################################################


# Read data
data <- read.table('../week2/unicorn_expression.txt', header=TRUE)
unicorns <- read.csv('../week1/unicorn_data.csv')

# Order unicorn_genes data
fun_to_apply <- function(x){strtoi(str_replace(x, "U_", ""))}
data$ID <- lapply(data$ID, FUN=fun_to_apply)

data <- as.data.frame(lapply(data, unlist))
unicorn_genes <- arrange(data, ID)

# Delete ID columns
unicorns$id <- NULL
unicorn_genes$ID <- NULL

###########################################################################
# Exercise 2
###########################################################################

# Principal components
principal_components <- prcomp(unicorn_genes)
#print(summary(principal_components$x$PC1))

# Plot first vs second principal components
p <- ggplot(data.frame(principal_components$x[,1:2]), aes(x=PC1, y=PC2))
p <- p + geom_point()
print(p)
ggsave('principal_components_1_vs_2.png')

# I don't see any apparent pattern.

# Plot second vs third principal components
p <- ggplot(data.frame(principal_components$x[,2:3]), aes(x=PC2, y=PC3))
p <- p + geom_point()
print(p)
ggsave('principal_components_2_vs_3.png')

# I don't see any apparent pattern again.

pc_individuals_data_frame <- data.frame(principal_components$x)
pc_individuals_data_frame <- merge(pc_individuals_data_frame, unicorns, by="row.names")

# Plot 1 vs 2 principal component with different colours for diet
p <- ggplot(pc_individuals_data_frame, aes(x=PC1, y=PC2))
p <- p + geom_point(aes(colour = diet))
print(p)
ggsave('principal_components_diet.png')

# It is difficult to find apparent pattern

# Plot 1 vs 2 principal component with different colours for colours
p <- ggplot(pc_individuals_data_frame, aes(x=PC1, y=PC2))
p <- p + geom_point(aes(colour = colour))
print(p)
ggsave('principal_components_colour.png')

# We can see 2 big apparent separated clusters

# Plot 2 vs 3 principal component with different colours for diet
p <- ggplot(pc_individuals_data_frame, aes(x=PC2, y=PC3))
p <- p + geom_point(aes(colour = diet))
print(p)
ggsave('principal_components_diet_2_vs_3.png')

# We can see red (candy) cluster and blue (flowers) points around

# Plot 1 vs 2 principal component with different colours for colours
p <- ggplot(pc_individuals_data_frame, aes(x=PC2, y=PC3))
p <- p + geom_point(aes(colour = colour))
print(p)
ggsave('principal_components_colour_2_vs_3.png')

# I don't see any apparent pattern

###########################################################################
# Exercise 3
###########################################################################
unicorn_genes_merged <- merge(unicorn_genes, unicorns, by="row.names")
unicorn_genes_melted <- melt(unicorn_genes_merged, id=c("diet", "colour", "horn.length", "weight", "Row.names"))

# Plot all expressions together without colouring

# Boxplot
p <- ggplot(unicorn_genes_melted, aes(x=variable, y=value))
p <- p + geom_boxplot()
print(p)
ggsave('all_genes_boxplot.png')

# Jitter
p <- ggplot(unicorn_genes_melted, aes(x=variable, y=value))
p <- p + geom_jitter()
print(p)
ggsave('all_genes_jitter.png')

# Plot all expressions together with colouring

# Boxplot diet
p <- ggplot(unicorn_genes_melted, aes(x=variable, y=value))
p <- p + geom_boxplot(aes(colour=diet))
print(p)
ggsave('all_genes_boxplot_diet.png')

# Jitter diet
p <- ggplot(unicorn_genes_melted, aes(x=variable, y=value))
p <- p + geom_jitter(aes(colour=diet))
print(p)
ggsave('all_genes_jitter_diet.png')

# Boxplot colour
p <- ggplot(unicorn_genes_melted, aes(x=variable, y=value))
p <- p + geom_boxplot(aes(colour=colour))
print(p)
ggsave('all_genes_boxplot_colour.png')

# Jitter colour
p <- ggplot(unicorn_genes_melted, aes(x=variable, y=value))
p <- p + geom_jitter(aes(colour=colour))
print(p)
ggsave('all_genes_jitter_colour.png')

# I don't see any patterns

###########################################################################
# All principal components together (unnecessary)
###########################################################################
pc_together <- melt(pc_individuals_data_frame, id=c("diet", "colour", "horn.length", "weight", "Row.names"))

# Plot all pc together without colouring

# Boxplot
p <- ggplot(pc_together, aes(x=variable, y=value))
p <- p + geom_boxplot()
print(p)
ggsave('all_pc_boxplot.png')

# Jitter
p <- ggplot(pc_together, aes(x=variable, y=value))
p <- p + geom_jitter()
print(p)
ggsave('all_pc_jitter.png')

# We can see that figure narrows from PC1 to PC 12

# Plot all pc together with colouring

# Boxplot diet
p <- ggplot(pc_together, aes(x=variable, y=value))
p <- p + geom_boxplot(aes(colour=diet))
print(p)
ggsave('all_pc_boxplot_diet.png')

# Jitter diet
p <- ggplot(pc_together, aes(x=variable, y=value))
p <- p + geom_jitter(aes(colour=diet))
print(p)
ggsave('all_pc_jitter_diet.png')

# Boxplot colour
p <- ggplot(pc_together, aes(x=variable, y=value))
p <- p + geom_boxplot(aes(colour=colour))
print(p)
ggsave('all_pc_boxplot_colour.png')

# Jitter colour
p <- ggplot(pc_together, aes(x=variable, y=value))
p <- p + geom_jitter(aes(colour=colour))
print(p)
ggsave('all_pc_jitter_colour.png')

# I don't see any patterns

###########################################################################
# Exercise 4
###########################################################################
expression_melted <- unicorn_genes_melted

unicorn_expression_model <- function (x) {
  lm(value ~ diet + colour, data=x)
}

models <- dlply(expression_melted, "variable", unicorn_expression_model)
result_lm <- ldply(models, coef)

# The ldply function returns set of linear regressions models for each variable
print(summary(result_lm))

# Table of confidence intervals

models <- dlply(expression_melted, "variable", unicorn_expression_model)
result_confidence <- ldply(models, confint)

print(summary(result_confidence))