### Univariate analyses

data() # get familiar with built-in R data
# many packages come with an additional set of data
# thus, if you have a larger variety of packages, 
# this function will show you a larger variety of data sets


## Student's t-test

# load data on plant growth from datasets package
data("PlantGrowth")

# view data as table
View(PlantGrowth)

# select only treatment groups, i.e. eliminate control group
PlantGrowth <- PlantGrowth[11:30,]

# rename variables
group <- PlantGrowth$group # explanatory variable (x)
weight <- PlantGrowth$weight # response variable (y)

# test response variable for normality
shapiro.test(weight) # p > 0.05 -> data are normally distributed

# test for homogeneity of variance
library(car)
leveneTest(weight~group) # data are homogenous
# proceed with parametric test

# test for difference between levels of explanatory variable
t.test(weight~group) # p < 0.05 -> weight between groups is different

# calculate descriptive statistics for barplot
m <- tapply(weight, list(group), mean)
sd <- tapply(weight, list(group), sd)
n <- tapply(weight, list(group), length)
se <- sd/sqrt(n)

# turn descriptive statistics tables into a single data frame for ggplot
df <- data.frame(m = m,
                 sd = sd,
                 se = se,
                 trt = c("Control","Treatment 1","Treatment 2"))

# select only treatment groups, i.e. eliminate control group
df <- df[2:3,]

# plot barplot of data to visualise difference between treatments
library(ggplot2)

ggplot(df, aes(trt, m)) + # define data, x and y variables
  geom_col(fill = "grey70", width = 0.2) + # define plot type
  geom_errorbar(aes(ymin = m - se, ymax = m + se), width = 0.08, lwd = 0.4) + # define error bars
  xlab("Plant treatment") + # define x axis title
  ylab("Mean plant weight (± SE)") + # define y axis title
  theme_classic() # define theme


## Wilcoxon signed-rank test

# load data on CO2 uptake in plants from datasets package
data("CO2")

# view data as table
View(CO2)

# rename variables
type <- CO2$Type # explanatory variable (x)
uptake <- CO2$uptake # response variable (y)

# test response variable for normality
shapiro.test(uptake) # p < 0.05 -> data are non-normal
shapiro.test(log(uptake))
# data are still non-normal after transformation
# a non-parametric test is required

# test for difference between levels of explanatory variable
wilcox.test(uptake~type) # p < 0.05 -> uptake between groups is different

# plot boxplot of data to visualise that difference
library(ggplot2)

ggplot(CO2, aes(type, uptake)) + # define data, x and y variables
  geom_boxplot(fill = "grey95", width = 0.2) + # define plot type
  xlab("Plant type") + # define x axis title
  ylab("CO2 uptake") + # define y axis title
  theme_classic() # define theme
  

## Analysis of variance (ANOVA)

# load data on Titanic passengers from datasets package
data("Titanic")

# view data as table
View(Titanic)

# convert Titanic data matrix to data frame
Titanic <- as.data.frame(Titanic)

# rename variables
class <- Titanic$Class # explanatory variable (x)
freq <- Titanic$Freq # response variable (y)

# test data for homogeneity of variance
library(car)
leveneTest(freq~class) # p > 0.05 -> data are homogenous
# proceed with parametric test

# test for difference between levels of explanatory variable
mod <- aov(freq~class) 
summary(mod)
# p > 0.05 -> there are no differences in numbers of people between classes

# calculate descriptive statistics for plot
library(psych)
df <- describeBy(freq, list(class), mat = T, digits = 3)

# plot barplot of data to visualise lack difference between classes
library(ggplot2)

ggplot(df, aes(group1, mean)) + # define data, x and y variables
  geom_col(fill = "grey70", width = 0.2) + # define plot type
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.08, lwd = 0.4) + # define error bars
  xlab("Class on board RMS Titanic") + # define x axis title
  ylab("Mean number of people (± SE)") + # define y axis title
  theme_classic() # define theme


## Second analysis of variance (ANOVA)

# load built-in ggplot2 data on sleep in mammals
data(msleep) 

# view data as table
View(msleep)

# convert mammal sleep data matrix to data frame
msleep <- as.data.frame(msleep)

# order mammal orders alphabetically
msleep <- msleep[order(msleep[,"order"]),]

# delete chaotic row names
rownames(msleep) <- NULL

# extract first set of mammal orders
msleep1 <- msleep[2:22,]

# extract primates
msleep2 <- msleep[42:53,]

# bind all selected mammal orders into one data frame
msleep <- rbind(msleep1, msleep2)

# rename variables
order <- msleep$order # explanatory variable (x)
sleep <- msleep$sleep_total # response variable (y)

# test data for homogeneity of variance
leveneTest(sleep~order) # p > 0.05 -> data are homogenous
# proceed with parametric test

# test for difference between levels of explanatory variable
mod <- aov(sleep~order)
summary(mod)
# p < 0.05 -> there are differences in sleep time between orders

# perform post hoc test to find out where the differences lie
TukeyHSD(mod) # p < 0.05 indicates significant difference between each two orders
# Cetaceans and artiodactyls sleep the same amount of time
# Primates and carnivores sleep the same amount of time

# calculate descriptive statistics for plot
library(psych)
df <- describeBy(sleep, list(order), mat = T, digits = 3)

# plot barplot of data to visualise differences and similarities between orders
library(ggplot2)

ggplot(df, aes(group1, mean)) + # define data, x and y variables
  geom_col(fill = "grey70", width = 0.2) + # define plot type
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.08, lwd = 0.4) + # define error bars
  xlab("Mammal order") + # define x axis title
  ylab("Mean hours spent sleeping (± SE)") + # define y axis title
  theme_classic() # define theme


## Kruskal-Wallis test

# load data on human hair and eye colours from datasets package
data("HairEyeColor")

# view data as table
View(HairEyeColor)

# convert mammal hair and eye colour data matrix to data frame
HairEyeColor <- as.data.frame(HairEyeColor)

# rename variables
eye <- HairEyeColor$Eye # explanatory variable (x)
freq <- HairEyeColor$Freq # response variable (y)

# test data for homogeneity of variance
leveneTest(freq~eye) # p < 0.05 -> data are not homogenous
# this problem could be solved with a log transformation of y
# however, I am going to go ahead with the untransformed data for the purpose of demonstration
# hence, I will proceed with a non-parametric test

# test for difference between levels of explanatory variable
kruskal.test(freq~eye)
# p > 0.05 -> there are no differences in frequency between eye colours

# this would not normally require a post hoc test, as differences are insignificant
# however, I will go ahead and perform a post hoc test for the purpose of demonstration
library(DescTools)
NemenyiTest(x = freq, g = eye, dist = "tukey")
# for instance, we can see that the occurence of blue and brown is more similar than green and blue

# plot boxplot of data to visualise lack of difference
library(ggplot2)

ggplot(HairEyeColor, aes(eye, freq)) + # define data, x and y variables
  geom_boxplot(fill = "grey95", width = 0.2) + # define plot type
  xlab("Eye colour") + # define x axis title
  ylab("Number of people") + # define y axis title
  theme_classic() # define theme


## Linear regression

# load tree data from datasets package
data("trees")

# view data as table
View(trees)

# rename variables
girth <- trees$Girth # explanatory variable (x)
volume <- trees$Volume # response variable (y)

# generate linear model
mod <- lm(volume~girth)

# eyeball heteroskedasticity
par(mfrow = c(2,2)) # set display to 2x2 grid

plot(mod) # plot linear model
# spread of data can be improved witg transformation

# log tranform data
lv <- log(volume)
mod <- lm(lv~girth) # generate new model with transformed data
plot(mod) # plot new linear model

par(mfrow = c(1,1)) # reset display

# call results of model
summary(mod) # p < 0.05 -> the relationship between girth and volume is significant
# R2 = 0.94 -> the regression model explains 94% of the variation in the data

# generate predicted values from model
predicted <- predict(mod)

# plot data with regression line
library(ggplot2)

ggplot(trees, aes(girth, lv)) + # define data and x and y
  geom_point(colour = "grey70") + # define geometry for data points
  geom_line(aes(girth, predicted)) + # define geometry for regression line
  labs(x = "Tree diameter (in)", y = "Log tree volume (cubic ft)") + # define axis titles
  theme_classic() # define theme


## Clean up

detach(package:car)
detach(package:DescTools)
detach(package:psych)
detach(package:ggplot2)
rm(list = ls())
graphics.off()
cat("\014")
