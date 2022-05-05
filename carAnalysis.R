carData <- mtcars
head(carData)
str(carData)

# check for invomplete data

incompletData <- carData[!complete.cases(carData),]
nrow(incompletData)

install.packages("psych")
library(psych)

pairs.panels(carData,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

# examine does car weight effect mpg?
# vars are wt and mpg?


# We would like to examine whether there is a link between the weight of cars and
# their fuel efficiency as measured by MPG.
# Devise a hypothesis test that we use for this analysis.
# Describe the variables you will use for the analysis. What type of variables are they?
# Do they need to be converted? If they do, convert them to the relevant data types
# in R.

eff_vs_weight <- data.frame(carData$mpg, carData$wt)

colnames(eff_vs_weight) <- c("efficiency", "weight")
head(eff_vs_weight)

plot(eff_vs_weight, col = "blue", main = "Comparison od weight vs MPG",
     xlab = "MPG",
     ylab = "Weight (lbs)")
attach(carData)
plot(wt, mpg, col = "blue", main = "Comparison od weight vs MPG",
     xlab = "MPG",
     ylab = "Weight (lbs x 1000)")
# there is a negative correlation between the two variables

# Q-Q plot between both variables
with(carData, qqplot(wt, mpg,
                     main = "Comparing car weight and mpg",
                     xlab = "Weight (lbs x 1000)",
                     ylab = "MPG")

qqnorm(wt, main = "Normal Q-Q plot of weight data")
qqline(wt, col = "blue")

#formal test for normality
# using shpiro-wilk
normality_test <- shapiro.test(wt)
normality_test$p.value # 0.0926
# the shapiro-wilk test shows that the wt is normally distributed

normality_test <- shapiro.test(mpg)
normality_test$p.value # 0.1229
# the shapiro-wilk test shows that the wt is normally distributed (p = 0.1229)
# run a person correlation test as both variables are normally distributed

# if one dependent variable is normally distributed and the other is not,
# use a non-parametric test just to be sure.

cor.test(wt, mpg, method = "pearson") # p= 1.294e-10
# the correlation coefficient between weight and MPG is p = 1.294*10**-10. 
# This is below the 0.5 cut-off, therefore the null hypothesis can be rejected.

wilcox.test(wt~mpg)
# we get an error when you try to use the wrong test

     