mydata <- read.csv("data.csv")
#Remove Antarctica and Oceania
mydata<-mydata[mydata$realm!='Antarctica' & mydata$realm!='Oceania',]

mydata<-mydata[c("med_rich","vas_rich","realm")]
mydata<-na.omit(mydata)

# Convert 'realm' variable to a factor
mydata$realm <- as.factor(mydata$realm)

# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Fit linear regression models for each factor level in the 'realm' variable
lm_models <- lapply(levels(mydata$realm), function(level) {
  lm(med_rich ~ vas_rich, data = subset(mydata, realm == level))
})

# Extract slopes and standard errors from each model
slopes_df <- data.frame(
  realm = levels(mydata$realm),
  slope = sapply(lm_models, function(model) coef(model)[2]),
  se = sapply(lm_models, function(model) summary(model)$coefficients[2, "Std. Error"])
)

realm<-unique(mydata$realm)
realm_level<-levels(realm)
realm_level<-sort(realm_level, decreasing = TRUE)

# Create a bar plot of slopes with 95% confidence interval (1.96 * se)
ggplot(slopes_df, aes(x = factor(realm, level=realm_level), y = slope, ymin = slope - 1.96 * se, ymax = slope + 1.96 * se)) +
  geom_bar(stat = "identity", fill = "#66cc99", color = "black") +
  geom_errorbar(width = 0.2, position = position_dodge(0.9)) +
  theme_minimal() +
  coord_flip()


#****************************************************************************************************************
#***********************************Differences between slopes across six realms*********************************
#****************************************************************************************************************
#----------------------------------------------------------------------------------------------------------------
#--------------------------------------METHOD 1: USING EMMEANS PACKAGE-------------------------------------------
#----------------------------------------------------------------------------------------------------------------
mydata <- read.csv("data.csv")
#Remove Antarctica and Oceania
mydata<-mydata[mydata$realm!='Antarctica' & mydata$realm!='Oceania',]

mydata<-mydata[c("med_rich","vas_rich","realm")]
mydata<-na.omit(mydata)

# Convert 'realm' variable to a factor
mydata$realm <- as.factor(mydata$realm)

# Fit the linear model with interaction
model1 <- lm(med_rich ~ vas_rich * realm, data = mydata)

# Summary of the model
summary(model1)

# Perform ANOVA to test if the interaction term is significant
anova(model1)

# Fit another linear model without the interaction but with region as a covariate
model2 <- lm(med_rich ~ vas_rich + realm, data = mydata)

# ANOVA (analysis of variance) to compare the two models
anova (model2, model1)

#If the p-value is significant (typically < 0.05), it suggests that the interaction term (vas_rich:realm) 
#significantly improves model fit, meaning region modifies the effect of vascular plant diversity on medicinal plant diversity.

#Check the significance of the realm in model2
summary(model2)
#If the region coefficient is statistically significant (p < 0.05), it means that   region significantly affects medicinal plant diversity.

# Compare slopes using emmeans
library(emmeans)
emtrends_results <- emtrends(model, pairwise ~ realm, var = "vas_rich")

# Print the results
print(emtrends_results)

# Using emmeans package for pairwise comparisons with Bonferroni correction
pairwise_comparisons <- pairs(emtrends_results, adjust = "bonferroni")
print(pairwise_comparisons)
