#**********************************************************************************************************************************
#****************************************Medicinal Plant Diversity across different realms*****************************************
#**********************************************************************************************************************************


df <- read.csv("data.csv")
#Remove Antarctica and Oceania
df<-df[df$realm!='Antarctica' & df$realm!='Oceania',]
df<-df[c("med_rich", "realm")]
df<-na.omit(df)
boxplot(med_rich~realm, data=df, xlab="", ylab="Medicinal plant diversity")


# Perform ANOVA and Tukey's HSD post-hoc test to see if there is significant differences
# between medicinal plant diversity across regions
model<-aov(med_rich~realm, data=df)
summary(model)

library(stats)
library(multcomp)

# Perform Tukey post hoc test
tukey_result <- TukeyHSD(model)

# Summarize the Tukey post hoc test results
summary(tukey_result)
print(tukey_result)


#**********************************************************************************************************************************
#****************************************Area of botanical countries across different realms***************************************
#**********************************************************************************************************************************


df <- read.csv("data.csv")
#Remove Antarctica and Oceania
df<-df[df$realm!='Antarctica' & df$realm!='Oceania',]
df<-df[c("Area", "realm")]
df<-na.omit(df)
boxplot(Area~realm, data=df, xlab="", ylab="Area")

# Perform ANOVA and Tukey's HSD post-hoc test to see if there is significant differences in area across regions
model<-aov(Area~realm, data=df)
summary(model)

library(stats)
library(multcomp)

# Perform Tukey post hoc test
tukey_result <- TukeyHSD(model)

# Summarize the Tukey post hoc test results
summary(tukey_result)
print(tukey_result)



