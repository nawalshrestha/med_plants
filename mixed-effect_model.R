df <- read.csv("data.csv")
#Remove Antarctica and Oceania
df<-df[df$realm!='Antarctica' & df$realm!='Oceania',]

df<-df[c("med_rich", "Clim.PC1", "Clim.PC2", "MAT_ANO", "MAT_STD", "Human", "vas_rich", "glottolog", "realm")]

df<-na.omit(df)

# Convert 'realm' variable to a factor
df$realm <- as.factor(df$realm)

# Identify numeric columns
numeric_cols <- sapply(df, is.numeric)

# Scale only numeric variables to have a mean of 0 and a standard deviation of 1.
scaled_df <- df
scaled_df[, numeric_cols] <- scale(scaled_df[, numeric_cols])



# Load the required library
library(lme4)

# Fit the linear mixed-effects model
mixed_model <- lmer(med_rich ~ Clim.PC1 + Clim.PC2 + MAT_ANO + MAT_STD + Human + vas_rich + glottolog + (1 | realm), 
                    data = scaled_df, na.action = na.fail)

fixed_model<-lm(med_rich ~ Clim.PC1 + Clim.PC2 + MAT_ANO + MAT_STD + Human + vas_rich + glottolog , data = scaled_df)

anova(mixed_model, fixed_model)

# Extract variance components
var_components <- as.data.frame(VarCorr(mixed_model))

# Extract the variance and standard deviation for realm
realm_variance <- var_components$vcov[var_components$grp == "realm"]
realm_sd <- sqrt(realm_variance)
# Print results
cat("Variance for realm:", realm_variance, "\n")
cat("Standard deviation for realm:", realm_sd, "\n")


#*******************************************************************************************************************************
#*********************************************Calculate significance level and plot*********************************************
#*******************************************************************************************************************************

# Extract the coefficients and standard errors from the summary output
coefficients <- fixef(mixed_model)
standard_errors <- sqrt(diag(vcov(mixed_model)))

# Calculate the t-values
t_values <- coefficients / standard_errors

# Calculate the degrees of freedom
df <- df.residual(mixed_model)

# Calculate the two-tailed p-values
p_values <- 2 * pt(abs(t_values), df = df, lower.tail = FALSE)

# Print the t-values and p-values
results <- data.frame("Coefficient" = coefficients,
                      "Standard_Error" = standard_errors,
                      "t_value" = t_values,
                      "p_value" = p_values)

results$sig[results$p_value>0.05]<-""
results$sig[results$p_value<0.05]<-"*"
results$sig[results$p_value<0.01]<-"**"
results$sig[results$p_value<0.001]<-"***"

print(results)

a<-results[2:nrow(results),] #Extract second row to last row
a <- a[order(abs(a$Coefficient),decreasing = TRUE),] #Order in descending order of absolute value of coefficient

mybar<-barplot(height=abs(a$Coefficient), names=rownames(a), las=2, col=ifelse(a$Coefficient>0,'#66CC99','#FF9900'))

# Add the text above the bar
text(mybar, abs(a$Coefficient) +0.02, paste(a$sig, sep="") ,cex=1.7) 


#**********************************************************************************************************************************
#*********************************************Correlation of variables**************************************************************
#**********************************************************************************************************************************
library(corrplot)
df <- read.csv("data.csv")
#Remove Antarctica and Oceania
df<-df[df$realm!='Antarctica' & df$realm!='Oceania',]
cor.df<-df[c("Clim.PC1", "Clim.PC2" , "MAT_ANO", "MTCQ_ANO", "ELE_STD", "MAT_STD", "Human", "vas_rich", "glottolog" )]
cor.df<-na.omit(cor.df)

corrplot(cor(cor.df), method='color', addCoef.col = "black", number.cex = 0.7)

