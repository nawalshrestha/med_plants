# Load the necessary libraries
library(lme4)
library(dplyr)

df <- read.csv("data.csv")
#Remove Antarctica and Oceania
df<-df[df$realm!='Antarctica' & df$realm!='Oceania',]

df<-df[c("med_rich", "Clim.PC1", "Clim.PC2", "MAT_ANO", "MAT_STD", "Human", "vas_rich", "glottolog", "realm")]

df<-na.omit(df)

# Convert 'realm' variable to a factor
df$realm <- as.factor(df$realm)

# Function to generate new Human values while maintaining the order
get_randomized_human <- function(human_values) {
  n <- length(human_values)
  scaling_factors <- runif(n, 0.9, 1.1) # This creates values anywhere between lower and upper 10% of existing value
                                        # Change to (0.8, 1.2) to create lower and upper 20% of existing value
  return(human_values * scaling_factors)
}

# Function to standardize numeric columns
standardize <- function(df, exclude_cols) {
  df %>% 
    mutate(across(
      .cols = !all_of(exclude_cols), 
      .fns = ~ scale(.)[, 1],
      .names = "{.col}"))
}

# Identify the columns to exclude from standardization
exclude_cols <- c("realm")

# Placeholder to store Human coefficients
human_coeff <- numeric(1000)

# Fitting 1000 models
set.seed(123)  # Set seed for reproducibility
for (i in 1:1000) {
  # Generate new Human values while maintaining the order
  new_human <- get_randomized_human(df$Human)
  
  # Create a new dataframe with altered Human2 values
  df_iter <- df %>%
    mutate(Human = new_human)
  
  # Standardize the dataframe except for the specified columns
  df_iter_standardized <- standardize(df_iter, exclude_cols)
  
  # Fit the linear mixed-effects model
  model <- lmer(med_rich ~ Clim.PC1 + Clim.PC2 + MAT_ANO + MAT_STD + Human + vas_rich + glottolog + (1|realm), data = df_iter_standardized)
  
  # Extract the coefficient for Human2
  human_coeff[i] <- fixef(model)["Human"]
}

# Standardize original dataframe
df_standardized <- standardize(df, exclude_cols)

# Current coefficient of Human
current_model <- lmer(med_rich ~ Clim.PC1 + Clim.PC2 + MAT_ANO + MAT_STD + Human + vas_rich + glottolog + (1|realm), data = df_standardized)
current_human_coef <- fixef(current_model)["Human"]

# Evaluate statistically significant difference from null distribution
p_value <- (sum(human_coeff >= current_human_coef) + 1) / (length(human_coeff) + 1)

# Calculate the 95% confidence interval for the simulated coefficients
CI <- quantile(human_coeff, c(0.025, 0.975))

# Print the results
print(paste("Current coefficient of Human:", current_human_coef))
print(paste("P-value:", p_value))
print(paste("95% Confidence Interval of simulated coefficients:", CI[1], "to", CI[2]))

# Plotting the distribution of coefficients
hist(human_coeff, main="Distribution of Human Coefficients from Simulated Models", xlab="Coefficient of Human", breaks=20)
abline(v=current_human_coef, col="red", lwd=2)
abline(v=CI[1], col="blue", lty=2)
abline(v=CI[2], col="blue", lty=2)