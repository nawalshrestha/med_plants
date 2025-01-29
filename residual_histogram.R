df<- read.csv("data.csv")
df<-df[c("TDWG", "residuals")]

# Define the color gradient functions with updated lighter shades
get_red_shade <- colorRampPalette(c("#F52C2C", "#FCE6E6"))
get_blue_shade <- colorRampPalette(c("#D8E0F0", "#002673"))  # Reversed order

# Break residuals into 10 bins
breaks <- seq(min(df$residuals), max(df$residuals), length.out=11)

# Create the histogram data
hist_data <- hist(df$residuals, breaks=breaks, plot=FALSE)

# Define colors based on residuals
colors <- sapply(hist_data$mids, function(x) {
  if (x < 0) {
    return(get_red_shade(100)[floor(100 * (x - min(df$residuals)) / (0 - min(df$residuals))) + 1])
  } else {
    return(get_blue_shade(100)[floor(100 * (x - 0) / (max(df$residuals) - 0)) + 1])
  }
})

# Plot the histogram
plot(hist_data, col=colors, border="black", main="Histogram of Residuals",
     xlab="Residuals", ylab="Frequency", freq=FALSE) # freq = FALSE will plot normal line otherwise only histogram is plotted.

# Add the normal distribution line
x_vals <- seq(min(df$residuals), max(df$residuals), length=100)
y_vals <- dnorm(x_vals, mean=mean(df$residuals), sd=sd(df$residuals))
lines(x_vals, y_vals, col="black", lwd=2)

# Add dashed lines for upper and lower critical values
critical_value_upper <- mean(df$residuals) + 1.96 * sd(df$residuals)
critical_value_lower <- mean(df$residuals) - 1.96 * sd(df$residuals)
abline(v = critical_value_upper, col="red", lty=2, lwd=2)
abline(v = critical_value_lower, col="red", lty=2, lwd=2)

