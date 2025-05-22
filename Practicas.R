library(readxl)
library(ggplot2)  # For plotting
library(dplyr)    # For data manipulation
library(robust) 
library(robustbase)
#LMC.JK <- read.csv("~/Universidad/Tesis/Códigos/LMC JK.csv")
#View(LMC.JK)
#periodos <- LMC.JK$Period
#mag_j <- LMC.JK$J
#mag_ks <- LMC.JK$K_s
#log_p <- log(periodos, base=10)
#plot(x=log_p, y=mag_j,ylim = rev(range(mag_j)), xlab = "Log P", ylab = "J", main = "J vs. LogP para LMC")


# Read the CSV file
datos <- read.csv("~/Universidad/Tesis/Códigos/LMC JK.csv")

# Extract the relevant columns
periodos <- datos$Period
log_p <- log10(periodos)
mag_j <- datos$J

# Create a data frame for regression
regression_data <- data.frame(log_p, mag_j)

# Perform Major Axis (MM) regression using robust method
mm_model <- lmrob(mag_j ~ log_p, data = regression_data)


LMCmed <- lm(mag_j ~ log_p, data = regression_data)
yhat<- predict(LMCmed, se.fit =FALSE)
plot(yhat, LMCmed$residuals,xlim = rev(range(yhat)),  xlab="Predicción J", ylab="Residuos J", main="Residuos J vs. Predicción J", sub = "LMC",col.sub = "purple")
plot(log_p, LMCmed$residuals,xlab="Log P", ylab="Residuos J", main="Residuos J vs. Log P", sub = "LMC",col.sub = "purple")

# Extract slope and intercept
slope <- coef(mm_model)[2]  # Slope is the second coefficient
intercept <- coef(mm_model)[1]  # Intercept is the first coefficient

# Calculate standard errors for slope and intercept
slope_se <- summary(mm_model)$coefficients[2, "Std. Error"]
intercept_se <- summary(mm_model)$coefficients[1, "Std. Error"]

# Print the results
cat("Slope:", slope, "±", slope_se, "\n")
cat("Intercept:", intercept, "±", intercept_se, "\n")

# Print the results
#cat("Slope:", slope, "\n")
#cat("Intercept:", intercept, "\n")

# Optional: Plot the data and the regression line with inverted y-axis
#ggplot(regression_data, aes(x = log_p, y = mag_j)) +
#  geom_point() +
#  geom_abline(slope = slope, intercept = intercept, color = "blue") +
#  labs(title = "MM Regression of J vs LogP",
#       x = "log(Period)",
#       y = "Magnitude (J)") +
#  scale_y_reverse() +  # Invert the y-axis
#  theme_minimal()
#Graph
plot(log_p, mag_j,ylim = rev(range(mag_j)), main="Regresión MM para J vs. LogP", xlab="LogP", ylab="Magnitud J", sub = "LMC",col.sub = "purple" )
#scale_y_reverse()
#add the fitted regression line to the scatterplot
abline(mm_model, col="steelblue")
# Add annotations for slope and intercept
annotate("text", x = min(log_p), y = max(mag_j), 
         label = paste("Slope:", round(slope, 3), "\nIntercept:", round(intercept, 3)), 
         hjust = 0, vjust = 1, size = 4, color = "red")

#Calculos para Ks
mag_ks <- datos$K_s
# Create a data frame for regression
regression_data2 <- data.frame(log_p, mag_ks)

# Perform Major Axis (MM) regression using robust method
mm_model2 <- lmrob(mag_ks ~ log_p, data = regression_data2)

LMCmed2 <- lm(mag_ks ~ log_p, data = regression_data2)
yhat2<- predict(LMCmed2, se.fit =FALSE)
plot(yhat2, LMCmed2$residuals,xlim = rev(range(yhat2)),  xlab="Predicción Ks", ylab="Residuos Ks", main="Residuos Ks vs. Predicción Ks", sub = "LMC",col.sub = "purple")
plot(log_p, LMCmed2$residuals,xlab="Log P", ylab="Residuos Ks", main="Residuos Ks vs. Log P", sub = "LMC",col.sub = "purple")


# Extract slope and intercept
slope2 <- coef(mm_model2)[2]  # Slope is the second coefficient
intercept2 <- coef(mm_model2)[1]  # Intercept is the first coefficient

# Calculate standard errors for slope and intercept
slope_se2 <- summary(mm_model2)$coefficients[2, "Std. Error"]
intercept_se2 <- summary(mm_model2)$coefficients[1, "Std. Error"]

# Print the results
cat("Slope:", slope2, "±", slope_se2, "\n")
cat("Intercept:", intercept2, "±", intercept_se2, "\n")

# Print the results
#cat("Slope:", slope, "\n")
#cat("Intercept:", intercept, "\n")

#Graph
plot(log_p, mag_ks,ylim = rev(range(mag_ks)), main="Regresión MM para Ks vs. LogP", xlab="LogP", ylab="Magnitud kS", sub = "LMC",col.sub = "purple" )
#scale_y_reverse()
#add the fitted regression line to the scatterplot
abline(mm_model2, col="steelblue")

# Optional: Plot the data and the regression line with inverted y-axis
ggplot(regression_data2, aes(x = log_p, y = mag_ks)) +
  geom_point() +
  geom_abline(slope = slope2, intercept = intercept2, color = "blue",linetype="dashed") +
  labs(title = "MM Regression of Ks vs LogP",
       x = "log(Period)",
       y = "Magnitude (Ks)") +
  scale_y_reverse() +  # Invert the y-axis
  theme_minimal()

# Add annotations for slope and intercept
annotate("text", x = min(log_p), y = max(mag_ks), 
         label = paste("Slope:", round(slope2, 3), "\nIntercept:", round(intercept2, 3)), 
         hjust = 0, vjust = 1, size = 4, color = "red")
