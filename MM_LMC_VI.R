library(readxl)
library(ggplot2)  # For plotting
library(dplyr)    # For data manipulation
library(robust) 
library(robustbase)
library(car)
library(lmtest)

# Load necessary libraries
library(readr)    # For reading CSV files
library(ggplot2)  # For plotting
LMC_VI_ready <- read_excel("LMC_VI_si.xlsx")

# Extract the relevant columns
periodos <- LMC_VI_ready$P_0
log_p <- LMC_VI_ready$LogP
mean_v <- LMC_VI_ready$`mean V`
mean_i <- LMC_VI_ready$`mean I`
#REGRESION V
# Create a data frame for regression
regression_data <- data.frame(log_p, mean_v)

# Perform Major Axis (MM) regression using robust method
mm_model <- lmrob(mean_v ~ log_p, data = regression_data)
summary(mm_model)
#Graficos revision
hist(mm_model$'residuals')
boxplot(mm_model$'residuals', main="Residuals")
adjbox(mm_model$'residuals')
adjbplot<-adjbox(mm_model$'residuals')
adjbplot$out
plot.lmRob(mm_model, which.plots = 3)
plot.lmRob(mm_model, which.plots = 5)


LMCmed <- lm(mean_v ~ log_p, data = regression_data)
influence.measures(LMCmed)
qqPlot(LMCmed$residuals)

yhat<- predict(LMCmed, se.fit =FALSE)
plot(yhat, LMCmed$residuals,xlim = rev(range(yhat)),  xlab="Predicción V", ylab="Residuos V", main="Residuos V vs. Predicción V", sub = "LMC",col.sub = "purple")
plot(log_p, LMCmed$residuals,xlab="Log P", ylab="Residuos V", main="Residuos V vs. Log P", sub = "LMC",col.sub = "purple")


bptest(LMCmed, ~ log_p + I(log_p^2), data=LMC_VI_ready )

pruebHet <- lm(I(LMCmed$residuals^2) ~ I(yhat^2))
summary(pruebHet)
# Extract slope and intercept
slope <- coef(mm_model)[2]  # Slope is the second coefficient
intercept <- coef(mm_model)[1]  # Intercept is the first coefficient

# Calculate standard errors for slope and intercept
slope_se <- summary(mm_model)$coefficients[2, "Std. Error"]
intercept_se <- summary(mm_model)$coefficients[1, "Std. Error"]

# Print the results
cat("Slope:", slope, "±", slope_se, "\n")
cat("Intercept:", intercept, "±", intercept_se, "\n")
#Graph
plot(log_p, mean_v,ylim = rev(range(mean_v)), main="Regresión MM para V vs. LogP", xlab="LogP", ylab="Magnitud V", sub = "LMC",col.sub = "purple" )
#scale_y_reverse()
#add the fitted regression line to the scatterplot
abline(mm_model, col="steelblue")
# Add annotations for slope and intercept
annotate("text", x = min(log_p), y = max(mean_v), 
         label = paste("Slope:", round(slope, 3), "\nIntercept:", round(intercept, 3)), 
         hjust = 0, vjust = 1, size = 4, color = "red")
#Forzando pendiente
mm_model_forzv <- lmrob(mean_v ~ 1+offset(-2.67*log_p), data = regression_data)
summary(mm_model_forzv)

#Calculos para I

# Create a data frame for regression
regression_data2 <- data.frame(log_p, mean_i)

# Perform Major Axis (MM) regression using robust method
mm_model2 <- lmrob(mean_i ~ log_p, data = regression_data2)


LMCmed2 <- lm(mean_i ~ log_p, data = regression_data2)
yhat2<- predict(LMCmed2, se.fit =FALSE)
plot(yhat2, LMCmed2$residuals,xlim = rev(range(yhat2)),  xlab="Predicción I", ylab="Residuos I", main="Residuos I vs. Predicción I", sub = "LMC",col.sub = "purple")
plot(log_p, LMCmed2$residuals,xlab="Log P", ylab="Residuos I", main="Residuos I vs. Log P", sub = "LMC",col.sub = "purple")



# Extract slope and intercept
slope2 <- coef(mm_model2)[2]  # Slope is the second coefficient
intercept2 <- coef(mm_model2)[1]  # Intercept is the first coefficient

# Calculate standard errors for slope and intercept
slope_se2 <- summary(mm_model2)$coefficients[2, "Std. Error"]
intercept_se2 <- summary(mm_model2)$coefficients[1, "Std. Error"]

# Print the results
cat("Slope:", slope2, "±", slope_se2, "\n")
cat("Intercept:", intercept2, "±", intercept_se2, "\n")

#Graph
plot(log_p, mean_i,ylim = rev(range(mean_i)), main="Regresión MM para I vs. LogP", xlab="LogP", ylab="Magnitud I", sub = "LMC",col.sub = "purple" )
#scale_y_reverse()
#add the fitted regression line to the scatterplot
abline(mm_model2, col="steelblue")
# Add annotations for slope and intercept
annotate("text", x = min(log_p), y = max(mean_i), 
         label = paste("Slope:", round(slope, 3), "\nIntercept:", round(intercept, 3)), 
         hjust = 0, vjust = 1, size = 4, color = "red")
