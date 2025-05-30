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
LMC_VI_ready <- read_excel("LMC_VI_filtrada_MM.xlsx")

# Extract the relevant columns
log_periodos_i <- LMC_VI_ready$logPI
log_periodos_v <- LMC_VI_ready$logPV
mag_v <- LMC_VI_ready$V0mean
mag_i <- LMC_VI_ready$I0mean

log_pi <- na.omit(log_periodos_i)
log_pv <- na.omit(log_periodos_v)
mean_v <- na.omit(mag_v)
mean_i <- na.omit(mag_i)

#REGRESION V

# Create a data frame for regression
regression_data <- data.frame(log_pv, mean_v)

# Perform Major Axis (MM) regression using robust method
mm_model <- lmrob(mean_v ~ log_pv, data = regression_data)
summary(mm_model)
#Graficos revision
hist(mm_model$'residuals')
boxplot(mm_model$'residuals', main="Residuals")
adjbox(mm_model$'residuals')
adjbplot<-adjbox(mm_model$'residuals')
adjbplot$out
plot.lmRob(mm_model, which.plots = 3)
plot.lmRob(mm_model, which.plots = 5)

LMCmed <- lm(mean_v ~ log_pv, data = regression_data)
influence.measures(LMCmed)
qqPlot(LMCmed$residuals)

yhat<- predict(LMCmed, se.fit =FALSE)
plot(yhat, LMCmed$residuals,xlim = rev(range(yhat)),  xlab="Predicción V", ylab="Residuos V", main="Residuos V vs. Predicción V", sub = "LMC",col.sub = "purple")
plot(log_pv, LMCmed$residuals,xlab="Log P", ylab="Residuos V", main="Residuos V vs. Log P", sub = "LMC",col.sub = "purple")

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
plot(log_pv, mean_v,ylim = rev(range(mean_v)), xlab="LogP", ylab="Magnitud V")
#scale_y_reverse()
#add the fitted regression line to the scatterplot
abline(mm_model, col="steelblue")


#Calculos para I

# Create a data frame for regression
regression_data2 <- data.frame(log_pi, mean_i)

# Perform Major Axis (MM) regression using robust method
mm_model2 <- lmrob(mean_i ~ log_pi, data = regression_data2)


LMCmed2 <- lm(mean_i ~ log_pi, data = regression_data2)
yhat2<- predict(LMCmed2, se.fit =FALSE)
plot(yhat2, LMCmed2$residuals,xlim = rev(range(yhat2)),  xlab="Predicción I", ylab="Residuos I", main="Residuos I vs. Predicción I", sub = "LMC",col.sub = "purple")
plot(log_pi, LMCmed2$residuals,xlab="Log P", ylab="Residuos I", main="Residuos I vs. Log P", sub = "LMC",col.sub = "purple")

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
plot(log_pi, mean_i,ylim = rev(range(mean_i)), xlab="LogP", ylab="Magnitud I")
#scale_y_reverse()
#add the fitted regression line to the scatterplot
abline(mm_model2, col="steelblue")

