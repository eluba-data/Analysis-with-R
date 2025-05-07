install.packages("tidyverse")
colnames(Healthcare_Diabetes)
summary(Healthcare_Diabetes)
colSums(is.na(Healthcare_Diabetes))
cor(Healthcare_Diabetes[,c("Glucose", "BloodPressure","SkinThickness","Insulin","BMI","Age")])
ggplot(Healthcare_Diabetes, aes(x = BMI, y = Glucose, color=Outcome)) + 
  geom_point()
library(corrplot)
install.packages("corrplot")
correlation_matrix <- cor(Healthcare_Diabetes[, c("Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI", "Age")])
corrplot(correlation_matrix, method = "color")
install.packages("GGally")
library("GGally")
ggpairs(Healthcare_Diabetes[,c("Glucose", "BloodPressure", "BMI", "Age","Diabetes Indicator")])
Healthcare_Diabetes$`Diabetes Indicator` <- as.factor(Healthcare_Diabetes$`Diabetes Indicator`)
ggpairs(
  Healthcare_Diabetes[, c("Glucose", "BloodPressure", "BMI", "Age", "Diabetes Indicator")],
  mapping = aes(color = `Diabetes Indicator`)
)
install.packages("pROC")
library(pROC)
model <-glm(`Diabetes Indicator` ~ Glucose + BMI + Age, data= Healthcare_Diabetes, family = "binomial")
roc_curve <- roc(Healthcare_Diabetes$`Diabetes Indicator`, predict(model, type = "response"))
plot(roc_curve, main="ROC Curve for Diabetes Prediction Model", col="blue", lwd=2)
abline(a=0, b=1, lty=2, col="gray")
auc_value <- auc(roc_curve)
text(0.6, 0.2, paste("AUC =", round(auc_value,3)), cex=1.2)
legend("bottomright", legend=c("Model", "Random Guess"), col = c("blue", "gray"), lwd=c(2,1), lty=c(1,2))
library(ggplot2)
library(pROC)

# Extract coordinates from the ROC object
roc_data <- data.frame(
  specificity = 1 - roc_curve$specificities,
  sensitivity = roc_curve$sensitivities
)

# Create plot
library(pROC)
library(ggplot2)

# Generate the ROC object
roc_curve <- roc(Healthcare_Diabetes$`Diabetes Indicator`, fitted(model))

# Find the optimal threshold (Youden index)
coords <- coords(roc_curve, "best", ret = c("threshold", "sensitivity", "specificity"))

# Create a data frame for plotting
roc_data <- data.frame(
  specificity = rev(roc_curve$specificities),
  sensitivity = rev(roc_curve$sensitivities)
)

# Plot with proper scalar access to coords
ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  annotate("text", x = 0.6, y = 0.2, 
           label = paste("AUC =", round(auc(roc_curve), 3)), size = 5) +
  labs(
    title = "ROC Curve for Diabetes Prediction Model",
    x = "1 - Specificity (False Positive Rate)",
    y = "Sensitivity (True Positive Rate)"
  ) +
  theme_minimal() +
  coord_equal() +
  theme(
    panel.grid.minor = element_blank(),
    text = element_text(size = 12)
  )
