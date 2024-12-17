library(dplyr)
library(ggplot2)
library(corrplot)
library(cluster)
library(factoextra)
library(randomForest)
library(Metrics)
library(gridExtra)
library(car)
library(caret)
library(clusterR)
library(tidyr)

set.seed(71)

# Importing dataset
carsData = read.csv("Automobile_data.csv")

# Removing the irrelevant columns
data = carsData %>%
  select(-symboling, -normalized.losses)

# Replace "?" with NA for all columns
data[data == "?"] = NA

# Check for missing values or '?' in the dataset
data %>%
  summarise(across(everything(), ~ sum(is.na(.))))

# Convert columns with missing values to appropriate data types
data$horsepower = as.numeric(data$horsepower)
data$peak.rpm = as.numeric(data$peak.rpm)
data$num.of.doors = as.character(data$num.of.doors)
data$price = as.numeric(data$price)
data$bore = as.numeric(data$bore)
data$stroke = as.numeric(data$stroke)

# Handle missing values for numerical columns by replacing with the median
data$horsepower[is.na(data$horsepower)] = median(data$horsepower, na.rm = TRUE)
data$peak.rpm[is.na(data$peak.rpm)] = median(data$peak.rpm, na.rm = TRUE)
data$price[is.na(data$price)] = median(data$price, na.rm = TRUE)
data$bore[is.na(data$bore)] = median(data$bore, na.rm = TRUE)
data$stroke[is.na(data$stroke)] = median(data$stroke, na.rm = TRUE)

# Handle missing values for categorical columns by replacing with the highest occuring value
data$num.of.doors[is.na(data$num.of.doors)] = "four"

# Replacing a typo with the most occuring value
data$engine.type[data$engine.type == "l"] = "ohc"
# Check again for missing values
data %>%
  summarise(across(everything(), ~ sum(is.na(.))))

# Converting categorical columns into factors
categoricalColumns = names(data)[!sapply(data, is.numeric)]

for (col in categoricalColumns) {
  data[[col]] = as.factor(data[[col]])
}

attach(data)

# Exploratory Data analysis

# Summary Statistics
summary(data)

# Correlation Analysis (for numeric variables)
numeric_vars = data %>%
  select_if(is.numeric)

# Checking and removing highly correlated variables
cor_matrix = cor(numeric_vars, use = "complete.obs")
corrplot(cor_matrix, method = "color", tl.cex = 0.75, title = "Correlation between numeric variables", mar = c(0, 0, 1.5, 0))

# Extracting highly correlated variables
highlyCorrelated = findCorrelation(cor_matrix, cutoff = 0.9)
highlyCorrelated
colnames(cor_matrix)[highlyCorrelated]

#Since highway and cily mileage is exactly correlated to each other, we will take average and then
#remove both columns
data = data %>%
  mutate(mileage = rowMeans(select(., highway.mpg, city.mpg), na.rm = TRUE))
data = data %>%
  select(-highway.mpg, -city.mpg)

# For outliers detection of Target Variable (Horsepower)
ggplot(data, aes(y = horsepower)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16) +
  theme_minimal() +
  labs( title = "Horsepower Outliers", y = "Horsepower")

# Removing outliers
data$zScoreHP = scale(data$horsepower)

# Remove outliers based on Z-score (greater or less than 2.5)
data = data %>%
  filter(abs(zScoreHP) <= 2.5)

data = data %>%
  select(-zScoreHP)

attach(data)
# Relationships Between Horsepower and Other Variables
# a. Scatter plots for numeric variables
ggplot(data, aes(x = engine.size, y = horsepower)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Horsepower vs Engine Size", x = "Engine Size", y = "Horsepower")

ggplot(data, aes(x = curb.weight, y = horsepower)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Horsepower vs Curb Weight", x = "Curb Weight", y = "Horsepower")

ggplot(data, aes(x = compression.ratio, y = horsepower)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Horsepower vs Compression Ratio", x = "Compression Ratio", y = "Horsepower")

ggplot(data, aes(x = mileage, y = horsepower)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Horsepower vs Average Mileage", x = "Average Mileage (MPG)", y = "Horsepower")

ggplot(data, aes(x = price, y = horsepower)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Horsepower vs Price", x = "Price", y = "Horsepower")

# b. Box plots for categorical variables
ggplot(data, aes(x = make, y = horsepower, fill = make)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(title = "Horsepower by Make", x = "Make", y = "Horsepower")

ggplot(data, aes(x = fuel.type, y = horsepower, fill = fuel.type)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Horsepower by Fuel Type", x = "Fuel Type", y = "Horsepower")

ggplot(data, aes(x = aspiration, y = horsepower, fill = aspiration)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Horsepower by Aspiration", x = "Aspiration", y = "Horsepower")

ggplot(data, aes(x = num.of.doors, y = horsepower, fill = num.of.doors)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Horsepower by Number of Doors", x = "Number of Doors", y = "Horsepower")

ggplot(data, aes(x = body.style, y = horsepower, fill = body.style)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Horsepower by Body Style", x = "Body Style", y = "Horsepower")

grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)

plot1 = ggplot(data, aes(x = drive.wheels, y = horsepower, fill = drive.wheels)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Horsepower by Drive Wheels", x = "Drive Wheels", y = "Horsepower")

ggplot(data, aes(x = engine.location, y = horsepower, fill = engine.location)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Horsepower by Engine Location", x = "Engine Location", y = "Horsepower")
# After outliers, we can ignore this variable since there is no variation
data = data %>%
  select(-engine.location)
attach(data)

plot2 = ggplot(data, aes(x = engine.type, y = horsepower, fill = engine.type)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Horsepower by Engine Type", x = "Engine Type", y = "Horsepower")

plot3 = ggplot(data, aes(x = num.of.cylinders, y = horsepower, fill = num.of.cylinders)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Horsepower by Number of Cylinders", x = "Number of Cylinders", y = "Horsepower")

plot4 = ggplot(data, aes(x = fuel.system, y = horsepower, fill = fuel.system)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Horsepower by Fuel System", x = "Fuel System", y = "Horsepower")

# Train a Random Forest model for feature selection
rf_model = randomForest(horsepower ~ ., data = data, importance = TRUE)

# Check feature importance
importance(rf_model)

# Plot feature importance
varImpPlot(rf_model)

# Create a subset with only the important features
dataSelected = data %>%
  select(-num.of.doors, -body.style, -height, -drive.wheels)

# Removing price for a more specs focused analysis
dataSelected = dataSelected %>%
  select(-price)

# Apply hierarchical clustering
gowerDist = daisy(dataSelected, metric = "gower")
hc = hclust(gowerDist)
plot(hc)

# Adding clusters
clusters = cutree(hc, k = 5)

# Add the clusters to your data
dataSelected$cluster = as.factor(clusters)
dataSelected$cluster

# Cluster Numerical summary
clusterNumSummary <- dataSelected %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), list(mean = ~ mean(.), sd = ~ sd(.)), .names = "{.col}_{.fn}"))

categorical_vars = c("make", "fuel.type", "aspiration", "body.style", "drive.wheels", "num.of.cylinders")

# Mode function to calculate mods of categorical variables
mode_function <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

# Calculating modes and adding it to mode_resukt
mode_results = list()

for (var in categorical_vars) {
  mode_results[[var]] = dataSelected %>%
    group_by(cluster) %>%
    summarise(mode = mode_function(!!sym(var)))
}

finalSummary = clusterNumSummary

# Add categorical mode results
for (var in categorical_vars) {
  finalSummary[[paste(var, "mode", sep = "_")]] = mode_results[[var]]$mode
}

# Final Summary
finalSummary

# Writing it to a csv file to explore it
write.csv(finalSummary, "summary.csv", row.names = FALSE)

# Train the Random Forest model
rf_model1 = randomForest(horsepower ~ ., data = dataSelected, importance = TRUE)
rf_model1

importance(rf_model1)
varImpPlot(rf_model1)

# Removing the weakest feature
dataSelected = dataSelected %>%
  select(-fuel.type )


# Train the Random Forest model
rf_model2 = randomForest(horsepower ~ ., data = dataSelected, importance = TRUE, ntree = 500, do.trace=50)
rf_model2

importance(rf_model2)
varImpPlot(rf_model2)

dataSelected1 = dataSelected %>%
  select(-make, -length)


# Train the Random Forest model
rf_model3 = randomForest(horsepower ~ ., data = dataSelected1, importance = TRUE, ntree = 500, do.trace=50)
rf_model3

importance(rf_model3)
varImpPlot(rf_model3)

prediction = rf_model3$predicted
actual = dataSelected1$horsepower

residuals = actual - prediction
residuals
plot(residuals, main = "Residuals", ylab = "Residuals", xlab = "Observations")
abline(h = 0, col = "red")
