# Green-Financial-Index
This research  honed my skills in econometric modeling, data analysis, and index construction. Through secondary data collection, cleaning, and the Low Entropy Weight model, I assessed green finance efficiency. This experience enables me to apply similar quantitative methods in economics, enhancing my research and career prospects.
library(dplyr)
inputs <- DEAdata[, 2:11]  # First 10 columns for inputs
outputs <- DEAdata[, 12:14]  # Last 3 columns for outputs
cat("Number of columns in inputs:", ncol(inputs), "\n")
cat("Number of columns in outputs:", ncol(outputs), "\n")
# Step 3: Normalize the data (Min-Max normalization)
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
# Normalize inputs and outputs
inputs_normalized <- as.data.frame(lapply(inputs, normalize))
outputs_normalized <- as.data.frame(lapply(outputs, normalize))
# Step 4: Combine normalized inputs and outputs
data_normalized <- cbind(inputs_normalized, outputs_normalized)
# Verify the number of columns in data_normalized (it should be 13)
cat("Number of columns in data_normalized:", ncol(data_normalized), "\n")
# Ensure data_normalized is a numeric matrix
data_normalized <- as.matrix(data_normalized)
# Step 5: Transpose the data so that we have indicators (columns) as rows
data_transposed <- t(data_normalized)
# Step 6: Calculate the proportion of each value for each indicator
P <- data_transposed / rowSums(data_transposed)

# Step 7: Calculate the entropy for each indicator (column of the original data)
entropy <- -rowSums(P * log(P + 1e-10)) / log(ncol(data_transposed))  # Adding small value to avoid log(0)

# Step 8: Calculate the weights for each indicator
weights <- (1 - entropy) / sum(1 - entropy)

# Ensure weights is a numeric vector and matches the number of indicators (13)
weights <- as.numeric(weights)
# Verify the length of the weights vector (it should be 13, one for each indicator)
cat("Length of weights vector:", length(weights), "\n")
# Step 9: Compute the Green Financial Index by summing the weighted values of indicators
green_index <- data_normalized %*% weights
# Results: Displaying the weights and the Green Financial Index
print("Indicator Weights (Entropy-Based):")
print(weights)
print("Green Financial Index for Each Observation:")
print(green_index)
DEAdata$green_index <- green_index
head(DEAdata)
install.packages("openxlsx")  # or use writexl for simpler writing to xlsx
library(openxlsx)
# Step 2: Save the updated dataset back to an Excel file
# Specify the path where you want to save the updated file
write.xlsx(DEAdata, "Updated_DEAdata.xlsx")
