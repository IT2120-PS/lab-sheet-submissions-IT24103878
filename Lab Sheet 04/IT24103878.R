setwd('C:\\Users\\it24103878\\Desktop\\IT24103878')
#Q 1.
branch_data <- read.table("Exercise.txt", header = TRUE, sep = ",")
print("Dataset Imported Successfully")
print(head(branch_data))

#Q2.
str(branch_data)
# Interpretation:


# Q3
boxplot(branch_data$Sales_X1, main = "Boxplot of Sales", ylab = "Sales", 
        outline = TRUE, outpch = 8, horizontal = TRUE)


# Q4.
summary(branch_data$Advertising_X2)
iqr_advertising <- IQR(branch_data$Advertising_X2)
cat("IQR for Advertising:", iqr_advertising, "\n")

# Q5
find_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_val
  upper_bound <- Q3 + 1.5 * IQR_val
  outliers <- x[x < lower_bound | x > upper_bound]
  return(outliers)
}

# Q6
outliers_years <- find_outliers(branch_data$Years_X3)
print("Outliers in Years:")
print(outliers_years)
