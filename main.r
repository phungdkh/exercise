# import helper.r
source("helper.r")

# installing libs
using("dplyr", "magrittr", "ggplot2", "gridExtra")

# use libs
library(dplyr)
library(magrittr)
library(ggplot2)
library(gridExtra)

# Preprocessing

# Step1: Reading data from csv file
data <- read.csv("test_data_source.csv")
View(data)

# Step 2: clean rows missing values
rows_cleaned_data <- na.omit(data)
View(rows_cleaned_data)

# Step 3: clean cols missing values
# In the test data the condition col is missing values
cols_cleaned_data <- data[, colSums(is.na(data)) == 0]
View(cols_cleaned_data)

# Step 4: Calculate the mean of each column, ignoring missing values
cols_mean <- sapply(data, function(col) {
  if (is.numeric(col)) {
    mean(col, na.rm = TRUE)
  } else {
    col
  }
})

# Step 5: Replace missing values with the calculated means
data_mean_filled <- data
for (i in seq_along(data_mean_filled)) {
  if (is.numeric(data_mean_filled[[i]])) {
    data_mean_filled[[i]][is.na(data_mean_filled[[i]])] <- cols_mean[[i]]
  }
}

# Step 6: Calculate the median of each column, ignoring missing values
cols_median <- sapply(data, function(col) {
  if (is.numeric(col)) {
    median(col, na.rm = TRUE)
  } else {
    col
  }
})

# Step 7: Replace missing values with the calculated medians
data_median_filled <- data
for (i in seq_along(data_median_filled)) {
  if (is.numeric(data_median_filled[[i]])) {
    data_median_filled[[i]][is.na(data_median_filled[[i]])] <- cols_median[[i]]
  }
}

# Visualization
ggplot(data_mean_filled, aes(x = condition)) +
  geom_bar() +
  ggtitle("Biểu đồ thanh") +
  xlab("condition") +
  ylab("count")

barplot(table(data_mean_filled$condition),
  main = "Biểu đồ thanh",
  xlab = "condition",
  ylab = "Count",
  col = "blue"
)

# Vẽ biểu đồ tần suất cho biến định lượng 'price'
ggplot(data_mean_filled, aes(x = sellingprice)) +
  geom_histogram(binwidth = 1000, fill = "green", color = "pink") +
  ggtitle("Histogram of Car Prices") +
  xlab("sellingprice") +
  ylab("Frequency")


# Vẽ boxplot cho cột 'mmr'
ggplot(data_mean_filled, aes(x = "", y = mmr)) +
  geom_boxplot(fill = "skyblue", color = "blue") +
  ggtitle("Boxplot of MMR") +
  xlab("") +
  ylab("mmr")

# Vẽ biểu đồ mật độ cho cột 'condition'
ggplot(data_mean_filled, aes(x = condition)) +
  geom_density(fill = "gray", alpha = 0.5) +
  ggtitle("Density Plot of condition") +
  xlab("Value") +
  ylab("Density")

# 2 biến phân loại/ định tính/ qualitative variable (categorial).

# Tính toán số lượng mỗi màu sắc
color_counts <- table(data_mean_filled$color)

# Vẽ biểu đồ tròn
pie(color_counts,
  labels = names(color_counts),
  main = "Pie Chart of Color Distribution"
)

ggplot(data_mean_filled, aes(x = "", fill = condition)) +
  geom_bar(width = 1, fill = "yellow") +
  coord_polar("y") +
  labs(title = "Pie Chart of Condition", fill = "Condition")

# Vẽ biểu đồ heaheatmap
ggplot(data_mean_filled, aes(x = color, y = condition, fill = odometer)) +
  geom_tile() +
  labs(
    title = "Heatmap of Odometer by Color and Condition",
    x = "Color", y = "Condition", fill = "Odometer"
  )

# vẽ biểu đồ xếp chồng
ggplot(data_mean_filled, aes(x = condition, fill = color)) +
  geom_bar() +
  labs(
    title = "Stacked Bar Chart of Color by Condition",
    x = "Condition", y = "Count"
  )

# Biểu đồ Scatterplot:
ggplot(data_mean_filled, aes(x = odometer, y = sellingprice)) +
  geom_point() +
  labs(
    title = "Scatterplot of Odometer vs Selling Price",
    x = "Odometer", y = "Selling Price"
  )

# Biểu đồ Density plot
ggplot(data_mean_filled, aes(x = sellingprice)) +
  geom_density(fill = "orange", color = "black") +
  labs(
    title = "Density Plot of Selling Price",
    x = "Selling Price", y = "Density"
  )

# Linebox
ggplot(data_mean_filled, aes(x = year, y = sellingprice)) +
  geom_line() +
  labs(
    title = "Line Plot of Year vs Selling Price",
    x = "Year", y = "Selling Price"
  )

# Vẽ biểu đồ cột
ggplot(data_mean_filled, aes(x = condition, y = odometer)) +
  geom_bar(stat = "summary", fun = "median", fill = "red") +
  labs(
    title = "Median Odometer Reading by Condition",
    x = "Condition",
    y = "Median Odometer"
  )

# Vẽ biểu đồ violin
ggplot(data_mean_filled, aes(x = condition, y = odometer, fill = condition)) +
  geom_violin() +
  labs(
    title = "Violin Plot of Odometer Reading by Condition",
    x = "Condition",
    y = "Odometer Reading",
    fill = "Condition"
  )

# Vẽ biểu đồ dò
ggplot(data_mean_filled, aes(x = condition, y = odometer, color = condition)) +
  geom_point() +
  labs(
    title = "Scatter Plot of Odometer Reading by Condition",
    x = "Condition",
    y = "Odometer Reading",
    color = "Condition"
  )
#
p1 <- ggplot(data_mean_filled, aes(x = condition, y = odometer)) +
  geom_bar(stat = "summary", fun = "mean", fill = "skyblue") +
  labs(
    title = "Average Odometer by Condition",
    x = "Condition", y = "Average Odometer"
  )

# Vẽ biểu đồ boxplot
p2 <- ggplot(data_mean_filled, aes(x = color, y = sellingprice)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "Boxplot of sellingprice by color",
    x = "color", y = "sellingprice"
  )

p1
p2
