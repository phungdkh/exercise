# installing libs
install.packages("dplyr")
install.packages("magrittr")

# use libs
library(dplyr)
library(magrittr)

# Preprocessing
data <- read.csv("car_prices.csv")
View(data)

# Xóa các hàng có giá trị missing
data_clean <- na.omit(data)
print("Data sau khi xóa các hàng chứa giá trị bị thiếu:")
print(data_clean)
print("DataFrame sau khi xóa các hàng chứa giá trị bị thiếu:")

# Xóa các cột chứa giá trị bị thiếu
data_dropna_cols <- data %>% select(where(~ !any(is.na(.))))

print("DataFrame sau khi xóa các cột chứa giá trị bị thiếu:")
print(data_dropna_cols)

# Thay thế missing values bằng giá trị trung bình của cột
data_mean <- data %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

print("DataFrame sau khi thay thế missing values bằng giá trị trung bình:")
print(data_mean)

# Thay thế missing values bằng giá trị trung vị của cột
data_median <- data %>%
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

print("DataFrame sau khi thay thế missing values bằng giá trị trung vị:")
print(data_median)

# PHẦN2_VISULIZATION

# Qualitative variable (categorial)
# Kiểm tra và cài đặt ggplot2 nếu chưa cài đặt
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# Đọc dữ liệu từ tệp CSV
data <- read.csv("car_prices.csv")

# Hiển thị tên các cột trong dataframe
print(colnames(data))

# Hiển thị một số dòng đầu tiên của dữ liệu để kiểm tra
print(head(data))
# Kiểm tra và cài đặt ggplot2 nếu chưa cài đặt
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# Giả sử tên cột là "Car_Type"
ggplot(data, aes(x = condition)) +
  geom_bar() +
  ggtitle("Biểu đồ thanh") +
  xlab("condition") +
  ylab("count")

barplot(table(data$condition),
  main = "Biểu đồ thanh",
  xlab = "condition",
  ylab = "Count",
  col = "blue"
)

# 1_bien_dịnh_luong

library(ggplot2)
# bieu_do_tan_suatsuat
# Đọc dữ liệu từ file CSV và đặt tên cột trong dấu ngoặc kép
data <- read.csv("car_prices.csv")

# Kiểm tra lại tên các cột trong tập dữ liệu
print(names(data))

# Vẽ biểu đồ tần suất cho biến định lượng 'price'
ggplot(data, aes(x = sellingprice)) +
  geom_histogram(binwidth = 1000, fill = "green", color = "pink") +
  ggtitle("Histogram of Car Prices") +
  xlab("sellingprice") +
  ylab("Frequency")


# Vẽ boxplot cho cột 'mmr'
ggplot(data, aes(x = "", y = mmr)) +
  geom_boxplot(fill = "skyblue", color = "blue") +
  ggtitle("Boxplot of MMR") +
  xlab("") +
  ylab("mmr")


# Vẽ biểu đồ mật độ cho cột 'condition'
ggplot(data, aes(x = condition)) +
  geom_density(fill = "gray", alpha = 0.5) +
  ggtitle("Density Plot of condition") +
  xlab("Value") +
  ylab("Density")

# 2 biến phân loại/ định tính/ qualitative variable (categorial).

# Tính toán số lượng mỗi màu sắc
color_counts <- table(data$color)

# Vẽ biểu đồ tròn
pie(color_counts,
  labels = names(color_counts),
  main = "Pie Chart of Color Distribution"
)
ggplot(data, aes(x = "", fill = condition)) +
  geom_bar(width = 1, fill = "yellowyellow") +
  coord_polar("y") +
  labs(title = "Pie Chart of Condition", fill = "Condition")

# Vẽ biểu đồ heaheatmap
ggplot(data, aes(x = color, y = condition, fill = odometer)) +
  geom_tile() +
  labs(
    title = "Heatmap of Odometer by Color and Condition",
    x = "Color", y = "Condition", fill = "Odometer"
  )
# vẽ biểu đồ xếp chồng
ggplot(data, aes(x = condition, fill = color)) +
  geom_bar() +
  labs(
    title = "Stacked Bar Chart of Color by Condition",
    x = "Condition", y = "Count"
  )
#
# Biểu đồ Scatterplot:
library(ggplot2)
ggplot(data, aes(x = odometer, y = sellingprice)) +
  geom_point() +
  labs(
    title = "Scatterplot of Odometer vs Selling Price",
    x = "Odometer", y = "Selling Price"
  )
# Biểu đồ Density plot
ggplot(data, aes(x = sellingprice)) +
  geom_density(fill = "orange", color = "black") +
  labs(
    title = "Density Plot of Selling Price",
    x = "Selling Price", y = "Density"
  )
# Linebox
ggplot(data, aes(x = year, y = sellingprice)) +
  geom_line() +
  labs(
    title = "Line Plot of Year vs Selling Price",
    x = "Year", y = "Selling Price"
  )
###### 5
# Sử dụng thư viện ggplot2
library(ggplot2)

# Vẽ biểu đồ cột
ggplot(data, aes(x = condition, y = odometer)) +
  geom_bar(stat = "summary", fun = "median", fill = "red") +
  labs(
    title = "Median Odometer Reading by Condition",
    x = "Condition",
    y = "Median Odometer"
  )
#
# Vẽ biểu đồ violin
ggplot(data, aes(x = condition, y = odometer, fill = condition)) +
  geom_violin() +
  labs(
    title = "Violin Plot of Odometer Reading by Condition",
    x = "Condition",
    y = "Odometer Reading",
    fill = "Condition"
  )
#
# Vẽ biểu đồ dò
ggplot(data, aes(x = condition, y = odometer, color = condition)) +
  geom_point() +
  labs(
    title = "Scatter Plot of Odometer Reading by Condition",
    x = "Condition",
    y = "Odometer Reading",
    color = "Condition"
  )
#
ggplot(data, aes(x = condition, y = odometer)) +
  geom_bar(stat = "summary", fun = "mean", fill = "skyblue") +
  labs(
    title = "Average Odometer by Condition",
    x = "Condition", y = "Average Odometer"
  )
#
# Vẽ biểu đồ boxplot
gplot(data, aes(x = color, y = sellingprice)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "Boxplot of sellingprice by color",
    x = "color", y = "sellingprice"
  )
