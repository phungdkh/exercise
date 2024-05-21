# Get the library.
library(plotrix)
library("readxl")


students <- read_excel("student-mat.xlsx")
students
sexs = students[[2]]
mEdu = students[[7]]


# Plot the bar chart
# Plot the bar chart.
pie(mEdu, sexs)
