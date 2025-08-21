getwd()
setwd("C:\\Users\\it24103414\\Desktop\\IT24103414")

##1
branch_data<-read.csv("Exercise.txt" ,header=TRUE)

##2
str(branch_data)
names(branch_data)

##3
boxplot(branch_data$Sales_X1, main ="Boxplot of Sales",ylab="Sales")

##4
summary(branch_data$Advertising_X2)
iqr_Advertising <- IQR(branch_data$Advertising_X2)
print(paste("IQR of advertising:", iqr_Advertising))

##5
find_outliers <- function(x){
  q1 <- quantile(x,0.25, na.rm = TRUE)
  q3 <- quantile(x,0.75, na.rm = REUE)
  IQR_val <- q3 - q1
  lower_bound <- q1 - 1.5 * IQR_val
  upper_bound <- q3 + 1.5 * IQR_val
  outlines <- X[X < lower_bound|X > upper_bound]
  return(outliers)
}
outliers_years <- find_outliers(branch_data$Years_X3)
print("Outliers in 'years' variables:")
print(outliers_years)