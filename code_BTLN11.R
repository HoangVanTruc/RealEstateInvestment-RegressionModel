getwd()
#setwd("C:/Users/Admin/OneDrive/Tài liệu/PTDL & RQD/BTL")

#install.packages(c("readr", "dplyr", "ggplot2", "forecast", 
#                   "Metrics", "caret", "tseries", "pROC", "psych))
library(readr)
library(dplyr)
library(ggplot2)
library(forecast)
library(Metrics)
library(caret)
library(tseries)
library(pROC)

data <- read.csv("C:/Users/Admin/OneDrive/Tài liệu/PTDL & RQD/BTL/Dữ liệu bds.csv", header = TRUE, sep = ",")

head(data)
names(data)
str(data)

# Xu ly dl
data <- data %>% select(-STT)

data$Ngay.giao.dich <- as.Date(data$Ngay.giao.dich, format = "%m/%d/%Y")

data$Quyet.dinh.dau.tu <- ifelse(data$Quyet.dinh.dau.tu == "yes", 1, 0)

sum(is.na(data)) 
data <- na.omit(data)

# Visualize
## 1. Gia bat dong san theo thgian
ggplot(data, aes(x = Ngay.giao.dich, y = Gia.trung.binh.tren.mot.don.vi.dien.tich)) +
  geom_line() +
  labs(title = "Giá bất động sản theo thời gian", x = "Thời gian", y = "Giá trung bình") +
  theme_minimal()

## 2. Qhe giua Gia va Khoang cach
ggplot(data, aes(x = Khoang.cach.toi.cac.tram.MRT.gan.nhat, y = Gia.trung.binh.tren.mot.don.vi.dien.tich)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(title = "Mối quan hệ giữa giá và khoảng cách tới MRT", x = "Khoảng cách tới MRT", y = "Giá trung bình") +
  theme_minimal()

## 3. Tan suat Quyet dinh dau tu
ggplot(data, aes(x = as.factor(Quyet.dinh.dau.tu))) +
  geom_bar(fill = "orange") +
  labs(title = "Tần suất các quyết định đầu tư", x = "Quyết định đầu tư (0: Không, 1: Có)", y = "Tần suất") +
  theme_minimal()

# Hoi quy logistic
logistic_model <- glm(Quyet.dinh.dau.tu ~ So.nam.su.dung.dat + 
                        Khoang.cach.toi.cac.tram.MRT.gan.nhat + 
                        So.cua.hang.tien.loi.gan.do +
                        Gia.trung.binh.tren.mot.don.vi.dien.tich, 
                      data = data, family = "binomial")

summary(logistic_model)

# Thống kê dự báo
data$Predicted_Prob <- predict(logistic_model, type = "response")

# ROC
roc_curve <- roc(data$Quyet.dinh.dau.tu, data$Predicted_Prob)
plot(roc_curve, col="blue", main="ROC Curve for Logistic Regression")
auc(roc_curve)  # Tính AUC

# Conf matrix
data$Predicted_Decision <- ifelse(data$Predicted_Prob >= 0.5, 1, 0)
conf_matrix <- table(Predicted = data$Predicted_Decision, Actual = data$Quyet.dinh.dau.tu)
conf_matrix
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy: ", round(accuracy * 100, 2), "%"))

# ARIMA
# Chuẩn bị dữ liệu thời gian
data <- data[order(data$Ngay.giao.dich), ]
price_series <- ts(data$Gia.trung.binh.tren.mot.don.vi.dien.tich, frequency = 12)

# Kiểm tra tính dừng 
adf_test <- adf.test(price_series)
print(adf_test)

# Xây dựng mô hình 
arima_model <- auto.arima(price_series)
summary(arima_model)

# Dự báo 
forecast_arima <- forecast(arima_model, h=12)

# visualize
autoplot(forecast_arima) +
  labs(title="ARIMA Forecast", x="Time", y="Price") +
  theme_minimal()

# MAPE
actual_values <- tail(price_series, length(forecast_arima$mean))
predicted_values <- as.numeric(forecast_arima$mean)
if (length(actual_values) == length(predicted_values)) {
  mape <- mean(abs((predicted_values - actual_values) / actual_values)) * 100
  print(paste("MAPE of ARIMA: ", round(mape, 2), "%"))
} else {
  print("Error: Actual values and predicted values do not match in length.")
}
