data<- read.csv("AirPassengers.csv",header=TRUE,sep=";")
str(data)
summary(data)
passenger<-ts(data$Passengers,start=c(1949),frequency=12)
plot.ts(passenger)
logpassenger<-log(passenger)
plot.ts(logpassenger)
#install.packages("TTR")
library(TTR)
datacomponents <- decompose(passenger)
datacomponents$seasonal
datacomponents$trend
plot(datacomponents)
#Mevsimsel olmayan seriyi düzlestirme-HAREKETLI ORTALAMA
datasma3<-SMA(passenger,n=3)
datasma3
plot.ts(datasma3)
datasma8<-SMA(passenger,n=8)
datasma8
plot.ts(datasma8)
#ÇARPIMSAL AYRISTIRMA
library(forecast)
model <- forecast(passenger)
print(model)
#MERKEZI HAREKETLI ORTALAMA
#install.packages("TTR")
library(TTR)
# Merkezi hareketli ortalama periyodu
period <- 5
# Merkezi hareketli ortalama hesapla
ema <- EMA(passenger, n = period)
print(ema)
#MEVSIMSEL INDEKS 
# Yolcu veri setini aylik frekansla dönüstürme
passenger_ts <- ts(passenger, start = c(1949, 1), frequency = 12)
# Mevsimsel indeksi uygun frekansa dönüstürme
seasonal_index_ts <- ts(seasonal_index, start = c(1949, 1), frequency = 12)
# Mevsimsel etkileri giderilmis verileri hesaplama
deseasonalized_data <- passenger_ts / seasonal_index_ts
deseasonalized_data
# Mevsimsel etkileri giderilmis verilerinizi görsellestirin
plot(deseasonalized_data, type = "l", xlab = "Month", ylab = "Deseasonalized Data")
#Mevsimsel Diyagramlar
#install.packages('fpp2', dependencies = TRUE)
library(fpp2)
autoplot(passenger)
ggseasonplot(passenger)
ggseasonplot(passenger, year.labels=TRUE, year.labels.left=TRUE)  + ggtitle("Seasonal plot: Passenger") 
ggseasonplot(passenger, polar=TRUE)  + ggtitle("Polar seasonal plot: Passenger") 
ggsubseriesplot(passenger) 
ggsubseriesplot(passenger) + ggtitle("Seasonal subseries plot: Passenger") 
#Serpilme Diyagrami Matrisi
autoplot(passenger, facets=TRUE) + ylab("Passengers") 
#install.packages('GGally', dependencies = TRUE)
library(GGally)
GGally::ggpairs(as.data.frame(passenger))
# Gecikme diyagramlari
passenger2 <- window(passenger, start=1950) 
#BEYAZ GÜRÜLTÜ-RANDOM WALK
# Assuming passenger is your time series data
plot(passenger, main = "Random walk")
library(ggplot2)
ggAcf(passenger)
# Assuming passenger is your time series data
acf_data <- acf(passenger, plot = FALSE)  # Compute ACF values
acf_df <- data.frame(lag = acf_data$lag, acf = acf_data$acf)  # Convert ACF values to a dataframe
ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.5) +
  labs(x = "Lag", y = "Autocorrelation", title = "Autocorrelation Function (ACF)")
library(ggplot2)
df <- data.frame(date = time(passenger), value = as.numeric(passenger))
#MEAN,NAIVE,SEASONAL NAIVE
#install.packages("forecast")
library(forecast)
p <- autoplot(passenger2)
p + autolayer(meanf(passenger2, h = 11), series = "Mean", PI = FALSE) +
  autolayer(naive(passenger2, h = 11), series = "Naïve", PI = FALSE) +
  autolayer(snaive(passenger2, h = 11), series = "Seasonal naïve", PI = FALSE)
#NAIFTEN SONRA KALINTILAR
res <- residuals(naive(passenger2))
autoplot(res) + xlab("Year") + ylab("") +
  ggtitle("Residuals from naive method")
#OTOKORALASYON IÇIN PORTMANTO TESTI
Box.test(res, lag=10)  
Box.test(res,lag=10, type="Lj")  

#ÜSTEL DÜZLESTIRME YÖNTEMLERI
new<-autoplot(passenger) +labs(y = "% of GDP", title = "Passengers")
# Estimate parameters
fit <- passenger |>
  ets(model = "AAN")
library(broom)
fit <- ets(passenger, model = "AAN")
summary(fit)
library(forecast)
forecast_values <- forecast(fit)
fitted_values <- fitted(forecast_values)

autoplot(passenger) +
  geom_line(aes(y = fitted_values), col = "#D55E00") +
  labs(y = "Yillar", title = "Passenger") +
  guides(colour = "none")

#install.packages("fpp3")
library("fpp3")
passenger
passenger <- data.frame(Month = month.abb, Passengers = passenger)
passenger$Month <- factor(passenger$Month, levels = month.abb)
passenger$Year <- rep(c(1949:1960), each = 12)

passenger_1950 <- subset(passenger, Year == 1950)
passenger_1950
library(ggplot2)
library(tidyr)

passenger_1950_long <- passenger_1950 %>%
  mutate(Year = 1950) %>%
  pivot_longer(cols = -c(Month, Year), names_to = "Category", values_to = "Value")

ggplot(passenger_1950_long, aes(x = Month, y = Value, group = Year, color = Category)) +
  geom_line() +
  labs(y = "Passengers", title = "Passengers in 1950") +
  scale_color_manual(values = c("blue", "red"))

library("fpp3")
library(ggplot2)
library(tidyr)
# Holt's Linear Trend yöntemini uygulama
fit_holt <- holt(data$Passengers)
# Tahmin yapma
fc_holt <- forecast(fit_holt, h = 5)
# Tahmin sonuçlarini yazdirma
print(fc_holt)
# Tahmin sonuçlarini görsellestirme
autoplot(fc_holt) +
  labs(y = "Passengers", title = "Forecast using Holt's Linear Trend Method")

# ETS modelini uygulama
passenger<-ts(data$Passengers,start=c(1949),frequency=12)
fit <- ets(passenger)

# Tahmin yapma
fc <- forecast(fit, h = 5)

# Tahmin sonuçlarini yazdirma
print(fc)
autoplot(fc) +
  labs(y = "Passengers", title = "Forecast using ETS Model")


##ENDEKSLER GÖRSELLESTIRME
# Verileri ve endeksleri tanimlama
enflasyon_oranlari <- c(10.38, 6.34, 6.41, 10.45, 6.16)
laspeyres_miktar_endeksi <- 273.33
laspeyres_fiyat_indeksi <- 241.37
paasche_fiyat_indeksi <- 98.53
paasche_miktar_indeksi <- 98.53
fisher_miktar_indeksi <- 164.10
fisher_fiyat_indeksi <- 154.21

# Verileri bir veri çerçevesine dönüstürme
veriler <- data.frame(
  Endeks = c("Laspeyres Miktar Endeksi", "Laspeyres Fiyat Indeksi", "Paasche Fiyat Indeksi", 
             "Paasche Miktar Indeksi", "Fisher Miktar Indeksi", "Fisher Fiyat Indeksi"),
  Deger = c(laspeyres_miktar_endeksi, laspeyres_fiyat_indeksi, paasche_fiyat_indeksi,
            paasche_miktar_indeksi, fisher_miktar_indeksi, fisher_fiyat_indeksi),
  Enflasyon_Orani = c(0, enflasyon_oranlari)
)

# Grafik olusturma
library(ggplot2)
ggplot(data = veriler, aes(x = reorder(Endeks, -Deger), y = Deger, fill = Endeks)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_line(aes(group = 1, y = Enflasyon_Orani, color = "Enflasyon Orani")) +
  geom_text(aes(label = round(Deger, 2)), vjust = -0.5, color = "black", size = 3) +
  geom_point(aes(y = Enflasyon_Orani), color = "red", size = 3) +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "Endeks Karsilastirmasi ve Enflasyon Oranlari",
       x = "Endeks",
       y = "Deger / Enflasyon Orani",
       fill = "Endeks",
       color = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_log10()

                                     
