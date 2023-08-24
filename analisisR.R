library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

# Cargar datos ------------------------------------------------------------
data <- read.table(file = "microclimate.csv", header = T, sep = ";")
data$date <- as.Date(data$date, format ="%m-%d-%y")
data$date_time<-strptime(paste(data$date,data$time),format= "%Y-%m-%d %H:%M:%S") 


# Crear un gráfico con dos líneas (variables de respuesta) - Temp  --------
plot(data$date_time, data$temp,
     type = "l",
     ylab = "Air temperature (°C)",
     xlab = "Time",
     xaxt = "n",
     col = "red",
     ylim = c(0, 100))

r <- round(range(data$date_time), "hours")
axis.POSIXct(side = 1, at = seq(r[1], r[2], by = "hour"), format = "%a %H")
abline(h = mean(data$temp), col = "red")

# Vamos a agregar una nueva variable a nuestro gráfico, en este caso la humedad
lines(data$date_time, data$hum, col = "blue")
abline(h = mean(data$hum), col = "blue", lty = "dashed")

# Vamor a agregar una leyenda
legend("center", c("Temperature", "Humidity"),
       fill = c("red", "blue"))

# Agregar varios gráficos en una misma página -----------------------------
par(mfcol = c(2,1))

#Primer gráfico para temperatura
plot(data$date_time,data$temp,
     type = "l",
     ylab = "Air temperature (ºC)",
     xlab = "Time",
     xaxt = "n",
     col = "red")
r <- round(range(data$date_time), "hours")
axis.POSIXct(1, at = seq(r[1], r[2], by = "hour"), format = "%H %a")
abline(h = mean(data$temp), col = "red")

#Segundo gráfico para humedad
plot(data$date_time,data$hum,
     type = "l",
     ylab = "Relative humidity (%)",
     xlab = "Time",
     xaxt = "n",
     col = "blue")
r <- round(range(data$date_time), "hours")
axis.POSIXct(1, at = seq(r[1], r[2], by = "hour"), format = "%H %a")
abline(h = mean(data$hum), col = "blue")


# Crear gráficos con ggplot2 -----------------------------------------------
library(ggplot2)
library(scales)

plot_1 <- ggplot(data = data)
plot_1

plot_1 <- ggplot(data = data) +
  geom_line(mapping = aes(x = as.POSIXct(date_time), y = temp), col = "red") +
  scale_x_datetime(labels = date_format("%H %a"), date_breaks = "12 hours")
plot_1

plot_1 <- ggplot(data = data) +
  geom_line(mapping = aes(x = as.POSIXct(date_time), y = temp), col = "red") +
  scale_x_datetime(labels = date_format("%H %a"), date_breaks = "12 hours")+
  xlab("Time")+
  ylab("Air temperature (°C)")+
  geom_hline(yintercept = mean(data$temp), col = "red")
plot_1

plot_2 <- ggplot(data = data, aes(x= as.POSIXct(date_time))) +
  geom_line(aes(y = temp, col = "Temperature"))+
  geom_line(aes(y = hum, col = "Humidity"))+
  scale_color_manual(values = c("Temperature" = "red", "Humidity" = "blue"))+
  theme(legend.position = "top")+
  xlab("Time")+
  ylab("Microclimatic variable")+
  geom_hline(yintercept = mean(data$temp), col = "red")+
  geom_hline(yintercept = mean(data$hum), col = "blue")+
  scale_x_datetime(labels = date_format("%H %a"), date_breaks = "12 hours")+
  scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0,100))
plot_2


# Vamos a exportar nuestro gráfico ----------------------------------------
ggsave(filename = "grafico.svg", plot = plot_2,
       width = 20 ,
       height = 13,
       units = "cm",
       dpi = 1000)


