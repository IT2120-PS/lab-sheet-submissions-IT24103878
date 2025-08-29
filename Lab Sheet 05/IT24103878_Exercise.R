#1
getwd()
setwd("C:\\Users\\It24103878\\Desktop\\IT24103878")
getwd()
Delivery_Times <- read.table('Exercise - Lab 05.txt', header = TRUE)

#2
cat("Dataset structure:\n")
str(Delivery_Times)
cat("\nFirst few rows:\n")
head(Delivery_Times)
breaks <- seq(20, 70, length.out = 10) 

cat("\nClass intervals (right open):\n")
intervals <- paste0("(", head(breaks, -1), ", ", tail(breaks, -1), "]")
print(intervals)

hist(Delivery_Times$Delivery_Time_.minutes., 
     breaks = breaks,
     right = TRUE,  
     main = "Histogram of Delivery Times",
     xlab = "Delivery Time (minutes)",
     ylab = "Frequency",
     col = "lightblue",
     border = "black",
     xlim = c(20, 70))

cat("\n3. Shape of the distribution:\n")
dist_shape <- "The distribution appears to be approximately symmetric with a slight right skew. "
dist_shape <- paste0(dist_shape, "Most delivery times are concentrated between 35-55 minutes. ")
dist_shape <- paste0(dist_shape, "There are fewer deliveries at the extremes (very fast or very slow delivery times).")
cat(dist_shape, "\n")

freq <- hist(Delivery_Times$Delivery_Time_.minutes., breaks = breaks, plot = FALSE)$counts
cum_freq <- cumsum(freq)

cat("\nFrequency distribution:\n")
freq_table <- data.frame(Interval = intervals, Frequency = freq, Cumulative = cum_freq)
print(freq_table)

plot(breaks[-1], cum_freq,
     type = "o",
     pch = 16,
     col = "red",
     main = "Cumulative Frequency Polygon (Ogive) of Delivery Times",
     xlab = "Delivery Time (minutes)",
     ylab = "Cumulative Frequency",
     xlim = c(20, 70),
     ylim = c(0, max(cum_freq) + 5))

grid() 

text(breaks[-1], cum_freq, labels = cum_freq, pos = 3, col = "blue")

png("delivery_times_histogram.png", width = 800, height = 600)
hist(Delivery_Times$Delivery_Time_.minutes., 
     breaks = breaks,
     right = TRUE,
     main = "Histogram of Delivery Times",
     xlab = "Delivery Time (minutes)",
     ylab = "Frequency",
     col = "lightblue",
     border = "black",
     xlim = c(20, 70))
dev.off()

png("delivery_times_ogive.png", width = 800, height = 600)

plot(breaks[-1], cum_freq,
     type = "o",
     pch = 16,
     col = "red",
     main = "Cumulative Frequency Polygon (Ogive) of Delivery Times",
     xlab = "Delivery Time (minutes)",
     ylab = "Cumulative Frequency",
     xlim = c(20, 70),
     ylim = c(0, max(cum_freq) + 5))
grid()
text(breaks[-1], cum_freq, labels = cum_freq, pos = 3, col = "blue")
dev.off()

cat("\nAnalysis completed successfully!\n")
cat("Plots have been saved as 'delivery_times_histogram.png' and 'delivery_times_ogive.png'\n")


