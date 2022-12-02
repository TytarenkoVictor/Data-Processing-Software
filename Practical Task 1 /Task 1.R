setwd("/Users/viktort/Desktop/Data Processing Software/")
data <- read.csv("airquality.csv")

# Task 1.1
colnames(data)

# Task 1.2
rownames(data)

# Task 1.3 
head(data, 6)

# Task 1.4  
nrow(data)

# Task 1.5  
tail(data, 6)

# Task 1.6 
sum(is.na(data$Ozone))

# Task 1.7 
dt2 = data[!is.na(data$Ozone),]
dt2
mean(dt2$Ozone)

# Task 1.8
subset(data, data$Temp > 90 & data$Ozone > 31)


# Task 1.9
vector <- c()
for (i in 1:6)
  vector <- append(vector, mean(data[!is.na(data[, i]),][,i]))
vector

# Task 1.10
sapply(data, function(x) sd(x, na.rm = TRUE))

# Task 1.11
dt4 <- aggregate(data$Ozone, list(data$Month), function(x) mean(x, na.rm = TRUE))
print(dt4$x)


# Task 1.12
data[sample(nrow(data), 5),]

