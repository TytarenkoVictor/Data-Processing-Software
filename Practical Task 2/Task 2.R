setwd("/Users/viktort/Desktop/Data Processing Software/")



# Part 1
pollutantmean <- 
  function(directory, pollutant, ids) {
    avg <- c()
    for (i in ids) {
      data_i <- read.csv(paste(getwd(),"/",directory, "/", sprintf("%03d", i),".csv", sep=""))
      dt <- data_i[pollutant]
      avg <- c(avg, dt[!is.na(dt)])
    }
    mean(avg)
  }

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)

# Part 2
complete <- function(directory, ids){
  count <- data.frame(id=numeric(0), nobs=numeric(0))
  for (i in ids) {
     data_i <- read.csv(paste(getwd(),"/",directory, "/", sprintf("%03d", i),".csv", sep=""))
     data_i <- data_i[(!is.na(data_i$sulfate)), ]
     data_i <- data_i[(!is.na(data_i$nitrate)), ]
     count <- rbind(count, data.frame(id=i, nobs=nrow(data_i)))
  }
  count
}

complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)

# Part 3
corr <- function(directory, threshold ) {
  complete_f <- complete(directory, 1:332)
  complete_f <- complete_f[complete_f$nobs>=threshold, ]
  corr_data <- numeric(0)
  if(nrow(complete_f)>0){
    for(i in complete_f$id){
      data_i <- read.csv(paste(getwd(),"/",directory, "/", sprintf("%03d", i),".csv", sep=""))
      s <- data_i[(!is.na(data_i$sulfate)), ]
      s <- s[(!is.na(s$nitrate)), ]
      corr_data <- c(corr_data, cor(s["sulfate"], s["nitrate"]))
    }
  }
  corr_data
}

cr <- corr("specdata", 150)
head(cr)
## [1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814
summary(cr)
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## -0.21057 -0.04999  0.09463  0.12525  0.26844  0.76313

cr <- corr("specdata", 400)
head(cr)
## [1] -0.01895754 -0.04389737 -0.06815956 -0.07588814  0.76312884 -0.15782860
summary(cr)
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## -0.17623 -0.03109  0.10021  0.13969  0.26849  0.76313

cr <- corr("specdata", 5000)
summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##
length(cr)
## [1] 0


