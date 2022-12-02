setwd("/Users/viktort/Desktop/Data Processing Software/")

df <- readRDS("hike_data.rds")


# Part 1: Tidying dataset
#1.  Convert columns `gain`, `highpoint`, `rating` to numeric values.
gain = as.numeric(df$gain)
highpoint = as.numeric(df$highpoint)
rating = as.numeric(df$rating)

#2.  Add new column `trip` with the type of trip from column `length` ("roundtrip", "trails", "one-way").
trip <- c()
for (i in strsplit(df$length, split = " ")){
  trip <- c(trip, tail(i, 1))
}

#3.  Add new column `length_total` with the route length from column `length`, considering that for "one-way" trip you must double the route length.
length_total <- c()
for (i in strsplit(df$length, split = " ")){
  mile <-as.numeric(head(i, 1))
  if (tail(i, 1) == "one-way") {
    mile <- mile * 2
  } 
  length_total <- c(length_total, mile)
}

#4.  Add new column `location_general` with location from column `location` (a part before "--").
location_general <- c()
for (i in strsplit(df$location, split = " -- ")){
  location_general <- c(location_general, head(i, 1))
}

#5. Add column `id` with row number
id = seq.int(nrow(df))

#Create clean_hike_trails
clean_hike_trails <- data.frame(gain, highpoint, rating, trip, length_total, location_general, id=id)
head(clean_hike_trails)



### Questioning dataset

# Question 1. How many routes have rating more than 4.9
nrow(subset(clean_hike_trails, rating > 4.9))

# Question 2. How many routes are "Good for kids" (hint: you can use (`unnest` function)?
c=0
for (i in df$features){
  if (length(i) > 0) {
    for (n in length(i)) {
      if (i[n]=="Good for kids"){
        c=c+1
      }
    }
  }
}
print(c)

# Question 3. Which unique features can routes have?
unique_features <- c()
print(typeof(unique_features))
for (i in df$features){
  if (length(i) > 0) {
    for (n in length(i)) {
      unique_features <- c(unique_features, i[n])
    }
  }
}

unique(unique_features)

# Question 4. What is the most common rating of a route?
names(tail(sort(table(clean_hike_trails$rating)), 1))

# Question 5. Your own question and answer.
## How many trips is one way
nrow(subset(clean_hike_trails, trip =="one-way"))

