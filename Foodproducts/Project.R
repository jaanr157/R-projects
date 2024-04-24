### Importing all the needed libraries.
library(dplyr)
library(tidyr)
library(lubridate)
library(tibbletime)
library(data.table)
library(ggplot2)

### Importing the datasets from the github and converting them into a dataframe.
url1 = "https://raw.githubusercontent.com/jsicotte/data-analyst-exercise/main/detroit_purchases.csv" 
detroit_purchases = read.csv(url1)
detroit_purchases = data.frame(detroit_purchases)
head(detroit_purchases)
View(detroit_purchases)
summary.data.frame(detroit_purchases)
### Removing the $.
detroit_purchases$amount <- gsub('[$]','', detroit_purchases$amount)
### Changing the data type of amount from character to mumeric.
detroit_purchases$amount = as.numeric(as.character(detroit_purchases$amount)) 
print(sapply(detroit_purchases, class))
summary.data.frame(detroit_purchases)

url2 = "https://raw.githubusercontent.com/jsicotte/data-analyst-exercise/main/new_york_purchases.csv"
new_york_purchases = read.csv(url2)
new_york_purchases = data.frame(new_york_purchases)
head(new_york_purchases)
View(new_york_purchases)
print(sapply(new_york_purchases, class))
summary.data.frame(new_york_purchases)

### Changing the product line to a normalized version.
new_york_purchases$type

new_york_purchases$type[new_york_purchases$type %in% c('cakes','pizzas','puffs')]<-'bakery'
new_york_purchases$type[new_york_purchases$type %in% c('milk','cheese')]<-'dairy'
new_york_purchases$type[new_york_purchases$type %in% c('tomato','carrot','beans')]<-'vegetable'
View(new_york_purchases)

#### Merging the two datasets.
merged_data <- rbind(detroit_purchases,new_york_purchases)
View(merged_data)


### Changing the time stamp to date year_month_day_hour_minute_second format.
merged_data$purchase_timestamp =  ymd_hms(merged_data$purchase_timestamp)
print(sapply(merged_data, class))

### Sorting the data according to the date and time.
merged_data <- setorder(merged_data,purchase_timestamp)
View(merged_data)

## Filtering the merged data for the date 2023-01-02.
filter_data = merged_data %>% filter(between(purchase_timestamp, as.Date('2023-01-02'), as.Date('2023-01-03')))
View(filter_data)

### Total revenue of each items per day.
grouped_data = filter_data %>% group_by(type)  %>%
  summarise(total_sales = sum(amount),
            .groups = 'drop')
grouped_data = as.data.frame(grouped_data)
View(grouped_data)
print(sapply(grouped_data, class))

### Create the bar chart
barplot(height = grouped_data$total_sales, names = grouped_data$type)
title(xlab = "Total_Revenue", ylab = "Products")

### Format only the hourly data 
filter_data$hours <- as.numeric(format(as.POSIXct(filter_data$purchase_timestamp), format = "%H"))
View(filter_data)
print(sapply(filter_data, class))

### Count the number of items per hour
Count_data <- filter_data %>% group_by(hours) %>% 
  summarise(Count = n())
View(Count_data)
print(sapply(Count_data, class))

## Creating the histogram.
Histogram = Count_data %>% 
  ggplot(aes(x = hours)) +
  geom_histogram(bins = 6)
Histogram






