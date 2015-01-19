# Business Analytics
# Data Preparation

# Load data
data = read.csv("raw_data.csv")

# Show structure of data
str(data)

# Separator is ";"
data = read.csv("raw_data.csv", sep=";")
# data = read.csv("raw_data.csv", sep=";", colClasses=c("a9"="factor"))
str(data)

# Rename columns
names(data)
names(data)[names(data) == "od"] = "order_date"
names(data)[names(data) == "dd"] = "delivery_date"
names(data)[names(data) == "a6"] = "salutation"
names(data)[names(data) == "a7"] = "date_of_birth"
names(data)[names(data) == "a8"] = "state"
names(data)[names(data) == "a9"] = "return_shipment"
str(data)

# "salutation" is a nominal attribute
table(data$salutation)
data$salutation = factor(data$salutation, labels=c("Company", "Mr.", "Mrs."))
table(data$salutation)

# "state" is a nominal attribute as well
table(data$state)
data$state = factor(data$state, labels=c("BW", "BY", "BE", "BB", "HB", "HH", "HE", "MV", "NI", "NW", "RP", "SL", "SN", "ST", "SH", "TH"))
table(data$state)

# "return_shipment" is a nominal attribute
table(data$return_shipment)
data$return_shipment = factor(data$return_shipment, labels=c("no", "yes"))
table(data$return_shipment)

# unify "size" column
table(data$size, useNA="always")
data$size[data$size == "s"] = "S"
data$size[data$size == "m"] = "M"
data$size[data$size == "l"] = "L"
data$size[data$size == "xl"] = "XL"
data$size[data$size == "xxl"] = "XXL"
data$size = factor(data$size, levels=c(levels(data$size), "XXXL"))
data$size[data$size == "xxxl"] = "XXXL"
table(data$size, useNA="always")
data$size = factor(data$size)
table(data$size, useNA="always")
# or
data$size = toupper(data$size)
table(data$size)
# convert "size" to ordinal
data$size = ordered(data$size, levels=c("S", "M", "L", "XL", "XXL", "XXXL"))
str(data)

# date attribute 
# alternative: package lubridate
table(data$order_date)
date_format = "%Y-%m-%d"
data$order_date = as.Date(data$order_date, date_format)
data$delivery_date = as.Date(data$delivery_date, date_format)
data$date_of_birth = as.Date(data$date_of_birth, date_format)
# get weekday, year, month, day and quarter of "order date"
data$order_date_weekday = weekdays(data$order_date)
data$order_date_year = as.numeric(format(data$order_date, "%Y"))
data$order_date_month = as.numeric(format(data$order_date, "%m"))
data$order_date_day = as.numeric(format(data$order_date, "%d"))
data$order_date_quarter = ceiling(as.numeric(format(data$order_date, "%m")) / 3)
str(data)

# Find missing values (only NA)
colSums(is.na(data))
# fill missing prices with average
data$price[is.na(data$price)] = mean(data$price, na.rm=TRUE)
data$tax[is.na(data$tax)] = mean(data$tax, na.rm=TRUE)
colSums(is.na(data))
# or fill missing prices with median
data$price[is.na(data$price)] = median(data$price, na.rm=TRUE)
data$tax[is.na(data$tax)] = median(data$tax, na.rm=TRUE)
colSums(is.na(data))
# or remove instances with missing price
data = data[!is.na(data$price), ]
colSums(is.na(data))

# Calculate new column "delivery time" as difference of order and delivery dates in days
data$delivery_time = as.numeric(data$delivery_date - data$order_date)
# Remove delivery and order date columns
# data$delivery_date = NULL
# data$order_date = NULL
table(data$delivery_time, useNA="ifany")
# negative delivery time is impossible
data$order_date[data$delivery_time < 0] = NA
data$delivery_date[data$delivery_time < 0] = NA
data$delivery_time[data$delivery_time < 0] = NA
table(data$delivery_time, useNA="ifany")
str(data)
sum(is.na(data$delivery_time))/length(data$delivery_time)
# remove instances with N/A delivery time
data = data[!is.na(data$delivery_time), ]
table(data$delivery_time, useNA="always")

colSums(is.na(data))



# Discretization
hist(data$delivery_time)
table(data$delivery_time, useNA="ifany")
# manual discretization
data$delivery_time_discret = factor(rep("NA", nrow(data)), levels=c("NA", "<= 5d", "> 5d"))
data$delivery_time_discret[data$delivery_time <= 5] = "<= 5d"
data$delivery_time_discret[data$delivery_time > 5] = "> 5d"
table(data$delivery_time_discret)
str(data)

# Show near zero variance attributes
library(caret)
names(data)[nearZeroVar(data, freqCut=95.0/5.0)]
table(data$salutation)
# remove "salutation"
# data$salutation = NULL
# str(data)

# Correlation
library(caret)
numeric_columns = c("price", "tax", "order_date_day", "order_date_month", "delivery_time")
numeric_columns_correlation = cor(data[, numeric_columns])
numeric_columns_correlation
# works for non-N/A only (remove N/A rows or fill with mean, median, etc)
high_cor_columns = findCorrelation(numeric_columns_correlation)
high_cor_columns
# "price" and "tax" are perfectly correlated --> remove "tax" column
data$tax = NULL
str(data)

