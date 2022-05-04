#-------------------------- Seattle Airbnb Dataset ----------------------------#
# Link: https://www.kaggle.com/datasets/adriangonzalezcortes/seattle-airbnb-dataset?resource=download
library(ggplot2)
library(funModeling)
library(tidyverse)
library(ggpubr)
library(viridis)
library(gridExtra)
library(corrplot)

# Loading csv file into a dataframe:
Seattle_Airbnb = read_csv("listings.csv") 
attach(Seattle_Airbnb)

# Data cleaning
summary(Seattle_Airbnb)
str(Seattle_Airbnb)

sum(is.na(Seattle_Airbnb))
Seattle_Airbnb %>% filter(is.na(reviews_per_month)) %>% count(.)
Seattle_Airbnb %>% filter(duplicated(.)) %>%  count(.)
Seattle_Airbnb <- Seattle_Airbnb %>% mutate(last_review = as.Date(last_review))
Seattle_Airbnb$reviews_per_month[is.na(Seattle_Airbnb$reviews_per_month)] <- 0

#------------------------------------ EDA -------------------------------------#

# Histogram for numeric variables
Seattle_Airbnb %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# Histogram for categorical variables
Seattle_Airbnb_Histogram <- (Seattle_Airbnb[c(5,6,9)])

Seattle_Airbnb_Histogram %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(stat = "count")

# Relationship of Price Per Night & Reviews Per Month
qplot(x = reviews_per_month, y = price, color = room_type, geom = 'point') +
  theme(axis.title.x = element_text(color = "Steelblue4", face='bold'),
        axis.title.y = element_text(color = "Steelblue4", face= 'bold'),
        plot.title = element_text(color = "Steelblue4", face= 'bold', hjust = 0.5)) +
  labs(title='Prices of Airbnb Per Night') +
  xlab('Reviews Per Month') + ylab('Price Per Night') +
  scale_color_manual(values = c("Steelblue4", "Steelblue3", "lightsteelblue4"))

# Zoomed Picture of Relationship of Price Per Night & Reviews Per Month
qplot(x = reviews_per_month, y = price, color = room_type, geom = 'point') +
  theme(axis.title.x = element_text(color = "Steelblue4", face='bold'),
        axis.title.y = element_text(color = "Steelblue4", face= 'bold'),
        plot.title = element_text(color = "Steelblue4", face= 'bold',hjust = 0.5),) +
  labs(title = 'Prices of Airbnb Per Night') +
  xlab('Reviews per month') + ylab('Price per night')+
  xlim(0,20) + ylim(0,500) +
  scale_color_manual(values = c("Steelblue4", "Steelblue3", "lightsteelblue4")) 

# Number of Room Types Listed
ggplot(Seattle_Airbnb, aes(room_type)) +
  geom_bar(fill = c("Steelblue4", "Steelblue3", "Steelblue2"), width = 0.7) +
  theme_pubclean() +
  theme(axis.title.x = element_text(color = "Steelblue4", face='bold'),
        axis.title.y = element_text(color = "Steelblue4", face= 'bold'),
        plot.title = element_text(color = "Steelblue4", face= 'bold', hjust = 0.5),) +
  labs(title = 'Number of Room Types Listed') +
  xlab('Room Type') + ylab('Count')

# Number of Room Types Listed in Each Neighborhood Group
ggplot(Seattle_Airbnb, aes(fill = room_type, x = neighbourhood_group, y = price )) +
  geom_bar(width = 0.7, position="dodge", stat="identity") +
  theme_pubclean()+
  theme(axis.text.x = element_text(angle =45, vjust = 0.8),
        axis.title.x = element_text(color = "grey40", face='bold'),
        axis.title.y = element_text(color = "grey40", face= 'bold'),
        plot.title = element_text(color = "grey40", face= 'bold'),) +
  labs(title='Number of Room Types Listed in Each Neighborhood Group') +
  xlab('Neighbourhood Group') + ylab('Count') +
  scale_fill_viridis(discrete = T, option = "E") 

# Average Number of Minimum Nights Per Neighbourhood Group
mean_min_nights <- Seattle_Airbnb %>% group_by(neighbourhood_group) %>% 
  summarise(mean_min_nights = mean(minimum_nights), .groups = 'drop') 

ggplot(mean_min_nights, aes(x = reorder(neighbourhood_group, -mean_min_nights), 
                            y = mean_min_nights)) +
  geom_bar(stat="identity", fill = '#FFDB6D') + theme_pubclean() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title.x = element_text(color = '#C4961A', face = 'bold'),
        axis.title.y = element_text(color = '#C4961A', face = 'bold'),
        plot.title = element_text(color = '#C4961A', face = 'bold', hjust = 0.5)) +
  ggtitle('Average Number of Minimum Nights Per Neighbourhood Group') +
  xlab('Neighbourhood Group') + ylab('Average of Minimum Nights')

# Average Price Per Night Per Neighbourhood Group
avg_price_per_nights <- Seattle_Airbnb %>% group_by(neighbourhood_group) %>% 
  summarise(avg_price = mean(price), avg_price = round(avg_price, 2), .groups = 'drop')

ggplot(avg_price_per_nights, aes(x = reorder(neighbourhood_group, avg_price), y = avg_price)) +
  geom_bar(stat = 'identity', fill = '#C3D7A4', color = '#52854C') + theme_pubclean() +
  coord_flip() + ggtitle('Average Price Per Night At Each Neighbourhood Group') +
  theme(axis.title.x = element_text(color = '#52854C', face = 'bold'),
        axis.title.y = element_text(color = '#52854C', face = 'bold'),
        plot.title = element_text(color = '#52854C', face = 'bold', hjust = 0.5)) +
  xlab('Neighbourhood Group') + ylab('Average Price Nights') + 
  geom_text(label = avg_price_per_nights$avg_price, hjust = 0)

# Minimum of Nights
p1 <- ggplot(data = Seattle_Airbnb, aes(y = minimum_nights, x = room_type, color = room_type)) +
  geom_boxplot(outlier.colour = "black", outlier.size = 2, outlier.shape = 4, notch = FALSE) +
  theme(plot.title = element_text(hjust = .5, face="bold")) +
  ggtitle("\nMinimum of Nights\n") +
  xlab("") + ylab("")

# Availability In The Year
p2 <- ggplot(data = Seattle_Airbnb, aes(y=availability_365, x=room_type, color=room_type)) +
  geom_boxplot(outlier.colour = "black",
               outlier.size = 2, outlier.shape = 4, notch = FALSE) +
  theme(plot.title = element_text(hjust = .5, face="bold")) +
  ggtitle("\nAvailability in the year\n") +
  xlab("") + ylab("")

# Price Per Night
p3 <- ggplot(data = Seattle_Airbnb, aes(y=price, x=room_type, color=room_type)) +
  geom_boxplot(outlier.colour = "black",
               outlier.size = 2, outlier.shape = 4, notch = FALSE) +
  theme(plot.title = element_text(hjust = .5, face = "bold")) +
  ggtitle("\nPrice per night\n") +
  xlab("") + ylab("")

# Number of Reviews
p4 <- ggplot(data = Seattle_Airbnb, aes(y = number_of_reviews, x = room_type, color = room_type)) +
  geom_boxplot(outlier.colour = "black",
               outlier.size = 2, outlier.shape = 4, notch = FALSE) +
  theme(plot.title = element_text(hjust = .5, face="bold")) +
  ggtitle("\nNumber of Reviews\n") +
  xlab("") + ylab("")

# Number of Times Host Are In List
p5 <- ggplot(data = Seattle_Airbnb, aes(y = calculated_host_listings_count, x = room_type, color = room_type)) +
  geom_boxplot(outlier.colour = "black",
               outlier.size = 2, outlier.shape = 4, notch = FALSE) +
  theme(plot.title = element_text(hjust = .5, face="bold")) +
  ggtitle("\nNumber of times Host are in List\n") +
  xlab("") + ylab("")

# Reviews Per Month
p6 <- ggplot(data = Seattle_Airbnb, aes(y=reviews_per_month, x=room_type, color=room_type)) +
  geom_boxplot(outlier.colour = "black",
               outlier.size = 2, outlier.shape = 4, notch = FALSE) +
  theme(plot.title = element_text(hjust = .5, face="bold")) +
  ggtitle("\nReviews per Month\n") +
  xlab("") + ylab("")

grid.arrange(p1,p2,p3,p4,p5,p6,nrow = 3)

# Most Popular Neighborhood
ggplot(data=Seattle_Airbnb, aes(x = neighbourhood_group)) +
  geom_histogram(stat = "count", color = "blue", linetype = "dashed", fill = "light blue") +
  theme(plot.title = element_text(hjust = .5, face="bold"), 
        axis.text.x =element_text(angle = 45, vjust = 0.5)) +
  ggtitle("\nMost Popular Neighborhood\n") + xlab("") + ylab("")

# Correlation Matrix
new <- Seattle_Airbnb %>%
  keep(is.numeric)
a <- cor(new)
corrplot(a, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
