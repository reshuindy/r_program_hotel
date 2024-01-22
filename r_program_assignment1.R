# ########################################
# Question 1
#A numeric summary of data for at least 2 columns of dat
#For categorical columns, this should include unique values and counts
#For numeric columns, this includes min/max, central tendency, and some notion of distribution (e.g., quantiles)
#These summaries can be combined
# ########################################
# Question 3:
#Address at least one of the above questions using an aggregation function
# ######################################
# Question 4:
#Visual summaries (i.e., visualizations) of 2 or more columns of your data
# This should include distributions at least
# In addition, you should consider trends, correlations, and interactions between variables
# Use different channels (e.g., color) to show how categorical variables interact with continuous variables

library(ggplot2)
#Load the hotel.csv dataset
hotel_data <- read.csv('/Users/utkarsh/Desktop/hotels.csv')

# Answer of Question 1  Numeric summary for two columns
numeric_summary <- summary(hotel_data[c('lead_time', 'stays_in_weekend_nights')])

# Answer of Question 1 Display unique values and counts for categorical columns
categorical_summary1 <- table(hotel_data$meal)
categorical_summary2 <- table(hotel_data$market_segment)


# Answer of Question 1 Aggregating lead time impact on cancellations
lead_time_cancellation_aggregate <- aggregate(is_canceled ~ lead_time, data = hotel_data, FUN = function(x) mean(x == 1))


# Answer of Question 3 Aggregating effect of meal type on customer satisfaction
meal_satisfaction_aggregate <- aggregate(adr ~ meal, data = hotel_data, FUN = mean)


# Answer of Question No 4 : Visual summaries (i.e., visualizations) of 2 or more columns of your data
# Lead Time Distribution
lead_time_plot <- ggplot(hotel_data, aes(x = lead_time)) +
  geom_histogram(binwidth = 50, fill = "#66c2a5", color = "#1f78b4", alpha = 0.7) +
  labs(title = "Lead Time Distribution",
       x = "Lead Time (days)",
       y = "Frequency") +
  theme_minimal()


# Correlation between Lead Time and Cancellations
lead_time_cancellation_plot <- ggplot(hotel_data, aes(x = lead_time, fill = factor(is_canceled))) +
  geom_density(alpha = 0.7) +
  labs(title = "Correlation between Lead Time and Cancellations",
       x = "Lead Time (days)",
       y = "Density",
       fill = "Cancellation") +
  theme_minimal()


# Answer of Question 1 Display the numeric summary
print(numeric_summary)

# Answer of  Question 1 Display the categorical summary for column1 - "meal"
print(categorical_summary1)

# Answer of Question 1 Display the categorical summary for column2 - "market_segment"
print(categorical_summary2)



# Answer of Question 3 Display the result - Aggregating lead time impact on cancellations -"lead_time_cancellation_aggregate"
print(lead_time_cancellation_aggregate)


# Answer of Question  3 Display the result - Aggregating effect of meal type on customer satisfaction - "meal_satisfaction_aggregate"
print(meal_satisfaction_aggregate)

# Answer of Question 4 Plot - Lead Time Distribution  Question 4
print(lead_time_plot)

# Answer of Question 4 Plot - Correlation between Lead Time and Cancellations   
print(lead_time_cancellation_plot)
