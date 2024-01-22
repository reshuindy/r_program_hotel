library(ggplot2)
#Load the hotel.csv dataset
hotel_data <- read.csv('/Users/utkarsh/Desktop/hotels.csv')

# Numeric summary for two columns
numeric_summary <- summary(hotel_data[c('lead_time', 'stays_in_weekend_nights')])

# Display unique values and counts for categorical columns
categorical_summary1 <- table(hotel_data$meal)
categorical_summary2 <- table(hotel_data$market_segment)


# Aggregating lead time impact on cancellations
lead_time_cancellation_aggregate <- aggregate(is_canceled ~ lead_time, data = hotel_data, FUN = function(x) mean(x == 1))


# Aggregating effect of meal type on customer satisfaction
meal_satisfaction_aggregate <- aggregate(adr ~ meal, data = hotel_data, FUN = mean)



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


# Display the numeric summary
print(numeric_summary)

# Display the categorical summary for column1 - "meal"
print(categorical_summary1)

# Display the categorical summary for column2 - "market_segment"
print(categorical_summary2)



# Display the result - Aggregating lead time impact on cancellations -"lead_time_cancellation_aggregate"
print(lead_time_cancellation_aggregate)


# Display the result - Aggregating effect of meal type on customer satisfaction - "meal_satisfaction_aggregate"
print(meal_satisfaction_aggregate)

#Plot - Lead Time Distribution 
print(lead_time_plot)

#Plot - Correlation between Lead Time and Cancellations
print(lead_time_cancellation_plot)
