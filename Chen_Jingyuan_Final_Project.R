library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
library(readxl)

rename_experiment_table <- read_excel("rename experiment-table.xlsx")
View(rename_experiment_table)

#find the mean for each statistics
summary_data <- rename_experiment_table %>%
  group_by(immunity_susceptible_rate) %>%
  summarise(
    mean_susceptible = mean(susceptible_rate),
    mean_infectious = mean(infectious_rate),
    mean_recovered = mean(recovered_rate)
  )

#summarize to long format
summary_1<- summary_data %>%
  pivot_longer(cols = starts_with("mean_"), 
               names_to = "statistic", 
               values_to = "mean_rate")

ggplot(summary_1, aes(x = immunity_susceptible_rate, y = mean_rate, color = statistic)) +
  geom_line() +
  labs(x = "Immunity Susceptible Rate", y = "Mean rates of Statistics", 
       title = "Mean Statistics for Different Immunity Susceptible Rates") +
  scale_color_manual(values = c("mean_susceptible" = "red", 
                                "mean_infectious" = "green", 
                                "mean_recovered" = "blue")) +
  theme_minimal()



# Original  data without calculating means
summary_data1 <- rename_experiment_table %>%
  group_by(immunity_susceptible_rate) %>%
  select(susceptible_rate, infectious_rate, recovered_rate)

# Pivot the data to long format for plotting
summary_2 <- summary_data1 %>%
  pivot_longer(cols = c("susceptible_rate", "infectious_rate", "recovered_rate"), 
               names_to = "statistic", 
               values_to = "rate")

# Plot the data using points instead of lines
ggplot(summary_2, aes(x = immunity_susceptible_rate, y = rate, color = statistic)) +
  geom_point() +  # Use geom_point for plotting points
  labs(x = "Immunity Susceptible Rate", y = "Rates of Statistics", 
       title = "Statistics for Different Immunity Susceptible Rates") +
  scale_color_manual(values = c("susceptible_rate" = "red", 
                                "infectious_rate" = "green", 
                                "recovered_rate" = "blue")) +
  theme_minimal()








