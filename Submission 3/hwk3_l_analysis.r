# Meta --------------------------------------------------------------------

## Title:         CDC Tax Burden on Tobacco Analysis File
## Author:        Leila Mulveny
## Date Created:  3/11/2024
## Description:   R Analysis File for Questions 

# Load the necessary libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, fixest, knitr, modelsummary, AER, here)

library(broom)
# Load the data
final.data <- read_rds("data/output/TaxBurden_Data.rds")

#First, convert to 2012 Dollars
final.data <- final.data %>%
  mutate(tax_dollar_2012 = tax_dollar / index * cpi_2012, 
          cost_per_pack_2012 = cost_per_pack / index * cpi_2012)


# Q1: 
# Present a bar graph showing the proportion of states with a change 
# in their cigarette tax in each year from 1970 to 1985.
data_1970_1985 <- final.data %>% 
  filter(Year >= 1970 & Year <= 1985)

# Create the tax_change column
data_1970_1985 <- data_1970_1985 %>%
  arrange(state, Year) %>%
  group_by(state) %>%
  mutate(tax_change = ifelse(tax_state - lag(tax_state) > 0, 1, 0))

# Calculate the proportion of states with a change in their cigarette tax for each year
proportions <- data_1970_1985 %>%
  group_by(Year) %>%
  summarise(Proportion = mean(tax_change, na.rm = TRUE), .groups = 'drop')

# Create a bar graph of the proportions
Q1 <- ggplot(proportions, aes(x = Year, y = Proportion)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportion of States with a Tax Change (1970-1985)",
       x = "Year", y = "Proportion") +
  theme_minimal()



# Q2:
# Plot on a single graph the average tax (in 2012 dollars) on cigarettes and 
# the average price of a pack of cigarettes from 1970 to 2018.

# Filter the data for the years 1970 to 2018
data_1970_2018 <- final.data %>% 
  filter(Year >= 1970 & Year <= 2018)

# Calculate the average tax and the average price for each year
averages <- data_1970_2018 %>%
  group_by(Year) %>%
  summarise(Avg_Tax = mean(tax_dollar_2012, na.rm = TRUE),
            Avg_Price = mean(cost_per_pack_2012, na.rm = TRUE))

# Reshape data for plotting
averages_long <- gather(averages, key = "Variable", value = "Value", -Year)

# Create a line plot of the average tax and the average price over time
Q2 <- ggplot(averages_long, aes(x = Year, y = Value)) +
  geom_line(aes(linetype = Variable, color = Variable)) +
  scale_color_manual(values = c("black", "black")) +  # Make both lines black
  scale_linetype_manual(values = c("solid", "dashed")) +  # Make one line solid and the other dashed
  labs(title = "Average Tax and Price of Cigarettes (1970-2018)",
       x = "Year", y = "Value (in 2012 dollars)") +
  theme_minimal()



# Q3:
# Identify the 5 states with the highest increases in cigarette prices 
# (in dollars) over the time period. Plot the average number of packs 
# sold per capita for those states from 1970 to 2018.

# Calculate the increase in cigarette prices for each state over the time period
price_increase <- final.data %>%
  group_by(state) %>%
  summarise(Price_Increase = max(cost_per_pack_2012, na.rm = TRUE) - min(cost_per_pack_2012, na.rm = TRUE))

# Identify the 5 states with the highest increases
top_states <- price_increase %>%
  top_n(5, Price_Increase) %>%
  pull(state)

# Filter the data for these states and the years 1970 to 2018
data_top_states <- final.data %>%
  filter(state %in% top_states & Year >= 1970 & Year <= 2018)

# Calculate the average number of packs sold per capita for each state and year
averages <- data_top_states %>%
  group_by(state, Year) %>%
  summarise(Avg_Packs = mean(sales_per_capita, na.rm = TRUE))

# Plot the average number of packs sold per capita over time
Q3 <- ggplot(averages, aes(x = Year, y = Avg_Packs, color = state)) +
  geom_line() +
  labs(title = "Average Number of Packs Sold per Capita (1970-2018)",
       x = "Year", y = "Average Number of Packs Sold per Capita") +
  theme_minimal()



# Q4:
# Identify the 5 states with the lowest increases in cigarette prices over 
# the time period. Plot the average number of packs sold per capita for 
# those states from 1970 to 2018.

# Calculate the increase in cigarette prices for each state over the time period
price_increase <- final.data %>%
  group_by(state) %>%
  summarise(Price_Increase = max(cost_per_pack_2012, na.rm = TRUE) - min(cost_per_pack_2012, na.rm = TRUE))

# Identify the 5 states with the lowest increases
bottom_states <- price_increase %>%
  top_n(-5, Price_Increase) %>%
  pull(state)

# Filter the data for these states and the years 1970 to 2018
data_bottom_states <- final.data %>%
  filter(state %in% bottom_states & Year >= 1970 & Year <= 2018)

# Calculate the average number of packs sold per capita for each state and year
averages <- data_bottom_states %>%
  group_by(state, Year) %>%
  summarise(Avg_Packs = mean(sales_per_capita, na.rm = TRUE))

# Plot the average number of packs sold per capita over time
Q4 <- ggplot(averages, aes(x = Year, y = Avg_Packs, color = state)) +
  geom_line() +
  labs(title = "Average Number of Packs Sold per Capita (1970-2018)",
       x = "Year", y = "Average Number of Packs Sold per Capita") +
  theme_minimal()



# Q5:
# Compare the trends in sales from the 5 states with the highest price 
# increases to those with the lowest price increases.

# Identify the 5 states with the highest and lowest price increases
top_states <- price_increase %>%
  top_n(5, Price_Increase) %>%
  pull(state)

bottom_states <- price_increase %>%
  top_n(-5, Price_Increase) %>%
  pull(state)

# Combine the states
combined_states <- c(top_states, bottom_states)

# Filter the data for these states and the years 1970 to 2018
data_combined_states <- final.data %>%
  filter(state %in% combined_states & Year >= 1970 & Year <= 2018)

# Calculate the total sales for each state and year
totals <- data_combined_states %>%
  group_by(state, Year) %>%
  summarise(Total_Sales = sum(sales_per_capita, na.rm = TRUE))

# Add a new column to identify whether the state is a top state or a bottom state
data_combined_states <- data_combined_states %>%
  mutate(State_Type = ifelse(state %in% top_states, "Top States", "Bottom States"))

# Calculate the average sales for each state type and year
averages <- data_combined_states %>%
  group_by(State_Type, Year) %>%
  summarise(Avg_Sales = mean(sales_per_capita, na.rm = TRUE))

# Plot the average sales over time for these state types
Q5 <- ggplot(averages, aes(x = Year, y = Avg_Sales, linetype = State_Type, color = State_Type)) +
  geom_line() +
  scale_linetype_manual(values = c("solid", "dashed")) +  # Make one line solid and the other dashed
  scale_color_manual(values = c("black", "black")) +  # Make both lines black
  labs(title = "Average Sales per Capita (1970-2018)",
       x = "Year", y = "Average Sales per Capita") +
  theme_minimal()



##Estimate ATEs

# Now let’s work on estimating a demand curve for cigarettes. Specifically, we’re going to estimate the 
# price elasticity of demand for cigarettes. When explaining your findings, try to limit your discussion just to a couple of sentences.

#Define New Log Variables
cig.data <- read_rds(here("data/output/TaxBurden_Data.rds"))
cig.data <- cig.data %>% mutate(ln_sales=log(sales_per_capita),
                                price_cpi_2012 = cost_per_pack*(cpi_2012/index), 
                                ln_price=log(price_cpi_2012),
                                total_tax_cpi=tax_dollar*(cpi_2012/index),
                                ln_total_tax=log(total_tax_cpi) )                            


#Q6:
# Focusing only on the time period from 1970 to 1990, regress log sales on log prices to estimate the 
# price elasticity of demand over that period. Interpret your results.

# Filter the data for the years 1970 to 1990
cig_data_1970_1990 <- cig.data %>% 
  filter(Year >= 1970 & Year <= 1990)

# Regress log sales on log prices
model1 <- lm(ln_sales ~ ln_price, data=cig_data_1970_1990)

# Print the summary of the model
summary(model1)

# Tidy the regression summary
tidy_model1 <- broom::tidy(model1)

# Store the coefficients and their standard errors
coefficients_model1 <- tidy_model1$estimate
std_errors_model1 <- tidy_model1$std.error

# Create labels for each row
beta_labels <- c("Beta 0", "Beta 1")

# Combine coefficients, standard errors, and labels into a data frame
regression_table_model1 <- data.frame(Label = beta_labels, Coefficient = coefficients_model1, `Standard Error` = std_errors_model1)

# Print the regression table
print(regression_table_model1)



# Q7:
# Again limiting to 1970 to 1990, regress log sales on log prices using the total (federal and state) 
# cigarette tax (in dollars) as an instrument for log prices. Interpret your results and compare your estimates to those without an instrument. 
# Are they different? If so, why?

# Regress log sales on log prices using the total cigarette tax as an instrument
# model2 <- ivreg(ln_sales ~ ln_price | ln_price + ln_total_tax, data = cig_data_1970_1990)

model2 <- feols(ln_sales ~ 1 | ln_price ~ ln_total_tax, 
             data=cig_data_1970_1990)

# Print the summary of the model
summary(model2)

# Tidy the regression summary for model2
tidy_model2 <- broom::tidy(model2)

# Store the coefficients and their standard errors for model2
coefficients_model2 <- tidy_model2$estimate
std_errors_model2 <- tidy_model2$std.error
beta_labels <- c("Beta 0", "Beta 1")


# Combine coefficients and standard errors into a data frame for model2
regression_table_model2 <- data.frame(Label = beta_labels, Coefficient = coefficients_model2, `Standard Error` = std_errors_model2)

# Print the regression table for model2
print(regression_table_model2)

# Q8: 
# Show the first stage and reduced-form results from the instrument.

# First stage regression: Regress ln_price on ln_total_tax
first_stage <- lm(ln_price ~ ln_total_tax, data = cig_data_1970_1990)
summary(first_stage)

# Reduced-form regression: Regress ln_sales on ln_total_tax
reduced_form <- lm(ln_sales ~ ln_total_tax, data = cig_data_1970_1990)
summary(reduced_form)

# Tidy the regression summary for first_stage
tidy_first_stage <- broom::tidy(first_stage)

# Store the coefficients and their standard errors for first_stage
coefficients_first_stage <- tidy_first_stage$estimate
std_errors_first_stage <- tidy_first_stage$std.error
beta_labels <- c("Beta 0", "Beta 1")

# Combine coefficients and standard errors into a data frame for first_stage
regression_table_first_stage <- data.frame(Label = beta_labels, Coefficient = coefficients_first_stage, `Standard Error` = std_errors_first_stage)

# Print the regression table for first_stage
print(regression_table_first_stage)

# Tidy the regression summary for reduced_form
tidy_reduced_form <- broom::tidy(reduced_form)

# Store the coefficients and their standard errors for reduced_form
coefficients_reduced_form <- tidy_reduced_form$estimate
std_errors_reduced_form <- tidy_reduced_form$std.error
beta_labels <- c("Beta 0", "Beta 1")

# Combine coefficients and standard errors into a data frame for reduced_form
regression_table_reduced_form <- data.frame(Label = beta_labels, Coefficient = coefficients_reduced_form, `Standard Error` = std_errors_reduced_form)

# Print the regression table for reduced_form
print(regression_table_reduced_form)


# Q9:
# Repeat questions 1-3 focusing on the period from 1991 to 2015.
cig_data_1991_2015 <- cig.data %>% 
  filter(Year >= 1991 & Year <= 2015)

model_1 <- lm(ln_sales ~ ln_price, data=cig_data_1991_2015)
summary(model_1)

model_2 <- feols(ln_sales ~ 1 | ln_price ~ ln_total_tax, 
             data=cig_data_1991_2015)

summary(model_2)

# First stage regression: Regress ln_price on ln_total_tax
first_stage_2 <- lm(ln_price ~ ln_total_tax, data = cig_data_1991_2015)
summary(first_stage_2)

# Reduced-form regression: Regress ln_sales on ln_total_tax
reduced_form_2 <- lm(ln_sales ~ ln_total_tax, data = cig_data_1991_2015)
summary(reduced_form_2)

# Tidy the regression summary for model_1
tidy_model_1 <- broom::tidy(model_1)

# Store the coefficients and their standard errors for model_1
coefficients_model_1 <- tidy_model_1$estimate
std_errors_model_1 <- tidy_model_1$std.error
beta_labels <- c("Beta 0", "Beta 1")

# Combine coefficients and standard errors into a data frame for model_1
regression_table_model_1 <- data.frame(Label = beta_labels, Coefficient = coefficients_model_1, `Standard Error` = std_errors_model_1)

# Print the regression table for model_1
print(regression_table_model_1)

# Repeat the process for model_2, first_stage_2, and reduced_form_2


tidy_model_2 <- broom::tidy(model_2)
coefficients_model_2 <- tidy_model2$estimate
std_errors_model_2 <- tidy_model2$std.error
beta_labels <- c("Beta 0", "Beta 1")
regression_table_model_2 <- data.frame(Label = beta_labels, Coefficient = coefficients_model_2, `Standard Error` = std_errors_model_2)
print(regression_table_model_2)

tidy_first_stage_2 <- broom::tidy(first_stage_2)
coefficients_first_stage_2 <- tidy_first_stage_2$estimate
std_errors_first_stage_2 <- tidy_first_stage_2$std.error
beta_labels <- c("Beta 0", "Beta 1")
regression_table_first_stage_2 <- data.frame(Label = beta_labels, Coefficient = coefficients_first_stage_2, `Standard Error` = std_errors_first_stage_2)
print(regression_table_first_stage_2)

tidy_reduced_form_2 <- broom::tidy(reduced_form_2)
coefficients_reduced_form_2 <- tidy_reduced_form_2$estimate
std_errors_reduced_form_2 <- tidy_reduced_form_2$std.error
beta_labels <- c("Beta 0", "Beta 1")
regression_table_reduced_form_2 <- data.frame(Label = beta_labels, Coefficient = coefficients_reduced_form_2, `Standard Error` = std_errors_reduced_form_2)
print(regression_table_reduced_form_2)



rm(list=c("cig.data", "data_1970_1985", "data_1970_2018", "data_top_states", "data_bottom_states", "data_combined_states", "cig_data_1991_2015", "cig_data_1970_1990"))

save.image("submission 1/Hwk3_workspace.Rdata")