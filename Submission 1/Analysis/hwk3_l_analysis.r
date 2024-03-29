# Meta --------------------------------------------------------------------

## Title:         CDC Tax Burden on Tobacco Analysis File
## Author:        Leila Mulveny
## Date Created:  3/11/2024
## Description:   R Analysis File for Questions 1-5

# Load the necessary libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, fixest, knitr, modelsummary, AER, here)

# Load the data
final.data <- read_rds("data/output/TaxBurden_Data.rds")

## Summarize the data
sum.vars <- final.data %>% select('Sales per Capita' = sales_per_capita, 'Real Price' = price_cpi, 'Nominal Price'=cost_per_pack)

datasummary(All(sum.vars) ~ Mean + SD + Histogram, data=sum.vars)


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
  labs(title = "Proportion of States with a Change in Cigarette Tax (1970-1985)",
       x = "Year", y = "Proportion") +
  theme_minimal()

#print(Q1)



# Q2:
# Plot on a single graph the average tax (in 2012 dollars) on cigarettes and 
# the average price of a pack of cigarettes from 1970 to 2018.

# Filter the data for the years 1970 to 2018
data_1970_2018 <- final.data %>% 
  filter(Year >= 1970 & Year <= 2018)

# Calculate the average tax and the average price for each year
averages <- data_1970_2018 %>%
  group_by(Year) %>%
  summarise(Avg_Tax = mean(tax_dollar, na.rm = TRUE),
            Avg_Price = mean(cost_per_pack, na.rm = TRUE))

# Reshape data for plotting
averages_long <- gather(averages, key = "Variable", value = "Value", -Year)

# Create a line plot of the average tax and the average price over time
Q2 <- ggplot(averages_long, aes(x = Year, y = Value, color = Variable)) +
  geom_line() +
  labs(title = "Average Tax and Price of Cigarettes (1970-2018)",
       x = "Year", y = "Value (in 2012 dollars)") +
  theme_minimal()

#print(Q2)



# Q3:
# Identify the 5 states with the highest increases in cigarette prices 
# (in dollars) over the time period. Plot the average number of packs 
# sold per capita for those states from 1970 to 2018.

# Calculate the increase in cigarette prices for each state over the time period
price_increase <- final.data %>%
  group_by(state) %>%
  summarise(Price_Increase = max(cost_per_pack, na.rm = TRUE) - min(cost_per_pack, na.rm = TRUE))

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

#print(Q3)



# Q4:
# Identify the 5 states with the lowest increases in cigarette prices over 
# the time period. Plot the average number of packs sold per capita for 
# those states from 1970 to 2018.

# Calculate the increase in cigarette prices for each state over the time period
price_increase <- final.data %>%
  group_by(state) %>%
  summarise(Price_Increase = max(cost_per_pack, na.rm = TRUE) - min(cost_per_pack, na.rm = TRUE))

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

#print(Q4)



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

# Plot the total sales over time for these states
Q5 <- ggplot(totals, aes(x = Year, y = Total_Sales, color = state)) +
  geom_line() +
  labs(title = "Total Sales per Capita (1970-2018)",
       x = "Year", y = "Total Sales per Capita") +
  theme_minimal()

#print(Q5)

##Estimate ATEs

# Now let’s work on estimating a demand curve for cigarettes. Specifically, we’re going to estimate the 
# price elasticity of demand for cigarettes. When explaining your findings, try to limit your discussion just to a couple of sentences.

cig.data <- read_rds(here("data/output/TaxBurden_Data.rds"))
cig.data <- cig.data %>% mutate(ln_sales=log(sales_per_capita),
                                ln_price_cpi=log(price_cpi),
                                ln_price=log(cost_per_pack),
                                tax_cpi=tax_state*(210/index),
                                total_tax_cpi=tax_dollar*(210/index),
                                ln_total_tax=log(total_tax_cpi),                             
                                ln_state_tax=log(tax_cpi))


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


# Q7:
# Again limiting to 1970 to 1990, regress log sales on log prices using the total (federal and state) 
# cigarette tax (in dollars) as an instrument for log prices. Interpret your results and compare your estimates to those without an instrument. 
# Are they different? If so, why?

# Regress log sales on log prices using the total cigarette tax as an instrument
model2 <- ivreg(ln_sales ~ ln_price | ln_total_tax, data = cig_data_1970_1990)

# Print the summary of the model
summary(model2)


# Q8:
# Show the first stage and reduced-form results from the instrument.

# First stage regression: Regress ln_price on ln_total_tax
first_stage <- lm(ln_price ~ ln_total_tax, data = cig_data_1970_1990)
summary(first_stage)

# Reduced-form regression: Regress ln_sales on ln_total_tax
reduced_form <- lm(ln_sales ~ ln_total_tax, data = cig_data_1970_1990)
summary(reduced_form)


# Q9:
# Repeat questions 1-3 focusing on the period from 1991 to 2015.
cig_data_1991_2015 <- cig.data %>% 
  filter(Year >= 1991 & Year <= 2015)

model_1 <- lm(ln_sales ~ ln_price, data=cig_data_1991_2015)
summary(model_1)

model_2 <- ivreg(ln_sales ~ ln_price | ln_total_tax, data = cig_data_1991_2015)
summary(model_2)

first_stage_2 <- lm(ln_price ~ ln_total_tax, data = cig_data_1991_2015)
summary(first_stage_2)

reduced_form_2 <- lm(ln_sales ~ ln_total_tax, data = cig_data_1991_2015)
summary(reduced_form_2)


save.image("submission 1/Hwk3_workspace.Rdata")
