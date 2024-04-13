# Are new hires getting paid too much?
# Load libraries
libs <- c( "readr", "dplyr", "ggplot2", "tidyr", "broom" )
lapply( libs, library, character.only = TRUE )

# Import the data
pay <- read_csv( "fair_pay_data.csv" )

# Get an overview of the data
summary( pay )

# Check average salary of new hires and non-new hires
pay |> 
  group_by( new_hire ) |>
  summarize( avg_salary = mean( salary ) )

# Perform the correct statistical test
# Is the result significant?
(significant <- t.test(salary ~ new_hire, data = pay) |>
    tidy() |>
    pull( p.value ) <= .05 )

# Create a stacked bar chart
ggplot( pay, aes( x = new_hire, fill = job_level ) ) +
  geom_bar()

# Create a 100% filled stacked bar chart
pay |>
  ggplot( aes( x = new_hire, fill = job_level ) ) +
  geom_bar( position = "fill" )

# Calculate the average salary for each group of interest in the pay dataset
pay_grouped <- pay |>
  group_by( new_hire, job_level ) |>
  summarize( avg_salary = mean( salary ) )

# Graph the results using facet_wrap()  
pay_grouped |>
  ggplot( aes( x = new_hire, y = avg_salary ) ) +
  geom_col() +
  facet_wrap( ~ job_level )

# Filter the data to include only hourly employees
pay_filter <- pay |>
  filter( job_level == "Hourly" )

# Test the difference in pay
t.test(salary ~ new_hire, data = pay_filter) |>
  tidy() |>
  pull( p.value ) <= 0.05


# Run the simple regression
model_simple <- lm(salary ~ new_hire, data = pay)

# Display the summary of model_simple
model_simple |> 
  summary()

# Display a tidy summary
model_simple |> 
  tidy()

# Is new hire pay significantly higher in this model?
( significant <- model_simple |>
  tidy() |>
  filter( term == "new_hireYes" ) |>
  pull( p.value ) <= 0.05 )

# Run the multiple regression
model_multiple <- lm(salary ~ new_hire + job_level, data = pay)

# Display the summary of model_multiple
model_multiple |> 
  summary()

# Display a tidy summary
model_multiple |> 
  tidy()

# Is new hire pay significantly higher in this model?
( significant <- model_multiple |>
    tidy() |>
    filter( term == "new_hireYes" ) |>
    pull( p.value ) <= 0.05 )

