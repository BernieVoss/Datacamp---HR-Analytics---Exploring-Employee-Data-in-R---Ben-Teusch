# What is driving low employee engagement?
# Load libraries
libs <- c( "readr", "dplyr", "ggplot2", "tidyr", "broom" )
lapply( libs, library, character.only = TRUE )

# Import survey data
survey <- read_csv("Data/survey_data.csv")

# Get an overview of the data
survey |>
  summary()

# Examine the counts of the department variable
survey |>
  count(department)

# Output the average engagement score for each department, sorted
survey |>
  group_by( department ) |>
  summarize( avg_engagement = mean( engagement) ) |>
  arrange( avg_engagement )

# Create the disengaged variable and assign the result to survey
survey_disengaged <- survey |> 
  mutate(disengaged = ifelse(engagement <= 2, 1, 0)) 

survey_disengaged

# Summarize the three variables by department
survey_summary <- survey_disengaged |>
  group_by( department ) |>
  summarize( pct_disengaged = mean( disengaged ),
             avg_salary = mean( salary ),
             avg_vacation_days = mean ( vacation_days_taken ) )

survey_summary

# Gather data for plotting
survey_unpivoted <- survey_summary |> 
  pivot_longer( !department,
                names_to = "measure", values_to = "value" )

# Create three bar charts
ggplot(survey_unpivoted, aes( x = measure, y = value, fill = department ) ) +
  geom_col( position = "dodge" )

# Create three faceted bar charts
survey_unpivoted |>
  ggplot( aes( x = measure, y = value, fill = department ) ) +
  geom_col( position = "dodge" ) +
  facet_wrap( ~ measure, scales = "free" )

# Add the in_sales variable
survey_sales <- survey_disengaged |>
  mutate( in_sales = ifelse( department == "Sales", department, "Other" ) )

# Test the hypothesis using survey_sales
chisq.test( survey_sales$in_sales, survey_sales$disengaged )

# Is the result significant?
(significant <- chisq.test( survey_sales$in_sales, survey_sales$disengaged ) |>
  tidy() |>
  pull( p.value ) <= 0.05 )

# Test the hypothesis using the survey_sales data
t.test(vacation_days_taken ~ in_sales, data = survey_sales)

# Is the result significant?
(significant <- t.test(vacation_days_taken ~ in_sales, data = survey_sales) |>
  tidy() |>
  pull( p.value ) <= 0.05 )




