# Are performance ratings being given consistently?
# Load libraries
libs <- c( "readr", "dplyr", "ggplot2", "tidyr", "broom" )
lapply( libs, library, character.only = TRUE )

# Import the data
hr_data <- read_csv( "hr_data.csv" )
performance_data <- read_csv( "performance_data.csv" )

# Examine the datasets
summary( hr_data )
summary( performance_data )

# Join the two tables
joined_data <- hr_data |>
  left_join( performance_data, by = "employee_id" )

# Examine the result
joined_data |>
  summary()

# Check whether the average performance rating differs by gender 
joined_data |>
  group_by( gender ) |>
  summarize( avg_rating = mean( rating ) )

# Add the high_performer column
performance <- joined_data |>  
  mutate( high_performer = ifelse( rating >= 4, 1, 0 ) )

# Test whether one gender is more likely to be a high performer
chisq.test( performance$gender, performance$high_performer ) |>
  tidy()

# Is the test result significant?
significant <- chisq.test(performance$gender, performance$high_performer) |> 
  tidy() |>
  pull( p.value ) <= 0.05

# Visualize the distribution of high_performer by gender
performance |>
  ggplot( aes( x = gender, fill = factor( high_performer ) ) ) +
  geom_bar( position = "fill" )

# Visualize the distribution of all ratings by gender
performance |>
  ggplot( aes( x = gender, fill = factor( rating ) ) ) +
  geom_bar( position = "fill" )

# Visualize the distribution of job_level by gender
performance |> 
  ggplot( aes( x = gender, fill = job_level ) ) +
  geom_bar( position = "fill" )

# Test whether men and women have different job level distributions
chisq.test( performance$gender, performance$job_level )

# Visualize the distribution of high_performer by gender, faceted by job level
performance |> 
  ggplot( aes( x = gender, y = high_performer ) ) +
  geom_col( ) +
  facet_wrap( ~ job_level )

# Run a simple logistic regression
logistic_simple <- glm( high_performer ~ gender, family = "binomial", data = performance )

# View a tidy version of the result
logistic_simple |>
  tidy()

# Is the result significant?
significant <- logistic_simple |>
  tidy()|>
  filter( term == "genderMale" ) |>
  pull( p.value ) <= 0.05

# Run a multiple logistic regression
logistic_multiple <- glm( high_performer ~ gender + job_level, family = "binomial", data = performance )

# View the result with summary() or tidy()
logistic_multiple |>
  tidy()

# Is the result significant?  
significant <- logistic_multiple |>
  tidy() |>
  filter( term == "genderMale" ) |>
  pull( p.value ) <= 0.05









