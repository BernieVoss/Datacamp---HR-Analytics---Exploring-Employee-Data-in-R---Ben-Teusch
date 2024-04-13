# Improving employee safety with data
# Load libraries
libs <- c( "readr", "dplyr", "ggplot2", "tidyr", "broom" )
lapply( libs, library, character.only = TRUE )

# Import the data 
hr_data <- read_csv( "Data/hr_data_2.csv" )
accident_data <- read_csv( "Data/accident_data.csv" )

# Create hr_joined with left_join() and mutate()
hr_joined <- left_join( hr_data, accident_data, by = c( "year", "employee_id" ) ) %>% 
  mutate( had_accident = ifelse( is.na( accident_type ), 0, 1 ) ) 

head( hr_joined )

# Find accident rate by year
hr_joined %>%
  group_by( year ) %>%
  summarize( accident_rate = mean( had_accident ) )

# Test difference in accident rate between years
 glm( had_accident ~ year, family = "binomial", data = hr_joined ) %>%
  summary()

chisq.test( hr_joined$year, hr_joined$had_accident )


# Which location had the highest accident rate?
hr_joined %>%
  group_by( location ) %>%
  summarize( accident_rate = mean( had_accident ) ) %>%
  arrange( desc( accident_rate ) )

# Compare annual accident rates by location
accident_rates <- hr_joined %>%
  group_by( year, location ) %>%
  summarize( accident_rate = mean( had_accident ) )

head( accident_rates )

# Graph it
accident_rates %>% 
  ggplot( aes( x = factor( year ), y = accident_rate ) ) +
  geom_col() +
  facet_wrap( ~ location )

# Answer the question
( increased_most <- accident_rates %>%
  mutate( year = paste0( "y", year ) ) %>%
  pivot_wider( names_from = year, values_from = accident_rate ) %>%
  mutate( increase = y2017 - y2016 ) %>%
  slice_max( order_by = increase, n = 1 ) %>% 
  pull( location ) )

# Focusing on the Problem Location
# Filter out the other locations
southfield <- hr_joined %>%
  filter( location == "Southfield" )

# Find the average overtime hours worked by year
southfield %>%
  group_by( year ) %>%
  summarize( average_overtime_hours = mean( overtime_hours ) )

# Test difference in Southfield's overtime hours between years
t.test( overtime_hours ~ year, data = southfield )

# Do the years have significantly different average overtime hours?  
significant <- t.test(overtime_hours ~ year, data = southfield) %>%
  tidy() %>%
  pull( p.value ) <= 0.05

# Import the survey data
survey_data <- read_csv( "Data/survey_data_2.csv" )

# Create the safety dataset
safety <- survey_data %>%
  left_join( hr_joined, by = c( "year", "employee_id" ) ) %>%
  mutate( disengaged = ifelse( engagement <= 2, 1, 0 ),
          year = factor( year ) )

# Create southfield_safety dataset
southfield_safety <- safety |>
  filter( location == "Southfield" )

# Visualize the difference in % disengaged by year in Southfield
southfield_safety %>%
  ggplot( aes( x = year, fill = factor( disengaged ) ) ) +
  geom_bar( position = "fill" )

# Test whether one year had significantly more disengaged employees
chisq.test( southfield_safety$disengaged, southfield_safety$year )

# Is the result significant?
significant <- chisq.test( southfield_safety$year, southfield_safety$disengaged ) %>%
  tidy() %>%
  pull( p.value ) <= 0.05

# Filter out Southfield
other_locs <- safety %>%
  filter( location != "Southfield" )

head( other_locs )

# Test whether one year had significantly more overtime hours worked
# Test whether one year had significantly more disengaged employees
# Are the results significant?
significant_overtime <- t.test( overtime_hours ~ year, data = other_locs ) %>%
  tidy() %>%
  pull( p.value ) <= 0.05

significant_disengaged <- chisq.test( other_locs$year, other_locs$disengaged )  %>%
  tidy() %>%
  pull( p.value ) <= 0.05

# Use multiple regression to test the impact of year and disengaged on accident rate in Southfield
regression <- glm( had_accident ~ year + disengaged, family = "binomial", data = southfield_safety )

# Examine the output
regression %>%
  tidy()

# Interpret the regression output
( significant_year <- regression %>%
  tidy() %>%
  filter( term == "year2017" ) %>%
  pull( p.value ) <= 0.05 )
( significant_disengaged <- regression %>%
  tidy() %>%
  filter( term == "disengaged" ) %>%
  pull( p.value ) <= 0.05 )





