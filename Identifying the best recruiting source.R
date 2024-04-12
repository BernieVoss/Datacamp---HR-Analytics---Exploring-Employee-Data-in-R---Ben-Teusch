# Looking at the recruiting data

# Load packages
libs <- c( "readr", "dplyr", "ggplot2" )
lapply( libs, library, character.only = TRUE )

?lapply

# Import the recruitment data
recruitment <- read_csv("recruitment_data.csv")

# Look at the first few rows of the dataset
head(recruitment)

# Get an overview of the recruitment data
summary(recruitment)

# See which recruiting sources the company has been using
count( recruitment, recruiting_source)

# Find the average sales quota attainment 
recruitment %>%
  summarize( avg_sales_quota_pct = mean( sales_quota_pct ))

# Find the average sales quota attainment for each recruiting source
avg_sales <- recruitment %>%
  group_by(recruiting_source) %>%
  summarize( avg_sales_quota_pct = mean( sales_quota_pct ) )

# Display the result
avg_sales

# Find the average attrition for the sales team, by recruiting source, sorted from lowest attrition rate to highest
avg_attrition <- recruitment %>%
  group_by( recruiting_source ) %>% 
  summarize( attrition_rate = mean( attrition ) ) %>% 
  arrange( desc(attrition_rate) )

# Display the result
avg_attrition

# Plot the avg_sales bar chart
avg_sales %>%
  ggplot( aes( x= recruiting_source, y = avg_sales_quota_pct ) ) +
  geom_col()

# Plot the avg_attrition bar chart
avg_attrition %>%
  ggplot( aes( x = recruiting_source, y = attrition_rate ) ) +
  geom_col()
