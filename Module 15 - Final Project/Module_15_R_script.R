# Import necessary libraries
library(dplyr)              # For data manipulation
library(countrycode)        # For country name conversion
library(tibble)             # For data frames as tibbles
library(ggplot2)            # For data visualization
library(viridis)            # For color scales in plots
library(sf)                  # 'sf' for handling spatial data in simple features format
library(rnaturalearth)       # 'rnaturalearth' to access natural earth map data

# Load and clean the data
data <- read.csv("master.csv") %>%
  filter(year != 2016, # Remove data for 2016 and countries with 0 data
         country != 'Dominica', # Exclude data for Dominica
         country != 'Saint Kitts and Nevis') %>% # Exclude data for Saint Kitts and Nevis
  # Standardize country names to ensure consistency
  mutate(country = recode(country,
                          "Bahamas" = "The Bahamas",
                          "Cabo Verde" = "Cape Verde",
                          "Republic of Korea" = "South Korea",
                          "Russian Federation" = "Russia",
                          "Serbia" = "Republic of Serbia",
                          "United States" = "United States of America")) 

# Reorder levels of the 'age' column for proper ordering in plots and analysis
data$age <- factor(data$age, levels = c("5-14 years", "15-24 years", "25-34 years", "35-54 years", "55-74 years", "75+ years"))

# Create a tibble summarizing yearly statistics on suicide rates
overall_tibble <- data %>%
  select(year, suicides_no, population) %>%
  group_by(year) %>%
  summarize(suicide_capita = round((sum(suicides_no) / sum(population)) * 100000, 2)) # Calculate suicides per 100,000 people and round off

# Create a line plot to visualize the trend of suicide rates over years
global_average <- mean(overall_tibble$suicide_capita) # Calculate the global average suicide rate

ggplot(overall_tibble, aes(x = year, y = suicide_capita)) +
  geom_line(color = "red", size = 1) + # Red line for trend
  geom_point(color = "red", size = 2) + # Red points for each year
  geom_hline(yintercept = global_average, color = "black", linetype = "dashed", linewidth = 1) + 
  labs(
    title = "Worldwide Suicides by Year",
    subtitle = "1985-2015",
    x = "Year",
    y = "Suicides per 100K people"
  ) +
  scale_x_continuous(breaks = seq(1985, 2015, 2)) +
  scale_y_continuous(breaks = seq(10, 20))
  

# Select relevant columns and group data by year and age to calculate suicide rates per 100,000 individuals
age_tibble <- data %>%
  select(year, age, suicides_no, population) %>%
  group_by(year, age) %>%
  summarize(suicide_capita = round((sum(suicides_no) / sum(population)) * 100000, 2), .groups = "drop") # Compute suicide rates

# Create a line plot
ggplot(age_tibble, aes(x = year, y = suicide_capita, color = age)) + 
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Worldwide Suicides by Age",
    subtitle = "1985-2015",
    x = "Year",
    y = "Suicides per 100K people"
  ) +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = seq(1985, 2015, 2)) +
  scale_y_continuous(breaks = seq(0, 40, 2)) +
  theme_minimal()

# Convert the 'sex' column to a factor with specified levels to ensure accurate categorization
data$sex <- factor(data$sex, levels = c("male", "female"))

# Select relevant columns and compute the suicide rate per 100,000 people grouped by year and sex
sex_tibble <- data %>%
  select(year, sex, suicides_no, population) %>%
  group_by(year, sex) %>%
  summarize(suicide_capita = round((sum(suicides_no) / sum(population)) * 100000, 2), .groups = "drop") # Calculate and round suicide rates

# Create a line plot with specific color settings to distinguish between genders
ggplot(sex_tibble, aes(x = year, y = suicide_capita, color = sex)) + 
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Worldwide Suicides by Gender", # Main title of the plot
    subtitle = "1985-2015",
    x = "Year",
    y = "Suicides per 100K people", 
    color = "Gender"
  ) +
  scale_color_manual(values = c("female" = "pink", "male" = "blue")) +
  scale_x_continuous(breaks = seq(1985, 2015, 2)) +
  scale_y_continuous(breaks = seq(10, 40, 2)) +
  theme_minimal()

# Convert country names to continent names using the 'countrycode' library
data$continent <- countrycode(data$country, "country.name", "continent")

# Reclassify 'Americas' into 'North America' and 'South America' based on specific countries
south_america <- c('Argentina', 'Brazil', 'Chile', 'Colombia', 'Ecuador', 'Guyana', 'Paraguay', 'Suriname', 'Uruguay')
data$continent[data$country %in% south_america] <- 'South America' # Assign South America to specific countries
data$continent[data$continent == 'Americas'] <- 'North America' # Rename remaining 'Americas' to 'North America'

# Group data by continent and sex, then calculate the suicide rate per 100,000 population
continent_sex_tibble <- data %>%
  select(continent, sex, suicides_no, population) %>%
  group_by(continent, sex) %>%
  summarize(suicide_capita = round((sum(suicides_no)/sum(population)) * 100000, 2), .groups = "drop")

# Group data by continent and age, then calculate the suicide rate per 100,000 population
continent_age_tibble <- data %>%
  select(continent, age, suicides_no, population) %>%
  group_by(continent, age) %>%
  summarize(suicide_capita = round((sum(suicides_no)/sum(population)) * 100000, 2), .groups = "drop")

# Plot for suicides by continent and gender
ggplot(continent_sex_tibble, aes(x = continent, y = suicide_capita, fill = sex)) +
  geom_col(position = "dodge") +
  labs(title = "Suicides by Continent and Gender", subtitle = "1985-2015",
       x = "Continent", y = "Suicides per 100K", fill = "Gender") +
  theme_minimal()

# Plot for suicides by continent and age
ggplot(continent_age_tibble, aes(x = continent, y = suicide_capita, fill = age)) +
  geom_col(position = "dodge") +
  labs(title = "Suicides by Continent and Age", subtitle = "1985-2015",
       x = "Continent", y = "Suicides per 100K", fill = "Age") +
  scale_fill_viridis_d() +
  theme_minimal()

# Group data by country and calculate suicide rate per 100k population
country_tibble <- data %>%
  select(country, suicides_no, population) %>%
  group_by(country) %>%
  summarize(suicide_capita = round((sum(suicides_no) / sum(population)) * 100000, 2)) %>%
  arrange(desc(suicide_capita))

ggplot(country_tibble, aes(x = reorder(country, suicide_capita), y = suicide_capita, fill = suicide_capita)) +
  geom_col() +
  coord_flip() +
  labs(title = "Suicides by Country", 
       subtitle = "1985-2015", 
       x = "Country", 
       y = "Suicides per 100K people",
       fill = "Suicide Rate") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 5, angle = 0),
        plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 10)) +
  scale_fill_gradient(low = "blue", high = "red")

# Retrieve world geometry data
world <- ne_countries(scale = "medium", returnclass = "sf")
# Group data by country, calculate the suicide rate per 100,000 people
suicide_data <- data %>%
  group_by(country) %>%
  summarize(suicide_capita = round((sum(suicides_no) / sum(population)) * 100000, 2), .groups = 'drop')

# Join suicide rates with world geometrical data
world_suicide <- world %>%
  left_join(suicide_data, by = c("name" = "country"))

# Create and plot the choropleth map
ggplot(data = world_suicide) +
  geom_sf(aes(fill = suicide_capita), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "magma", na.value = "grey80", label = scales::comma) +
  labs(
    title = "Global Suicide Rates by Country",
    subtitle = "Suicides per 100,000 people (1985-2015)",
    fill = "Suicide\nRate"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Renaming the column with trailing dots to 'gdp_per_capita'
data <- data %>%
  rename(gdp_per_capita = `gdp_per_capita....`)

# Calculate Suicide Rates and Average GDP by Country and Year
country_summary <- data %>%
  group_by(country, year) %>%
  summarize(
    suicide_capita = round((sum(suicides_no) / sum(population)) * 100000, 2),  # Calculate suicide rates per 100,000 people
    avg_gdp_per_capita = mean(gdp_per_capita, na.rm = TRUE),  # Calculate average GDP per capita, ignoring NA values
    .groups = 'drop'
  )

# Calculate Global Suicide Rates and Average GDP by Year
global_stats <- data %>%
  group_by(year) %>%
  summarize(
    suicide_rate = round((sum(suicides_no) / sum(population)) * 100000, 2),  # Aggregate and calculate global suicide rates
    avg_gdp_per_capita = mean(gdp_per_capita, na.rm = TRUE),  # Average GDP per capita at the global level
    .groups = 'drop'
  ) %>%
  mutate(continent = 'GLOBAL')  # Add a 'GLOBAL' identifier for these entries for easy reference in plots

# Calculate Continent-wise Suicide Rates and Average GDP by Year
continent_stats <- data %>%
  group_by(year, continent) %>%
  summarize(
    suicide_rate = round((sum(suicides_no) / sum(population)) * 100000, 2),  # Calculate suicide rates by continent
    avg_gdp_per_capita = mean(gdp_per_capita, na.rm = TRUE),  # Average GDP per capita by continent
    .groups = 'drop'
  )

# Combine Global and Continent Data for comparative analysis
combined_stats <- bind_rows(global_stats, continent_stats) %>%
  mutate(continent = factor(continent, levels = c('GLOBAL', unique(continent[continent != 'GLOBAL']))))  # Ensure 'GLOBAL' appears first in plots

# Calculate scaling factors for plotting normalized data to compare GDP and suicide rates directly
max_gdp <- max(combined_stats$avg_gdp_per_capita, na.rm = TRUE)
max_suicide_rate <- max(combined_stats$suicide_rate, na.rm = TRUE)
scaling_factor <- max_suicide_rate / max_gdp  # Scaling factor to normalize suicide rates against GDP values

# Create line plots showing trends of suicide rates and GDP per Capita over time across continents
ggplot(combined_stats, aes(x = year)) +
  geom_line(aes(y = avg_gdp_per_capita, color = "GDP per Capita"), linetype = 1) +  # Plot GDP trends
  geom_line(aes(y = suicide_rate / scaling_factor, color = "Suicide Rate"), linetype = 2) +  # Plot normalized suicide rates
  geom_hline(yintercept = mean(global_stats$suicide_rate) / scaling_factor, linetype = "dashed", color = "grey") +  # Global average line for reference
  labs(
    title = "Trend of Suicide Rates and GDP per Capita Over Time",
    subtitle = "Normalized by the highest value for comparison across continents",
    x = "Year",
    y = "Normalized GDP per Capita",
    color = "Indicator"
  ) +
  scale_y_continuous(sec.axis = sec_axis(~ . * scaling_factor, name = "Suicide Rate per 100k")) +  
  facet_wrap(~ continent, scales = 'free_y') +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)) +
  scale_color_manual(values = c("darkblue", "orange"))

# Create a scatter plot comparing suicide rate against GDP per capita
suicide_rate_plot <- data %>%
  group_by(country, gdp_per_capita) %>%
  summarize(suicide_rate = round((sum(suicides_no) / sum(population)) * 100000, 2), .groups = 'drop') %>%
  ggplot(aes(x = gdp_per_capita, y = suicide_rate)) +
  geom_point(alpha = 0.4) +
  scale_x_continuous(labels = function(x) paste("$", format(x, big.mark = ",", scientific = FALSE))) +
  labs(
    title = "Suicide Rate vs. GDP per capita",
    x = "GDP per capita",
    y = "Suicides per 100k"
  )
plot(suicide_rate_plot)