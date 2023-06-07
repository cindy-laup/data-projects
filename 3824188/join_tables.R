# Libraries -------------------------------------------------------------------- 
# install.packages("tidyverse")
  library(tidyverse)
# install.packages("maps")
  library(maps)

# Scientific notation ----------------------------------------------------------
options(scipen = 999)

# Data download ----------------------------------------------------------------
unicef_metadata <- read_csv("unicef_metadata.csv")
unicef_indicator_1 <- read_csv("unicef_indicator_1.csv")
unicef_indicator_2 <- read_delim("unicef_indicator_2.csv", 
delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Full table data --------------------------------------------------------------
manual_join <- right_join(unicef_metadata, unicef_indicator_1, by = c("country", 
        "alpha_2_code", "alpha_3_code", "numeric_code", "year" = "time_period"))
write_csv(manual_join, "manual_join.csv")

# Prep Map ---------------------------------------------------------------------
map_deprivation <- filter(manual_join, obs_value > 0.02)
map_world <- map_data("world") 
map_world_deprivation <- left_join(map_world, map_deprivation, by = c("region" = 
                                                                     "country"))

# Map --------------------------------------------------------------------------
ggplot(data = map_world_deprivation) +
  aes(x = long, y = lat, group = group, fill = obs_value) +
  geom_polygon(color = "black", size = 0.3) +
  scale_fill_gradient(low = "yellow", high = "darkblue", na.value = "grey47") +
  labs(
    title = "Countries where children suffer deprivation",
    subtitle = "Countries couloured have registered >0,02% of deprivation",
    x = "Longitude", 
    y = "Latitude",
    fill = "Percentage of children 
suffering deprivation"
  ) +
  theme_light() +
  theme(text = element_text(size = 10, colour = "deepskyblue4"))

# Prep Bar chart ---------------------------------------------------------------
deprivation <- map_deprivation %>%
  filter(sex == "Total") %>%
  mutate(pop = (obs_value*`Population, total`)/1000000)

# Bar chart --------------------------------------------------------------------
ggplot(data = deprivation) +
  aes(x = alpha_3_code, y = obs_value, group = year, fill = alpha_3_code) +
  geom_col(position = position_dodge(1), col="blue", size = 0.3) +
  scale_fill_brewer(palette = "Set2") +
  geom_text(aes(label = pop), vjust = 20, size = 2.5, col = "orangered3") +
  geom_text(aes(label = year), vjust = 2.5, size = 4, col = "blue") +
  geom_text(aes(label = country), vjust = 5, hjust = 0.5, size = 3, col = "grey15") +
  labs(
    title = "Rate of children over 5 suffering deprivation per country per year",
    subtitle = "Data from 2014 to 2018", 
    x = "Country", 
    y = "Percentage of children suffering deprivation", 
    fill = "Country",
    caption = "In red, the number of children suffering deprivation in million.",
  ) +
  theme_light() +
  theme(text = element_text(size = 10, colour = "deepskyblue4")) +
  theme(legend.position = "none")

# Prep Scatterplot -------------------------------------------------------------
full_join <- full_join(unicef_metadata, unicef_indicator_1, by = c("country", 
        "alpha_2_code", "alpha_3_code", "numeric_code", "year" = "time_period"))

life_exp <- full_join %>% 
  select(country, year, GDP = `GDP per capita (constant 2015 US$)`) %>%
  filter(country == c('Angola', 'Madagascar', 'Ethiopia', 'Togo', 'Benin', 'Chad',
                                   "Lao" = "Lao People's Democratic Republic"))

# Scatterplot ------------------------------------------------------------------
ggplot(life_exp) +
  aes(x = year, y = GDP, color = country) +
  geom_point(alpha = 0.6, size = 1) +
  geom_smooth(method = "lm") +
  facet_wrap(~ country, nrow = 1) +
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2020)) +
  labs(
    x = "Year",
    y = "GDP per capita (US$)",
    title = "Evolution of the GDP per capita from 1960 to 2018 per country"
  ) +
  theme_classic() +
  theme(text = element_text(size = 10, colour = "deepskyblue4")) +
  guides(color ="none")         

# Prep Time Serie Chart --------------------------------------------------------
comparision <- full_join %>% 
  select(country, year, population = 'Population, total') %>%
  mutate (pop = population/1000000) %>%
  filter(country == c('Madagascar', 'Ethiopia'))

# Time Serie Chart -------------------------------------------------------------
ggplot(comparision, aes(x = year, y = pop)) + 
  geom_area(aes(color = country, fill = country), 
            alpha = 0.5, position = position_dodge(0.8)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  labs(
    x = "Year",
    y = "Population (million)",
    title = "Evolution of the populations of Ethiopia and Madagascar in 60 years"
  ) +
  theme_classic() +
  theme(text = element_text(size = 10, colour = "deepskyblue4"))






# Prep Bar chart (not used) ----------------------------------------------------
full_join <- full_join(unicef_metadata, unicef_indicator_1, by = c("country", 
                                                                   "alpha_2_code", "alpha_3_code", "numeric_code", "year" = "time_period"))

tableau <- full_join %>% 
  select(country, year, population = 'Population, total') %>%
  mutate (pop = population/1000000) %>%
  filter(country == c('Angola', 'Madagascar', 'Ethiopia', 'Togo', 'Benin')) %>%
  filter(year == 2018) 

# Bar chart (not used) ---------------------------------------------------------
ggplot(data = tableau) +
  aes(x = country, y = pop, group = year, fill = country) +
  geom_col(position = position_dodge(1), col="blue", size = 0.3) +
  scale_fill_brewer(palette = "Set2") +
  geom_text(aes(label = pop), vjust = 1.5, size = 4, col = "blue") +
  labs(
    title = "Total population comparision",
    subtitle = "Data from 2018",
    x = "Country",
    y = "Population (million)",
  ) +
  theme_light() +
  theme(text = element_text(size = 14))

# Brouillon (not used) ---------------------------------------------------------
new_tableau <- full_join %>% 
  select(country, year, population = 'Population, total') %>%
  mutate (pop = population/1000000) %>%
  filter(country == c('Madagascar', 'Ethiopia'))

ggplot(data = new_tableau) +
  aes(x = country, y = pop, group = year, fill = country) +
  geom_col(position = position_dodge(1), col="blue", size = 0.3) +
  scale_fill_brewer(palette = "Set2") +
  geom_text(aes(label = pop), vjust = 1.5, size = 4, col = "blue") +
  labs(
    title = "Total population comparision",
    subtitle = "Data from 2018",
    x = "Country",
    y = "Population (million)",
  ) +
  theme_light()
  
  