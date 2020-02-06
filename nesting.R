library(ggplot2)
library(tidyr)
library(dplyr)
library(gapminder)
library(purrr)
library(broom)

# quick look at the data
gapminder

# plot the data... damn ungly
ggplot(gapminder, aes(year, lifeExp)) +
  geom_line(aes(group = country))

# use nest() to collapse the data to a single row per goup
by_country <- gapminder %>%
  group_by(continent, country) %>%
  nest()

by_country

# look at the first one, it contains the complete data for Afghanistan (minus the grouping columns)
by_country$data[[1]]

# fit a linear model to the data column
by_country <- by_country %>%
  mutate(model = purrr::map(data, ~ lm(lifeExp ~ year, data = .))
  )

# mutate added a new column 'model' containing the stuff from lm
by_country

# unnest() gets your out of the odd form
by_country %>% unnest(data)

# use purrr::map and broom::glance to extract model summeries
by_country %>% unnest(model %>% map(glance))

# extract coefficients
by_country %>% unnest(model %>% map(tidy))

# extract residuals etc:
by_country %>% unnest(model %>% map(augment))

# plot the lm data by country...
ggplot(by_country, aes(year, lifeExp)) +
  geom_line(aes(group = country))
