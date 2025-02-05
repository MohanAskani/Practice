# test

# adding new line

library(babynames)
head(babynames)
library(tidyr)
library(dplyr)

babynames_untidy <- babynames %>% select(-prop) %>% pivot_wider(names_from = year, values_from = n)
head(babynames_untidy)


library(nycflights13)
glimpse(flights)

flights %>% select(month, day, sched_dep_time, carrier, flight, dep_delay, arr_delay) %>% 
  pivot_longer(cols = c(dep_delay,arr_delay),names_to = "delay_type", values_to = "delay")

flights %>% select(month, day, sched_dep_time, carrier, flight, dep_delay, arr_delay) %>% 
  unite(col = "delays", dep_delay:arr_delay, sep = ";")


library(gapminder)
library(tidyverse)

gapminder_wide <- gapminder %>% filter(country %in% c("France","Germany")) %>% 
  select(country, year, gdpPercap) %>% 
  pivot_wider(names_from = year, values_from = gdpPercap, names_prefix = 'Y')

gapminder_long <- gapminder_wide %>% 
  pivot_longer(cols = c(starts_with('Y')), names_to = "year", values_to = "gdpPercap") %>% 
  mutate(year = parse_number(year))

head(gapminder_wide)
head(gapminder_long)


