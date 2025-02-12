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

install.packages("Lahman", repos='https://cran.r-project.org', dependencies=TRUE)

library(Lahman)
head(LahmanData,20)

Batting_t <- tibble(Batting)
Pitching_t <- tibble(Pitching)
Fielding_t <- tibble(Fielding)

labels(Batting_t)
labels(Pitching_t)
labels(Fielding_t)
glimpse(Fielding_t)

Batting_t %>% count(playerID, yearID, stint) %>% filter(n>1)

Pitching_t %>% count(playerID, yearID, stint) %>% filter(n>1)

Fielding_t %>% count(playerID, yearID) %>% filter(n>1) %>% 
  summarise(max_n = max(n))

Batting.small <- Batting_t %>% select(playerID,yearID,stint,teamID, lgID, G, AB, HR)
head(Batting.small)


People.small <- People %>% select(playerID, nameGiven)
head(People.small)

Batting.small %>% left_join(People.small, by = "playerID") %>% 
  filter(nameGiven =="David Allan")


(People.small %>% anti_join(Batting.small, by = "playerID"))$playerID

head(Fielding_t)

dim(Fielding_t %>% 
  group_by(playerID) %>% 
  summarise(InnOuts = sum(InnOuts)) %>% 
  filter(InnOuts == 1) %>% 
  left_join(People, by="playerID") %>% 
  select(nameGiven, birthDate, debut) %>% 
  arrange(debut))




# Visualization Lecture

library(tidyverse)

mpg %>% ggplot(mapping = aes(x = displ, y = hwy, color = class)) + geom_point()

mpg %>% ggplot(mapping = aes(x = displ, y = hwy, shape = class)) + geom_point()

mpg %>% ggplot(mapping = aes(x = displ, y = hwy)) + 
  geom_point(shape = 21)


mpg %>% ggplot(mapping = aes(x = displ, y = hwy, col = drv)) +
  geom_point() + geom_smooth()

mpg %>% ggplot(mapping = aes(x = displ, y = hwy, color = drv)) + geom_point() + geom_smooth()

mpg %>% ggplot(mapping = aes(x = displ, y = hwy)) + 
  geom_point() + geom_smooth() + facet_grid(drv ~ cyl, scales = "free")

mpg %>% ggplot(mapping = aes(x = hwy), color = drv) + 
  geom_density()

mpg %>% ggplot(mapping = aes(x = class)) + geom_bar()

mpg %>% ggplot(aes(x=drv,y=class)) + geom_count()

mpg %>% ggplot(aes(x=drv,y=class,size = count(drv))) + geom_point()

mpg %>% ggplot(mapping = aes(x = displ, y = hwy, shape = class)) + geom_point() +
  labs(title = "title")+
  theme_classic()



# Exercise in class
library(babynames)
head(babynames)

babynames %>% filter(name == "Dakota") %>% 
  ggplot(mapping = aes(x = year, y = n, color = sex)) +
  geom_line() +
  ggtitle("Frequency of the 'Dakota' Baby Name in US") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Year", y = "Number") +
  xlim(1980,2016)+
  ylim(0,6300)+
  scale_color_discrete(labels = c("Female", "Male"))


library(plotly)

library(readxl)

iris %>% plot_ly(x = ~Sepal.Length, y = ~Petal.Length)


library(gapminder)
head(gapminder)


library(gganimate)
install.packages("gganimate")

gapminder %>% group_by(continent, year) %>% 
  summarise(GDP = mean(gdpPercap), lifeExp = mean(lifeExp),Sz = sum(pop), .groups = "drop") %>% 
  ggplot(aes(x=GDP, y=lifeExp, colour = continent, size = Sz)) +
  geom_point()+
  ggtitle("{frame_time}")+
  transition_time(year)

install.packages("gifski")
library(gifski)

head(mtcars)

gr <- mtcars %>% ggplot(aes(factor(cyl), mpg))+
  geom_boxplot() + labs()









