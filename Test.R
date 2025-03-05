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


# Learning Functions

X <- 2
sample_func <- function(x,p){
  X^p
}
sample_func(3,3)


f <- function(x,...)
{
  dots <- list(...)                   
  if(length(dots) == 0) return(NULL) 
  cat("The arguments in ... are\n")
  print(dots)
  f(...)                              
}
f(1,2)



rsquared <- function(x, value = as.formula('r.squared')){
  print(summary(x)$value)
}

model1 <- lm(mpg$hwy ~ mpg$cyl)

class(model1)

rsquared(model1)



# Exercise

mpg_split <- split(mpg, mpg$manufacturer)

models <- mpg_split %>% 
  map(~ lm(hwy ~ displ, data = .x)) %>% 
  map_dbl(~summary(.x)$r.squared) %>% 
  tibble(manufacturer = names(.), R_squared = .)
  
models

models %>% filter(R_squared < 0.1)

models <- mpg %>% 
  split(.$manufacturer) %>% 
  map(~ lm(hwy ~ displ, data = .x)) %>% 
  map_dbl(~summary(.x)$r.squared) %>% 
  tibble(manufacturer = names(.), R_squared = .)

library(rvest)
library(tidyverse)
URL <- "https://en.wikipedia.org/wiki/List_of_Marvel_Cinematic_Universe_films"

page <- URL %>% read_html() 
table <- URL %>% read_html() %>% html_elements("table") %>% html_table(fill = TRUE)

write_lines(page, file = "text.txt")
write_lines(page, file = "text.html")

movies_tables <- table[3:8]

table_cleaned <- URL %>% read_html() %>% html_elements("table") %>% html_table(fill = TRUE) %>% map(janitor::clean_names)
table_cleaned[[1]]
table[[1]]
URL %>% read_html() %>% html_elements("h1") %>% html_text()
URL %>% read_html() %>% html_elements("h2") %>% html_text()
URL %>% read_html() %>% html_elements("h3") %>% html_text()
URL %>% read_html() %>% html_elements("h4") %>% html_text()


paragraphs <- URL %>% read_html() %>% html_elements("p") 
paragraphs[2] %>% html_text()


rutgers_stat_dept_url <- "https://statistics.rutgers.edu/people-pages/faculty"
rutgers_stat_dept <- rutgers_stat_dept_url %>% read_html()

rutgers_stat_dept %>% html_elements(".hasTooltip span") %>% html_text()

url <- "http://www.omdbapi.com/?apikey=29edb997&i=tt3896198"
browseURL(url)

url2 <- "http://www.omdbapi.com/?apikey=29edb997&i=tt3896198&plot=short&r=JSON?"
browseURL(url2)

url3 <- "http://www.omdbapi.com/?apikey=29edb997&i=tt3896198&s=Iron&plot=short&r=json?"
browseURL(url3)

library(curl)
json_result <- url %>%  curl() %>%  readLines()

movie <- url3 %>% fromJSON() %>% pluck("Search")
movie <- url %>% fromJSON()
movie$Actors


function_actors <- function(url){
  movie <- url %>% fromJSON()
  actors <- movie$Actors
  actors <- str_trim(str_split(actors,",")[[1]])
  return(actors)
}

function_actors(url)




library(nycflights13)
flights %>% tabyl(carrier, month) %>% adorn_totals()

