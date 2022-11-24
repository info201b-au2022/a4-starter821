library(tidyverse)
library(dplyr)
library(ggplot2)
library(scales)

# The functions might be useful for A4
source("../source/a4-helpers.R")
# source("~/Documents/info201/data/incarceration_trends.csv")

library(readr)
incarceration_trends <- read_csv("~/Documents/info201/data/incarceration_trends.csv")


## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}




## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#
jail_pop_in_2018 <- incarceration_trends %>% 
  filter(year == max(year))

highest_total_pop <- jail_pop_in_2018 %>%
  arrange(desc(total_pop)) %>% 
  head(1)

highest_total_pop_state <- highest_total_pop %>% 
  pull(state)

highest_total_pop_num <- highest_total_pop %>% 
  pull(total_pop)

highest_total_pop_black <- highest_total_pop %>% 
  pull(black_pop_15to64)

highest_total_pop_aapi <- highest_total_pop %>% 
  pull(aapi_pop_15to64) 

highest_total_pop_latinx <- highest_total_pop %>% 
  pull(latinx_pop_15to64) 

highest_total_pop_white <- highest_total_pop %>% 
  pull(white_pop_15to64)

highest_total_pop_native <- highest_total_pop %>% 
  pull(native_pop_15to64)

black_rate <- paste0(round(highest_total_pop_num / highest_total_pop_black, 2), "%")


#of the population, how percentage. 
#total_num of population, total num of black population
#tot

total_pop_in_2018 <- jail_pop_in_2018 %>% 
  summarize(total_population = sum(total_pop_15to64)) %>% 
  pull()

black_pop_in_2018 <- jail_pop_in_2018 %>% 
  summarize(black_population = sum(black_pop_15to64)) %>% 
  pull()

black_pop_rate_2018 <- round(black_pop_in_2018/ total_pop_in_2018, 2) * 100

aapi_pop_2018 <- jail_pop_in_2018 %>% 
  summarize(aapi_population = sum(aapi_pop_15to64)) %>% 
  pull() 

aapi_pop_rate_2018 <- round(aapi_pop_2018/ total_pop_in_2018, 2) * 100

summary_info <- list(total_pop_in_2018,
                     black_pop_in_2018,
                     black_pop_rate_2018,
                     aapi_pop_2018,
                     aapi_pop_rate_2018
                     )




  
## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#

# This function gets the total U.S. jail population each year from 1970 to 2018 
get_year_jail_pop <- function() {
  jail_pop_df <- incarceration_trends %>% 
    group_by(year) %>% 
    summarize(total_pop_per_year = sum(total_jail_pop, na.rm = TRUE, default = 0)) %>% 
    group_by(year)
return(jail_pop_df)   
}


# This function plots a bar chart of the total U.S. jail population from 1970 to 2018 and portrays the trend
plot_jail_pop_for_us <- function()  {
  bar_chart <- ggplot(data = get_year_jail_pop(), 
                      aes (x = year, y = total_pop_per_year)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(label = comma) +
    labs(title = "Increase of Jail Population in U.S. (1970-2018)" ,
         y = "Total Jail Population",
         caption= "Figure 1. Increase of Jail Population in U.S. (1970-2018). \nThe chart shows the growth in jail population in the United States between 1970 and 2018.") +
    theme(plot.caption = element_text(hjust = 0),
          plot.title = element_text(hjust = 0.5))
  return(bar_chart)   
} 


## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
#----------------------------------------------------------------------------#

# This function returns the U.S. prison population each year from 1970 to 2018 
# by one or more states
get_jail_pop_by_states <- function(states) {
  jail_pop_by_states <- incarceration_trends %>% 
    group_by(state, year, total_jail_pop) %>% 
    filter(state %in% states) %>% 
    group_by(state, year) %>% 
    summarize(jail_pop = sum(total_jail_pop, na.rm = TRUE))
  return (jail_pop_by_states)
}

hi <- get_jail_pop_by_states(c("WA", "CA", "OR", "TX"))


# This function plots and returns a line chart showing the growth of the U.S.
# population from 1970 to 2018 by one or more states
plot_jail_pop_by_states <- function(states) {
  line_plot <- ggplot(data = get_jail_pop_by_states(states),
                      aes(x = year, y = jail_pop, group = state)) +
    geom_line(aes(color = state)) +
    scale_y_continuous(label = comma) +
    labs(title = "Increase of Jail Population in Washington, California, Oregon, and Texas\n(1970-2018)" ,
         y = "Total Jail Population",
         caption= "Figure 2. Increase of Jail Population in Washington, California, Oregon, and Texas. (1970-2018). \nThe chart shows the growth in jail population in the Washington, California, Oregon, and Texas between 1970 and 2018.") +
    theme(plot.caption = element_text(hjust = 0),
          plot.title = element_text(hjust = 0.5))
  return (line_plot)
}

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <Black Population Rate in Prison in Rural and Urban Areas (1970-2018)>
#----------------------------------------------------------------------------#

# this function 
get_prison_pop_by_urbanicity<- function() {
  prison_pop_by_urbanicity <- incarceration_trends %>% 
    filter(urbanicity == "rural" | urbanicity == "urban") %>% 
    group_by(year, urbanicity) %>%
    summarize(prison_pop = sum(total_prison_pop, na.rm = TRUE), 
              black_prison_pop = sum(black_prison_pop, na.rm = TRUE)) %>% 
    mutate(black_prison_rate = black_prison_pop/prison_pop * 100)
  return (prison_pop_by_urbanicity)
}



plot_prison_pop_by_urbanicity <- function() {
  line_plot <- ggplot(data = get_prison_pop_by_urbanicity(),
                      aes(x = year, y = black_prison_rate, group = urbanicity)) +
    geom_line(aes(color = urbanicity)) +
    scale_y_continuous(label = comma) +
    labs(title = "Black Population Rate in Prison in Rural and Urban Areas (1970-2018)",
         y = "Rate of black population in prison",
         caption= "Figure 2. Black Population Rate in Prison in Rural and Urban Areas (1970-2018). \nThe chart shows the difference in black population in prison between rural and urban areas") +
    theme(plot.caption = element_text(hjust = 0))
  return (line_plot)
}








## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


