library(tidyverse)
library(dplyr)
library(usmap)
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

total_jail_pop_in_2018 <- jail_pop_in_2018 %>% 
  summarize(total_jail_population = sum(total_jail_pop, na.rm = TRUE)) %>% 
  pull()

black_jail_pop_in_2018 <- jail_pop_in_2018 %>% 
  summarize(black_jail_pop = sum(black_jail_pop, na.rm = TRUE)) %>% 
  pull()

black_jail_pop_rate_2018 <- round(black_jail_pop_in_2018/ total_jail_pop_in_2018, 2) * 100

aapi_jail_pop_2018 <- jail_pop_in_2018 %>% 
  summarize(aapi_jail_pop = sum(aapi_jail_pop, na.rm = TRUE)) %>% 
  pull() 

aapi_jail_pop_rate_2018 <- round(aapi_jail_pop_2018/ total_jail_pop_in_2018, 2) * 100

latinx_jail_pop_2018 <- jail_pop_in_2018 %>% 
  summarize(latinx_jail_pop = sum(latinx_jail_pop, na.rm = TRUE)) %>% 
  pull() 

latinx_jail_pop_rate_2018 <- round(latinx_jail_pop_2018/ total_jail_pop_in_2018, 2) * 100

summary_info <- list(total_jail_pop_in_2018,
                     black_jail_pop_in_2018,
                     black_jail_pop_rate_2018,
                     aapi_jail_pop_2018,
                     aapi_jail_pop_rate_2018,
                     latinx_jail_pop_2018,
                     latinx_jail_pop_rate_2018
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
    theme(plot.caption = element_text(hjust = 0.5),
          plot.title = element_text(hjust = 0))
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
    labs(title = "Increase of Jail Population in Washington, California, Oregon, and Texas \n(1970-2018)" ,
         y = "Total Jail Population",
         caption= "Figure 2. Increase of Jail Population in Washington, California, Oregon, and Texas. (1970-2018). \nThe chart shows the growth in jail population in the Washington, California, Oregon, and Texas between 1970 and 2018.") +
    theme(plot.caption = element_text(hjust = 0.5),
          plot.title = element_text(hjust = 0))
  return (line_plot)
}

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <Black Population Rate in Prison in Rural and Urban Areas (1970-2018)>
#----------------------------------------------------------------------------#

# This function returns the black prison population rate rural and urban
# areas each year from 1970 to 2018
get_prison_pop_by_urbanicity<- function() {
  prison_pop_by_urbanicity <- incarceration_trends %>% 
    filter(urbanicity == "rural" | urbanicity == "urban") %>% 
    group_by(year, urbanicity) %>%
    summarize(prison_pop = sum(total_prison_pop, na.rm = TRUE), 
              black_prison_pop = sum(black_prison_pop, na.rm = TRUE)) %>% 
    mutate(black_prison_rate = black_prison_pop/prison_pop * 100)
  return (prison_pop_by_urbanicity)
}


# This function plots a line plot of the difference of the trend of 
# black prison population rate between rural and urban areas from 1970 to 2018
plot_prison_pop_by_urbanicity <- function() {
  line_plot <- ggplot(data = get_prison_pop_by_urbanicity(),
                      aes(x = year, y = black_prison_rate, group = urbanicity)) +
    geom_line(aes(color = urbanicity)) +
    scale_y_continuous(label = comma) +
    labs(title = "Black Population Rate in Prison in Rural and Urban Areas (1970-2018)",
         y = "Rate of black population in prison",
         caption= "Figure 3. Black Population Rate in Prison in Rural and Urban Areas (1970-2018). \nThe chart shows the difference in black population in prison between rural and urban areas") +
    theme(plot.caption = element_text(hjust = 0.5))
  return (line_plot)
}




## Section 6  ---- 
#----------------------------------------------------------------------------#
# <Jail Population Trend Map between female and male >
#----------------------------------------------------------------------------#
get_female_percent_jail_2018 <- function() {
  female_percent <- incarceration_trends %>% 
    group_by(state) %>% 
    filter(year == max(year)) %>% 
    select(state, female_jail_pop, total_jail_pop) %>% 
    group_by(state) %>% 
    mutate(female_jail_rate = round(female_jail_pop / total_jail_pop * 100, 2)) %>% 
    summarize(female_jail_rate = mean(female_jail_rate, na.rm = TRUE)) 
  female_percent$female_jail_rate[is.nan(female_percent$female_jail_rate)] <- 0
  
  return (female_percent)
}


get_male_percent_jail_2018 <- function() {
  male_percent <- incarceration_trends %>% 
    group_by(state) %>% 
    filter(year == max(year)) %>% 
    select(state, male_jail_pop, total_jail_pop) %>% 
    group_by(state) %>% 
    mutate(male_jail_rate = round(male_jail_pop / total_jail_pop * 100, 2)) %>% 
    summarize(male_jail_rate = mean(male_jail_rate, na.rm = TRUE)) 
  male_percent$male_jail_rate[is.nan(male_percent$male_jail_rate)] <- 0
  return (male_percent)
}



plot_female_map <- function() {
  female_map <- plot_usmap(data = get_female_percent_jail_2018(),
                              values = "female_jail_rate",
                              color = "#deebf7") +
  scale_fill_continuous(low = "white",
                        high = "#3182bd", 
                        name = "Jail Population Rate (%)",
                        label = scales::comma) +
  theme(legend.position = "right") +
  labs(title = "U.S. Female Jail Average Population Rate in 2018",
       caption = "Figure 5. U.S. Female Jail Average Population Rate in 2018. \nThe chart shows the geographical difference in female jail average population rate.") +
    theme(plot.title = element_text(hjust = 0), plot.caption = element_text(hjust = 0.5))
  return (female_map)
}



plot_male_map <- function() {
  male_map <- plot_usmap(data = get_male_percent_jail_2018(),
                              values = "male_jail_rate",
                              color = "#deebf7") +
    scale_fill_continuous(low = "white",
                        high = "#3182bd", 
                        name = "Jail Population Rate (%)",
                        label = scales::comma) +
    theme(legend.position = "right") +
    labs(title = "U.S. Male Jail Average Population Rate in 2018",
         caption = "Figure 6. U.S. Male Jail Average Population Rate in 2018. \nThe chart shows the geographical difference in male jail average population rate.") +
    theme(plot.title = element_text(hjust = 0), plot.caption = element_text(hjust = 0.5))
    
  return (male_map)
}






