incarceration_df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

library(dplyr)

incarceration_by_race <- incarceration_df %>%
  select(total_pop_15to64, latinx_pop_15to64, aapi_pop_15to64, black_pop_15to64, native_pop_15to64, white_pop_15to64) %>%
  summarise(total_pop = sum(total_pop_15to64, na.rm = TRUE), total_asian_pop = sum(aapi_pop_15to64, na.rm = TRUE), total_latinx_pop = sum(latinx_pop_15to64, na.rm = TRUE), total_black_pop = sum(black_pop_15to64, na.rm = TRUE), total_native_pop = sum(native_pop_15to64, na.rm = TRUE), total_white_pop = sum(white_pop_15to64, na.rm = TRUE)) %>%
  summarise(prop_asian = total_asian_pop / total_pop, prop_latinx = total_latinx_pop / total_pop, prop_black = total_black_pop / total_pop, prop_native = total_native_pop / total_pop, prop_white = total_white_pop / total_pop)

#Summary
summary_info <- list()
  summary_info$black_proportion = incarceration_by_race$prop_black 
  summary_info$white_proportion = incarceration_by_race$prop_white
  summary_info$asian_proportion = incarceration_by_race$prop_asian
  summary_info$native_proportion = incarceration_by_race$prop_native
  summary_info$latinx_proportion = incarceration_by_race$prop_latinx

mn_data <- incarceration_df %>%
  filter(state == "MN") %>%
  filter(total_pop > 100000)

#Plot 1: Population over time in large Minnesota counties
library(ggplot2)
mn_over_time <- ggplot(data = mn_data, aes(x = year, y = total_pop, color = county_name)) + 
  geom_point() +
  ylab("Total Incarcerated Population")+
  xlab("Year") +
  labs(color = "County Name") +
  ggtitle("Highest Minnesota Counties in Incarcerated Population Over Time")
  
#Plot 2: Black vs White pop in by region
black_v_total_pop <- incarceration_df %>%
  mutate(prop_black = black_pop_15to64 / total_pop, na.rm = TRUE) %>%
  mutate(prop_white = white_pop_15to64 / total_pop, na.rm = TRUE) %>%
  filter(year == "2018")

prop_by_region <- ggplot(black_v_total_pop, aes(x = prop_black, y = prop_white, color = region)) + 
  geom_point() +
  labs(
    title = "Proportion of Black vs. White Incarceration by region",
    x = "Proportion of Black Incarcerated Individuals",
    y = "Proportion of White Incarcerated Individuals",
    color = "Region"
  )

#Plot 3: Map of Black Proportion by State
library(usdata)
prop_by_state <- black_v_total_pop %>%
  mutate(state = abbr2state(state)) %>%
  mutate(state = tolower(state))

state_shape <- map_data("state") %>%
  dplyr::rename(state = region) %>%
  left_join(prop_by_state, by = "state")

prop_map <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = prop_black, size = 0.1)) +
  labs(
    y = "Longitude",
    x = "Latitude",
    title = "Proportion of Black Incarceration Across the United States",
    fill = "Proportion of Black Incarceration")


  



              
              
