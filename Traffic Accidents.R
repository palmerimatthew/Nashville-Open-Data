require(ggmap)
require(tidyverse)


# Comparing # of Injuries/Fatalities across different circumstances ----

# Weather Differences
Weather <- Traffic_Accidents_Clean %>%
  group_by(Weather) %>%
  summarise(Total = n(),
            Total_Injuries = sum(Number.of.Injuries),
            Total_Fatalaties = sum(Number.of.Fatalities),
            Number_With_Injury = sum(Number.of.Injuries >= 1),
            Number_With_Fatality = sum(Number.of.Fatalities >= 1)) %>%
  rename(Condition = Weather)

#percentages of Weather condition with injury
Weather %>%
  filter(Condition == 'CLEAR' | Condition == 'CLOUDY' | Condition == 'RAIN' | Condition == 'SNOW' | 
           Condition == 'FOG' | Condition == 'UNKNOWN') %>%
  select(Condition, Total, Number_With_Injury) %>%
  mutate(Percent = Number_With_Injury/Total*100) %>%
  ggplot(aes(x = reorder(Condition, Percent), y = Percent)) + geom_bar(stat = 'identity', fill = 'Dark Red') + 
  geom_text(aes(label = paste0(round(Percent, 2), '%'), y = Percent + 2)) +
  geom_text(aes(label = Total, y = -2)) + theme_bw() + coord_flip() +
  labs(x = 'Weather Condition', title = 'Percent of Car Crashes with Injuries Broken Down by Weather Condition') +
  theme(plot.title = element_text(hjust = 0.5))

#percentages of weather conditins with fatality
Weather %>%
  filter(Condition == 'CLEAR' | Condition == 'CLOUDY' | Condition == 'RAIN' | Condition == 'SNOW' |
           Condition == 'FOG' | Condition == 'UNKNOWN') %>%
  select(Condition, Total, Number_With_Fatality) %>%
  mutate(Percent = Number_With_Fatality/Total*100) %>%
  ggplot(aes(x = reorder(Condition, Percent), y = Percent)) + geom_bar(stat = 'identity', fill = 'Dark Red') +
  geom_text(aes(label = paste0(round(Percent, 2), '%'), y = Percent + 0.2)) +
  geom_text(aes(label = Total, y = -0.05)) + theme_bw() + coord_flip() +
  labs(x = 'Weather Condition', title = 'Percent of Car Crashes with Fatalities Broken Down by Weather Condition')
