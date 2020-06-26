require(tidyverse)


Budget_2019 <- Nashville_Budget_Clean %>%
  filter(Budget_Type == 'Budgeted'
         & Year == 2019)

#Budget by Department in 2019
Nashville_Budget_Clean %>%
  filter(Budget_Type == 'Actual' & Year == 2019) %>%
  group_by(Department.Description) %>%
  summarise(Budget = sum(Budget)) %>%
  mutate(log_budget = log(Budget),
         Budget_M = Budget/1000000) %>%
  ggplot(aes(x = reorder(Department.Description, Budget_M), y = Budget_M)) + geom_bar(stat = 'identity') + coord_flip() +
  geom_text(aes(label = round(Budget_M, 3), y = Budget_M + 100)) + theme_bw() + 
  labs(x = 'Department', y = 'Budget (in Millions)')

#Deeper dive into Police spending ----
Police_Spending_2019 <- Nashville_Budget_Clean %>%
  filter(Budget_Type == 'Actual' &
           Year == 2019 &
           Department.Description == 'Police') %>%
  mutate(Object.Account.Description = as.character(Object.Account.Description),
         Expense_Type = case_when(grepl('Pay$', Object.Account.Description) ~ 'Pay',
                                  grepl('^Employer', Object.Account.Description) ~ 'Benefits',
                                  T ~ Object.Account.Description))

Police_Salaries <- Nashville_Government_Demographics_Clean %>%
  filter(Current.Department == 'Police' &
           grepl('^Police', Title) &
           Title != 'Police Identification Spec 1' &
           Title != 'Police Info Svcs Admin' &
           Title != 'Police Officer 3') %>%
  mutate(Police_Level = case_when(grepl('Chief', Title) ~ 'Chiefs',
                                  Title == 'Police Captain' ~ 'Mid-Level',
                                  Title == 'Police Commander' ~ 'Mid-Level',
                                  Title == 'Police Lieutenant' ~ 'Mid-Level',
                                  Title == 'Police Sergeant' ~ 'Sergeant',
                                  grepl('Trainee', Title) ~ 'Trainee',
                                  grepl('Officer', Title) ~ 'Officer',
                                  grepl('Crisis', Title) ~ 'Crisis Counseling',
                                  grepl('Operations' , Title) ~ 'Operations',
                                  grepl('Security', Title) ~ 'Security',
                                  T ~ 'Other'))

#Gender and Race differences in pay and position
Police_Salaries %>% #Gender Salary
  ggplot(aes(x = Police_Level, y = Annual.Salary, fill = Gender)) + geom_boxplot() + coord_flip() + theme_bw()
Police_Salaries %>%
  group_by(Police_Level, Gender) %>%
  summarise(count = n(),
            Salary = mean(Annual.Salary)) %>%
  ungroup() %>%
  mutate(sal = Salary*count,
         total_sal = ave(sal, Police_Level, FUN = sum),
         total_pep = ave(count, Police_Level, FUN = sum),
         percent = round(count/total_pep*100, 2),
         percent = paste0(percent, '%'),
         avg_sal = total_sal/total_pep,
         adj_sal = round(avg_sal/1000),
         adj_sal = paste0(adj_sal, 'K'),
         adj_sal = if_else(Gender == 'F', NA_character_, adj_sal)) %>%
  ggplot(aes(x = reorder(Police_Level, avg_sal), y = count, fill = Gender)) + 
  geom_bar(position = position_dodge(), stat = 'identity') + coord_flip() + theme_bw() + 
  geom_text(aes(label = adj_sal, y = -50)) +
  geom_text(aes(label = percent, y = count + 75), position = position_dodge(width = 0.75)) + 
  labs(x = 'Police Level', y = 'Count')


Police_Salaries %>%
  mutate(Race = if_else(Ethnic.Code.Description == 'White', 'White', 'Non-White')) %>%
  ggplot(aes(x = Police_Level, y = Annual.Salary, fill = Race)) + geom_boxplot() + coord_flip() + theme_bw()
Police_Salaries %>%
  mutate(Race = if_else(Ethnic.Code.Description == 'White', 'White', 'Non-White')) %>%
  group_by(Police_Level, Race) %>%
  summarise(count = n(),
            Salary = mean(Annual.Salary)) %>%
  ungroup() %>%
  mutate(sal = Salary*count,
         total_sal = ave(sal, Police_Level, FUN = sum),
         total_pep = ave(count, Police_Level, FUN = sum),
         percent = round(count/total_pep*100, 2),
         percent = paste0(percent, '%'),
         avg_sal = total_sal/total_pep,
         adj_sal = round(avg_sal/1000),
         adj_sal = paste0(adj_sal, 'K'),
         adj_sal = if_else(Race == 'White', NA_character_, adj_sal)) %>%
  ggplot(aes(x = reorder(Police_Level, avg_sal), y = count, fill = Race)) + 
  geom_bar(position = position_dodge(), stat = 'identity') + coord_flip() + theme_bw() + 
  geom_text(aes(label = adj_sal, y = -50)) +
  geom_text(aes(label = percent, y = count + 75), position = position_dodge(width = 0.75)) +
  labs(x = 'Police Level', y = 'Count')


#Odds Ratios based on upper-level vs lower-level for both race and gender differences

Three_Way_G_R_PL_Count <- Police_Salaries %>%
  mutate(Police_Tier = case_when(Police_Level == 'Chiefs' ~ 'Upper',
                                 Police_Level == 'Mid-Level' ~ 'Upper',
                                 Police_Level == 'Sergeant' ~ 'Upper',
                                 T ~ 'Lower'),
         Race = if_else(Ethnic.Code.Description == 'White', 'White', 'Non-White')) %>%
  group_by(Police_Tier, Race, Gender) %>%
  summarise(count = n()) %>%
  ungroup()

#Gender OR
Three_Way_G_R_PL_Count %>%
  group_by(Police_Tier, Gender) %>%
  summarise(count = sum(count)) %>%
  pivot_wider(names_from = Gender, values_from = count) %>%
  ungroup() %>%
  select(-Police_Tier) %>%
  as.matrix() %>%
  `rownames<-`(c('Lower', 'Upper')) %>%
  epitools::oddsratio()

Three_Way_G_R_PL_Count %>%
  group_by(Police_Tier, Race) %>%
  summarise(count = sum(count)) %>%
  pivot_wider(names_from = Race, values_from = count) %>%
  ungroup() %>%
  select(-Police_Tier) %>%
  as.matrix() %>%
  `rownames<-`(c('Lower', 'Upper')) %>%
  epitools::oddsratio()




  