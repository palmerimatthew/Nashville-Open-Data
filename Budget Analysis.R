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
  filter(Current.Department == 'Police')

data.table::fwrite(Police_Spending_2019, 'Police_Spending.csv')

#Advertising
### Advertising & Promot'n, 

#Transportation
### Aircraft Fuel, Auto Fuel, Auto Supply

  