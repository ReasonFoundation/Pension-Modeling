library(tidyverse)
library(readxl)
library(here)

active_count <- read_excel(here('data', 'ASRS_2019_members_count_salary.xlsx'), sheet = 'active_count') %>% 
  filter(`Current Age` != "Total") %>% 
  mutate(age = case_when(
    str_sub(`Current Age`, 1, 2) == 'Un' ~ 20L,
    TRUE ~ as.integer(str_sub(`Current Age`, 1, 2))
  ),
  age = age + 2) %>% 
  select(age, count = Total)

actives <- vector()
j <- 22
max_age = max(active_count$age)
# translating the active count to population for age distribution plot
for (i in active_count$count) {
  actives <- c(actives, rep(j, times = i))
  j = j + 5
  if (j > max_age)
    break
}

actives <- tibble(age = actives, population = 'actives')

# retirees
retiree_count <- read_excel(here('data', 'ASRS_2019_members_count_salary.xlsx'), sheet = 'retiree_count') %>% 
  filter(`Current Age` != "Total") %>% 
  mutate(age = case_when(
    str_sub(`Current Age`, 1, 2) == 'Be' ~ 50L,
    str_sub(`Current Age`, 1, 2) == 'Ov' ~ 95L,
    TRUE ~ as.integer(str_sub(`Current Age`, 1, 2))
  ),
  age = age + 2) %>% 
  select(age, count = Total)

retirees <- vector()
j <- 52
max_age = max(retiree_count$age)
# translating the active count to population for age distribution plot
for (i in retiree_count$count) {
  retirees <- c(retirees, rep(j, times = i))
  j = j + 5
  if (j > max_age)
    break
}

retirees <- tibble(age = retirees, population = 'retirees')

members <- bind_rows(actives, retirees)

ggplot(members, aes(color = population)) +
  geom_histogram(aes(age), binwidth = 5)
