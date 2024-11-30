#1
library(tidyverse)
library(janitor)
library(broom)
dat <- read.csv('FacultySalaries_1995.csv')

dat_keep <- dat %>%
  select(-c(AvgProfSalaryAll, AvgProfCompAll, NumInstructors, NumFacultyAll))

dat_both <- dat_keep %>%
  select(-c(starts_with('Num')))

dat_salary <- dat_both %>%
  select(-c(ends_with('Comp'))) %>%
rename('Full' = 'AvgFullProfSalary',
       'Assoc' = 'AvgAssocProfSalary',
       'Assist' = 'AvgAssistProfSalary') %>%
  pivot_longer(c('Full', 'Assoc', 'Assist'), names_to = 'Rank', values_to = 'Salary')

dat_comp <- dat_both %>%
  select(-c(ends_with('Salary'))) %>%
rename('Full' = 'AvgFullProfComp',
       'Assoc' = 'AvgAssocProfComp',
       'Assist' = 'AvgAssistProfComp') %>%
  pivot_longer(c('Full', 'Assoc', 'Assist'), names_to = 'Rank', values_to = 'Comp')

dat_num <- dat_keep %>%
  select(-c(starts_with('Avg'))) %>%
   rename('Full' = 'NumFullProfs',
         'Assoc' = 'NumAssocProfs',
         'Assist' = 'NumAssistProfs') %>%
  pivot_longer(c('Full', 'Assoc', 'Assist'), names_to = 'Rank', values_to = 'Number')
 
dat_clean <- dat_num %>%
  full_join(dat_comp) %>%
  full_join(dat_salary) %>%
  filter(Tier != 'VIIB')


dat_clean %>%
  ggplot(aes(x = Rank,
             y = Salary,
             fill = Rank)) +
  geom_boxplot(outlier.color = 'black', outlier.size = 3, size = .8) +
  facet_wrap(~Tier) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        title = element_text(size = 20),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15))


#2
anova_model <- aov(data = dat_clean, Salary ~ State + Tier + Rank)
summary(anova_model)


#3
dat_oils <- read.csv('Juniper_Oils.csv')

dat_oils <- dat_oils %>%
  clean_names() %>%
  rename_with(~ gsub("_", "-", .))

dat_oils_clean <- dat_oils %>%
  pivot_longer( cols = c("alpha-pinene","para-cymene","alpha-terpineol",
                         "cedr-9-ene","alpha-cedrene","beta-cedrene",
                         "cis-thujopsene","alpha-himachalene","beta-chamigrene",
                         "cuparene","compound-1","alpha-chamigrene","widdrol",
                         "cedrol","beta-acorenol","alpha-acorenol",
                         "gamma-eudesmol","beta-eudesmol","alpha-eudesmol",
                         "cedr-8-en-13-ol","cedr-8-en-15-ol","compound-2",
                         "thujopsenal"), 
                names_to = 'ChemicalID',
                values_to = 'Concentration') %>%
  clean_names()

View(dat_oils_clean)
#4
dat_oils_clean %>%
ggplot(aes(x = years_since_burn,
           y = concentration)) +
  geom_smooth() +
  facet_wrap(~chemical_id, scales = 'free') +
  theme_minimal()

#5
model <- glm(data = dat_oils_clean, concentration ~ years_since_burn * chemical_id)
tidy_model <- tidy(model) %>%
  filter(p.value < 0.05)

View(tidy_model)
