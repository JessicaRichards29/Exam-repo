#I read in data
library(tidyverse)

df <- read_csv("./Exam_1/cleaned_covid_data.csv")
View(df)

#II A_states
A_states <- df %>%
  filter(grepl('A', Province_State))
View(A_states)

#III plot A_states
A_states %>%
ggplot(aes(x = Last_Update,
           y = Deaths)) +
  geom_point() +
  stat_smooth() +
  facet_wrap('Province_State', scales = "free")

#IV state_max_fatality_rate
state_max_fatality_rate <- df %>%
  group_by(Province_State) %>%
  mutate(Maximum_Fatality_Rate = max(Case_Fatality_Ratio, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(Province_State, Maximum_Fatality_Rate, .keep_all = TRUE) %>% 
  select(Province_State, Maximum_Fatality_Rate) %>%
  arrange(desc(Maximum_Fatality_Rate))
View(state_max_fatality_rate)

#V plot state_max_fatality_rate
state_max_fatality_rate %>%
  mutate(Province_State = as.factor(Province_State)) %>% 
  mutate(Province_State = reorder(Province_State, -Maximum_Fatality_Rate)) %>%
  ggplot(aes(x = Province_State,
             y = Maximum_Fatality_Rate)) +
  geom_col() +
  theme(axis.text.x = element_text (angle = 90))

#VI plot cumulative deaths for entire US over time
deaths_data <- df %>%
  group_by(Last_Update) %>%
  summarize(Total_Deaths = sum(Deaths, na.rm = TRUE)) %>%
  arrange(Last_Update) %>%
  mutate(Cumulative_Deaths = cumsum(Total_Deaths))
  
deaths_data%>% 
ggplot(aes(x = Last_Update, y = Cumulative_Deaths)) +
  geom_col()
