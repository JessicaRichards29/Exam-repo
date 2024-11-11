
#Task 1: Read in unicef data
library(tidyverse)
library(easystats)

dat <- read_csv('unicef-u5mr.csv')


#Task 2: Get data into tidy format
dat_tidy <- dat %>%
  pivot_longer(starts_with('U5MR'),
               names_to = 'Year',
               values_to = 'U5MR') %>% 
separate(Year, into = c('U5MR.', 'Year')) %>% 
  mutate(U5MR. = NULL) %>% 
  mutate(Country = CountryName,
         CountryName = NULL)

dat_tidy$Year <- as.numeric(dat_tidy$Year) 


#Task 3: Plot each country's U5MR over time
dat_plot_1 <- dat_tidy %>%
  filter(!if_any(everything(), is.na))

dat_plot_1 %>%
  group_by(Country) %>%
  ggplot(aes(x = Year,
             y = U5MR,
             group = Country)) +
  geom_line() +
  facet_wrap(~ Continent)


#Task 4: Save the plot as RICHARDS_Plot_1.png
#Done


#Task 5: Create another plot that shows mean U5MR for all 
#countries within a given continent each year

dat_plot_2 <- dat_tidy %>%
  group_by(Continent, Year) %>%
  summarise(Mean_U5MR = mean(U5MR, na.rm = TRUE), .groups = 'drop') 

dat_plot_2 %>%
  ggplot(aes(x = Year,
             y = Mean_U5MR,
             color = Continent)) +
  geom_line(linewidth = 1.5) +
  theme_minimal()


#Task 6: Save plot as RICHARDS_Plot_2.png
#Done


#Task 7: Create 3 models of U5MR
mod1 <- glm(data = dat_tidy, 
            formula = U5MR ~ Year)

mod2 <- glm(data = dat_tidy, 
            formula = U5MR ~ Year + Continent)
  
mod3 <- glm(data = dat_tidy, 
            formula = U5MR ~ Year * Continent)


#Task 8: Compare 3 models with respect to performance
compare_performance(mod1, mod2, mod3) %>% plot()
compare_performance(mod1, mod2, mod3)
#mod3 is the best model because it has the smallest AIC with the highest R2


#Task 9: Plot 3 models' predictions like so:

dat_plot_3 <- dat_tidy 
  
dat_plot_3$mod1 <- predict(mod1, dat_tidy)
dat_plot_3$mod2 <- predict(mod2, dat_tidy)
dat_plot_3$mod3 <- predict(mod3, dat_tidy)

dat_plot_3 <- dat_plot_3 %>%
  pivot_longer(starts_with('mod'), names_to = 'Model', values_to = 'Predicted_U5MR')

dat_plot_3 %>%
  ggplot(aes(x = Year, 
             y = Predicted_U5MR, 
             color = Continent)) +
  geom_smooth() +
  facet_wrap(~ Model)


#Task 10: BONUS - Using preferred model, predict what U5MR 
#would be for Ecuador in year 2020. Real value was 13 per
# 1000 live births. How far off were you?

new_data <- data.frame(Year = 2020, Continent = 'Americas')

predicted_U5MR <- predict(mod3, newdata = new_data)

print(predicted_U5MR)

#the value I got was -10.58 per 1000 live births, this is obviously
#wrong because you can't have a negative amount of births. I think
#the reason it's so far off (~24 off) is because the model only used 
#Continent and Year data, not Country data, and the U5MR can vary a lot between
#Countries within a Continent. Or I did the prediction wrong haha

