install.packages(c("rvest","scales","tidyverse"))
library(tidyverse)
#install rvest to read html and scales to map data values visual properties 
#and format graph labels. Scales works behind the scenes as the scaling agent
#for ggplot2


library(rvest)
library(scales)

#Official SSA life table
ssa_url<-"https://www.ssa.gov/oact/STATS/table4c6.html"

#read all html tables from webpage
tables<-read_html(ssa_url)%>%
  html_table(fill=TRUE)

#select the main mortality table
mortality_raw<-tables[[1]]

head(mortality_raw)
view(mortality_raw)


#to replace a double header with clear column names
names(mortality_raw)<-c("age","male_qx","male_Lives","male_life_expectancy",
                        "female_qx", "female_lives","female_life_expectancy")
#qx=probability of dying within 1 year
#lives=survivors out og group of 100,000
#life expectancy= average remaining numbers of years expected

view(mortality_raw)
#convert entries from text to numbers
mortality<-mortality_raw %>%
  mutate(across(everything(),readr::parse_number)) %>%
  filter(!is.na(age)) %>%
  arrange(age)

glimpse(mortality)
head(mortality)
write_csv(mortality,"mortality_data.csv")

#examine selected ages
selected_ages<-mortality %>%
  filter(age %in% c(20,30,40,50,60,65,70,80)) %>%
  select(age,male_qx,female_qx,male_life_expectancy,female_life_expectancy)
print(selected_ages)

#summarize data
mortality_summary<-mortality %>%
  filter(age<=100) %>%
  mutate(
    age_group=case_when(
      age<20 ~ "Under 20",
      age<40 ~ "20-39",
      age<60 ~ "40-59",
      age<80 ~ "60-79",
      TRUE ~ "80 and Older"
    )
  )  %>%
  group_by(age_group) %>%
  summarise(
    average_male_qx=mean(male_qx,na.rm=TRUE),
    average_female_qx=mean(female_qx,na.rm=TRUE),
    average_male_life_expectancy=mean(male_life_expectancy,na.rm=TRUE),
    average_female_life_expectancy=mean(female_life_expectancy,na.rm=TRUE),
    .groups="drop"
  )
print(mortality_summary)      

#changing male and female mortality columns from wide to long format
mortality_long<-mortality %>%
  select(age,male_qx,female_qx) %>%
  pivot_longer(
    cols=c(male_qx,female_qx),
    names_to = "sex",
    values_to = "death_probability"
  )%>%
  mutate(
    sex=recode(sex,male_qx="Male",female_qx="Female"
    )
  )
head(mortality_long)

#creating a mortality rate chart
mortality_chart<-mortality_long %>%
  filter(age<=100) %>%
  ggplot(
    aes(
      x=age,y=death_probability,linetype=sex
    )
  ) +
  geom_line(linewidth=0.9) +
  scale_y_log10(
    labels=label_percent(accuracy = 0.01)
  ) +
  labs(
    title="U.S. Mortality Rates by Age and Sex",
    subtitle="SSA 2023 Period Life Table",
    x="Age",
    y="Annual Probability of Death",
    linetype="Sex",
    caption="Source: Social Security Administration"
  ) +
  theme_minimal()
mortality_chart

#saving chart
ggsave(
  filename="mortality_rate.png",
  plot=mortality_chart,
  width=9,
  height=6,
  dpi=300
)

#create life expectancy chart
life_expectance_long<-mortality %>%
  select(age,male_life_expectancy,female_life_expectancy) %>%
  pivot_longer(cols=c(male_life_expectancy,female_life_expectancy),
               names_to = "sex",
               values_to = "remaining_life_expectancy") %>%
  mutate(
    sex=recode(sex,male_life_expectancy="Male",female_life_expectancy="Female")
  )
view(life_expectance_long)

#create chart for life expectancy
life_expectancy_chart<-life_expectance_long %>%
  filter(age<=100) %>%
  ggplot(
    aes(x=age,y=remaining_life_expectancy, linetype=sex)
  ) +
  geom_line(linewidth=0.9)+
  labs(title="Remaining Life Expectancy by Age and Sex",
       subtitle="SSA 2023 Period Life Table",
       x="Current Age",
       y="Expected Remining Years",
       linetype="Sex",
       caption="Source: Social Security Administration") +
  theme_minimal()
life_expectancy_chart

#saving
ggsave(
  filename="life_expectancy.png",
  plot=life_expectancy_chart,
  width=9,
  height=6,
  dpi=300
)

