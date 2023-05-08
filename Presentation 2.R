library(tidyverse)
library(readr)
library(ggplot2)
library(patchwork)
library(knitr)
police_killings <- read_csv("police_killings.csv", +
                            + col_types = cols(age = col_number(),
                                               + day = col_number(), latitude = col_number(), 
                                               + longitude = col_number(), state_fp = col_number(), 
                                               + county_fp = col_number(), tract_ce = col_number(), 
                                               + geo_id = col_number(), county_id = col_number(), 
                                               + pop = col_number(), share_white = col_number(), 
                                               + share_black = col_number(), share_hispanic = col_number(), 
                                               + p_income = col_number(), h_income = col_number(), 
                                               + county_income = col_number(), comp_income = col_number(), 
                                               + county_bucket = col_number(), nat_bucket = col_number(), 
                                               + pov = col_number(), urate = col_number(), 
                                               + college = col_number()))

summary(police_killings)


p1 = police_killings %>%
  ggplot(aes(fct_infreq(state))) +                    
  geom_bar(width = 0.8, aes(fill=raceethnicity)) + 
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70)) +
  labs(x='State', y='Number of Killings')

a1 = police_killings %>%
  ggplot(aes(fct_infreq(state))) +
  geom_bar(width = 0.8, fill="red4") + 
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  labs(x='State', y='Number of Killings per state')

a2= police_killings %>%
  ggplot(aes(fct_infreq(raceethnicity))) +
  geom_bar(width = 0.8, fill="red4") + 
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  labs(x='race/ethnicity', y='Number of Killings by race')

a1/a2 

p5 = police_killings %>% 
  ggplot(aes(fct_infreq(raceethnicity))) +                    
  geom_bar(aes(fill=cause), width = 0.5) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  scale_fill_manual(values=c("#000000", "red4", "blue4", "darkgreen", "tan3")) +
  scale_y_continuous(breaks=c(0,25,50,75,100,125,150,175,200,225)) +
  labs(x='Ethnicity') +
  ggtitle("Cause of death by ethnicity") +
  theme(plot.title = element_text(hjust = 0.5))

b1 = police_killings %>% 
  ggplot(aes(fct_infreq(cause))) +                    
  geom_bar(aes(fill=raceethnicity), width = 0.5) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  scale_fill_manual(values=c("#000000", "tan4", "orangered3", "forestgreen", "grey37", 'coral3')) +
  labs(x='Cause', y='Number of deaths') +
  ggtitle("Cause of death by ethnicity") +
  theme(plot.title = element_text(hjust = 0.5))



p6 = police_killings %>% 
  ggplot(aes(fct_infreq(raceethnicity))) +                    
  geom_bar(aes(fill=armed), width = 0.5) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  scale_y_continuous(breaks=c(0,25,50,75,100,125,150,175,200,225)) +
  labs(x='Ethnicity') +
  ggtitle("Armed victim by State") +
  theme(plot.title = element_text(hjust = 0.5))

b2 = police_killings %>% 
  ggplot(aes(fct_infreq(armed))) +                    
  geom_bar(aes(fill=raceethnicity), width = 0.5) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  scale_fill_manual(values=c("#000000", "tan4", "orangered3", "forestgreen", "grey37", 'coral3')) +
  labs(x='Arm',  y='Number of deaths') +
  ggtitle("Armed victim by State") +
  theme(plot.title = element_text(hjust = 0.5))


# Patchwork

b1/b2

a1/a2 

