# This code export mobility figures generated in R.
# raw data from Google https://www.google.com/covid19/mobility/
# set workding directory
getwd()
# setwd("./data")

rm(list=ls())
gc()

library(tidyverse)
library(data.table)
library(zoo)

# import cleaned data =========================================================
dt = fread("Google-state-mobility-clean.csv")
# dt$date = as.Date(dt$date, format = "%Y-%m-%d")

# glimpse(dt)
# Variables: 5
# $ statename      <chr> "Alabama", "Alabama", "Alabama", "Alabama", "Alabama", "Alabama", "Ala...
# $ statepostal    <chr> "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL"...
# $ date           <date> 2020-02-15, 2020-02-16, 2020-02-17, 2020-02-18, 2020-02-19, 2020-02-2...
# $ category       <chr> "retail & recreation", "retail & recreation", "retail & recreation", "...
# $ percent_change <int> 5, 0, 3, -4, 4, -7, 3, 5, 3, -2, 7, 16, 12, 12, 15, 16, 8, 20, 7, 7, 9...

# get 7-day moving average ====================================================
dt_GG <- dt %>%
  arrange(statepostal,category,date) %>%
  group_by(statepostal,category) %>%
  mutate(percent_change_ma = rollmean(percent_change, 7, fill = NA)) %>%
  select(date,statepostal,category,percent_change_ma)

# Plot figures (GG) ===========================================================
gg_cat = "grocery & pharmacy"
p0_GG_grocery <- ggplot(dt_GG[which(dt_GG$category == gg_cat),],
                 aes(x = date, y = percent_change_ma, group = statepostal)) +
  geom_line(size = 1, color = "coral3", alpha = 0.2) + 
  labs(title = paste0("percent_change: in mobility relative to baseline -- ",gg_cat),  
       subtitle = "[source: Google] This data set starts on 2020-02-15") +
  xlab('date') + ylab('7-day moving average') + 
  scale_x_date(date_breaks="2 weeks", date_labels="%b %e", 
               limits = as.Date(c('2020-02-15','2020-08-01'))) +
  theme_light() + theme(text = element_text(size = 10),
                        panel.grid = element_blank(), 
                        plot.title = element_text(face = 'bold',size = 10),
                        plot.subtitle = element_text(size = 9))
p0_GG_grocery

gg_cat = "parks"
p0_GG_parks <- ggplot(dt_GG[which(dt_GG$category == gg_cat),],
                        aes(x = date, y = percent_change_ma, group = statepostal)) +
  geom_line(size = 1, color = "aquamarine4", alpha = 0.2) + 
  labs(title = paste0("percent_change: in mobility relative to baseline -- ",gg_cat),  
       subtitle = "[source: Google] This data set starts on 2020-02-15") +
  xlab('date') + ylab('7-day moving average') + 
  scale_x_date(date_breaks="2 weeks", date_labels="%b %e", 
               limits = as.Date(c('2020-02-15','2020-08-01'))) +
  theme_light() + theme(text = element_text(size = 10),
                        panel.grid = element_blank(), 
                        plot.title = element_text(face = 'bold',size = 10),
                        plot.subtitle = element_text(size = 9))
p0_GG_parks

gg_cat = "residential"
p0_GG_residential <- ggplot(dt_GG[which(dt_GG$category == gg_cat),],
                      aes(x = date, y = percent_change_ma, group = statepostal)) +
  geom_line(size = 1, color = "darkgoldenrod3", alpha = 0.2) + 
  labs(title = paste0("percent_change: in mobility relative to baseline -- ",gg_cat),  
       subtitle = "[source: Google] This data set starts on 2020-02-15") +
  xlab('date') + ylab('7-day moving average') + 
  scale_x_date(date_breaks="2 weeks", date_labels="%b %e", 
               limits = as.Date(c('2020-02-15','2020-08-01'))) +
  theme_light() + theme(text = element_text(size = 10),
                        panel.grid = element_blank(), 
                        plot.title = element_text(face = 'bold',size = 10),
                        plot.subtitle = element_text(size = 9))
p0_GG_residential

gg_cat = "retail & recreation"
p0_GG_retail <- ggplot(dt_GG[which(dt_GG$category == gg_cat),],
                            aes(x = date, y = percent_change_ma, group = statepostal)) +
  geom_line(size = 1, color = "cornflowerblue", alpha = 0.2) + 
  labs(title = paste0("percent_change: in mobility relative to baseline -- ",gg_cat),  
       subtitle = "[source: Google] This data set starts on 2020-02-15") +
  xlab('date') + ylab('7-day moving average') + 
  scale_x_date(date_breaks="2 weeks", date_labels="%b %e", 
               limits = as.Date(c('2020-02-15','2020-08-01'))) +
  theme_light() + theme(text = element_text(size = 10),
                        panel.grid = element_blank(), 
                        plot.title = element_text(face = 'bold',size = 10),
                        plot.subtitle = element_text(size = 9))
p0_GG_retail

gg_cat = "transit stations"
p0_GG_transit <- ggplot(dt_GG[which(dt_GG$category == gg_cat),],
                       aes(x = date, y = percent_change_ma, group = statepostal)) +
  geom_line(size = 1, color = "darkgray", alpha = 0.2) + 
  labs(title = paste0("percent_change: in mobility relative to baseline -- ",gg_cat),  
       subtitle = "[source: Google] This data set starts on 2020-02-15") +
  xlab('date') + ylab('7-day moving average') + 
  scale_x_date(date_breaks="2 weeks", date_labels="%b %e", 
               limits = as.Date(c('2020-02-15','2020-08-01'))) +
  theme_light() + theme(text = element_text(size = 10),
                        panel.grid = element_blank(), 
                        plot.title = element_text(face = 'bold',size = 10),
                        plot.subtitle = element_text(size = 9))
p0_GG_transit

gg_cat = "workplaces"
p0_GG_work <- ggplot(dt_GG[which(dt_GG$category == gg_cat),],
                        aes(x = date, y = percent_change_ma, group = statepostal)) +
  geom_line(size = 1, color = "mediumorchid4", alpha = 0.2) + 
  labs(title = paste0("percent_change: in mobility relative to baseline -- ",gg_cat),  
       subtitle = "[source: Google] This data set starts on 2020-02-15") +
  xlab('date') + ylab('7-day moving average') + 
  scale_x_date(date_breaks="2 weeks", date_labels="%b %e", 
               limits = as.Date(c('2020-02-15','2020-08-01'))) +
  theme_light() + theme(text = element_text(size = 10),
                        panel.grid = element_blank(), 
                        plot.title = element_text(face = 'bold',size = 10),
                        plot.subtitle = element_text(size = 9))
p0_GG_work


# select_state = 1:nrow(dt_GG) # nrow = 5 in figure
select_state = which(dt_GG$statepostal %in% c("NC","SC","GA","VA")) # nrow = 2 in figure
p1_GG <- ggplot(dt_GG[select_state,],
                aes(x = date, y = percent_change_ma, color = category)) +
  geom_line(size = 1) + 
  theme_light() + theme(text = element_text(size = 12),
                        panel.grid = element_blank(), 
                        plot.title = element_text(face = 'bold'),
                        plot.subtitle = element_text(size = 11))
p1_GG <- p1_GG + facet_wrap(~ statepostal, nrow = 2) + 
  labs(title = "percent_change: percentage change in mobility relative to baseline",  
       subtitle = "[source: Google] This data set starts on 2020-02-15") +
  xlab('date') + ylab('7-day moving average') + 
  scale_color_manual(values=c("coral3", "aquamarine4", "darkgoldenrod3",
                              "cornflowerblue","darkgray","mediumorchid4")) +
  scale_x_date(date_breaks="1 month", date_labels="%b",  
               limits = as.Date(c('2020-02-15','2020-08-01'))) 
p1_GG


# export figures =============================================================
pdf("Rplot_Google_mobility_sample.pdf",width = 14, height = 6)
p0_GG_grocery
p0_GG_parks
p0_GG_work
p1_GG
dev.off()
