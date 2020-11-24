## DS 303 Final Project
# Authors: Jessie Bustin and Ben Litterer
#
# Mapping

# Load Libraries
library(ggplot2)
library(tidyverse)

# Load Data
turnout <- read.csv("Turnout_Demos_County.csv")

# Data cleaning
turnout <- turnout %>%
  select(-X) %>%
  mutate(turnout = as.numeric(turnout)) %>%
  mutate(county = tolower(county)) %>%
  mutate(state = tolower(state))

turnout2012 <- turnout %>%
  filter(year == 2012)

turnout2016 <- turnout %>%
  filter(year == 2016)

counties <- map_data('county')

counties <- counties %>%
  rename(state = region, county = subregion)

# Merge County Mapping Data with turnout data
counties2012 <- left_join(turnout2012, counties, by = c("county", "state"))

# Check for merge fails
fails <- counties2012 %>%
  filter(is.na(lat))

# Plot Map
plot <- ggplot() +
  geom_polygon(data = counties2012, aes(x = long, y = lat, group = group, fill = turnout)) +
  coord_map() +
  ggtitle("2012 Voter Turnout As A Percent of Population Over 18") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("") +
  ylab("")

plot <- plot + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text = element_blank(), axis.ticks = element_blank())

plot + scale_fill_gradient(low = "#f7fcfd", high = "#00441b")

# 2016
counties2016 <- left_join(turnout2016, counties, by = c("county", "state"))

# Check for merge fails
fails <- counties2016 %>%
  filter(is.na(lat))

# Plot Map
plot <- ggplot() +
  geom_polygon(data = counties2016, aes(x = long, y = lat, group = group, fill = turnout)) +
  coord_map() +
  ggtitle("2016 Voter Turnout As A Percent of Population Over 18") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("") +
  ylab("")

plot <- plot + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text = element_blank(), axis.ticks = element_blank())

plot + scale_fill_gradient(low = "#f7fcfd", high = "#00441b")
