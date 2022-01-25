###########################################################
## Preliminary Analysis of Turtle Capture Data & FP Data ##
###########################################################

### directory fetch
#getwd()
#setwd("/Users/aidanperez/Documents/FP_incidence_CR/Scripts")


# library(ggplot2)
# library(dplyr)
# library(tidyr)
# library(readr)
library(ggthemes)
library(gridExtra)
library(tidyverse)
library(lubridate)
library(janitor)
library(reshape)

### import data
capture_data <- read.csv("2021-10_Cm_capture_data.csv")
fp_scores <- read.csv("2021-10_Cm_FP_Scores.csv")

# explore data
head(capture_data)  #probably only need to print the first few rows
summary(capture_data)
str(capture_data)

head(fp_scores)
summary(fp_scores)
str(fp_scores)
table(capture_data$Fibropapilloma.Visible, useNA = "ifany")  #counts each response, including NAs
str(capture_data$Fibropapilloma.Visible)

# Make sure all "No"s are consistent in spelling; removes any extra space
capture_data$Fibropapilloma.Visible<- gsub(pattern = " *",  #detect space(s)
                                           replacement = "", #replace w/ nothing
                                           x = capture_data$Fibropapilloma.Visible)
table(capture_data$Fibropapilloma.Visible, useNA = "ifany")  #confirm that issue is fixed

# Create two data frames, one including turtles wtih FP visible and without
green_w_fp <- filter(capture_data, Fibropapilloma.Visible == "Yes")
green_wo_fp <- filter(capture_data, Fibropapilloma.Visible == "No")

#check it if worked
table(capture_data$Fibropapilloma.Visible) # should have yes and no
table(green_w_fp$Fibropapilloma.Visible) # should only have yes
table(green_wo_fp$Fibropapilloma.Visible) # should only have no 
#it worked
#72 of 112 listed with FP visible / 64.28% ; 7 turtles listed as "NA"







##################
#### viz data ####
##################

### fp and balaz frequency among captured turtles

## Hist of Balazs scores frequency per among captured turtles
ggplot(green_w_fp, aes(x = FP.Balazs.Score)) +
  geom_histogram(binwidth = 1, colour = "grey", fill = "#008080") +
  theme_bw() +
  ylab("Frequency\n") +
  xlab("\nFibropapilloma Balazs Score")  +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "plain"),
        panel.grid = element_blank(),
        plot.margin = unit(c(1,1,1,1), units = , "cm"))

## FP Tumor Score frequency among captured turtle 
#FP.Tumor.Score is a character, needs to be converted to an integer
str(green_w_fp$FP.Tumor.Score) # is chr
green_w_fp$FP.Tumor.Score <- as.integer(green_w_fp$FP.Tumor.Score) # tumor scores will now be integers
str(green_w_fp$FP.Tumor.Score) #it worked

ggplot(green_w_fp, aes(x=FP.Tumor.Score))+
  geom_histogram(binwidth = 1, colour = "grey", fill = "#008080")+
  theme_bw() +
  ylab("Frequency\n")+
  xlab("\nFibropapilloma Tumor Score") +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "plain"),
        panel.grid = element_blank(),
        plot.margin = unit(c(1,1,1,1), units = , "cm"))

### FP Balazs scores by season

# seasons are listed as numbers, will be renamed with the following naming convention 
#fp score distribution by seasons
# 1 - Spring
# 2 - Summer
# 3 - Fall
# 4 - Winter

green_w_fp$Season <- replace(green_w_fp$Season, green_w_fp$Season == 1, "Spring")
green_w_fp$Season <- replace(green_w_fp$Season, green_w_fp$Season == 2, "Summer")
green_w_fp$Season <- replace(green_w_fp$Season, green_w_fp$Season == 3, "Fall")
green_w_fp$Season <- replace(green_w_fp$Season, green_w_fp$Season == 4, "Winter")
# check if it worked
table(green_w_fp$Season)

# it worked

## plot FP Balazs scores by season
ggplot(green_w_fp, aes(x=FP.Balazs.Score, y=Season)) + 
  geom_point(col = "#008080", size = 3) +
  coord_flip()


### FP Balazs scores by year

ggplot(green_w_fp, aes(x=FP.Balazs.Score, y=Year)) + 
  geom_point(col = "#008080", size = 3) +
  coord_flip()


###frequency of FP presence by year and season

fp_visibility <- capture_data %>%
  group_by(Year,Fibropapilloma.Visible) %>%
  summarise(fp_visibility = length((Fibropapilloma.Visible)))

# make this a stacked bar graph, with one internal bar being "yes" and one bar being "yes and no"
# have this displayed for each year


fp_year_summ<- capture_data %>%
  group_by(Year, Fibropapilloma.Visible) %>%
  tally() %>%
  drop_na() %>%
  group_by(Year) %>%
  mutate(total.N = sum(n),
         prop = n / total.N)


# Plotting counts
ggplot(fp_year_summ, aes(Year, n, fill = Fibropapilloma.Visible)) +
  geom_col() +
  scale_fill_brewer("FP Visible", palette = 'Dark2') +
  theme_bw()

# Plotting proportions
ggplot(fp_year_summ, aes(Year, prop, fill = Fibropapilloma.Visible)) +
  geom_col() +
  scale_fill_brewer("FP Visible", palette = 'Dark2') +
  theme_bw()

### FP Balazs boxplot per year 

charyear <- as.character(green_w_fp$Year) # changing "Year" from integer to character

balazsyearbox <- ggplot(green_w_fp, aes(charyear, FP.Balazs.Score ))
balazsyearbox + geom_boxplot(varwidth = T, fill="#008080") +
  labs(title = "Year box plot",
       x = "Years",
       y = "FP Balazs Scores")

### FP Balazs Boxplot per season 

str(green_w_fp$Season)

balazsSZNbox <- ggplot(green_w_fp, aes(Season, FP.Balazs.Score ))
balazsSZNbox + geom_boxplot(varwidth = T, fill="#008080") +
  labs(title = "Year box plot",
       x = "Seasons",
       y = "FP Balazs Scores")

### Linear Regression Balazs Score  by Year 

# with standard error lines 
ggplot(green_w_fp, aes(x=FP.Balazs.Score, y=Year)) + 
  geom_point(col = "#008080", size = 3) +
  geom_smooth(method = 'lm') +
  coord_flip()

# without standard error lines 
ggplot(green_w_fp, aes(x=FP.Balazs.Score, y=Year)) + 
  geom_point(col = "#008080", size = 3) +
  geom_smooth(method = 'lm', se = FALSE) +
  coord_flip()
