# Packages ----------------------------------------------------------------


library(tidyverse)
library(psych)
library(ggpubr)


# Data loading ------------------------------------------------------------


corona <- read.csv('train.csv')
big_five <- read.csv('data-final.csv', sep="\t", header=T)
ISO <- read.csv("iso.csv")


# Covid data manipulations ------------------------------------------------------


corona <- corona[order(corona$Date),]
corona <- select(corona,"Country_Region","Date","ConfirmedCases")
corona <- filter(corona,ConfirmedCases>=50)
head(corona)

corona_rlvnt <- corona %>% 
  group_by(Country_Region,Date) %>% 
  summarise(confirmed = sum(ConfirmedCases))
head(corona_rlvnt)

rlvnt_countries <- corona_rlvnt %>% 
  group_by(Country_Region) %>% 
  mutate(n_day = row_number()) %>% 
  filter(n_day >= 14) %>% 
  select(Country_Region) %>% 
  unique()

covid <- corona_rlvnt %>% 
  filter(Country_Region %in% rlvnt_countries$Country_Region)

covid <- covid %>%  
  group_by(Country_Region) %>% 
  filter(Date == tail(Date,1))


# ISO ---------------------------------------------------------------------


ISO <- ISO %>% 
  select(c(1,2)) %>% 
  rename(Country_Region = 1,
         Country_abbr = 2)

ISO$Country_Region <- as.character(ISO$Country_Region) 
ISO[117, "Country_Region"] = "Korea, South"

covid_abbr <- merge(covid, ISO, by = "Country_Region")


# Big 5 -------------------------------------------------------------------


big_five_q <- big_five %>% 
  select(c(1:50)) %>% 
  mutate_all(funs(as.numeric(as.character(.))))

# Big five keys
keys <- c(1,-1,1,-1,1,-1,1,-1,1,-1,
          1,-1,1,-1,1,1,1,1,1,1,
          -1,1,-1,1,-1,1,-1,1,1,1,
          1,-1,1,-1,1,-1,1,-1,1,1,
          1,-1,1,-1,1,-1,1,1,1,1)

rev_big_five <- as.data.frame(reverse.code(keys,
                                           big_five_q,
                                           mini = rep(1,50),
                                           maxi = rep(5,50)))

rev_big_five$Country_abbr <- as.character(big_five$country)

rev_big_five_s <- rev_big_five %>% 
  group_by(Country_abbr) %>% 
  mutate(n = row_number()) %>% 
  filter(n >= 1000) %>% 
  select(Country_abbr) %>% 
  unique()

rev_big_five_ron <- rev_big_five %>% 
  filter(Country_abbr %in% rev_big_five_s$Country_abbr)

rev_big_five_ron$EXT_m = rowMeans(rev_big_five_ron[,c(1:10)], na.rm = T)
rev_big_five_ron$EST_m = rowMeans(rev_big_five_ron[,c(11:20)], na.rm = T)
rev_big_five_ron$AGR_m = rowMeans(rev_big_five_ron[,c(21:30)], na.rm = T)
rev_big_five_ron$CSN_m = rowMeans(rev_big_five_ron[,c(31:40)], na.rm = T)
rev_big_five_ron$OPN_m = rowMeans(rev_big_five_ron[,c(41:50)], na.rm = T)

rev_big_five_m <- rev_big_five_ron %>% 
  select(c(51:56)) %>% 
  group_by(Country_abbr) %>% 
  summarize(EXT = mean(EXT_m, na.rm = T),
            EST = mean(EST_m, na.rm = T),
            AGR = mean(AGR_m, na.rm = T),
            CSN = mean(CSN_m, na.rm = T),
            OPN = mean(OPN_m, na.rm = T))

death <- merge(covid_abbr, rev_big_five_m, by = "Country_abbr")


# Plotting Big 5 by dimension and country ---------------------------------------------------------


ggscatter(death, x = "EXT", y = "confirmed", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Extraversion", ylab = "Confirmed Cases")

ggscatter(death, x = "EST", y = "confirmed", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Emotional Stability", ylab = "Confirmed Cases")

ggscatter(death, x = "AGR", y = "confirmed", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Agreeableness", ylab = "Confirmed Cases")

ggscatter(death, x = "CSN", y = "confirmed", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Conscientiousness", ylab = "Confirmed Cases")

ggscatter(death, x = "OPN", y = "confirmed", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Openness to experience", ylab = "Confirmed Cases")


# Bye bye China (this is relevant to the data from 22-3-2020 ~) -


death_c <- death %>% 
  filter(Country_Region != "China")

ggscatter(death_c, x = "EXT", y = "confirmed", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Extraversion", ylab = "Confirmed Cases")

ggscatter(death_c, x = "EST", y = "confirmed", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Emotional Stability", ylab = "Confirmed Cases")

ggscatter(death_c, x = "AGR", y = "confirmed", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Agreeableness", ylab = "Confirmed Cases")

ggscatter(death_c, x = "CSN", y = "confirmed", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Conscientiousness", ylab = "Confirmed Cases")

ggscatter(death_c, x = "OPN", y = "confirmed", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Openness to experience", ylab = "Confirmed Cases")