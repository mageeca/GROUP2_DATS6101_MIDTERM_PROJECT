library(ezids)
library(dplyr)
library(ggplot2)

# importing data
gss1 <- data.frame(read.csv("GSS.csv"))

str(gss1)

summary(gss1)

head(gss1)

# Data Cleaning - Removing the rows that we don't require

gss1_h = subset(gss1, happy == "Very happy" | happy == "Pretty happy" | happy == "Not too happy")

gss1_hsj = subset(gss1_h, satjob == "Very satisfied" | satjob == "Moderately satisfied" | satjob == "A little dissatisfied" | satjob == "Very dissatisfied")

gss1_hsjc = subset(gss1_hsj, class_ == "Lower class" | class_ == "Working class" | class_ == "Middle class" | class_ == "Upper class")

gss1_hsjcsf = subset(gss1_hsjc, satfin == "Pretty well satisfied" | satfin == "More or less satisfied" | satfin == "Not satisfied at all")

gss1_hsjcsf = select(gss1_hsjcsf, -id_)

nrow(gss1_hsjcsf)

# Year wise dataframe subsets

gss_1972 = subset(gss1_hsjcsf, year == 1972)
gss_1973 = subset(gss1_hsjcsf, year == 1973)
gss_1974 = subset(gss1_hsjcsf, year == 1974)
gss_1975 = subset(gss1_hsjcsf, year == 1975)
gss_1976 = subset(gss1_hsjcsf, year == 1976)
gss_1977 = subset(gss1_hsjcsf, year == 1977)
gss_1978 = subset(gss1_hsjcsf, year == 1978)
gss_1980 = subset(gss1_hsjcsf, year == 1980)
gss_1982 = subset(gss1_hsjcsf, year == 1982)
gss_1983 = subset(gss1_hsjcsf, year == 1983)
gss_1984 = subset(gss1_hsjcsf, year == 1984)
gss_1985 = subset(gss1_hsjcsf, year == 1985)
gss_1986 = subset(gss1_hsjcsf, year == 1986)
gss_1987 = subset(gss1_hsjcsf, year == 1987)
gss_1988 = subset(gss1_hsjcsf, year == 1988)
gss_1989 = subset(gss1_hsjcsf, year == 1989)
gss_1990 = subset(gss1_hsjcsf, year == 1990)
gss_1991 = subset(gss1_hsjcsf, year == 1991)
gss_1993 = subset(gss1_hsjcsf, year == 1993)
gss_1994 = subset(gss1_hsjcsf, year == 1994)
gss_1996 = subset(gss1_hsjcsf, year == 1996)
gss_1998 = subset(gss1_hsjcsf, year == 1998)
gss_2000 = subset(gss1_hsjcsf, year == 2000)
gss_2002 = subset(gss1_hsjcsf, year == 2002)
gss_2004 = subset(gss1_hsjcsf, year == 2004)
gss_2006 = subset(gss1_hsjcsf, year == 2006)
gss_2008 = subset(gss1_hsjcsf, year == 2008)
gss_2010 = subset(gss1_hsjcsf, year == 2010)
gss_2012 = subset(gss1_hsjcsf, year == 2012)
gss_2014 = subset(gss1_hsjcsf, year == 2014)
gss_2016 = subset(gss1_hsjcsf, year == 2016)
gss_2018 = subset(gss1_hsjcsf, year == 2018)
gss_2021 = subset(gss1_hsjcsf, year == 2021)


# % Generally Happy People 1972 

# Approach 1

# vh_1972 <- nrow(filter(gss_1972, happy == "Very happy"))
# ph_1972 <- nrow(filter(gss_1972, happy == "Pretty happy"))
# nh_1972 <- nrow(filter(gss_1972, happy == "Not too happy"))
# 
# print("% of Very Happy People")
# (vh_1972 * 100 )/(vh_1972 + ph_1972 + nh_1972)
# print("% of Pretty Happy People")
# (ph_1972 * 100 )/(vh_1972 + ph_1972 + nh_1972)
# print("% of Not Very Happy People")
# (nh_1972 * 100 )/(vh_1972 + ph_1972 + nh_1972)
# 
# # % Generally Happy People 1973
# 
# vh_1973 <- nrow(filter(gss_1973, happy == "Very happy"))
# ph_1973 <- nrow(filter(gss_1973, happy == "Pretty happy"))
# nh_1973 <- nrow(filter(gss_1973, happy == "Not too happy"))
# 
# print("% of Very Happy People")
# (vh_1973 * 100 )/(vh_1973 + ph_1973 + nh_1973)
# print("% of Pretty Happy People")
# (ph_1973 * 100 )/(vh_1973 + ph_1973 + nh_1973)
# print("% of Not Very Happy People")
# (nh_1973 * 100 )/(vh_1973 + ph_1973 + nh_1973)

# Approach 2 using group_by()

# Class vs job-sat vs happy

gss_whole <- gss1_hsjcsf %>% group_by(year, class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_72to84 <- subset(gss_whole, year == 1972 | year == 1973 | year == 1974 | year == 1975 | year == 1976 | year == 1977 | year == 1978 | year == 1980 | year == 1982 | year == 1983 | year == 1984 )


gss_1972_group_a <- gss_1972 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1973_group_a <- gss_1973 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1974_group_a <- gss_1974 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1975_group_a <- gss_1975 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1976_group_a <- gss_1976 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1977_group_a <- gss_1977 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1978_group_a <- gss_1978 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1980_group_a <- gss_1980 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1982_group_a <- gss_1982 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1983_group_a <- gss_1983 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1984_group_a <- gss_1984 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1985_group_a <- gss_1985 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1986_group_a <- gss_1986 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1987_group_a <- gss_1987 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1988_group_a <- gss_1988 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1989_group_a <- gss_1989 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1990_group_a <- gss_1990 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1991_group_a <- gss_1991 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1993_group_a <- gss_1993 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1994_group_a <- gss_1994 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1996_group_a <- gss_1996 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1998_group_a <- gss_1998 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_2000_group_a <- gss_2000 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_2002_group_a <- gss_2002 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_2004_group_a <- gss_2004 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_2006_group_a <- gss_2006 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_2008_group_a <- gss_2008 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_2010_group_a <- gss_2010 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_2012_group_a <- gss_2012 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_2014_group_a <- gss_2014 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_2016_group_a <- gss_2016 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_2018_group_a <- gss_2018 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_2021_group_a <- gss_2021 %>% group_by(class_, satjob, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

# Class vs fin-sat vs happy

gss_1972_group_b <- gss_1972 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1973_group_b <- gss_1973 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1974_group_b <- gss_1974 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1975_group_b <- gss_1975 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1976_group_b <- gss_1976 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1977_group_b <- gss_1977 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1978_group_b <- gss_1978 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1980_group_b <- gss_1980 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1982_group_b <- gss_1982 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1983_group_b <- gss_1983 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1984_group_b <- gss_1984 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1985_group_b <- gss_1985 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1986_group_b <- gss_1986 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1987_group_b <- gss_1987 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1988_group_b <- gss_1988 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1989_group_b <- gss_1989 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1990_group_b <- gss_1990 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1991_group_b <- gss_1991 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1993_group_b <- gss_1993 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1994_group_b <- gss_1994 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1996_group_b <- gss_1996 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_1998_group_b <- gss_1998 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_2000_group_b <- gss_2000 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_2002_group_b <- gss_2002 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_2004_group_b <- gss_2004 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_2006_group_b <- gss_2006 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_2008_group_b <- gss_2008 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_2010_group_b <- gss_2010 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_2012_group_b <- gss_2012 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_2014_group_b <- gss_2014 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_2016_group_b <- gss_2016 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_2018_group_b <- gss_2018 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_2021_group_b <- gss_2021 %>% group_by(class_, satfin, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()



ggplot(filter(gss_whole, class_=="Middle class"),
       aes(x = satfin,
           y = total_count,
           fill = happy)) + 
  geom_bar(stat = "identity",
           position = "fill") +
  facet_grid(~ year)

ggplot(gss_2021_group_b,
       aes(x = satfin,
           y = total_count,
           fill = happy)) + 
  geom_bar(stat = "identity",
           position = "fill") +
  facet_grid(~ class_)

ggplot(filter(gss_72to84, class_=="Middle class"),
       aes(x = satfin,
           y = total_count,
           fill = happy)) + 
  geom_bar(stat = "identity",
           position = "fill") +
  facet_grid(~ year)


