library(ezids)
library(dplyr)
library(ggplot2)
#library(DataExplorer)
library(plotly)
library(ggeasy)
library(ggpubr)
library(gganimate)
library(gifski)
library(transformr)
library(scales)

# importing data
gss1 <- data.frame(read.csv("C:/Users/patha/OneDrive/Desktop/MS in USA/#GWU/SEMESTER 1/DATS 6101 INTRO TO DS/Midterm project/GROUP2_DATS6101_MIDTERM_PROJECT-main/GSS.csv"))

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


#ggplot(data = gss1_hsjcsf, aes(x = year, fill = happy)) +
#  geom_bar(position = "dodge")

#Stacked Numeric Data containing bar plot
ggplot(data = gss1_hsjcsf, aes(x = year, fill = happy)) +
  geom_bar(position = "fill") + ylab("proportion") +
  stat_count(geom = "text", 
             aes(label = stat(count)),
             position=position_fill(vjust=0.5), colour="white")


#just density curve

ggplot(gss1_hsjcsf) + 
  stat_density(aes(x=year, y=..scaled..,color=happy), position="dodge", geom="line")

# Histogram overlaid with kernel density curve
ggplot(gss1_hsjcsf, aes(x = year, fill = happy)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=0.7,
                 colour="black", fill="white") +
  geom_density(alpha=.7, fill="#FF6666") 



#HAPPINESS TSA

gss_happy <- gss1_hsjcsf %>% 
  group_by(year, happy) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_happy_perc1 <- transform(gss_happy,
                             perc = ave(total_count,
                                        year,
                                        FUN = prop.table))

gss_happy_perc1$perc = gss_happy_perc1$perc * 100

#plot

ggplotly(gss_happy_perc1 %>%
           ggplot( aes(x=year, y=perc)) +
           geom_line(aes(colour=happy)) +
           geom_point()+
           ylab("Percentage Happy Satisfaction "))

gif1<-gss_happy_perc1 %>%
           ggplot( aes(x=year, y=perc)) +
           geom_line(aes(colour=happy)) +
           geom_point()+
           ylab("Percentage Happy Satisfaction ")+
           transition_reveal(year)  


animate(gif1, renderer = gifski_renderer(), fps = 60, height = 400, width =400)
anim_save("gif1.gif")



#SATFIN TSA
gss_satfin <- gss1_hsjcsf %>% 
  group_by(year, satfin) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_satfin_perc1 <- transform(gss_satfin,
                              perc = ave(total_count,
                                         year,
                                         FUN = prop.table))

gss_satfin_perc1$perc = gss_satfin_perc1$perc * 100

#plot

ggplotly(gss_satfin_perc1 %>%
           ggplot( aes(x=year, y=perc)) +
           geom_line(aes(colour=satfin)) +
           geom_point()+
           ylab("Percentage Financial Satisfaction "))

gif2 <- gss_satfin_perc1 %>%
           ggplot( aes(x=year, y=perc)) +
           geom_line(aes(colour=satfin)) +
           geom_point()+
           ylab("Percentage Financial Satisfaction ")+
           transition_reveal(year)

animate(gif2, renderer = gifski_renderer(), fps = 60, height = 400, width =400)
anim_save("gif2.gif")
  


#SATJOB INTERACTIVE TSA

gss_satjob <- gss1_hsjcsf %>% 
  group_by(year, satjob) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_satjob_perc1 <- transform(gss_satjob,
                              perc = ave(total_count,
                                         year,
                                         FUN = prop.table))

gss_satjob_perc1$perc = gss_satjob_perc1$perc * 100

#plot

ggplotly(gss_satfin_perc1 %>%
           ggplot( aes(x=year, y=perc)) +
           geom_line(aes(colour=satfin)) +
           geom_point()+
           ylab("Percentage Financial Satisfaction "))

gif3<-gss_satjob_perc1 %>%
           ggplot( aes(x=year, y=perc)) +
           geom_line(aes(colour=satjob)) +
           geom_point()+
           ylab("Percentage Job Satisfaction")+
           transition_reveal(year)

animate(gif3, renderer = gifski_renderer(), fps = 60, height = 400, width =800)
anim_save("gif3.gif")



# making plot of relationship between class and job satisfaction
ggplot(gss1_hsjcsf, aes(x=satjob, fill=happy)) +
  geom_bar(position = "dodge")+
  facet_wrap(~class_)  +
  labs(title="Relationship Between Job Satisfaction and Happiness Levels by Social Class",x="Job Satisfaction", y = "Frequency",fill="Level of Happiness")+
  theme_bw()+theme(plot.title = element_text(face="bold",hjust = 0.5))+
  scale_fill_brewer(palette = "Set2")+scale_x_discrete(limits = c("Very dissatisfied","A little dissatisfied","Moderately satisfied","Very satisfied"))

# making plot of relationship between class and financial satisfaction
ggplot(gss1_hsjcsf, aes(x=satfin, fill=happy)) +
  geom_bar(position = "dodge")+
  facet_wrap(~class_)  +
  labs(title="Relationship Between Financial Satisfaction and Happiness Levels by Social Class",x="Financial Satisfaction", y = "Frequency",fill="Level of Happiness")+
  theme_bw()+theme(plot.title = element_text(face="bold",hjust = 0.5))+
  scale_fill_brewer(palette = "Set2")+scale_x_discrete(limits = c("Not satisfied at all","More or less satisfied","Pretty well satisfied"))

