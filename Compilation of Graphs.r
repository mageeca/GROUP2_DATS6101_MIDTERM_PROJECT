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
# ggplot(data = gss1_hsjcsf, aes(x = year, y= satfin,  fill = happy)) +
#   geom_bar(position = "fill") + ylab("proportion") +
#   stat_count(geom = "text",
#              aes(label = stat(count)),
#              position=position_fill(vjust=0.5), colour="white")


# kernel density curve
# ggplot(gss1_hsjcsf, aes(x = year, color=happy, fill=happy)) + 
#   geom_density(alpha=.5) +
#   scale_fill_manual(values=c("#200b33", "#1ec7fa", "#ff2626"))
# 
# 
# ggplot(gss1_hsjcsf, aes(x = year, color=satfin, fill=satfin)) + 
#   geom_density(alpha=.58) +
#   scale_fill_manual(values=c("#ff2626", "#1ec7fa", "#0f0354"))
# 
# ggplot(gss1_hsjcsf, aes(x = year, color=satjob, fill=satjob)) + 
#   geom_density(alpha=.5) +
#   scale_fill_manual(values=c("#5aed92", "#1ec7fa", "#ff2626", "#0f0354"))


#Histogram and kernel density 
# ggplot(gss1_hsjcsf, aes(x = year, color=satfin, fill=satfin)) + 
#   geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
#                  binwidth=1.39,alpha=0.9,
#                  colour="black", fill="#f5b576") +
#   geom_density(alpha=.58) +
#   scale_fill_manual(values=c("#ff2626", "#1ec7fa", "#0f0354"))


#TSA Calculation

gss_satfin <- gss1_hsjcsf %>% 
  group_by(year, happy, satfin) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_satfin_perc <- transform(gss_satfin,
                             perc = ave(total_count,
                                        year,
                                        FUN = prop.table))

gss_satfin_perc$perc = gss_satfin_perc$perc * 100

gss_satfin_perc



gss_satjob <- gss1_hsjcsf %>% 
  group_by(year, happy, satjob) %>% 
  summarise(total_count=n(),
            .groups = 'drop') %>% 
  as.data.frame()

gss_satjob_perc <- transform(gss_satjob,
                             perc = ave(total_count,
                                        year,
                                        FUN = prop.table))

gss_satjob_perc$perc = gss_satjob_perc$perc * 100


#satfin and happiness plot

ggplotly(gss_satfin_perc %>%
           ggplot( aes(x=year, y=perc)) +
           geom_line(aes(colour=happy)) +
           geom_point()+
           facet_wrap(. ~ satfin, nrow = 3)+
           ylab("Percentage Happiness ")+
           scale_color_manual(values=c("#19272e", "#c7a04e", "#39db7f")))  

gif1<-gss_satfin_perc %>%
  ggplot( aes(x=year, y=perc)) +
  geom_line(aes(colour=happy)) +
  geom_point()+
  facet_wrap(. ~ satfin, nrow = 3)+
  ylab("Percentage Happiness ")+
  scale_fill_manual(values=c("#19272e", "#c7a04e", "#39db7f"))+
  transition_reveal(year) 


animate(gif1, renderer = gifski_renderer(), fps = 60, height = 690, width =1000, duration = 3)
anim_save("gif1.gif")


##satjob and happiness plot

ggplotly(gss_satjob_perc %>%
           ggplot( aes(x=year, y=perc)) +
           geom_line(aes(colour=happy)) +
           geom_point()+
           facet_wrap(. ~ satjob, nrow = 4)+
           ylab("Percentage Happiness ")+
           scale_color_manual(values=c("#19272e", "#c7a04e", "#39db7f")))


gif2 <- gss_satjob_perc %>%
           ggplot( aes(x=year, y=perc)) +
           geom_line(aes(colour=happy)) +
           geom_point()+
           ylab("Percentage Happiness ")+
           facet_wrap(. ~ satjob, nrow = 4)+
           transition_reveal(year)+
           scale_color_manual(values=c("#19272e", "#c7a04e", "#39db7f"))


animate(gif2, renderer = gifski_renderer(), fps = 60, height = 690, width =1000, duration = 3)
anim_save("gif2.gif")


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

