library(ezids)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggeasy)
library(ggpubr)
library(gganimate)
library(gifski)
library(transformr)
library(scales)
library(grid)
library(gridExtra)
library(grid)
library(gridExtra)
library(cowplot)

knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)
gss1 <- data.frame(read.csv("GSS.csv"))

# Origially the data has labels/ values like inapplicable, no answer etc. We only need rows that have complete and specific rows. 
# To remove those rows we perform data cleaning
# Cleaning data 
# We begin my making subset of the original dataframe where we only keep the 3 variables of happy that we actually need.

gss1_h = subset(gss1, happy == "Very happy" | happy == "Pretty happy" | happy == "Not too happy")

# Then we make the subset of the above dataframe and keep only the 4 labels of job satisfaction. This process is followed for all other variables as well.

gss1_hsj = subset(gss1_h, satjob == "Very satisfied" | satjob == "Moderately satisfied" | satjob == "A little dissatisfied" | satjob == "Very dissatisfied")
gss1_hsjc = subset(gss1_hsj, class_ == "Lower class" | class_ == "Working class" | class_ == "Middle class" | class_ == "Upper class")
gss1_hsjcsf = subset(gss1_hsjc, satfin == "Pretty well satisfied" | satfin == "More or less satisfied" | satfin == "Not satisfied at all")
# We ultimately have the final dataframe that consists of the labels that are valid for us. We originally had 68846 rows but after cleaning we are left with 47251 rows. 

par(mfrow=c(1,4)) 

# Piechart for showing different percentages of level of happiness in the dataset
pie_happy <- gss1_hsjcsf %>% count(happy) %>%
  arrange(desc(happy)) %>% 
  mutate(percentages = n/sum(n) * 100, ypos = cumsum(percentages) - 0.5 * percentages) # ypos is calculated because it gives us the center of the angle thus helping us to place the % right in the middle of the slice.

# Piechart for showing different percentages of social class in the dataset
pie_class <- gss1_hsjcsf %>% count(class_) %>%
  arrange(desc(class_)) %>% 
  mutate(percentages = n/sum(n) * 100, ypos = cumsum(percentages) - 0.5 * percentages)

# Piechart for showing different percentages level of financial satisfaction in the dataset
pie_satfin <- gss1_hsjcsf %>% count(satfin) %>%
  arrange(desc(satfin)) %>% 
  mutate(percentages = n/sum(n) * 100, ypos = cumsum(percentages) - 0.5 * percentages)

# Piechart for showing different percentages of job satisfaction in the dataset
pie_satjob <- gss1_hsjcsf %>% count(satjob) %>%
  arrange(desc(satjob)) %>% 
  mutate(percentages = n/sum(n) * 100, ypos = cumsum(percentages) - 0.5 * percentages)


plot1 <- ggplot(pie_happy, aes(x = "", y=percentages, fill = happy)) +
  geom_bar(stat="identity", width = 1) + 
  coord_polar("y", start = 0) +
  geom_text(aes(x = "", y = ypos, label = scales::percent(percentages, scale = 1))) +
  scale_fill_manual(values = c("#0290d1", "#e88738", "#39db7f", "#7f2bed")) + 
  labs(title = "Happiness Variable", fill = "Happiness") +  
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

plot2 <- ggplot(pie_class, aes(x = "", y = percentages, fill = class_)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) +
  geom_text(aes(x = "", y = ypos, label = scales::percent(percentages, scale = 1))) +
  scale_fill_manual(values = c("#0290d1", "#e88738", "#39db7f", "#7f2bed")) + 
  labs(title = "Social Class Variable", fill = "Class") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

plot3 <- ggplot(pie_satfin, aes(x = "", y = percentages, fill = satfin)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) +
  geom_text(aes(x = "", y = ypos, label = scales::percent(percentages, scale = 1))) +
  scale_fill_manual(values = c("#0290d1", "#e88738", "#39db7f", "#7f2bed")) + 
  labs(title = "Financial Satisfaction Variable", fill = "Financial Satisfaction") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.2))

plot4 <- ggplot(pie_satjob, aes(x = "", y = percentages, fill = satjob)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) +
  geom_text(aes(x = "", y = ypos, label = scales::percent(percentages, scale = 1))) +
  scale_fill_manual(values = c("#0290d1", "#e88738", "#39db7f", "#7f2bed")) + 
  labs(title = "Job Satisfaction Variable", fill = "Job Satisfaction") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

plot_grid(plot1, plot2, plot3, plot4, align = "h", axis = "b", rel_widths = c(20, 20))
# We use the plot_grid function to plot the 4 pie charts in a 2x2 grid

# with sjPlot we can plot a simple Chi-Squared distribution that indicate at what p-level is.

sjPlot::tab_xtab(var.row = gss1_hsjcsf$happy, var.col = gss1_hsjcsf$satfin, title = "Level of Happiness by Financial Satisfaction Crosstabulation", var.labels = c("Happiness Level", "Financial Satisfaction"), show.row.prc = TRUE, emph.total = TRUE)

sjPlot::tab_xtab(var.row = gss1_hsjcsf$happy, var.col = gss1_hsjcsf$satjob, title = "Level of Happiness by Job Satisfaction Crosstabulation", var.labels = c("Happiness Level","Job Satisfaction"), show.row.prc = TRUE, emph.total = TRUE)


# Making plot of relationship between class and financial satisfaction

ggplot(gss1_hsjcsf, aes(x = satfin, fill = happy)) +
  geom_bar(position = "dodge") +
  facet_wrap(~class_)  +
  labs(title = "Relationship Between Financial Satisfaction and Happiness Levels by Social Class", x = "Financial Satisfaction", y = "Frequency", fill = "Happiness") +
  theme_bw(base_size = 14) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5), axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1)) +
  scale_fill_manual(values = c("#0290d1", "#e88738", "#39db7f", "#7f2bed")) + 
  scale_x_discrete(limits = c("Not satisfied at all", "More or less satisfied", "Pretty well satisfied"))



# making plot of relationship between class and job satisfaction

ggplot(gss1_hsjcsf, aes(x = satjob, fill = happy)) +
  geom_bar(position = "dodge") +
  facet_wrap(~class_)  +
  labs(title = "Relationship Between Job Satisfaction and Happiness Levels by Social Class", x = "Job Satisfaction", y = "Frequency", fill = "Happiness") +
  theme_bw(base_size = 14) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5), axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1)) +
  scale_fill_manual(values = c("#0290d1", "#e88738", "#39db7f", "#7f2bed")) + 
  scale_x_discrete(limits = c("Very dissatisfied", "A little dissatisfied", "Moderately satisfied","Very satisfied"))



# Calculations prior to plotting happiness levels compared to the job satisfaction from 1972 - 2018.

gss_satjob <- gss1_hsjcsf %>% 
  group_by(year, happy, satjob) %>% 
  summarise(total_count=n(), .groups = 'drop') %>% 
  as.data.frame()

gss_satjob_perc <- transform(gss_satjob,
                             perc = ave(total_count,
                                        year,
                                        FUN = prop.table))

gss_satjob_perc$perc = gss_satjob_perc$perc * 100

# Plot to analyze happiness levels compared to the job satisfaction from 1972 - 2018.

ggplotly(gss_satjob_perc %>%
           ggplot( aes(x = year, y = perc)) +
           geom_line(aes(colour=happy)) +
           geom_point(size = 0.5) +
           xlab("Years") +
           ylab("Percentage Happiness") +
           labs(title = "Job Satisfaction Levels") +
           theme(plot.title = element_text(hjust = 0.5, size = 10)) +
           facet_wrap(. ~ satjob, nrow = 4) +
           guides(color = guide_legend(title = "Happiness Type")) +
           transition_reveal(year) +
           scale_color_manual(values = c("#0290d1", "#e88738", "#39db7f")))


gif1 <- gss_satjob_perc %>%
        ggplot(aes(x = year, y = perc)) +
        geom_line(aes(colour = happy)) +
        geom_point(size = 0.5) +
        xlab("Years") +
        ylab("Percentage Happiness") +
        labs(title = "Job Satisfaction Levels") +
        theme(plot.title = element_text(hjust = 0.5, size = 10)) +
        facet_wrap(. ~ satjob, nrow = 4) +
        guides(color = guide_legend(title = "Happiness Type"))+
        transition_reveal(year) +
        scale_color_manual(values = c("#0290d1", "#e88738", "#39db7f"))

# We use the animate function to make an animation that shows differences over the years from 1972 - 2018
animate(gif1, renderer = gifski_renderer(), fps = 60, height = 690, width = 1000, duration = 3)
# We save the animation into a gif.
anim_save("gif1.gif")

# Calculations prior to plotting happiness levels compared to the financial satisfaction from 1972 - 2018.

gss_satfin <- gss1_hsjcsf %>% 
  group_by(year, happy, satfin) %>% 
  summarise(total_count=n(), .groups = 'drop') %>% 
  as.data.frame()

gss_satfin_perc <- transform(gss_satfin,
                             perc = ave(total_count,
                                        year,
                                        FUN = prop.table))

gss_satfin_perc$perc = gss_satfin_perc$perc * 100

# Plot to analyze happiness levels compared to the financial satisfaction from 1972 - 2018.

ggplotly(gss_satfin_perc %>%
           ggplot( aes(x = year, y = perc)) +
           geom_line(aes(colour = happy)) +
           geom_point(size = 0.5) +
           facet_wrap(. ~ satfin, nrow = 3) +
           xlab("Years") +
           ylab("Percentage Happiness ") +
           labs(title="Financial Satisfaction Levels ") +
           theme(plot.title = element_text(hjust = 0.5, size = 10)) +
           facet_wrap(. ~ satfin, nrow = 3) +
           guides(color = guide_legend(title = "Happiness Type")) +
           transition_reveal(year) +
           scale_color_manual(values = c("#0290d1", "#e88738", "#39db7f")))

# We save the plot into a variable gif to make an animation.
gif2 <- gss_satfin_perc %>%
        ggplot(aes(x = year, y = perc)) +
        geom_line(aes(colour = happy)) +
        geom_point(size = 0.5) +
        facet_wrap(. ~ satfin, nrow = 3) +
        xlab("Years") +
        ylab("Percentage Happiness") +
        labs(title = "Financial Satisfaction Levels") +
        theme(plot.title = element_text(hjust = 0.5, size = 10)) +
        facet_wrap(. ~ satfin, nrow = 3) +
        guides(color = guide_legend(title = "Happiness Type")) +
        transition_reveal(year) +
        scale_color_manual(values = c("#0290d1", "#e88738", "#39db7f"))


# We use the animate function to make an animation that shows differences over the years from 1972 - 2018
animate(gif2, renderer = gifski_renderer(), fps = 60, height = 690, width = 1000, duration = 3)
# We save the animation into a gif.
anim_save("gif2.gif")

