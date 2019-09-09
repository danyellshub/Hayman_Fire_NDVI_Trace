library(tidyverse)
library(tidyr)
library(ggthemes)
library(lubridate)

# Now that we have learned how to munge (manipulate) data
# and plot it, we will work on using these skills in new ways


####-----Reading in Data and Stacking it ----- ####
#Reading in files
files <- list.files('data',full.names=T)


#Read in individual data files
ndmi <- read_csv(files[1]) %>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndmi')


ndsi <- read_csv(files[2]) %>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndsi')

ndvi <- read_csv(files[3])%>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndvi')

# Stack as a tidy dataset
full_long <- rbind(ndvi,ndmi,ndsi) %>%
  gather(key='site',value='value',-DateTime,-data) %>%
  filter(!is.na(value))



##### Question 1 #####
#1 What is the correlation between NDVI and NDMI? - here I want you to
#convert the full_long dataset in to a wide dataset using the 
#function "spread" and then make a plot that shows the correlation as a
# function of if the site was burned or not

wide<- full_long %>%
  spread(key='data',value='value')

ggplot(wide, aes(x=ndmi, y=ndvi, color=site))+
  geom_point()+
  theme_few()+
  scale_color_few() + 
  theme(legend.position=c(0.3,0.2))
  
## End Code for Question 1 -----------


#### Question 2 ####
#2) What is the correlation between average NDSI (normalized 
# snow index) for January - April and average NDVI for June-August?
#In other words, does the previous year's snow cover influence vegetation
# growth for the following summer? 


#This is code to see mean NDSI vs NDMI in the specified months
#the unburned and burned sites are treated as separate, hence,
#four points per year, but the table seemed to copy weird with interjoin


full_wide <- wide %>%
  mutate(month=month(DateTime))%>%
  mutate(year=year(DateTime))%>%
  filter(!is.na(ndmi), !is.na(ndsi), !is.na(ndvi))

summer_months <- full_wide%>%
  filter(month %in% c(6,7,8))%>%
  group_by(site, year)%>%
  summarise(mean_NDVI = mean(ndvi))
  
winter_months <- full_wide%>%
  filter(month %in% c(1,2,3,4))%>%
  group_by(site, year)%>%
  summarise(mean_NDSI = mean(ndsi))

#I am stuck here 
ndsi_ndvi_mean <- inner_join(summer_months %>% dplyr::select(~site), 
                             winter_months %>% dplyr::select(~site),
                             by = "year")

ggplot(ndsi_ndvi_mean, aes(x=mean_NDSI, y=mean_NDVI, color=year))+
  geom_point()+
  theme_few()



#This work is to see NDSI vs NDMI in the specified months
#irrelevant to the site, per year.
#the two sites were avereaged into one value
ndsi_months <-ndsi%>%
  mutate(month=month(DateTime))%>%
  mutate(year=year(DateTime))%>%
  filter(month %in% c(1,2,3,4)) %>%
  filter(!is.na(burned), !is.na(unburned))%>%
  gather(key='site',
         value='value',
         -DateTime,-month,-data,-year)%>%
  group_by(year) %>%
  summarize(value=mean(value))%>%
  mutate(data = 'ndsi')


ndvi_months <-ndvi%>%
  mutate(month=month(DateTime))%>%
  mutate(year=year(DateTime))%>%
  filter(month %in% c(6,7,8)) %>%
  filter(!is.na(burned), !is.na(unburned))%>%
  gather(key='site',
         value='value',
         -DateTime,-month,-data, -year)%>%
  group_by(year) %>%
  summarize(value=mean(value))%>%
  mutate(data = 'ndvi')

ndvi_ndsi <-inner_join(ndvi_months %>% dplyr::select(-data), 
                          ndsi_months %>% dplyr::select(-data), 
                          by='year')%>%
  rename(ndvi = 2, ndsi = 3)
  
ggplot(ndvi_ndsi, aes(x=ndsi, y=ndvi, color=year))+
  geom_point()+
  theme_few()

## End code for question 2 -----------------


###### Question 3 ####
#How is the snow effect from question 2 different between pre- and post-burn
# and burned and unburned? 

#This code I worked with Kevin and Riley
pre_fire <- filter(full_wide, year <=2002)%>%
  filter(!is.na(ndmi), !is.na(ndsi), !is.na(ndvi))
post_fire <- filter(full_wide, year >2002)%>%
  filter(!is.na(ndmi), !is.na(ndsi), !is.na(ndvi))

ggplot(pre_fire, aes(x=ndsi, y=ndvi, color=site))+
  geom_point()+
  theme_few()

ggplot(post_fire, aes(x=ndsi, y=ndvi, color=site))+
  geom_point()+
  theme_few()

## End code for question 3

###### Question 4 #####
#What month is the greenest month on average? Does this change in the burned
# plots after the fire? 

ndvi_means <- ndvi%>%
  filter(!is.na(burned), !is.na(unburned))%>%
  mutate(month=month(DateTime))%>%
  mutate(year=year(DateTime))%>%
  gather(key='site',
         value='value',
         -DateTime,-month,-data, -year)%>%
  group_by(month)%>%
  summarise('mean_NDVI'=mean(value))

ggplot(ndvi_full, aes(x=month, y=mean_NDVI))+
  geom_point()+
  theme_few()

ndvi_pre_fire_means <- ndvi%>%
  filter(!is.na(burned), !is.na(unburned))%>%
  mutate(month=month(DateTime))%>%
  mutate(year=year(DateTime))%>%
  filter(year <= 2002)%>%
  gather(key='site',
         value='value',
         -DateTime,-month,-data, -year)%>%
  group_by(month)%>%
  summarise('mean_NDVI'=mean(value))

ndvi_post_fire_means <- ndvi%>%
  filter(!is.na(burned), !is.na(unburned))%>%
  mutate(month=month(DateTime))%>%
  mutate(year=year(DateTime))%>%
  filter(year > 2002)%>%
  gather(key='site',
         value='value',
         -DateTime,-month,-data, -year)%>%
  group_by(month)%>%
  summarise('mean_NDVI'=mean(value))

joined_pre_post <-inner_join(ndvi_pre_fire_means, ndvi_post_fire_means, by = 'month')%>%
  rename(Pre_Fire = 2, Post_Fire =3)%>%
  gather(key='site',
        value='mean_ndvi',-month)

ggplot(joined_pre_post, aes(x=month, y=mean_ndvi, color=site))+
  geom_point()+
  theme_few()+
  theme(legend.position=c(0.69,0.2))
           
##### Question 5 ####
#What month is the snowiest on average?

ndsi_means <- ndsi%>%
  filter(!is.na(burned), !is.na(unburned))%>%
  mutate(month=month(DateTime))%>%
  mutate(year=year(DateTime))%>%
  gather(key='site',
         value='value',
         -DateTime,-month,-data, -year)%>%
  group_by(month)%>%
  summarise('mean_ndsi'=mean(value))

ggplot(ndsi_means, aes(x=month, y=mean_ndsi))+
  geom_point()+
  theme_few()

