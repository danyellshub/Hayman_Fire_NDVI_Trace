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

#ndsi_ndvi_long <- full_long %>%
  #mutate(month=month(DateTime))%>%
  #filter(!data %in% 'ndmi')

ndsi_months <-ndsi%>%
  mutate(month=month(DateTime))%>%
  mutate(year=year(DateTime))%>%
  filter(month %in% c(1,2,3,4)) %>%
  filter(!is.na(burned), !is.na(unburned))%>%
  gather(key='site',
         value='value',
         -DateTime,-month,-data,-year)%>%
  group_by(year) %>%
  summarize(mean_ndsi=mean(value))


ndvi_months <-ndvi%>%
  mutate(month=month(DateTime))%>%
  mutate(year=year(DateTime))%>%
  filter(month %in% c(6,7,8)) %>%
  filter(!is.na(burned), !is.na(unburned))%>%
  gather(key='site',
         value='value',
         -DateTime,-month,-data, -year)%>%
  group_by(year) %>%
  summarize(mean_ndvi=mean(value))

ndvi_ndsi <- bind_rows(ndsi_months, ndvi_months)
  gather(key='data',value='value',-year)%>%
  filter(!is.na(value))
  
ggplot(ndvi_ndsi, aes(x=value, y=data, color=year))+
  geom_point()


## End code for question 2 -----------------


###### Question 3 ####
#How is the snow effect from question 2 different between pre- and post-burn
# and burned and unburned? 

## Your code here

## End code for question 3

###### Question 4 #####
#What month is the greenest month on average? Does this change in the burned
# plots after the fire? 

##### Question 5 ####
#What month is the snowiest on average?