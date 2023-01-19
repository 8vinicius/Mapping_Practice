#R CODE FOR PRODUCING GRAPHICS 
# Getting started -----------------------------------------------------------
library(readxl)
air <- read_excel("final_conditionair.xlsx")
#I probably should figure out how to do that code where you can run this code regardless of your working directory. 

# Data Cleaning -----------------------------------------------------------
table(air$cent_num)
#noticing there are variables listed as Q and N for missing 
#Q = Data withheld because either the relative standard error (RSE) was greater than 50% or fewer than 10 households were in the reporting sample.
#N = No households in reporting sample. 
#Q and N probably don't mean the same thing, but I'm going to pretend I do not see 

#CHANGING MISSING VARIABLES Q/N to MISSING
#This was arguably the hardest step and I still believe there is an easier way
library(tidyverse)
air_num<- mutate(air, across(where(is.character), ~ ifelse(. %in% c("N", "Q"), NA, .)))
table(air_num$cent_num)
#wondering whether it's best practice to change the naming of the datasets over time? 

#RECODING CHAR to NUMERIC VARIABLES- again want to figure out a loop for this, it's easier to do in SAS
air_num$cent_num <- as.numeric(air_num$cent_num)
air_num$cent_pt <- as.numeric(air_num$cent_pt)
air_num$dehum_num <- as.numeric(air_num$dehum_num)
air_num$dehum_pt <- as.numeric(air_num$dehum_pt)

#deleting the first columns because it was for all states
air_num <-air_num[-1,]
head(air_num)

#rename variable 
colnames(air_num)[1]<-"State"

# Univariate Plots --------------------------------------------------------

#UNIVARIATE PLOTS 
library(ggplot2)
#Histogram of Number of people using an air condition unit 
summary(air_num$aireq_num)


#creating annotations for histogram just because 
annotations <- data.frame(
  x = c(round(min(air_num$lifeExp), 2), round(mean(air_num$aireq_num), 2), round(max(air_num$aireq_num), 2)),
  y = c(0.020, 2.13, 9.8),
  label = c("Min:", "Mean:", "Max:")
) 

#The following code snippet draws a black line at the mean
#and dashed black lines at -1 and +1 standard deviation marks:
ggplot(air_num, aes(x = aireq_num)) +
  geom_histogram(bins=10, color = "#220000", fill = "#0099F8")+
  geom_vline(aes(xintercept = mean(aireq_num)),color = "#000000", size = 1.25) +
  geom_vline(aes(xintercept = mean(aireq_num) + sd(aireq_num)), color = "#000000", size = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(aireq_num) - sd(aireq_num)), color = "#000000", size = 1, linetype = "dashed")+
  labs(
    title = "Histogram of Number of Housing Units with Air Conditions Units in the US",
    caption = "Source: US Energy Information Administration",
    x = "Number of Air Conditions Units(million)",
    y = "Count (million)")

summary(air_num$fan_pt)
#UUNIVARIATE CEILING FAN
ggplot(air_num, aes(x = fan_pt)) +
  geom_histogram(bins=10, color = "#220000", fill = "#0099F8")+
  geom_vline(aes(xintercept = mean(fan_pt)), color ="#000000", size = 1.25) +
  geom_vline(aes(xintercept = mean(fan_pt) + sd(fan_pt)), color = "#000000", size = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(fan_pt) - sd(fan_pt)), color = "#000000", size = 1, linetype = "dashed")+
  labs(
    title = "Histogram of Percent of Housing Units with Ceiling Fans in the US",
    caption = "Source: US Energy Information Administration",
    x = "Percent of Ceiling Fans by State",
    y = "Percent Distribution")


##Importing Energy Data 
library(readxl)
energy <- read_excel("C:/Users/tjaer/OneDrive - Harvard University/Data Management R/energy.xlsx")
View(energy)


##Merging energy data to the air conditions access data 
insecure<- left_join(air_num,energy,by="State")

# Bivariate Plots- Scatter Plots ------------------------------------------
#BIVARIATE PLOTS- Scatter Plots 
ggplot(insecure,aes(y =insecure_pt,x =aireq_pt,label=State))+
  labs(
    title="Scatter plot of Energy Insecurity and Percent Housing Units with Air Conditioners by State",
    y="Percent Energy Insecure Households by State",
    x="Percent Housing Units with Air Conditioners")+
  geom_point()+
  geom_text()




# Creating a Map of Energy Insecurity  ------------------------------------
#I was going to use the code from the Public Health Geocoding Project, but then realized that the code wouldn't work for this dataset because it relies on already having geocoded data, whereas, I am linking the energy data to geocoded data. 
library(knitr)
library(tidyverse)
library(readxl)
library(ggplot2)
library(cowplot)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)
library(sf)
library(spdep)
library(viridis) 
library(Hmisc)
library(fastDummies)
library(lme4)
library(INLABMA)
library(broom)

#Found this online so idk, seems like good vibes 
library(devtools)
library(tidyverse)
library(urbnmapr)
devtools::install_github(“UrbanInstitute/urbnmapr”)
#also had to download new R tools 

#TRIYNG SOMETHING NEW FROM ONLINE - literally copied and pasted this code so it's pretty easy to use and adapt for certain data 
#definitely relies on understanding the coding though so I tried to add in more explanations 
#https://datavizpyr.com/how-to-make-us-state-and-county-level-maps-in-r/
library(tidyverse) 
theme_set(theme_bw(base_size=16)) 

library(maps)
us_states <- map_data("state")
us_counties <- map_data("county")
head(us_states) #latitude and long 

#Creates map of lat and long with filled color scheme 
p <- ggplot(data = us_states,
            mapping = aes(x = long, y = lat,
                          group = group, fill = region))
p + geom_polygon() +
  guides(fill = FALSE)

#adds in latitude and longitude lines and also colors for each state 
#more put together 
library(mapproj)
p + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  guides(fill = FALSE)

#create a nice basic plain map 
p + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  guides(fill = FALSE)+
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank())

#JOIN the state data to the insecure data 
#first have to make sure the variables are correct 
# Loading Library
library(stringr)
# Creating a string
us_states$Region <-str_to_title(us_states$region)

#Joining the datasets 
Mapenergy<- us_states %>% 
  left_join(insecure, by=c("Region"="State")) 

us_states %>% 
  left_join(insecure, by=c("Region"="State")) %>%
  ggplot(aes(x=long,y=lat,group=group, fill=insecure_pt)) +
  geom_polygon(color = "gray90", size = 0.1) +
  #coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
  scale_fill_continuous(type = "viridis")+
  #scale_fill_brewer("Oranges")+
  theme(legend.position="bottom",
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank()
  )

#AND DONE, only took me tHrEe hOuRs to figure out how to do this