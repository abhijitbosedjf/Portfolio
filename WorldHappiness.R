#World Happiness Index 2015-22

#Using R to :
# Clean and manipulate our data.
# Partly analysising data using R 
# Preparing a csv file to use in Tableau

# Data for Wrold Happiness Index 2015-22. 
# Data Source : https://www.kaggle.com/datasets/mathurinache/world-happiness-report
# All the data sets have been downloaded and sorted in chronological order.

#Primary Installation of Packages
install.packages("tidyverse")
install.packages("janitor") #For basic cleaning process
install.packages("dplyr")
install.packages("lubidate")

#Load Packages
library(tidyverse)
library(janitor)
library(dplyr)
library(lubridate)

#-------------------DATA PROCESSING AND CLEANING-------------------#


#-----------------Reading all files into data frames-----------------#
y15 <- read_csv("2015.csv")
y16 <- read_csv("2016.csv")
y17 <- read_csv("2017.csv")
y18 <- read_csv("2018.csv")
y19 <- read_csv("2019.csv")
y20 <- read_csv("2020.csv")
y21 <- read_csv("2021.csv")
y22 <- read_csv("2022.csv")


#-----------------Formatting data-frames to the same format-----------------#

#For 2015 
#Deleting columns that are not required and adding a Year column
y15 = y15 %>% select(-"Happiness Rank",-"Standard Error",-"Region",-"Dystopia Residual")
y15 = mutate(y15,Year = "2015/01/01")
y15$Year <- as.Date(y15$Year)
#Renaming Attributes to common names
names(y15)[names(y15)=="Economy (GDP per Capita)"] <- "Economy"
names(y15)[names(y15)=="Health (Life Expectancy)"] <- "Health"
names(y15)[names(y15)=="Trust (Government Corruption)"] <- "Trust"
         
           
#For 2016 
#Deleting columns that are not required and adding a Year column
y16 = y16 %>% select(-"Happiness Rank",-"Lower Confidence Interval",-"Upper Confidence Interval",-"Region",-"Dystopia Residual")
y16 = mutate(y16,Year = "2016/01/01")
y16$Year <- as.Date(y16$Year)
#Renaming Attributes to common names
names(y16)[names(y16)=="Economy (GDP per Capita)"] <- "Economy"
names(y16)[names(y16)=="Health (Life Expectancy)"] <- "Health"
names(y16)[names(y16)=="Trust (Government Corruption)"] <- "Trust"

#For 2017 
#Deleting columns that are not required and adding a Year column
y17 = y17 %>% select(-"Happiness.Rank",-"Whisker.high",-"Whisker.low",-"Dystopia.Residual")
y17 = mutate(y17,Year = "2017/01/01")
y17$Year <- as.Date(y17$Year)
#Renaming Attributes to common names
names(y17)[names(y17)=="Happiness.Score"] <- "Happiness Score"
names(y17)[names(y17)=="Economy..GDP.per.Capita."] <- "Economy"
names(y17)[names(y17)=="Health..Life.Expectancy."] <- "Health"
names(y17)[names(y17)=="Trust..Government.Corruption."] <- "Trust"

#For 2018 
#Deleting columns that are not required and adding a Year column
y18 = y18 %>% select(-"Overall rank")
y18 = mutate(y18,Year = "2018/01/01")
y18$Year <- as.Date(y18$Year)
#Renaming Attributes to common names
names(y18)[names(y18)=="Country or region"] <- "Country"
names(y18)[names(y18)=="Score"] <- "Happiness Score"
names(y18)[names(y18)=="GDP per capita"] <- "Economy"
names(y18)[names(y18)=="Social support"] <- "Family"
names(y18)[names(y18)=="Healthy life expectancy"] <- "Health"
names(y18)[names(y18)=="Freedom to make life choices"] <- "Freedom"
names(y18)[names(y18)=="Perceptions of corruption"] <- "Trust"

#For 2019 
#Deleting columns that are not required and adding a Year column
y19 = y19%>% select(-"Overall rank")
y19 = mutate(y19,Year = "2019/01/01")
y19$Year <- as.Date(y19$Year)
#Renaming Attributes to common names
names(y19)[names(y19)=="Country or region"] <- "Country"
names(y19)[names(y19)=="Score"] <- "Happiness Score"
names(y19)[names(y19)=="GDP per capita"] <- "Economy"
names(y19)[names(y19)=="Social support"] <- "Family"
names(y19)[names(y19)=="Healthy life expectancy"] <- "Health"
names(y19)[names(y19)=="Freedom to make life choices"] <- "Freedom"
names(y19)[names(y19)=="Perceptions of corruption"] <- "Trust"

#For 2020
#Deleting columns that are not required and adding a Year column
y20 = y20 %>% select(-"Regional indicator",-"Standard error of ladder score",-"upperwhisker",-"lowerwhisker",-"Ladder score in Dystopia",-"Dystopia + residual",-"Logged GDP per capita",-"Social support",-"Healthy life expectancy",-"Freedom to make life choices",-"Generosity",-"Perceptions of corruption")
y20 = mutate(y20,Year = "2020/01/01")
y20$Year <- as.Date(y20$Year)
#Renaming Attributes to common names
names(y20)[names(y20)=="Country name"] <- "Country"
names(y20)[names(y20)=="Ladder score"] <- "Happiness Score"
names(y20)[names(y20)=="Explained by: Log GDP per capita"] <- "Economy"
names(y20)[names(y20)=="Explained by: Social support"] <- "Family"
names(y20)[names(y20)=="Explained by: Healthy life expectancy"] <- "Health"
names(y20)[names(y20)=="Explained by: Freedom to make life choices"] <- "Freedom"
names(y20)[names(y20)=="Explained by: Generosity"] <- "Generosity"
names(y20)[names(y20)=="Explained by: Perceptions of corruption"] <- "Trust"

#For 2021 
#Deleting columns that are not required and adding a Year column
y21 = y21 %>% select(-"Regional indicator",-"Standard error of ladder score",-"upperwhisker", -"lowerwhisker",-"Logged GDP per capita",-"Social support",-"Healthy life expectancy",-"Freedom to make life choices",-"Generosity",-"Perceptions of corruption",-"Ladder score in Dystopia",-"Dystopia + residual" )  
y21 = mutate(y21,Year = "2021/01/01")
y21$Year <- as.Date(y21$Year)
#Renaming Attributes to common names
names(y21)[names(y21)=="Country name"] <- "Country"
names(y21)[names(y21)=="Ladder score"] <- "Happiness Score"
names(y21)[names(y21)=="Explained by: Log GDP per capita"] <- "Economy"
names(y21)[names(y21)=="Explained by: Social support"] <- "Family"
names(y21)[names(y21)=="Explained by: Healthy life expectancy"] <- "Health"
names(y21)[names(y21)=="Explained by: Freedom to make life choices"] <- "Freedom"
names(y21)[names(y21)=="Explained by: Generosity"] <- "Generosity"
names(y21)[names(y21)=="Explained by: Perceptions of corruption"] <- "Trust"

#For 2022
#Deleting columns that are not required and adding a Year column
y22 = y22 %>% select(-"RANK",-"Whisker-high",-"Whisker-low", -"Dystopia (1.83) + residual")
y22 = mutate(y22,Year = "2022/01/01")
y22$Year <- as.Date(y22$Year)
#Renaming Attributes to common names
names(y22)[names(y22)=="Happiness score"] <- "Happiness Score"
names(y22)[names(y22)=="Explained by: GDP per capita"] <- "Economy"
names(y22)[names(y22)=="Explained by: Social support"] <- "Family"
names(y22)[names(y22)=="Explained by: Healthy life expectancy"] <- "Health"
names(y22)[names(y22)=="Explained by: Freedom to make life choices"] <- "Freedom"
names(y22)[names(y22)=="Explained by: Generosity"] <- "Generosity"
names(y22)[names(y22)=="Explained by: Perceptions of corruption"] <- "Trust"


#Changing Numeric format for 2022 file
y22$`Happiness Score`<- y22$`Happiness Score`/1000 #Dividing by 1000
y22$Economy<-as.numeric(gsub(",",".",y22$Economy)) #Changing to numeric and , to .
y22$Health<-as.numeric(gsub(",",".",y22$Health))
y22$Family<-as.numeric(gsub(",",".",y22$Family))
y22$Freedom<-as.numeric(gsub(",",".",y22$Freedom))
y22$Generosity<-as.numeric(gsub(",",".",y22$Generosity))
y22$Trust<-as.numeric(gsub(",",".",y22$Trust))



#-------------Binding all data-frames into one big data frame -------------#
WHI <- rbind(y15,y16,y17,y18,y19,y20,y21,y22)
remove(y15,y16,y17,y18,y19,y20,y21,y22)
colnames(WHI)
View(WHI)



#-------------Cleaning Empty Rows and Columns-------------# 
WHI <- remove_empty(WHI, which = c("rows","cols"),quiet = FALSE)
WHI = WHI %>% drop_na(`Happiness Score`)


# which one of rows or cols to delete where it's empty
#Used both rows and cols to clean
#Kept quiet = FALSE so printed message can be shown to see how many rows/cols got deleted


#-------------------SAVING CSV FOR USE IN TABLEAU-------------------#

#Saving our Stitched,Cleaned and Manipulated data in a .csv file 
write_csv(WHI,file="Worldfile.csv", append =FALSE, col_names = TRUE)
