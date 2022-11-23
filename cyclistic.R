#Cyclistic Bike Share - A GOOGLE case study

#Using R to :
#• Clean and manipulate our data.
#• Partly analysising data using R 
#• Preparing a csv file to use in Tableau

# Data for Cyclistic Nov 2021 to Oct 2022
# Data Source : https://divvy-tripdata.s3.amazonaws.com/index.html
# All the data sets have been downloaded and named in chronological order.
# Nov 21 has been named BR1 and so on till Oct 22 as BR12 in designated Folder

#Primary Installation of Packages
install.packages("tidyverse")
install.packages("lubridate") #For date manipulation
install.packages("janitor") #For basic cleaning process
install.packages("chron") #to use times() for max,mean,etc
install.packages("geosphere") #For calculating distance between long/lat


#Load Packages
library(tidyverse)
library(janitor)
library(lubridate)
library(dplyr)
library(hms) #to use as_hms()
library(chron)
library(modeest) #to use mlv() for mode
library(geosphere)  #to use distm()



#-------------------DATA PROCESSING AND CLEANING-------------------#


#Reading all files into variables
BR1 <- read_csv("BR1.csv")
BR2 <- read_csv("BR2.csv")
BR3 <- read_csv("BR3.csv")
BR4 <- read_csv("BR4.csv")
BR5 <- read_csv("BR5.csv")
BR6 <- read_csv("BR6.csv")
BR7 <- read_csv("BR7.csv")
BR8 <- read_csv("BR8.csv")
BR9 <- read_csv("BR9.csv")
BR10 <- read_csv("BR10.csv")
BR11 <- read_csv("BR11.csv")
BR12 <- read_csv("BR12.csv")

#Test to see if all files have loaded correctly and display the correct information
#Replace file name to check all the file names and their accuracy

#USING rbind() function to stitch all the data sets together
BR_stitch <- rbind(BR1,BR2,BR3,BR4,BR5,BR6,BR7,BR8,BR9,BR10,BR11,BR12)

#Removing all other data frames to free up space in the environment
remove(BR1,BR2,BR3,BR4,BR5,BR6,BR7,BR8,BR9,BR10,BR11,BR12)

head(BR_stitch)
colnames(BR)


#Cleaning Empty Rows and Columns 
BR <- remove_empty(BR_stitch, which = c("rows","cols"),quiet = FALSE)
# which one of rows or cols to delete where it's empty
#Used both rows and cols to clean
#Kept quiet = FALSE so printed message can be shown to see how many rows/cols got deleted

#Cleaning Environment
remove(BR_stitch)





#-------------------DATA MANIPULATION FOR ANALYSIS-------------------#

#Finding Renting date from dataframe. Extracting only date from date and time
BR$date <- as.Date(BR$started_at)

#Adding Column day to find out the day of the week when rented
BR$day <- weekdays(as.Date(BR$date))

#Changing dates and time to a standard YMD HMS format
BR$started_at <- ymd_hms(BR$started_at)
BR$ended_at <- ymd_hms(BR$ended_at)

#Adding Column for Ride Time
BR$ride_time <- as_hms(difftime( BR$ended_at,BR$started_at))


#Final Check to see everything is in order
View(BR)


#-------------------SAVING CSV FOR TABLEAU-------------------#

#Saving our Stitched,Cleaned and Manipulated data in a .csv file 
write_csv(BR,file="Bike_rides_final.csv", append = TRUE, col_names = TRUE)




#-------------------PRIMARY ANALYSIS-------------------#


#Total number of rides
NROW(BR$ride_id)
#5755694

#Membertype
BR %>% group_by(member_casual) %>% count(member_casual)
#1 casual        2353033
#2 member        3402661


#Max Ride time
max(times(BR$ride_time),na.rm = TRUE)
#23:59:57


#Finding out the average Ride time
mean(times(BR$ride_time),na.rm = TRUE)
#00:16:20


#Average Ride Time per Member-type
BR %>% group_by(member_casual) %>% summarise_at(vars(ride_time),list(time = mean))
#1 casual        1750.2063 secs     29.17 mins
#2 member         762.6631 secs     12.71 mins


##Finding out most popular Day for Renting
mlv(BR$day, method = "mlv")
##"Saturday"
