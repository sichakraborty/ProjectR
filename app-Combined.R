rm(list = ls())

library(stringr)
library(dplyr)
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(choroplethr))
library(choroplethrMaps)

# Read data
df <- read.csv("gun-violence-data_01-2013_03-2018.csv", stringsAsFactors = FALSE,
               na.strings = c("NA", ""))

# Drop Columns
df$address <- NULL
df$incident_url<-NULL
df$source_url<-NULL
df$incident_url_fields_missing<-NULL
df$congressional_district<-NULL
df$gun_type<-NULL
df$incident_characteristics<-NULL
df$latitude<-NULL
df$longitude<-NULL
df$notes<-NULL
df$participant_age_group<-NULL
df$participant_name<-NULL
df$sources<-NULL
df$state_house_district<-NULL
df$state_senate_district<-NULL
df$participant_name<-NULL
df$location_description <-NULL


# Rename Columns
names(df)[names(df) == "incident_id"] <- "Incident_Id"
names(df)[names(df) == "date"] <- "Date"
names(df)[names(df) == "state"] <- "State"
names(df)[names(df) == "city_or_county"] <- "City_or_County"
names(df)[names(df) == "n_killed"] <- "Num_Killed"
names(df)[names(df) == "n_injured"] <- "Num_injured"
names(df)[names(df) == "gun_stolen"] <- "Gun_Stolen"
names(df)[names(df) == "n_guns_involved"] <- "Num_Guns_Involved"
names(df)[names(df) == "participant_age"] <- "Participant_Age"
names(df)[names(df) == "participant_gender"] <- "Participant_Gender"
names(df)[names(df) == "participant_relationship"] <- "Participant_Relationship"
names(df)[names(df) == "participant_status"] <- "Participant_Status"
names(df)[names(df) == "participant_type"] <- "Participant_Type"

# Change Data Type

df$Date <- as.Date(df$Date)
df$State <- factor(df$State)

#Remove any row where Num_Gun_Involved is less than 1
df<- subset(df, Num_Guns_Involved >= 1)  

#Remove any row which does not have any participant type
df<- subset(df, !is.na(Participant_Type))  

#Remove any row which does not have any participant status
df<- subset(df, !is.na(Participant_Status))  


# Count the number of stolen guns involved

df$Stolen_Gun_Count <- str_count(df$Gun_Stolen, "Stolen")

#Drop the column

df$Gun_Stolen <- NULL



#Participant type

df$Num_Victim <- str_count(df$Participant_Type, "Victim")

df$Num_Suspect <- str_count(df$Participant_Type, "Suspect")


#Add a new columns for participant status "Unharmed" 

df$Num_Unharmed <- (df$Num_Victim+ df$Num_Suspect)-(df$Num_Killed+df$Num_injured)


# Total People involved in an incident are those killed, injured and unharmed
df$People_Involved <- df$Num_Killed + df$Num_injured + df$Num_Unharmed

#Drop the status column
df$Participant_Status <- NULL



#Participant_relationship

# function to match the relationships, will use later

matchRelationship <- function(input = "") {
  
  ifelse(grepl(input, tolower(df$Participant_Relationship), fixed = TRUE), "Yes", "No")
  
}

df$Acquaintance <- ifelse(
  grepl("family|acquaintance|co-worker|friends|neighbor|significant others", 
        tolower(df$Participant_Relationship)),"Yes", "No")

df$Stranger <- ifelse(
  grepl("robbery|random|home invasion", 
        tolower(df$Participant_Relationship)),"Yes", "No")

df$Gang <- matchRelationship(input = "gang")

df$Mass_Shooting <- matchRelationship(input = "mass shooting")

#df$Gang <- ifelse(grepl("gang", tolower(df$Participant_Relationship), fixed = TRUE), "Yes", "No")

#df$Mass_Shooting <-ifelse(grepl("mass shooting", tolower(df$Participant_Relationship), fixed = TRUE), "Yes", "No")

df$Participant_Relationship <- NULL


df$Not_Reported <- "No"
for (i in 1:nrow(df)) {
  if((df$Acquaintance[i]=="No")&(df$Stranger[i]=="No")&(df$Gang[i]=="No")&(df$Mass_Shooting[i]=="No"))
    df$Not_Reported[i] <- "Yes"
  
  }


# Due to inconsistent Data, drop the following column
df$Participant_Age <- NULL
df$Participant_Gender <- NULL


#Reorder the columns

# Keeping age, gender and status column for ease of comparing. Will drop later
colnames(df)
colorder<- c("Incident_Id","Date", "State","City_or_County","Num_Killed","Num_injured","Num_Unharmed","Num_Guns_Involved","Stolen_Gun_Count",
             "Num_Victim","Num_Suspect","People_Involved","Acquaintance","Stranger",
             "Gang","Mass_Shooting","Not_Reported")
df<-df[,colorder]



