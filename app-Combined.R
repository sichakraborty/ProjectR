rm(list = ls())

library(stringr)
library(qdapRegex)
library(dplyr)

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
#df$Location <-NULL


# Rename Columns
names(df)[names(df) == "incident_id"] <- "Incident_Id"
names(df)[names(df) == "date"] <- "Date"
names(df)[names(df) == "state"] <- "State"
names(df)[names(df) == "city_or_county"] <- "City_or_County"
names(df)[names(df) == "n_killed"] <- "Num_Killed"
names(df)[names(df) == "n_injured"] <- "Num_injured"
names(df)[names(df) == "gun_stolen"] <- "Gun_Stolen"
names(df)[names(df) == "location_description"] <- "Location"
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

#Remove any row which does not have participant age
df<- subset(df, !is.na(Participant_Age))  

# Search for number of occurrences for the word Stolen to see if there is significant data on whether stolen guns were involved

gunstats <- grep("Stolen", df$Gun_Stolen, fixed = TRUE)


# Rename some data for convenience

df$Gun_Stolen <- gsub("Unknown", "U", df$Gun_Stolen)
df$Gun_Stolen <- gsub("Stolen", "S", df$Gun_Stolen)
df$Gun_Stolen <- gsub("Not-stolen", "N", df$Gun_Stolen)


# Count the number of stolen guns involved

df$Stolen_Gun_Count <- str_count(df$Gun_Stolen, "S")

#Drop the column

df$Gun_Stolen <- NULL


#Participant_Gender

df$Num_Male_Involved <- str_count(df$Participant_Gender, "Male")
df$Num_Female_Involved <- str_count(df$Participant_Gender, "Female")
df$Participant_Gender <- NULL 


#Participant type

df$Participant_Type <- gsub("Victim", "V", df$Participant_Type)
df$Participant_Type <- str_count(df$Participant_Type, "V")


## Change name of the column 
df <- rename(df, Num_Victims = Participant_Type)



# Participant Age

df$Participant_Age <- gsub("||", ",", df$Participant_Age, fixed = TRUE)
df$Participant_Age <- rm_between(df$Participant_Age, "::", ",", extract=TRUE)


patternTeen <- "(1[3-9])"
patternAdults <- "([2-8][0-9]|9[0-9]|100)"

df$Num_Teens_Involved <- str_count(df$Participant_Age,patternTeen )
df$Num_Adults_Involved <- str_count(df$Participant_Age,patternAdults )


#Participant_relationship

df$Acquaintance <- ifelse(
  grepl("family|acquaintance|co-worker|friends|neighbor|significant others", 
        tolower(df$Participant_Relationship)),"Yes", "No")

df$Stranger <- ifelse(
  grepl("robbery|random|home invasion", 
        tolower(df$Participant_Relationship)),"Yes", "No")

df$Gang <- ifelse(grepl("gang", tolower(df$Participant_Relationship), fixed = TRUE), "Yes", "No")

df$Mass_Shooting <-ifelse(grepl("mass shooting", tolower(df$Participant_Relationship), fixed = TRUE), "Yes", "No")

df$Participant_Relationship <- NULL

#People involved
df$People_Involved <- df$Num_Killed + df$Num_injured


# trans <- list()
# childCounter <- 0
# 
# for( i in df$Participant_Age)
# {
#   childCounter <- 0
#   
#   for(j in i)
#   {
#     
#     
#     ifelse(as.numeric(j)<=12, childCounter <- childCounter+1, 0)
#     
#   }
#   
#   trans <- append(trans,childCounter)
#   
# }
# 
# df$Num_Child_Involved <- trans
# 
# df$Participant_Age <- NULL









