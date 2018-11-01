rm(list = ls())

library(stringr)

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

# Search for number of occurrences for the word Stolen to see if there is significant data on whether stolen guns were involved

gunstats <- grep("Stolen", df$Gun_Stolen, fixed = TRUE)
print(gunstats)

# Rename some data for convenience

df$Gun_Stolen <- gsub("Unknown", "U", df$Gun_Stolen)
df$Gun_Stolen <- gsub("Stolen", "S", df$Gun_Stolen)
df$Gun_Stolen <- gsub("Not-stolen", "N", df$Gun_Stolen)

# Add a new column, StolenGunsInvolved, Mark it as Yes if Stolen Guns were involved

df$StolenGunsInvolved <- df$Gun_Stolen

df$StolenGunsInvolved <- ifelse(grepl("S", df$StolenGunsInvolved),"Yes", "No")

# Count the number of stolen guns involved

df$Stolen_Gun_Count <- str_count(df$Gun_Stolen, "S")

#Drop the column

df$Gun_Stolen <- NULL


#Participant_Gender
df$Num_Male_Involved <- str_count(df$Participant_Gender, "Male")
df$Num_Female_Involved <- str_count(df$Participant_Gender, "Female")
df$Participant_Gender <- NULL 

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
