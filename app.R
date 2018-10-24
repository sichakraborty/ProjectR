
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
names(df)[names(df) == "n_guns_involved"] <- "Guns_Involved"
names(df)[names(df) == "participant_age"] <- "Participant_Age"
names(df)[names(df) == "participant_gender"] <- "Participant_Gender"
names(df)[names(df) == "participant_relationship"] <- "Participant_Relationship"
names(df)[names(df) == "participant_status"] <- "Participant_Status"
names(df)[names(df) == "participant_type"] <- "Participant_Type"

# Change Data Type
df$Date <- as.Date(df$Date)
df$State <- factor(df$State)