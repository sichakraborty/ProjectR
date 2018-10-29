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
names(df)[names(df) == "n_guns_involved"] <- "Guns_Involved"
names(df)[names(df) == "participant_age"] <- "Participant_Age"
names(df)[names(df) == "participant_gender"] <- "Participant_Gender"
names(df)[names(df) == "participant_relationship"] <- "Participant_Relationship"
names(df)[names(df) == "participant_status"] <- "Participant_Status"
names(df)[names(df) == "participant_type"] <- "Participant_Type"

# Change Data Type
df$Date <- as.Date(df$Date)
df$State <- factor(df$State)


#WORKING WITH COLUMN Participant_Age
age <- strsplit(df$Participant_Age, "||", fixed = TRUE)
df$Participant_Age

#looking at the data
age
max(df$Participant_Age)
min(df$Participant_Age)

#This data frame is in the function: new_age <- strsplit(age[[3]], " ", fixed = TRUE)
nrow(df)
#
wide_tall_age <- function(p_age) {
  new_age <- strsplit(p_age, " ", fixed = TRUE)
  unlist_age <- unlist(new_age)
  #return(length(unlist_age))
  #return(unlist_age)
  new <- data.frame(10, 1:10)
  for(i in 1:length(unlist_age)){
    new$X10[i] <- unlist_age[[i]]
  }
  return(new)
}

#This is supposed to create a loop so all of the rows can be made (check how it works)
for(j in 1:nrow(df)){
wide_tall_age(age[[j]])
}

#We see the differen row lists
wide_tall_age(age[[3]])
df[239677, 10]
wide_tall_age(age[[239677]])




#WORKING WITH COLUMN Participant_Gender

gender <- strsplit(df$Participant_Gender, "||", fixed = TRUE)
head(gender) #Look at the data to make sure || got deleted

wide_tall_gender <- function(p_gender) {
  new_gender <- strsplit(p_gender, " ", fixed = TRUE)
  unlist_gender <- unlist(new_gender)
  new_g <- data.frame(10, 1:10)
  for(i in 1:length(unlist_gender)){
    new_g$X10[i] <- unlist_gender[[i]]
  }
  return(new_g)
}

wide_tall_gender(gender[[3]]) #Shows the outcome


#WORKING WITH Participant_Status

status <- strsplit(df$Participant_Status, "||", fixed = TRUE)
head(status)

wide_tall_status <- function(p_status) {
  new_status <- strsplit(p_status, " ", fixed = TRUE)
  new_s <- data.frame(10, 1:10)
  for(i in 1:length(new_status)) {
    new_s$X10[i] <- new_status[[i]]
  }
  return(new_s)
}

wide_tall_status(status[[3]]) #The outcome is w/o the unlist so I get it correctly


#WORKING WITH Participant_Type

type <- strsplit(df$Participant_Type, "||", fixed = TRUE)

wide_tall_type <- function(p_type) {
  new_type <- strsplit(p_type, " ", fixed = TRUE)
  unlist_type <- unlist(new_type)
  new_t <- data.frame(10, 1:10)
  for(i in 1:length(new_type)) {
    new_t$X10[i] <- new_type[[i]]
  }
  return(new_t)
}

wide_tall_type(type[[3]]) #Checking one of the outcomes

#STILL MISSING THE LOOPS TO MAKE SURE IT APPLIES TO ALL OF THE ROWS(FOR EACH FUNCTION)
#NOTES:
  ##Do we want to delete the NA's from Participant_Age??


