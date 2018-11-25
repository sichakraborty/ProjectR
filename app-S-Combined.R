rm(list = ls())

library(stringr)
library(dplyr)
suppressPackageStartupMessages(library(ggplot2))
library(scales)
library(gridExtra)

# Read data
df <- read.csv("gun-violence-data_01-2013_03-2018.csv", stringsAsFactors = FALSE,
               na.strings = c("NA", ""))

# Get memory used to store df in megabytes
print(format(object.size(df), units = "Mb"))

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

#Remove any row which does not have any participant age group
df<- subset(df, !is.na(participant_age_group))  


# Get latest memory used to store df in megabytes
print(format(object.size(df), units = "Mb"))

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

#Drop the status and type column
df$Participant_Status <- NULL
df$Participant_Type <- NULL


#Participant_Relationship

# function to match the relationships

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

colnames(df)
colorder<- c("Incident_Id","Date", "State","City_or_County","Num_Killed","Num_injured","Num_Unharmed","Num_Guns_Involved","Stolen_Gun_Count",
             "Num_Victim","Num_Suspect","People_Involved","Acquaintance","Stranger",
             "Gang","Mass_Shooting","Not_Reported","participant_age_group")
df<-df[,colorder]


# Sort the column based on incident date
df <- arrange(df, Date)

# Plot two graphs to compare the number of people injured and killed in gun related incidents
p1 <- ggplot() + 
      geom_line(data = df, aes(x = Date, y = Num_Killed), color="orange")+ ylab(label='People Killed')+scale_x_date(date_breaks = "6 months" , date_labels = "%b-%y")

p1 <- p1 + ggtitle("Total number of People Involved in Gun Violence")
p1 <- p1 + theme(panel.background = element_blank())
p1 <- p1 + theme(legend.text = element_text(size = 8))
p1 <- p1 + theme(title = element_text(size = 12))
p1 <- p1 + theme(plot.title = element_text(face = "bold"))
p1 <- p1 + theme(axis.title.x = element_blank())
p1 <- p1 + theme(axis.line = element_line(color="black", size = 0.5))
p1<- p1 + scale_y_continuous(breaks = 10*(0:10),expand = c(0,0))
p1 <- p1 + theme(plot.title = element_text(hjust = 0.5))


p2 <- ggplot() + 
  geom_line(data = df, aes(x = Date, y = Num_injured), color="navyblue")+ ylab(label='People Injured')+scale_x_date(date_breaks = "6 months" , date_labels = "%b-%y")

p2 <- p2 + theme(panel.background = element_blank())
p2 <- p2 + theme(legend.text = element_text(size = 8))
p2 <- p2 + theme(title = element_text(size = 12))
p2 <- p2 + theme(plot.title = element_text(face = "bold"))
p2 <- p2 + theme(axis.title.x = element_blank())
p2 <- p2 + theme(axis.line = element_line(color="black", size = 0.5))
p2<- p2 + scale_y_continuous(breaks = 10*(0:10),expand = c(0,0))


g <- grid.arrange(p1, p2, nrow=2) #generates g


ggsave(filename = "violence_data.pdf", plot = g, width = 6, height = 4,
       units = "in")
# Based on peak observed between April to August 2016, we filter the data to create a new df with just those 5 months

peakData <- subset(df, Date >= "2016-03-01" & Date <= "2016-08-30")

# Create a plot to understand the exact data better
p3 <- ggplot() + 
  geom_line(data = peakData, aes(x = Date, y = Num_Killed), color = "red") +
  geom_line(data = peakData, aes(x = Date, y = Num_injured), color = "blue") +
  xlab(label='Dates')+ylab(label='People Involved') + scale_x_date(date_breaks = "1 months" , date_labels = "%b-%y")

p3 <- p3 + theme(panel.background = element_blank())
p3 <- p3 + theme(legend.text = element_text(size = 8))
p3 <- p3 + theme(title = element_text(size = 12))
p3 <- p3 + theme(plot.title = element_text(face = "bold"))
p3 <- p3 + theme(axis.title.x = element_blank())
p3 <- p3 + theme(axis.line = element_line(color="black", size = 0.5))
p3<- p3 + scale_y_continuous(breaks = 10*(0:10),expand = c(0,0))

ggsave(filename = "High_violence_data.pdf", plot = p3, width = 6, height = 4,
       units = "in")
