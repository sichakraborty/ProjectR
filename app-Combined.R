rm(list = ls())

library(stringr)
library(dplyr)
suppressPackageStartupMessages(library(ggplot2))
library(scales)
library(gridExtra) #install.packages("gridExtra")
suppressPackageStartupMessages(library(choroplethr))
library(choroplethrMaps)
library(reshape2)
library(ggpubr) #install.packages("ggpubr")


# Read data
df <- read.csv("gun-violence-data_01-2013_03-2018.csv", stringsAsFactors = FALSE,
               na.strings = c("NA", ""))

# Get memory used to store df in megabytes
print(format(object.size(df), units = "Mb"))

#CLEANING THE DATA 
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
names(df)[names(df) == "participant_age_group"] <- "Participant_Age_Group"

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
df<- subset(df, !is.na(Participant_Age_Group))  

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
df$Participant_Age_Group <- NULL

#Reorder the columns
colnames(df)
colorder<- c("Incident_Id","Date", "State","City_or_County","Num_Killed","Num_injured","Num_Unharmed","Num_Guns_Involved","Stolen_Gun_Count",
             "Num_Victim","Num_Suspect","People_Involved","Acquaintance","Stranger",
             "Gang","Mass_Shooting","Not_Reported")
df<-df[,colorder]

#ANSWERING THE QUESTIONS

#Q1.HOW DOES GUN VIOLENCE VARY ACROSS DIFFERENT STATES OVER THE YEARS?

#Getting the year in another column
df$Year <- as.factor(format(as.Date(df$Date, format="%Y-%m-%d"),"%Y"))

#Creating a summary table to get the information of gun related incidents reported during the Years
df <- group_by(df, State, Year)
summ11 <- summarize(df, num_crimes = n())
summ11$State_abb <- summ11$State
#Create a column for the abbreviation of the States
summ11$State_abb <- state.abb[match(summ11$State_abb,state.name)] 

#Creating the plot
g11 <- ggplot(summ11, aes(x = State_abb, y = num_crimes))
g11 <- g11 + geom_point(aes(color = Year),
                        alpha = 0.75,
                        size = 2.5)
g11 <- g11 + theme(axis.title = element_blank()) 
g11 <- g11 + theme(axis.ticks.y = element_blank())
g11 <- g11 + theme(axis.text.x = element_text(size = 4.5))
g11 <- g11 + theme(axis.text.y = element_text(size = 7))
g11 <- g11 + theme(legend.text = element_text(size = 7))
g11 <- g11 + theme(legend.title = element_text(size = 6))
g11 <- g11 + theme(legend.key.size = unit(0.20, "in"))
g11 <- g11 + theme(legend.key = element_rect(fill = NA))
g11 <- g11 + theme(legend.position = c(0.90, 0.75))
g11

#Getting the plot in png
ggsave(filename = "g11.png", plot = g11, width = 6, height = 4,
       dpi = 600)
df <- ungroup(df)

#Another way to check the data
df <- group_by(df, State, Year)
summ1 <- summarize(df, num_crimes = n())

g1 <- qplot(Year, num_crimes, data = summ1, geom = "point", color = State)
g1 <- g1 + ggtitle("Gun Related Incidents per Year")
g1 <- g1 + theme(plot.title = element_text(face = "bold"))
g1 <- g1 + theme(axis.ticks = element_blank())
g1 <- g1 + theme(axis.title = element_blank()) 
g1 <- g1 + theme(legend.text = element_text(size = 5))
g1 <- g1 + theme(legend.title = element_text(size = 8))
g1 <- g1 + theme(legend.key.size = unit(0.10, "in"))
g1 <- g1 + theme(legend.spacing = unit(0, "in"))
g1 <- g1 + theme(legend.key = element_rect(fill = NA))
g1
#Getting the plot in png
ggsave(filename = "g1.png", plot = g1, width = 6, height = 4,
       dpi = 600)
df <- ungroup(df)

#How does gun violence vary over the years overall
g1a <- qplot(Year, data = df, geom = "bar", 
             fill = I("red"),                    
             color = I("red"))
g1a <- g1a + ggtitle("Number of Gun Related Incidents per Year")
g1a <- g1a + theme(plot.title = element_text(face = "bold"))
g1a <- g1a + theme(axis.ticks.y = element_blank())
g1a <- g1a + theme(axis.ticks.x = element_blank())
g1a <- g1a + theme(axis.title = element_blank()) 
g1a
#Getting the plot in png
ggsave(filename = "g1a.png", plot = g1a, width = 6, height = 4,
       dpi = 600)

#Creating a summary table to get the information of the gun related incidents of the States
df <- group_by(df, State)
summ1b <- summarize(df, num_crimes = n())
#What are the 10 states with highest incidents
summ1b <- arrange(summ1b, desc(num_crimes))
summ1b <- summ1b[1:10, ]

#Creating the plot
g1b <-ggdotchart(summ1b, x = "State", y = "num_crimes",
                 color = "State", size = 4,      # Points color and size
                 add = "segment",              # Add line segments
                 add.params = list(size = 1.5), 
                 palette = "jco",
                 ggtheme = theme_pubclean())
g1b <- g1b + ggtitle("States with highest Gun Related Incidents")
g1b <- g1b + theme(plot.title = element_text(face = "bold"))
g1b <- g1b + theme(axis.title = element_blank()) 
g1b <- g1b + theme(axis.ticks = element_blank())
g1b <- g1b + theme(legend.position = "none")
g1b
#Getting the plot in png
ggsave(filename = "g1b.png", plot = g1b, width = 6, height = 4,
       dpi = 600)
df <- ungroup(df)

#To narrow up the information, we want to see how do the States in top 6 crimes overall vary through the years
df <- group_by(df, State, Year)
summ1c <- summarize(df, num_crimes = n())
summ1c <- filter(summ1c, State == "Illinois"|State == "California"|State == "Florida"
                 |State == "Ohio"|State == "Texas"|State == "New York")
#Creating the plot
g1c <- ggplot(summ1c, aes(fill= State, y=num_crimes, x=Year)) + 
  geom_bar( stat="identity", position = "fill")
g1c <- g1c + ggtitle("States with the Highest Gun violence Incidents")
g1c <- g1c + theme(plot.title = element_text(face = "bold"))
g1c <- g1c + theme(axis.title = element_blank()) 
g1c <- g1c + theme(axis.ticks = element_blank())
g1c <- g1c + theme(axis.text.y = element_blank())
g1c <- g1c + theme(panel.background = element_blank())
g1c
#Getting the plot in png
ggsave(filename = "g1c.png", plot = g1c, width = 6, height = 4,
       dpi = 600)
df <- ungroup(df)

#For curiosity, Show the 15 cities in the US with highest crime.
df <- group_by(df, State, City_or_County)
summ1d <- summarize(df, num_crimes = n())
summ1d <- arrange(summ1d, desc(num_crimes))
summ1d <- summ1d[1:15, ]
summ1d$State_abb <- state.abb[match(summ1d$State,state.name)]
#Creating the plot
g1d <- qplot(State_abb, num_crimes, data = summ1d, geom = "point", color = City_or_County)
g1d <- g1d + ggtitle("Cities with highest Gun related Incidents")
g1d <- g1d + theme(plot.title = element_text(face = "bold"))
g1d <- g1d + theme(axis.ticks = element_blank())
g1d <- g1d + theme(axis.title = element_blank())
g1d <- g1d + theme(legend.text = element_text(size = 8))
g1d <- g1d + theme(legend.title = element_text(size = 10))
g1d <- g1d + theme(legend.key.size = unit(0.2, "in"))
g1d <- g1d + theme(legend.key = element_rect(fill = NA))
g1d <- g1d + geom_point(size= 3.5)
g1d
#Getting the plot in png
ggsave(filename = "g1d.png", plot = g1d, width = 6, height = 4,
       dpi = 600)
df <- ungroup(df)

#Delete the column Year created
df$Year <- NULL 

#Q2.WHICH STATES HAVE HIGHER AND LOWER NUMBER OF VIOLENCE CASES REPORTED DURING 2018? 

#Getting the year in another column
df$Years <- substr(df$Date,1,4)
df$Years <- factor(df$Years)

#Getting the months in another column, in order to know how many months 
#were evaluated during 2018
df$Months <- substr(df$Date,6,7)

#Creating a summary table to get the information of crimes reported in 2018
df <- group_by(df, State, Years)
summ_crimes_2018 <- summarize(df, Total_Crimes = n())
summ_crimes_2018 <- subset(summ_crimes_2018, Years == 2018)

#Fixing the data in summ_violence_2018_50 to create the map
#Function to change de name of the colums
change_name <- function(data, old, new){
  names(data)[names(data) == old] <- new
  return(data)
}
#Changing the names to apply state_choropleth
summ_crimes_2018 <- change_name(summ_crimes_2018, "State", "region")
summ_crimes_2018 <- change_name(summ_crimes_2018, "Total_Crimes", "value")

summ_crimes_2018$region <- as.character(summ_crimes_2018$region)

#Changing the states to lowercase to apply state_choropleth
for(i in 1:nrow(summ_crimes_2018)) {
  summ_crimes_2018$region[i] <- tolower(summ_crimes_2018$region[i])
}

#Creating the map
q2 <- state_choropleth(summ_crimes_2018, title = "Number of gun related incidents in 2018")
q2 <- q2 + scale_fill_brewer(palette = "OrRd")
q2 <- q2 + theme(plot.title = element_text(size = 24, face = "bold"))
q2 <- q2 + theme(legend.text = element_text(size = 10))
q2 <- q2 + theme(legend.title = element_text(size = 15, face = "italic"))
q2

#Getting the map in png
ggsave(filename = "Total Crimes 2018.png", plot = q2, width = 6, height = 4,
       dpi = 600)

df <- ungroup(df)

#Higher and lower number of violence cases reported every month during 2018
df <- group_by(df, Years, Months)
summ_2018_months <- summarize(df, Total_Crimes = n())
summ_2018_months <- subset(summ_2018_months, Years == 2018)

summ_2018_months$Months[summ_2018_months$Months=="01"] <- "January"
summ_2018_months$Months[summ_2018_months$Months=="02"] <- "February"
summ_2018_months$Months[summ_2018_months$Months=="03"] <- "March"
summ_2018_months$Months <- factor(summ_2018_months$Months)

months2018 <- barplot(summ_2018_months$Total_Crimes, main= "Violence Cases Reported 2018", 
                      names.arg =summ_2018_months$Months, col= "cadetblue4", xlab = "Months", 
                      ylab = "Crimes Reported")
df <- ungroup(df)

#Q2.1. WHICH STATE HAS THE MOST AND LEAST REPORTED GUN VIOLENCE THIS YEAR?

df <- group_by(df, State, Years)
summ_2018_Guns <- summarize(df, Guns_Involved = sum(Num_Guns_Involved))
summ_2018_Guns <- subset(summ_2018_Guns, Years == 2018)

#Changind the names to apply state_choropleth
summ_2018_Guns <- change_name(summ_2018_Guns, "State", "region")
summ_2018_Guns <- change_name(summ_2018_Guns, "Guns_Involved", "value")
summ_2018_Guns$region <- as.character(summ_2018_Guns$region)

#Changing the states to lowercase to apply state_choropleth
for(i in 1:nrow(summ_2018_Guns)) {
  summ_2018_Guns$region[i] <- tolower(summ_2018_Guns$region[i])
}

#Creating the map
q2.1 <- state_choropleth(summ_2018_Guns, title = "Gun Violence 2018")+
  scale_fill_brewer(palette = "RdPu")
q2.1 <- q2.1 + theme(plot.title = element_text(size = 24, face = "bold"))
q2.1 <- q2.1 + theme(legend.text = element_text(size = 10))
q2.1 <- q2.1 + theme(legend.title = element_text(size = 15, face = "italic"))

#Getting the map in png
ggsave(filename = "Gun Violence 2018.png", plot = q2.1, width = 6, height = 4,
       dpi = 600)
df <- ungroup(df)

#Comparison between number of crimes and the number of guns reported in 2018
df <- group_by(df, Years, Months)
summ_2018_Crimevsguns <- summarize(df, Total_Crimes = n(),Guns_Involved = sum(Num_Guns_Involved))
summ_2018_Crimevsguns <- subset(summ_2018_Crimevsguns, Years == 2018)

summ_2018_Crimevsguns$Months[summ_2018_Crimevsguns$Months=="01"] <- "1_January"
summ_2018_Crimevsguns$Months[summ_2018_Crimevsguns$Months=="02"] <- "2_February"
summ_2018_Crimevsguns$Months[summ_2018_Crimevsguns$Months=="03"] <- "3_March"
summ_2018_Crimevsguns$Months <- factor(summ_2018_Crimevsguns$Months)

#Getting a linear graph

#Eliminating the column "Years" to use melt
summ_2018_Crimevsguns <- summ_2018_Crimevsguns[,c(2,3,4)]
#Creating a melting data frame from my summary table "summ_2018_Crimevsguns"
dfm <- melt(summ_2018_Crimevsguns, id.vars = "Months")
crimevsguns <- ggplot(data=dfm, aes(x = Months, y = value, group = variable, colour = variable))+
  geom_line(size=1.5) + geom_point(size=3)
crimevsguns <- crimevsguns + ggtitle("Number of Incidents vs Number of Guns reported")
crimevsguns <- crimevsguns + ylim(3000, 5500)
crimevsguns <- crimevsguns + theme(plot.title = element_text(size = 15, face = "bold"))
crimevsguns <- crimevsguns + theme(legend.text = element_text(size = 10))
crimevsguns <- crimevsguns + theme(legend.title = element_text(size = 12, face = "italic")) 

ggsave(filename = "Number of Incidents vs Number of Guns reported.png", plot = crimevsguns, width = 6, height = 4,
       dpi = 600)
df <- ungroup(df)

#Q3. WAS THE VIOLENCE COMMITTED AGAINST STRANGERS OR DID THEY KNOW THE VICTIM BEFOREHAND? 

strangerplot <- ggplot(df, aes(x = Stranger, fill= I("blue") )) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(name="Percent of Shootings involving Strangers", labels=scales::percent)+
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25)
strangerplot <- strangerplot + theme(axis.text.y = element_blank())
strangerplot <- strangerplot + theme(axis.ticks.y = element_blank())
strangerplot <- strangerplot + theme(legend.text = element_text(size = 8))
strangerplot

#Was the violence committed gang related?
gangplot <- ggplot(df, aes(x = Gang, fill= I("red") )) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(name="Percent of Shootings that are Gang Related", labels=scales::percent)+
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25)
gangplot <- gangplot + theme(axis.text.y = element_blank())
gangplot <- gangplot + theme(axis.ticks.y = element_blank())
gangplot <- gangplot + theme(legend.text = element_text(size = 8))
gangplot

#Percent of crimes not reported as acquaintance, stranger,gang, or mass shooting?
Not_Reportedplot <- ggplot(df, aes(x = Not_Reported, fill= I("brown") )) +   
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(name="Percent of Crimes Not Reported", labels=scales::percent)+
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25)
Not_Reportedplot <- Not_Reportedplot + theme(axis.text.y = element_blank())
Not_Reportedplot <- Not_Reportedplot + theme(axis.ticks.y = element_blank())
Not_Reportedplot <- Not_Reportedplot + theme(legend.text = element_text(size = 8))
Not_Reportedplot

g <- grid.arrange(strangerplot,gangplot,Not_Reportedplot, ncol=3)

ggsave(filename = "Violence_Relationship.png", plot = g, width = 6, height = 4,
       dpi = 600)

#Q4. HOW GRAVE WAS THE VIOLENCE? DID IT RESULT IN DEATH OR INJURY ONLY? 

# Sort the column based on incident date
df <- arrange(df, Date)

# How grave was the gun related incident? Plot two graphs to compare the number of people injured and killed in these incidents
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
p1

p2 <- ggplot() + 
  geom_line(data = df, aes(x = Date, y = Num_injured), color="navyblue")+ ylab(label='People Injured')+scale_x_date(date_breaks = "6 months" , date_labels = "%b-%y")

p2 <- p2 + theme(panel.background = element_blank())
p2 <- p2 + theme(legend.text = element_text(size = 8))
p2 <- p2 + theme(title = element_text(size = 12))
p2 <- p2 + theme(plot.title = element_text(face = "bold"))
p2 <- p2 + theme(axis.title.x = element_blank())
p2 <- p2 + theme(axis.line = element_line(color="black", size = 0.5))
p2<- p2 + scale_y_continuous(breaks = 10*(0:10),expand = c(0,0))
p2

pTotal <- grid.arrange(p1, p2, nrow=2) #generates combined graph


ggsave(filename = "violence_data.png", plot = pTotal, width = 6, height = 4,
       dpi = 600)

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
p3

# We see that the highest number of gun related death and injury is based on incident between jun and july 2016. 
# We subset the data further to highlight the time frame
highData <- subset(peakData, Date >= "2016-06-01" & Date <= "2016-07-10")

p4 <- ggplot() + 
  geom_line(data = highData, aes(x = Date, y = Num_Killed), color="orange")+ ylab(label='People Killed')+scale_x_date(date_breaks = "1 week" , date_labels = "%d-%b")

p4 <- p4 + ggtitle("People Involved in Gun Violence between June-July 2016")
p4 <- p4 + theme(panel.background = element_blank())
p4 <- p4 + theme(legend.text = element_text(size = 8))
p4 <- p4 + theme(title = element_text(size = 12))
p4 <- p4 + theme(plot.title = element_text(face = "bold"))
p4 <- p4 + theme(axis.title.x = element_blank())
p4 <- p4 + theme(axis.line = element_line(color="black", size = 0.5))
p4<- p4 + scale_y_continuous(breaks = 10*(0:10),expand = c(0,0))
p4 <- p4 + theme(plot.title = element_text(hjust = 0.5))
p4

p5 <- ggplot() + 
  geom_line(data = highData, aes(x = Date, y = Num_injured), color="navyblue")+ ylab(label='People Injured')+scale_x_date(date_breaks = "1 week" , date_labels = "%d-%b")

p5 <- p5 + theme(panel.background = element_blank())
p5 <- p5 + theme(legend.text = element_text(size = 8))
p5 <- p5 + theme(title = element_text(size = 12))
p5 <- p5 + theme(plot.title = element_text(face = "bold"))
p5 <- p5 + theme(axis.title.x = element_blank())
p5 <- p5 + theme(axis.line = element_line(color="black", size = 0.5))
p5<- p5 + scale_y_continuous(breaks = 10*(0:10),expand = c(0,0))
p5


pHigh <- grid.arrange(p4, p5, nrow=2) #generates combined graph
pHigh

ggsave(filename = "Highest_violence_data.png", plot = pHigh, width = 6, height = 4,
       dpi = 600)

#Q5. HOW MANY GUNS WERE INVOLVED IN THE CRIME PER STATE ?


#Summarise the total number of guns involved for each state 
dm<-group_by(df,State)
dm$Date<-as.Date(dm$Date)
dm$State<-factor(dm$State)
sgps<-data.frame(summarise(dm,sum_gun=sum(Num_Guns_Involved)))
sgps$State_abb<-sgps$State
sgps$State_abb <- state.abb[match(sgps$State_abb,state.name)]

#Creating a map to visualize data over states
sgps$State<-tolower(sgps$State)
sgps$State<-as.character(sgps$State)
colnames(sgps)[1]<-"region"
colnames(sgps)[2]<-"value"

mapofgun<-state_choropleth(sgps,title = "Number of Guns involved per State",legend = "Numbers of Guns")+
  theme(plot.title = element_text(size = 18, face = "bold"))

print(mapofgun)

ggsave(filename = "Map of Num_of_Guns_per_State.png", plot = mapofgun, width = 6, height = 4,
       dpi = 600)

#Creating scatter chart to show the distribution of guns
scatterofgun <- qplot(State_abb, value,data = sgps, geom = "bin2d",
                      fill =value,alpha = I(0.5))+
  scale_fill_gradient(name = "sum_gun", low = "blue", high = "red")

scatterofgun<-scatterofgun+
  guides(fill=guide_colorbar(title="Number of Guns"))+
  xlab("State")+
  ylab("Number of Guns")+
  ggtitle("Number of Guns Involved per State")+
  theme(legend.text=element_text(size=7),
        legend.title=element_text(size=8),
        axis.text.x = element_text(size = 6),
        plot.title = element_text(size = 18, face = "bold"))

print(scatterofgun)

ggsave(filename = "Scatter of Num_of_Guns_per_State.png", plot = scatterofgun, width = 6, height = 4,
       dpi = 600)

#Find the top 10 states with most guns involved
top10statesw.guns<-arrange(top_n(sgps,n=10,wt=value),desc(value))
colnames(top10statesw.guns)[1]<-"State"
colnames(top10statesw.guns)[2]<-"Num_of_Guns"
print(top10statesw.guns)


#Q6.HOW MANY STOLEN GUNS WERE INVOLVED IN THE INCIDENTS REPORTED PER STATE ACROSS THE YEARS?

#Analyzing the percentage of stolen guns involved across the years
df <- group_by(df, Years)
summ_stolen_Guns <- summarize(df, Total_Guns=sum(Num_Guns_Involved, na.rm = TRUE), 
                              Stolen_Guns=sum(Stolen_Gun_Count, na.rm = TRUE), 
                              Percentage_Stolen_Guns=Stolen_Guns/Total_Guns)
#Creating the graph
stolenGuns <- ggplot(data=summ_stolen_Guns, aes(x=Years, y=Percentage_Stolen_Guns, group=1))+
  geom_line(color="blue")+
  geom_point(color="blue")
stolenGuns <- stolenGuns + ggtitle("Stolen Guns Through Years")
stolenGuns <- stolenGuns + theme(plot.title = element_text(size = 24, face = "bold"))
stolenGuns <- stolenGuns + ylab("Stolen Guns %")
stolenGuns <- stolenGuns + theme(panel.grid.minor.y = element_blank())

#Getting the graph in png
ggsave(filename = "Stolen Guns Through Years.png", plot = stolenGuns, width = 6, height = 4,
       dpi = 600)

df <- ungroup(df)

#States with stolen guns during 2015
df <- group_by(df, State, Years)
summ_2015_stolenguns <- summarize(df, Stolen_Guns=sum(Stolen_Gun_Count, na.rm = TRUE))
#Getting just 2015 information
summ_2015_stolenguns <- subset(summ_2015_stolenguns, Years == 2015)
#Creating a column with the percentage of stolen guns
summ_2015_stolenguns$Percentage_Stolen_Guns <- (summ_2015_stolenguns$Stolen_Guns/sum(summ_2015_stolenguns$Stolen_Guns))*100

#Changing the names to apply state_choropleth
summ_2015_stolenguns <- change_name(summ_2015_stolenguns, "State", "region")
summ_2015_stolenguns <- change_name(summ_2015_stolenguns, "Percentage_Stolen_Guns", "value")
summ_2015_stolenguns$region <- as.character(summ_2015_stolenguns$region)

#Changing the states to lowercase to apply state_choropleth
for(i in 1:nrow(summ_2018_Guns)) {
  summ_2015_stolenguns$region[i] <- tolower(summ_2015_stolenguns$region[i])
}
#Creating the map
stolenGuns2015 <- state_choropleth(summ_2015_stolenguns, title = "Stolen Guns 2015", 
                                   legend = "% Stolen Guns")
stolenGuns2015 <- stolenGuns2015 + theme(plot.title = element_text(size = 24, face = "bold"))
stolenGuns2015 <- stolenGuns2015 + theme(legend.text = element_text(size = 10))
stolenGuns2015 <- stolenGuns2015 + theme(legend.title = element_text(size = 15, face = "italic"))

#Getting the graph in png
ggsave(filename = "Stolen Guns per State 2015.png", plot = stolenGuns2015, width = 6, height = 4,
       dpi = 600)

df <- ungroup(df)
