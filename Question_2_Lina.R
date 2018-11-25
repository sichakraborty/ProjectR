library(stringr)
library(dplyr)
library(ggplot2)
library(choroplethr)
library(choroplethrMaps)
library(reshape2)

df <- read.csv("Gun_Violence_Clean.csv")

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
#Changind the names to apply state_choropleth
summ_crimes_2018 <- change_name(summ_crimes_2018, "State", "region")
summ_crimes_2018 <- change_name(summ_crimes_2018, "Total_Crimes", "value")

summ_crimes_2018$region <- as.character(summ_crimes_2018$region)

#Changing the states to lowercase to apply state_choropleth
for(i in 1:nrow(summ_crimes_2018)) {
  summ_crimes_2018$region[i] <- tolower(summ_crimes_2018$region[i])
}

#Creating the map
q2 <- state_choropleth(summ_crimes_2018, title = "Number of crimes 2018")
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

#Comparison 2018 between number of crimes and the number of guns reported 
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
crimevsguns <- crimevsguns + ggtitle("Number of crimes vs Number of Guns reported in the crimes")
crimevsguns <- crimevsguns + ylim(3000, 5500)
crimevsguns <- crimevsguns + theme(plot.title = element_text(size = 24, face = "bold"))
crimevsguns <- crimevsguns + theme(legend.text = element_text(size = 10))
crimevsguns <- crimevsguns + theme(legend.title = element_text(size = 15, face = "italic")) 

#Q7.HOW MANY STOLEN GUNS WERE INVOLVED IN THE INCIDENTS REPORTED PER STATE ACROSS THE YEARS?

#Analyzing the percentage of stolen guns involved across the years
df <- group_by(df, Years)
summ_stolen_Guns <- summarize(df, Total_Guns=sum(Num_Guns_Involved, na.rm = TRUE), 
                              Stolen_Guns=sum(Stolen_Gun_Count, na.rm = TRUE), 
                              Percentage_Stolen_Guns=Stolen_Guns/Total_Guns)
#Creating the map
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
summ_2015_stolenguns <- summarize(df, Stolen_Guns=sum(Stolen_Gun_Count, na.rm = TRUE), 
                        Percentage_Stolen_Guns=(Stolen_Guns/sum(summ_2015_stolenguns$Stolen_Guns))*100)
summ_2015_stolenguns <- subset(summ_2015_stolenguns, Years == 2015)

#Changind the names to apply state_choropleth
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

#States without stolen guns
df <- group_by(df, State, Years)
summ_No_Stolen_Guns <- summarize(df, Stolen_Guns=sum(Stolen_Gun_Count, na.rm = TRUE))
summ_No_Stolen_Guns <- subset(summ_No_Stolen_Guns, summ_No_Stolen_Guns$Stolen_Guns == 0)

df <- ungroup(df)
