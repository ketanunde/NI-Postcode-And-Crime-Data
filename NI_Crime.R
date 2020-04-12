library(dplyr)
library(plyr)
library(collections)
library(stringr)
######################------------------------########################
#
# a 
# amalgamate all of the crime data from each csv file into one dataset. 
# Save this dataset into a csv file called AllNICrimeData.
#
reading_csv <- dir(path = "Data", 
                   pattern=".*[.]csv", recursive = T, full.names =TRUE)

crime_data  <- bind_rows(lapply(reading_csv, read.csv))
head(crime_data, 10)

write.csv(crime_data, "Data/ALLCrimeData.csv")


print(nrow(crime_data))
###############################----------------------###################

#b
# Modify the structure of the newly created AllNICrimeData csv file 
# and remove the following attributes: CrimeID, Reported by, Falls within, LSOA code, LSOA name, last outcome and context. 
# Save this new structure and show the structure of the modified file.
# The structure of  file before removing the columns
#
str(crime_data, width = 80, strict.width = "cut")
updated_crime_data <- crime_data
# remove the columns 
#
updated_crime_data$Crime.ID <- NULL
updated_crime_data$Reported.by <- NULL
updated_crime_data$Falls.within <- NULL
updated_crime_data$LSOA.code <- NULL
updated_crime_data$LSOA.name <- NULL
updated_crime_data$Last.outcome.category <- NULL
updated_crime_data$Context <- NULL

str(updated_crime_data)

#####################------------------------##############################
#
# c
# In the AllNICrimeData csv file, shorten each crime type as follows: 
# Abbreventions
crime_data$Crime.type <- as.character(crime_data$Crime.type)
str(crime_data)
attach(crime_data)
crime_data$Crime.type[crime_data$Crime.type == "Anti-social behaviour"] <- "ASBO"
crime_data$Crime.type[crime_data$Crime.type == "Bicycle theft"] <- "BITH"
crime_data$Crime.type[crime_data$Crime.type == "Burglary"] <- "BURG"
crime_data$Crime.type[crime_data$Crime.type == "Criminal damage and arson"] <- "CDAR"
crime_data$Crime.type[crime_data$Crime.type == "Drugs"] <- "DRUG"
crime_data$Crime.type[crime_data$Crime.type == "Other theft"] <- "OTTH"
crime_data$Crime.type[crime_data$Crime.type == "Possession of weapons"] <- "POWS"
crime_data$Crime.type[crime_data$Crime.type == "Public order"] <- "PUBO"
crime_data$Crime.type[crime_data$Crime.type == "Robbery"] <- "ROBY"
crime_data$Crime.type[crime_data$Crime.type == "Shoplifting"] <- "SHOP"
crime_data$Crime.type[crime_data$Crime.type == "Theft from the person"] <- "THPR"
crime_data$Crime.type[crime_data$Crime.type == "Vehicle crime"] <- "VECR"
crime_data$Crime.type[crime_data$Crime.type == "Violence and sexual offences"] <- "VISO"
crime_data$Crime.type[crime_data$Crime.type == "Other crime"] <- "OTCR"
detach(crime_data)


######################-------------------------################################
#
# e
# Modify the AllNICrimeData dataset so that the Location attribute contains only a street name.
# For example, the attribute value “On or near Westrock Square” 
# should be modified to only contain “Westrock Square”. 
# Modify the resultant empty location attributes with a suitable identifie
#
head(crime_data, 5)
crime_data$Location <- str_replace(crime_data$Location, pattern = "On or near ", "")
# We have to replace the other missing values from the attribute also with NA's.
crime_data$Location[crime_data$Location == ""] <- NA
head(crime_data, 5)

######################----------------------##############################


# We have to use the CleanNIPostcode.csv for the searching location.
# so we have to load the file.
postcode <- read.csv("CleanNIPostcodeData.csv")
village <- read.csv("VillageList.csv")

# Change the column name 
str(village)
colnames(village)
names(village)[names(village) == "ï..CITY.TOWN.VILLAGE"] <- "CITY.TOWN.VILLAGE"



#
# Choose 5000 random samples of crime data from the AllNICrimeData dataset where the location attribute contains location information. 
# This means that the location information should NOT contain an NA identifier.
# Set the seed value to 100. Store this data in a data frame called random_crime_sample. 
#
set.seed(100)
random_crime_sample <- crime_data %>% filter(!is.na(crime_data$Location) 
                                             & crime_data$Location != "No Location") %>% sample_n(5000)
head(random_crime_sample, 5)

str(random_crime_sample)

# Then create a function called find_a_town that uses the CleanNIPostcodeData data frame to find correct town/city 
# information for each location variable within the random_crime_sample dataset. 
# Save each matched town into the random_crime_sample data frame under the column name Town. 
#
find_a_town <- function(crime_data, postcode) {
  crime_data$Town <-with(postcode,
                         Town[match(
                           toupper(crime_data$Location), Primary.Thorofare)])
  print(head(crime_data, 5))
}


add_town_data <- function(crime_data, village){
  crime_data$Population <- with(village,
                    Population[match(
                    crime_data$Town, 
                                      toupper(village$CITY.TOWN.VILLAGE))])
  print(head(crime_data))
}

random_crime_sample <- find_a_town(random_crime_sample, postcode)

random_crime_sample <- add_town_data(random_crime_sample, village)


####################---------------##########################
#
# d
# Using the plot() function, 
# show a plot of each crime frequency from the crime.type field
#

plot_crime <- function(crime_data){
  type_occurence <- sort(table(unlist(crime_data$Crime.type)))
  labels <- as.character(names(type_occurence))
  colour <- colorRampPalette(colors = c("red", "green"))(20)
  
  
  barplot(type_occurence, 
  main = "Crime Frequency", 
  ylab = "",
  xlab = "Count",
  col = colour,
  )
  axis(1, at = seq(0, range(type_occurence)[2],
                   by = 500))
  mtext("Type", side = 2, line = 5) 
}

plot_crime(crime_data)
######################-------------------------#######################


