

# TO display the the current working directory which consist the .cvs file
getwd()
# a
#
#Show the total number of rows, the structure of the data frame, and first 10 rows of the data frame containing all of the NIPostcode data. 
# To load the Postcode data
postcode <- read.csv(file = "NIPostcodes.csv", header = FALSE)
# For displayng the type or what is type of the data stored in  
# In this case it is data frame.
class(postcode)
# Displaying the structure of the file
str(postcode)
# Number of rows in the postcode csv file 
nrow(postcode)
# Displaying the starting 10 rows from the dataframe.
head(postcode, 10)
# Displaying the column nanmes which we need to replace.
colnames(postcode)

#############################--------------------#######################

# b)
#
# Adding a suitable title for each attribute of the data withing the dataframe.
# Changing the column names with suitable Names.
attach(postcode)
colnames(postcode)[which(names(postcode) == "V1")] <- "ORGANIZATION_NAME"
colnames(postcode)[which(names(postcode) == "V2")] <- "SUB_BUILDING_NAME"
colnames(postcode)[which(names(postcode) == "V3")] <- "BUILDING_NAME"
colnames(postcode)[which(names(postcode) == "V4")] <- "NUMBER"
colnames(postcode)[which(names(postcode) == "V5")] <- "PRIMARY_THORFARE"
colnames(postcode)[which(names(postcode) == "V6")] <- "ALT_THORFARE"
colnames(postcode)[which(names(postcode) == "V7")] <- "SECONDARY_THORFARE"
colnames(postcode)[which(names(postcode) == "V8")] <- "LOCALITY"
colnames(postcode)[which(names(postcode) == "V9")] <- "TOWNLAND"
colnames(postcode)[which(names(postcode) == "V10")] <- "TOWN"
colnames(postcode)[which(names(postcode) == "V11")] <- "COUNTY"
colnames(postcode)[which(names(postcode) == "V12")] <- "POSTCODE"
colnames(postcode)[which(names(postcode) == "V13")] <- "X_CORDINATE"
colnames(postcode)[which(names(postcode) == "V14")] <- "Y_CORDINATE"
colnames(postcode)[which(names(postcode) == "V15")] <- "PRIMARY_KEY"
detach(postcode)
colnames(postcode)

str(postcode)
head(postcode, 50)

###################-------------------#######################
#
# c
# Replace and recode all missing entries with a suitable identifier
# Dealing with the missing values withing each column
# Replacing with the NA's
postcode$NUMBER[postcode$NUMBER == ""] <- NA
postcode$PRIMARY_THORFARE[postcode$PRIMARY_THORFARE == ""] <- NA
postcode$TOWNLAND[postcode$TOWNLAND == ""] <- NA
postcode$TOWN[postcode$TOWN == ""] <- NA
postcode$COUNTY[postcode$COUNTY == ""] <- NA
postcode$POSTCODE[postcode$POSTCODE == ""] <- NA


###################--------------------########################

# e
#
# Move the primary key identifier to the start of the dataset. 
# For column Reorder 
# Shifted the column named Primary_Key from last to the first in the data frame
colnames(postcode)
postcode <- postcode[, c(15, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)]
colnames(postcode)
str(postcode)


######################------------------#########################

# f
#
# Create a new dataset called Limavady_data. 
# Store within it only information where locality, townland and town contain the name Limavady. 
# Count and display the number of rows. 
# Store this information in a csv file called Limavady

attach(postcode)
limavady_data <- postcode[(grepl('limavady', LOCALITY, ignore.case = TRUE) |
                             grepl('limavady', TOWNLAND, ignore.case = TRUE) |
                             grepl('limavady', TOWN, ignore.case =TRUE)),]


detach(postcode)
print(nrow(limavady_data))

write.csv(limavady_data, "limavady.csv")


#######################-------------------------######################
#
# c
# Replace and recode all missing entries with a suitable identifier. 
# Decide whether it is best to remove none, some or all of the missing data

# Deleting the columns which we not using for futher analysis
# Those attribute are having more than 60 % NA values whithin them so it 
# is better to delete those attribute as they could affect the analysis.

postcode$ORGANIZATION_NAME <- NULL
postcode$SUB_BUILDING_NAME <- NULL
postcode$BUILDING_NAME <-NULL
postcode$ALT_THORFARE <- NULL
postcode$SECONDARY_THORFARE <- NULL

# For displaying the total number of missing values
sum(is.na(postcode))
sum(is.na(postcode$PRIMARY_KEY))
sum(is.na(postcode$NUMBER))
sum(is.na(postcode$PRIMARY_THORFARE))
sum(is.na(postcode$LOCALITY))
sum(is.na(postcode$TOWNLAND))
sum(is.na(postcode$TOWN))
sum(is.na(postcode$COUNTY))
sum(is.na(postcode$POSTCODE))
sum(is.na(postcode$X_CORDINATE))
sum(is.na(postcode$Y_CORDINATE))

# Check the rows which having missing values from the postcode. 
nrow(postcode)
# After llokng the figure we got to know that we have 
# more than 50% data having NA values.


# Missing valuse in town are 19872 and in Postcode those are 8900
summary(missing(postcode))
postcode <- postcode[!(is.na(postcode$TOWN) | is.na(postcode$POSTCODE)),]
str(postcode)

missing_value <- aggr(postcode, prop = FALSE, numbers = TRUE, plot = FALSE)

#########################-------------------------################################
#
# d
# Show the total number of missing values for each column in the 
# postcode data frame both before and after your decision to deal with the missing data variables. 
# 
str(postcode)
colnames(postcode)
# Missing value in each of the attributes is as below
#
missing_na <- sapply(postcode, function(y) sum(which(is.na(postcode))))
missing_na

# mice librabr we use here for data plotting fro postcode.
library(mice)
md.pattern(postcode)

# VIM is used for the Missing Values
# Checking the missing values with the help of VIM library
#
library(VIM)
missing_Vlaues <- aggr(postcode, prop = FALSE, numbers = TRUE)
summary(missing_Vlaues)
#################################---------------------#####################
#
# g
# Save the modified NIPostcode dataset in a csv file called CleanNIPostcodeData.
#
str(postcode)
write.csv(postcode, "CleanNIPostcodeData.csv")
