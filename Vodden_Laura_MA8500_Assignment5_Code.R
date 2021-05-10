#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ASSIGNMENT 5 CAPSTONE PROJECT R CODE~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Set working directory and import data
setwd('C:/Users/laura/OneDrive/Desktop/MA5800')
AAC_Data <- read.csv("AAC_Outcomes.csv", header = TRUE, sep = ",", na.strings = c("", "NA"))
dim(AAC_Data)
#95367    12

# Add libraries
library(tidyr)
library(dplyr)
library(ggplot2)

# RStudio Version
RStudio.Version()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~DATA CLEANING~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Type conversion
Animal_ID <- as.numeric(Animal_ID)

Age <- as.character(Age)


# Create new variables
AAC_Data$Date_Birth <- as.POSIXct(AAC_Data$Date.of.Birth, format = "%m/%d/%Y")
Date_Birth <- as.Date(AAC_Data$Date_Birth)

AAC_Data$Date_Arrival <- as.POSIXct(AAC_Data$DateTime, format = "%m/%d/%Y")
Date_Arrival <- as.Date(AAC_Data$Date_Arrival)


#filter by years, convert to character to remove 'years', convert back to numerical, multiply by 12 to convert years to months
YearsOnly <- c("1 year", "2 years", "3 years", "4 years", "5 years", "6 years", "7 years", "8 years", "9 years", "10 years", "11 years", "12 years", "13 years", "14 years", "15 years", "16 years", "17 years", "18 years", "19 years", "20 years", "21 years", "22 years", "23 years", "24 years")

FilterByYears <- filter(AAC_Data, Age.upon.Outcome %in% YearsOnly)
FilterByYears$Age.upon.Outcome <- as.character(FilterByYears$Age.upon.Outcome)
FilterByYears$Age.upon.Outcome <- strtrim(FilterByYears$Age.upon.Outcome, 2)
FilterByYears$Age.upon.Outcome <- as.numeric(FilterByYears$Age.upon.Outcome)*12


#As above, for months, no conversion needed as already in months
MonthsOnly <- c("1 month", "2 months", "3 months", "4 months", "5 months", "6 months", "7 months", "8 months", "9 months", "10 months", "11 months", "12 months")
FilterByMonths <- filter(AAC_Data, Age.upon.Outcome %in% MonthsOnly)
FilterByMonths$Age.upon.Outcome <- as.character(FilterByMonths$Age.upon.Outcome)
FilterByMonths$Age.upon.Outcome <- strtrim(FilterByMonths$Age.upon.Outcome, 2)
FilterByMonths$Age.upon.Outcome <- as.numeric(FilterByMonths$Age.upon.Outcome)


#This omits any data containg 'days' or '0 years'


# Join two dataframes together
AAC_Data <- rbind(FilterByMonths, FilterByYears)


# Create new variables - BirthtoArrival and Duration, both in months
AAC_Data <- mutate(AAC_Data, BirthtoArrival = abs(Date_Birth - Date_Arrival)/365*12)
AAC_Data <- mutate(AAC_Data, Duration = abs(Age.upon.Outcome - BirthtoArrival))

# Rename variables
AAC_Data$Animal_ID <- AAC_Data$Animal.ID
AAC_Data$Outcome_Type <- AAC_Data$Outcome.Type
AAC_Data$Outcome_Sub <- AAC_Data$Outcome.Subtype
AAC_Data$Duration_Months <- AAC_Data$Duration
AAC_Data$Sex <- AAC_Data$Sex.upon.Outcome
AAC_Data$Age_Months <- AAC_Data$Age.upon.Outcome
AAC_Data$Animal_Type <- AAC_Data$Animal.Type

# Remove redundant rows
AAC_Data <- select(AAC_Data, -Name, -DateTime, -MonthYear, -Date.of.Birth, -Sex.upon.Outcome, -Age.upon.Outcome, -Duration, -Animal.ID, -Outcome.Type, -Outcome.Subtype, -Animal.Type, -BirthtoArrival)


#Reorder Rows
colnames(AAC_Data)
AAC_Data <- AAC_Data[,c(5,11,1,2,9,10,6,7,3,4,8)]


# Remove rows with missing data
AAC_Data <- na.omit(AAC_Data)

#Data frame has now been cleaned and organised
dim(AAC_Data)
# 36308    11

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~DATA SUMMMARISATION~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

AAC_Summary <- summarise(AAC_Data, Total_Animals = n(), Total_Cats = sum(Animal_Type == "Cat"), Total_Dogs = sum(Animal_Type == "Dog"), Total_Birds = sum(Animal_Type == "Bird"), Total_Livestock = sum(Animal_Type == "Livestock"), Total_Other = sum(Animal_Type == "Other"))
View(AAC_Summary)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~DATA EXPLORATION~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Set variables
Animal_ID <- AAC_Data$Animal_ID
Name <- AAC_Data$Name
Date <- AAC_Data$DateTime
DOB <- AAC_Data$Date_Birth
Outcome <- AAC_Data$Outcome_Type
Outcome_Sub <- AAC_Data$Outcome_Sub
Animal_Type <- AAC_Data$Animal_Type
Sex <- AAC_Data$Sex
Age <- AAC_Data$Age_Months
Breed <- AAC_Data$Breed
Colour <- AAC_Data$Color
Duration <- AAC_Data$Duration_Months

#SUBSAMPLE~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#CATS, DOGS, BIRDS, OTHER
Cats <- filter(AAC_Data, Animal_Type == "Cat")
Dogs <- filter(AAC_Data, Animal_Type == "Dog")
Birds <- filter(AAC_Data, Animal_Type == "Bird")
Livestock <- filter(AAC_Data, Animal_Type == "Livestock")
Other <- filter(AAC_Data, Animal_Type == "Other")

#ADOPTED CATS, DOGS, BIRDS, LIVESTOCK, OTHER
Adopted <- filter(AAC_Data, Outcome_Type == "Adoption")

Adopted_Cats <- filter(Adopted, Animal_Type == "Cat")
Adopted_Dogs <- filter(Adopted, Animal_Type == "Dog")
Adopted_Birds <- filter(Adopted, Animal_Type == "Bird")
Adopted_Livestock <- filter(Adopted, Animal_Type == "Livestock")
Adopted_Other <- filter(Adopted, Animal_Type == "Other")

#MEAN DURATION~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
AAC_Data$Duration_Months<- as.numeric(AAC_Data$Duration_Months)

Cat_Dur <- mean(Cats$Duration_Months)
Dog_Dur <- mean(Dogs$Duration)
Bird_Dur <- mean(Birds$Duration)
Livestock_Dur <- mean(Livestock$Duration)
Other_Dur <- mean(Other$Duration)


#Create dataframe
Animal <- c("Cat", "Dog", "Bird", "Livestock", "Other")
Mean_Duration <- c(Cat_Dur, Dog_Dur, Bird_Dur, Livestock_Dur, Other_Dur)

Duration_AAC <- data.frame(Animal, Mean_Duration)


#MEAN AGE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CatAge <- mean(Adopted_Cats$Age)
DogAge <- mean(Adopted_Dogs$Age)
BirdAge <- mean(Adopted_Birds$Age)
LivestockAge <- mean(Adopted_Livestock$Age)
OtherAge <- mean(Adopted_Other$Age)

#Create dataframe
Animal <- c("Cat", "Dog", "Bird", "Livestock", "Other")
Mean_Age <- c(CatAge, DogAge, BirdAge, LivestockAge, OtherAge)

Adopted_Age_AAC <- data.frame(Animal, Mean_Age)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~GENERAL PLOTS~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

Plot1 <- ggplot(data = AAC_Data) +
  geom_bar(mapping = aes(x = Animal_Type, fill = Outcome_Type), position = "fill")
Plot1 + (labs(title = "AAC outcomes by animal type"))

Plot2 <- ggplot(data = Duration_AAC) +
  geom_bar(mapping = aes(x = Animal, y = Mean_Duration, fill = Animal), stat = "identity")
Plot2 + (labs(title = "Mean duration (months) at AAC by animal type"))

Plot3 <- ggplot(data = Adopted_Age_AAC) +
  geom_bar(mapping = aes(x = Animal, y = Mean_Age, fill = Animal), stat = "identity")
Plot3 + (labs(title = "Mean age (months) at adoption by animal type"))

Adopted <- mutate(Adopted, log_Age_Months = log (Adopted$Age_Months))
Plot4 <- ggplot(data = Adopted) +
  geom_boxplot(mapping = aes(x = Animal_Type, y = log_Age_Months))
Plot4 + (labs(title = "Age at adoption (months) by animal type"))

Plot5 <- ggplot(data = Adopted) +
  geom_boxplot(mapping = aes(x = Animal_Type, y = Age_Months))
Plot5 + (labs(title = "Age at adoption (months) by animal type"))

Plot5 <- ggplot(data = Adopted) +
  geom_boxplot(mapping = aes(x = Animal_Type, y = Duration_Months))
Plot5 + (labs(title = "Duration at AAC (months) by animal type"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~CAT PLOTS~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
Cats$Age_Months <- as.factor(Cats$Age_Months)

Cat_Plot1 <- ggplot(data = Cats) +
  geom_bar(mapping = aes(x = Age_Months, fill = Outcome_Type), position = "fill")
Cat_Plot1 + labs(title = "Outcome type for cats by age (months)")

Cat_Plot2 <- ggplot(data = Cats) +
  geom_bar(mapping = aes(x = Sex, fill = Outcome_Type), position = "fill")
Cat_Plot2 + labs(title = "Outcome type for cats by sex") + coord_flip()


Cat_Plot3 <- ggplot(data = Adopted_Cats) +
  geom_bar(mapping = aes(x = Age_Months, y = Outcome_Type), stat = "identity")
Cat_Plot3 + (labs(title = "Cat adoption by age"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~DOG PLOTS~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
Dogs$Age_Months <- as.factor(Dogs$Age_Months)

Dog_Plot1 <- ggplot(data = Dogs) +
  geom_bar(mapping = aes(x = Age_Months, fill = Outcome_Type), position = "fill")
Dog_Plot1 + labs(title = "Outcome type for Dogs by age (months")

Dog_Plot2 <- ggplot(data = Dogs) +
  geom_bar(mapping = aes(x = Sex, fill = Outcome_Type), position = "fill")
Dog_Plot2 + labs(title = "Outcome type for dogs by sex") + coord_flip()

Dog_Plot3 <- ggplot(data = Adopted_Dogs) +
  geom_bar(mapping = aes(x = Age_Months, y = Outcome_Type), stat = "identity")
Dog_Plot3 + (labs(title = "Dog adoption by age"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~EUTHANASIA PLOTS~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#          
Euthanasia <- filter(AAC_Data, Outcome_Type == "Euthanasia")


Euth_Plot1 <- ggplot(data = Euthanasia) +
  geom_bar(mapping = aes(x = Animal_Type, fill = Outcome_Sub), position = "fill")
Euth_Plot1 + labs(title = "Euthanasia reason by animal type")


#>90% SURVIVAL?
Euthanasia <- filter(AAC_Data, Outcome_Type == "Euthanasia")
6873*100/95367
100-(6873*100/95367) 
#7.20% euthanasia; 92.79% survival rate




