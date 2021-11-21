#Cleaning Suicide rates 
#Reading in File
suicide <- read.csv("suicide.csv")
#Filter to only exclude gender specific information
suicide <- filter(suicide, X.1 == " Both sexes")
#Filter to only include data from 2015
suicide <- subset(suicide, select = c(1,3))
#Include only the Country and the respective suicide rates
names(suicide) <- c("Country", "AgeStandardizedSuicideRatesPer100000Population2015") 

#Cleaning Happiness Score 
#Importing Data Set:
HappinessScore2015 <- read.csv("2015.csv")
View(HappinessScore2015)
#Selecting columns:
HappinessScore2015selectcolumns <- select(HappinessScore2015, c("Country", "Happiness.Score"))
#Converting to numeric: 
HappinessScore2015selectcolumns[,2]<-sapply(HappinessScore2015selectcolumns[,2],as.numeric)
#Final Data Set: 
HappinessScore <- HappinessScore2015selectcolumns
#Exporting Data Set:
write.csv(HappinessScore, file ="HappinessScore2015.csv")

#Cleaned religion dataset:
religion <- WRP_national_1_
#Select the data in 2010, which is the closest to 2015
religion2010 <- subset(religion, year == 2010)

#Select the top five religions, atheism and other religions
religion2010pctName <- select(religion2010, c("name", "chrstgenpct", "judgenpct", "islmgenpct", "budgenpct", "nonreligpct", "othrgenpct"))
religion2010pct = religion2010[,grepl("*pct",names(religion2010))]

#Select the dominant religion in each country 
religion2010pctName$max = max(religion2010pctName$chrstgenpct, religion2010pctName$judgenpct, religion2010pctName$islmgenpct, religion2010pctName$budgenpct, religion2010pctName$nonreligpct, religion2010pctName$othrgenpct)

for(i in 1:length(religion2010pctName$name)) {
  ligion2010pctName$max[i] = max(religion2010pctName$chrstgenpct[i], religion2010pctName$judgenpct[i], religion2010pctName$islmgenpct[i], religion2010pctName$budgenpct[i], religion2010pctName$nonreligpct[i], religion2010pctName$othrgenpct[i])
}

for(i in 1:length(religion2010pctName$name)) {
  if (religion2010pctName$max[i] == religion2010pctName$chrstgenpct[i]) {
    religion2010pctName$max[i] = "Christian"}
  if(religion2010pctName$max[i] == religion2010pctName$judgenpct[i]){
    religion2010pctName$max[i] = "Judaism"}
  if(religion2010pctName$max[i] == religion2010pctName$islmgenpct[i]){
    religion2010pctName$max[i] = "Islam"}
  if(religion2010pctName$max[i] == religion2010pctName$budgenpct[i]){
    religion2010pctName$max[i] = "Buddhism"}
  if(religion2010pctName$max[i] == religion2010pctName$nonreligpct[i]){
    religion2010pctName$max[i] = "Atheism"}
  if(religion2010pctName$max[i] == religion2010pctName$othrgenpct[i])
  {religion2010pctName$max[i] = "Others"}
}

religion2010pctName <- religion2010pctName[, -c(2:7)]

names(religion2010pctName) <- c("CountryCode", "Religion")
write.csv(religion2010pctName, file="Religion2.csv")

#Cleaning Temperature
#Importing Data Set:
temps <- read.csv("Desktop/The College/Semester 4/Data Science/MAT295- Final Project/temps.csv")
View(temps)
#Selecting columns:
TempsColumns <- select(temps, c("ISO_3DIGIT", "Annual_temp"))
#Renaming columns: 
names(TempsColumns) <- c("Country", "Annual Temperature")
#Converting to numeric: 
TempsColumns[,2]<-sapply(TempsColumns[,2],as.numeric)
#Left joining with country code: 
trialtemp <- left_join(TempsColumns, CompleteCode, by = c("Country" = "alpha.3"))
#Renaming columns: 
names(trialtemp) <- c("CountryCode", "Annual.Temperature", "X", "Country")
#Selecting columns:
Temperature <- select(trialtemp, c("Country", "Annual.Temperature"))
#Exporting Data Set:  
write.csv(Temperature, file="AnnualTemperature.csv")

#Loading File with Country and it's Income Level
IncomeGroup <- read.csv("CLASS.csv")
IncomeGroup <- select(IncomeGroup, c(3,4,7))
IncomeGroup <- IncomeGroup[-c(1:5), ]
#Renaming 
names(IncomeGroup) <- c("Country", "CountryCode", "IncomeGroup")

#Added Income Groups
SuicideRatebyCountry <- left_join(suicide, IncomeGroup, by = "Country")

happiness$Country <- as.character(happiness$Country)
psychiatrist$Country <- as.character(psychiatrist$Country)
temps$Country <- as.character(temps$Country)
religion$Country <- as.character(religion$Country)

#Joining Suicide Rate Data to Explanatory variables
happinesssuicidetemp <- left_join(happiness, SuicideRatebyCountry, by ="Country")
psychsuicidetemp <- left_join(psychiatrist, SuicideRatebyCountry, by ="Country")
tempssuicidetemp <- left_join(temps, SuicideRatebyCountry, by ="Country")
religionsuicidetemp <- left_join(religion, religionsuicidetemp <- left_join(religion, test, by ="Country"), by ="Country")

#Transform temperature to fahrenheit
tempssuicidetemp <- mutate(tempssuicidetemp, Annual.Temperature = ((9 * Annual.Temperature)/5) +32 )
