
## Scott Morrison
## L00131687
## CA3 Analysis of hypothesis testing


# Extract data from CSO website to R datframe with relevant fields

library(pxR)
my.px.object <- read.px( " http://www.cso.ie/px/pxeirestat/Database/eirestat/House%20Prices/HPM02.px" )
Data  <-  as.data.frame( my.px.object ) 
str(Data)

#write.table(House_Data_Ireland, file = "C:/Users/501671/Downloads/House_Ireland_new.csv", sep=",", row.names = FALSE)

Data <- Data[Data$Type.of.Sale == 'All Sale Types' & Data$Type.of.Buyer == 'All Buyer Types' & Data$Stamp.Duty.Event == 'Executions'& Data$Dwelling.Status == 'All Dwelling Statuses',]

Data <- dcast(Data, Data$Type.of.Sale + Data$Type.of.Buyer + Stamp.Duty.Event + Dwelling.Status + County + Month ~ Statistic)

Data$Year <- substr(Data$Month,1,4)

library('dplyr')
Data_Summary <- Data %>% group_by(Year, County) %>% summarize( Value=sum(`Value of Sales (Euro Million)`), Volume=sum(`Volume of Sales (Number)`), Median_Price=median(`Median Price (Euro)`) )
str(Data_Summary)

# plot mean house price (3 columns in dataframe) over year, colour by county

ggplot(data = Data_Summary, aes(x = Year, y = Value, fill = County)) + geom_point(size = 5, shape = 21) 
ggplot(data = Data_Summary, aes(x = Year, y = Volume, fill = County)) + geom_point(size = 5, shape = 21) 
ggplot(data = Data_Summary, aes(x = Year, y = Median_Price, fill = County)) + geom_point(size = 5, shape = 21) 
ggplot(data = Data_Summary, aes(x = Year, y = Median_Price)) + geom_boxplot() + geom_point(aes(fill = County), size = 6, shape = 21) 

# Web scrapping with rVest to retieve data for current house sales

Get_Most_Expensive <- function(url){
  library('rvest')
  
  for(i in 1:100){
    if(i == 1){
      webpage <- read_html(url)
      
      Prices <- data.frame(Price = webpage %>% html_nodes(".text-price") %>% html_text()) 
      Address <- data.frame(Address = webpage %>% html_nodes(".listing-results-address") %>% html_text())
      Expensive_houses <- cbind(Address, Prices)
    } else {
      webpage <- read_html(paste0(url,"&pn=",i))
      Prices <- data.frame(Price = webpage %>% html_nodes(".text-price") %>% html_text()) 
      Address <- data.frame(Address = webpage %>% html_nodes(".listing-results-address") %>% html_text())
      temp <- cbind(Address, Prices)
      Expensive_houses <- rbind(Expensive_houses, temp)
      
    }
  } # End of for loop
  return(Expensive_houses)
} # End of function


Clean_Prices <- function(House_Data){
  ## Removing unwanted characters with regex to extract price as numeric field
  House_Data$Price <- gsub(".*£","", House_Data$Price)
  House_Data$Price <- sub(" .*", "", House_Data$Price)
  House_Data$Price <- as.numeric(gsub(",", "", House_Data$Price))
  
  ## Removing where price = NA, this is houses listed as POA
  House_Data <- House_Data[! is.na(House_Data$Price),]
  return(House_Data)
}


Get_Ireland_County <- function(House_Data){
  ## Predefined list of Irish counties & UK Cities
  Counties <- data.frame( County = c("Antrim","Armagh","Carlow","Cavan","Clare","Cork","Derry","Donegal","Down",
                                     "Dublin","Fermanagh","Galway","Kerry","Kildare","Kilkenny","Laois","Leitrim",
                                     "Limerick","Longford","Louth","Mayo","Meath","Monaghan","Offaly","Roscommon",
                                     "Sligo","Tipperary","Tyrone","Waterford","Westmeath","Wexford","Wicklow"))
  ## Add County to the dataset, joins a predefined list of counties to the dataset based on partially related rows
  House_Data$Address <- as.character(House_Data$Address)
  Counties$County <- as.character(Counties$County)
  library(fuzzyjoin)
  House_Data <- House_Data %>% regex_inner_join(Counties, by = c(Address = "County"))
  return(House_Data)
}

Get_UK_City <- function(House_Data){
  UK_Cities <- data.frame( City = c("Bath","Birmingham","Bradford","Brighton & Hove","Bristol",
                                    "Cambridge","Canterbury","Carlisle","Chelmsford","Chester","Chichester","Coventry","Derby","Durham","Ely",
                                    "Exeter","Gloucester","Hereford","Kingston upon Hull","Lancaster","Leeds","Leicester","Lichfield", 
                                    "Lincoln","Liverpool","London","Manchester","Newcastle upon Tyne","Norwich","Nottingham","Oxford",
                                    "Peterborough","Plymouth","Portsmouth","Preston","Ripon","Salford","Salisbury","Sheffield","Southampton",
                                    "St Albans","Stoke-on-Trent","Sunderland","Truro","Wakefield","Wells","Westminster","Winchester","Wolverhampton",
                                    "Worcester","York","Aberdeen","Dundee","Edinburgh","Glasgow","Inverness","Perth","Stirling","Bangor","Cardiff",
                                    "Newport","St Asaph","St David's","Swansea","Armagh","Belfast","Londonderry","Lisburn","Newry") )
  
  ## Add County to the dataset, joins a predefined list of counties to the dataset based on partially related rows
  House_Data$Address <- as.character(House_Data$Address)
  UK_Cities$City <- as.character(UK_Cities$City)
  
  library(fuzzyjoin)
  House_Data <- House_Data %>% regex_inner_join(UK_Cities, by = c(Address = "City"))
  return(House_Data)
}


House_Data_Ireland <- Get_Most_Expensive(' https://www.zoopla.co.uk/overseas/property/ireland/?keywords=-POA&results_sort=highest_price&page_size=100')

House_Data_UK <- Get_Most_Expensive(' https://www.zoopla.co.uk/for-sale/property/uk/?identifier=uk&q=UK&results_sort=highest_price&radius=0&page_size=100')

House_Data_Ireland <- Clean_Prices(House_Data_Ireland)

House_Data_UK <- Clean_Prices(House_Data_UK)

House_Data_Ireland <- Get_Ireland_County(House_Data_Ireland)

House_Data_UK <- Get_UK_City(House_Data_UK)

str(House_Data_UK)
str(House_Data_Ireland)
Data_Summary <- Data %>% group_by(Year, County) %>% summarize( Value=sum(`Value of Sales (Euro Million)`), Volume=sum(`Volume of Sales (Number)`) )

# The original of house sales in ireland has no sample size indication, its pre- aggregated and includes the mean prices and sales
# The current house sales in ireland

library(ggplot2)
ggplot(House_Data_Ireland, aes(x=Price)) + 
  geom_histogram(aes(y=..density..),      
                 binwidth=100000,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  

#Get distribution summary stats
summary(House_Data_Ireland$Price)


# Obtain sample sizes for Dublin and Cork
nrow(House_Data_Ireland[House_Data_Ireland$County == 'Dublin',])
nrow(House_Data_Ireland[House_Data_Ireland$County == 'Cork',])


# Plot distributions of prices in dublin and cork
ggplot(House_Data_Ireland[House_Data_Ireland$County == 'Dublin',], aes(x=Price)) + 
  geom_histogram(aes(y=..density..),      
                 binwidth=100000,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  
ggplot(House_Data_Ireland[House_Data_Ireland$County == 'Cork',], aes(x=Price)) + 
  geom_histogram(aes(y=..density..),      
                 binwidth=100000,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  

# Overlaid plots
ggplot(rbind(House_Data_Ireland[House_Data_Ireland$County == 'Dublin',], House_Data_Ireland[House_Data_Ireland$County == 'Cork',]),aes(x=Price, fill=County)) + geom_density(alpha=0.25)
ggplot(rbind(House_Data_Ireland[House_Data_Ireland$County == 'Dublin',], House_Data_Ireland[House_Data_Ireland$County == 'Cork',]),aes(x=Price, fill=County)) + geom_histogram(alpha=0.25)
ggplot(rbind(House_Data_Ireland[House_Data_Ireland$County == 'Dublin',], House_Data_Ireland[House_Data_Ireland$County == 'Cork',]),aes(x=County, y=Price, fill=County)) + geom_boxplot()

# Distribution stats of dublin and cork house prices
summary(House_Data_Ireland[House_Data_Ireland$County == 'Dublin',])
summary(House_Data_Ireland[House_Data_Ireland$County == 'Cork',])

library(psych)
describe(House_Data_Ireland[House_Data_Ireland$County == 'Dublin',2])
describe(House_Data_Ireland[House_Data_Ireland$County == 'Cork',2])

# Power analysis for sample size
powerchanges <- pwr.t2n.test(n1 = 175, n2= 202, power = .9, sig.level = 0.05)
plot(powerchanges)

# extract data from 2012 and current in Ireland to test hypothesis
# Pre-aggregated medians from the CSO website
Data_Summary[Data_Summary$Year == 2012 & Data_Summary$County == 'Dublin',5]
Data_Summary[Data_Summary$Year == 2012 & Data_Summary$County == 'Cork',5]
# Medians from current house sales
median(House_Data_Ireland[House_Data_Ireland$County == 'Dublin',2])
median(House_Data_Ireland[House_Data_Ireland$County == 'Cork',2]) 

############ Hypothesis test 2 – Housing prices are similar in London and dublin #################

# Extracting Data for mood median test
library(dplyr)
temp_df <- as.data.frame(House_Data_UK[House_Data_UK$City == 'London',] %>% group_by(City) %>% sample_n(202))
temp_df1 <- House_Data_Ireland[House_Data_Ireland$County == 'Dublin',]
colnames(temp_df1)[3] <- 'City'

Mood_DF <- rbind(temp_df,temp_df1)
rm(temp_df,temp_df1)

# Mood median test for two sample median hypothesis testing
library(RVAideMemoire)

mood.test(Mood_DF$Price,Mood_DF$City,na.rm=TRUE)
median(Mood_DF[Mood_DF$City == 'London',2])
median(Mood_DF[Mood_DF$City == 'Dublin',2])

