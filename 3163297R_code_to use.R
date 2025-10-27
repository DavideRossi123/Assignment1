
#STUDENT: DAVIDE ROSSI
#GUID: 3163297R
#DATASET: MIDDLESBROUGH.csv

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###########################Data Wrangling#######################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#  


#Load the necessary libraries####
library(tidyverse)
library(viridis)   #color palette that works well for colorblind readers  
#As explained on the R documentation:
#'cividis', a corrected version of 'viridis', 'cividis', 
#'developed by Jamie R. Nuñez, Christopher R. Anderton, and 
#'Ryan S. Renslow, and originally ported to R by Marco Sciaini.
#'It is designed to be perceived by readers with all forms of color blindness.

library(scales)    #to format numbers and axis labels  
library(GGally)    #extends some features of ggplot2  
library(gridExtra) #allows combining several plots into one figure
library(patchwork) #allows combining several plots into one figure
library(statmod)   #to compute quantile residuals
library(mgcv)      #used to fit Generalized Additive Models(GAM)  
library(magrittr)  #for pipe operators 
library(Metrics)   #offers functions to calculate model accuracy measures  
library(corrplot)  #for plotting correlation matrices
library(car)       #to compute the variance inflation factors
library(caret)     #to perform cross validation
library(knitr)     #to create tables
#Load the dataset####
#url of the GitHub repository were data are stored:
url <- "https://raw.githubusercontent.com/DavideRossi123/Assignment1/main/MIDDLESBROUGH.csv"
#Read the CSV file and store it in a tibble called 'house':
house<-read_csv(url)
#Take an initial look at the datset:
glimpse(house)
summary(house)


#Check for missing values####
#Compute the share of NAs for each of the 105 variables:
share.NA<-sapply(house,function(x) mean(is.na(x)))

#Let's create the corresponding tibble,since it's necessary for plotting 
#in ggplot:
share.NA.tib<- tibble(
  variable = names(share.NA),
  share = share.NA)

#Keep only the variables that have a non zero share of NAs:
share.NA.tib<-filter(share.NA.tib,share!=0)

#Let's plot now the share of NAs for each variable:
ggplot(data=share.NA.tib,aes(x=share,y=variable))+
  geom_col(fill="steelblue")+
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Share of missing values",
    y = "Variable",
    title = "Share of Missing Values per Variable")+
  theme_minimal()

#Let's remove all variables with a share of NAs greater than or equal to 0.20. 
#The exact threshold is not crucial here, since variables tend to have either 
#a very high or a very low proportion of missing values.
var.remove<-filter(share.NA.tib,share>=0.20)$variable
house<-select(house,-var.remove)

#Variables selection####
#Variables to be included in the model:

#1) price
#The dependent variable of the model. It represents the sale price of each property, 
#expressed in pounds.

#2) postcode
#Among the available location variables in the dataset, POSTCODE offers the highest
#spatial granularity. For this reason, we select it for now. 
#Further analysis will be conducted later to determine the most suitable 
#location variable for modelling purposes.
#The location variable will allow us to answer question 2.

#3) dateoftransfer
#Date when the sale was completed. It reflects the time dimension of the housing
#market.
#It influences property prices through inflation, interest rates, and market 
#cycles.

#4) year
#Year when the sale was completed. Useful for capturing temporal trends 
#and market dynamics.

#5) propertytype
#Indicates the structural form of the house, which heavily influences
#the sale price of the house.
#Categorical variable with 4 levels:
#D = Detached
#S = Semi-Detached
#T = Terraced  #equivalente a trifamiliare 
#F = Flat/Maisonette

#6) oldnew
#Binary variable indicating whether the property is newly built or already 
#established. We expect this factor to influence the sale price of the house.
#Y = Newly built property
#N = Existing residential building

#7) duration
#Binary variable indicating the tenure type. This might influence the sale price
#of the house.
#Freehold = Owner owns the land and property.
#Leasehold = Ownership for a fixed term; land remains owned by a freeholder.


#8) tfarea
#Total floor area, representing the total usable internal space of the 
#dwelling in square meters (m²).
#It is the main variable of interest for answering question 1.

#9)numberrooms
#Number of habitable rooms, excluding bathrooms, hallways, and other non-living
#areas (see data dictionary for details).
#Represents the property’s capacity and overall living space.
#This might or not affect significantly the sale price of the house, as it could
#simply act as a proxy of tfarea.

#10) CURRENT_ENERGY_RATING 
#Categorical variable (A–G) indicating current EPC rating.
#A = most energy efficient, G = least energy efficient.

#11) POTENTIAL_ENERGY_RATING 
#Estimated potential EPC rating (A–G) achievable through recommended improvements.

#12) CURRENT_ENERGY_EFFICIENCY 
#Numeric variable (integer). Energy cost index based on the cost of space heating, 
#water heating, and lighting in £/m²/year (derived from kWh usage and fuel costs).

#13) POTENTIAL_ENERGY_EFFICIENCY 
#Numeric variable (integer). The potential energy efficiency index if recommended 
#measures were implemented.

#Variables from 10 to 13 are the variables of interest to answer question 2
#Among the various energy-related variables available, these four were selected
#as they provide comprehensive summaries of a property’s energy performance.
#And because these are the measures directly published on housing advertisements,
#which are typically used by buyers when making purchasing decisions.

#14) FLOOR_LEVEL  
#Floor level relative to the lowest level of the property (0 for ground floor).
#If there is a basement, the basement is level 0 and the other floors are from 
#1 upwards. The significance of this variable has to be inspected.



#Let's focus now on those 14 variables only:
house<-house %>%
  select(
    price, postcode, dateoftransfer, propertytype, oldnew, duration,
    year, tfarea, numberrooms,
    CURRENT_ENERGY_RATING, POTENTIAL_ENERGY_RATING,
    CURRENT_ENERGY_EFFICIENCY, POTENTIAL_ENERGY_EFFICIENCY,
    FLOOR_LEVEL
  )


#Variables inspection####

#FLOOR_LEVEL

table(house$FLOOR_LEVEL)  
#the great majority of the observations are either NO DATA! or NODATA! so
#I decide to remove this variable:
house<-house %>%
  select(-FLOOR_LEVEL)

#postcode

#Postcode variable has a granularity level that is too high:
nlevels(as.factor(house$postcode))
#2382 levels are far too many.
#That is why we will consider only the outward code,and since in my dataset
#the outward code goes from TS1 to TS9, the outward code can be extracted as:
house$outward<-substr(house$postcode,1,3)
table(house$outward) 
#It's important to note that some of the levels have few representations
#in the dataset.
#Interestingly there are no observations from TS2 in our dataset.

#Remove now the variable postcode from the tibble:
house<-house%>%select(-postcode)

#Let's assure now that all the variables in the dataset are stored
#properly ,that is categorical variables are stored as factors.
#The variables year and numberrooms will be trated as a categorical variables,date
#of transfer will be stored as a date.
#But let's save the numerical versions of year and numberrooms before so 
#that they can be eventually used afterwards.
year.num<-as.numeric(house$year)
numberrooms.num<-as.numeric(house$numberrooms)


house <- house %>%
  mutate(
    dateoftransfer = as.Date(dateoftransfer),
    propertytype = as.factor(propertytype),
    oldnew = as.factor(oldnew),
    duration = as.factor(duration),
    year=as.factor(year),
    numberrooms=as.factor(numberrooms),
    CURRENT_ENERGY_RATING = as.factor(CURRENT_ENERGY_RATING),
    POTENTIAL_ENERGY_RATING = as.factor(POTENTIAL_ENERGY_RATING),
    outward=as.factor(outward) )


#Let's assure that no NA values are present in the house tibble:
sum(is.na(house)) 


#Let's check frequency counts for all categorical variables
#to identify levels with insufficient number of observations for 
#reliable estimation.

#propertytype

table(house$propertytype) 
#Here everything is fine.
#However I think it's clearer if we use Detached,Semi-detached,
#TerraceF and lat in place of D,S,T and F:
house<-mutate(house,
              propertytype=as.character(propertytype))
house$propertytype[house$propertytype == "D"] <- "Detached"
house$propertytype[house$propertytype == "S"] <- "Semi-Detached"
house$propertytype[house$propertytype == "T"] <- "Terraced"
house$propertytype[house$propertytype == "F"] <- "Flat"
#I make sure that house$propertytype is still stored as a factor:
house<-mutate(house,
              propertytype=as.factor(propertytype))

#oldnew

table(house$oldnew)  
#There are only 12 newly built properties in the dataset, so I decide to
#remove oldnew variable from the tibble:
house<-select(house,-oldnew)

#duration

table(house$duration) 
#Here everything is fine.
#It's clearer to store the full names of tenure types rather than abbreviations,
#so I replace F with Freehold and L with Leasehold:
house$duration<-as.character(house$duration)
house$duration[house$duration == "F"] <- "Freehold"
house$duration[house$duration == "L"] <- "Leasehold"
#I make sure that duration is stored as a factor in the tibble:
house<-mutate(house,duration=as.factor(duration))

#year

table(house$year)
#Here everything is fine.
house$year<-as.character(house$year)
#I will store all the ordinal categorical variables as an ordered factor,this
#will be necessary to compute Spearman correlation.
house <-mutate(house,year=factor(year,
                                 levels = c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                                 ordered = TRUE))

#numberrooms

table(house$numberrooms)
#Some categories have a number of observations that is too low
#So I merge some of the categories of numberrooms.
#I combine 1 and 2 into 1-2, and 10, 11, 12 into 10+ :
house$numberrooms <- as.integer(house$numberrooms)
house$numberrooms <- as.character(house$numberrooms)
house$numberrooms[house$numberrooms %in% c("1", "2")] <- "1-2"
house$numberrooms[house$numberrooms %in% c("10", "11", "12")] <- "10+"
house <- mutate(house,
                numberrooms = factor(
                  numberrooms,
                  levels = c("1-2", "3", "4", "5", "6", "7", "8", "9", "10+"),
                  ordered=TRUE))

#CURRENT_ENERGY_RATING

table(house$CURRENT_ENERGY_RATING)
#Combine level A and B since level A contains only 2 observations:
house$CURRENT_ENERGY_RATING <- as.character(house$CURRENT_ENERGY_RATING)
house$CURRENT_ENERGY_RATING[house$CURRENT_ENERGY_RATING %in% c("A", "B")] <- "A-B"
house <- house %>%
  mutate(CURRENT_ENERGY_RATING = factor(CURRENT_ENERGY_RATING,
                                        levels = c("G", "F", "E", "D", "C", "A-B"),
                                        ordered=TRUE))

#POTENTIAL_ENERGY_RATING

table(house$POTENTIAL_ENERGY_RATING)
#Same issue here. 
# Combine A and B into A-B.
# Combine E, F, and G into E-G.
house$POTENTIAL_ENERGY_RATING <- as.character(house$POTENTIAL_ENERGY_RATING)
house$POTENTIAL_ENERGY_RATING[house$POTENTIAL_ENERGY_RATING %in% c("A", "B")] <- "A-B"
house$POTENTIAL_ENERGY_RATING[house$POTENTIAL_ENERGY_RATING %in% c("E", "F", "G")] <- "E-G"
house <- house %>%
  mutate(POTENTIAL_ENERGY_RATING = factor(POTENTIAL_ENERGY_RATING,
                                          levels = c("E-G", "D", "C", "A-B"),
                                          ordered=TRUE))
#outward

table(house$outward)
#TS6 and TS9 have very few observations 13 and 3 respectively,however
#merging them to other categories requires care.
#Maybe one of the other location variables does a better job

#We can inspect this with the following code:
#I reload the original dataset in house.location tibble
url <- "https://raw.githubusercontent.com/DavideRossi123/Assignment1/main/MIDDLESBROUGH.csv"
house.location<-read_csv(url)
#I focus on the location variables only, I store their names in the
#location_vars object:
location_vars <- c("oa11","locality", "towncity", "district", "county",
                   "lsoa11", "msoa11", "laua", "lad11nm", "gor", "rgn11nm",
                   "LOCAL_AUTHORITY", "CONSTITUENCY", "COUNTY",
                   "LOCAL_AUTHORITY_LABEL", "CONSTITUENCY_LABEL")
#I print a summary table for each location variable:
for (var in location_vars) {
  cat("\n")
  cat("\n")
  cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~","\n")
  cat("Variable:", var)
  print(table(house[[var]]))
}

#All the other location variables share the same issues of outward
#and they are even worse, so the outward code is still the best choice to be our
#location variable.

table(house$outward)
#TS6 (South Bank, Grangetown, Normanby, Eston) has only 13 observations.
#TS9 (Stokesley, Great Ayton) has only 3 observations.
#Both are too small to stand alone.
#TS6 is merged with TS7 (neighboring suburban areas) in a new level. 
#thet I will call TS6-TS7.
#TS9 is merged with TS8 (both are southern fringe areas), I will 
#recall this new level TS8-TS9.
#This preserves geographic consistency and ensures adequate group size.
house$outward <- as.character(house$outward)
house$outward[house$outward %in% c("TS6", "TS7")] <- "TS6-TS7"
house$outward[house$outward %in% c("TS8", "TS9")] <- "TS8-TS9"
house <-mutate(house,outward = factor(outward)) 

#We are now ready to move to the exploratory analysis.




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##############################EXPLORATORY ANALYSIS##############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


##########################Response Distribution#################################

#Let's inspect now the distribution of the sale prices:
ggplot(data = house, aes(x = price)) +
  geom_histogram(fill = "steelblue", bins = 50,alpha=0.8) +
  theme_minimal() +
  labs(title = "Histogram of Sale Prices",
       x = "Sale Price (£)",
       y = "Count") +
  scale_x_continuous(labels = comma) #to avoid scientific notation on the x axis.
#The distribution of sale prices appears to be rightly skewed 
#and apparently binomial.


#Let's plot a density plot to investigate this deeper:
ggplot(data = house, aes(x = price)) +
  geom_density(fill = "steelblue",col=NA,alpha=0.8) +
  theme_minimal() +
  labs(title = "Density plot of Sale Prices",
       x = "Sale Price (£)",
       y = "Density") +
  scale_x_continuous(labels = comma)





ggplot(data = house, aes(x = price)) +
  geom_density(fill = "steelblue", col = NA, alpha = 0.8, size = 1.2) + 
  labs(
    title = "Density plot of Sale Prices",
    x = "Sale Price (£)",
    y = "Density") +
  scale_x_continuous(labels = comma)







#Let's inspect now the distribution of prices controlling for propertytype:
ggplot(data = house, aes(x = price,fill=propertytype)) +
  geom_density(col=NA,alpha=0.8,linewidth=1) +
  theme_minimal() +
  labs(title = "Density plot of Sale Prices by Property Type",
       x = "Sale Price (£)",
       y = "Density") +
  scale_x_continuous(labels = comma) +
  facet_wrap(~propertytype) +
  scale_fill_viridis_d(option = "cividis", end = 1)


#Once we control for property type, the distributions are no longer bimodal.
#This indicates that a Gamma distribution may be a suitable assumption.
#Indeed bimodality itself does not violate GLM assumptions.
#Theory bit:
#What matters is that, conditional on the predictors, the response
#variable Y follows a distribution from the exponential dispersion family (EDF).
#In this case:
#Yi|propertytype~Gamma(μi,φ),which satisfies the GLM framework.

#Let's plot all the densities obtained controlling for propertytype
#togheter on the same graph:
ggplot(house, aes(x = price, fill = propertytype))+
  geom_density(alpha = 0.5,col=NA) +
  theme_minimal() +
  labs(title = "Distributions of Sale Prices by Property Type",
       x = "Sale Price (£)",
       y = "Count") +
  scale_x_continuous(labels = comma) +
  scale_fill_viridis_d(option = "cividis", end = 1)

#boxplot by propertytype:
ggplot(house, aes(x = propertytype, y = price, fill = propertytype)) +
  geom_boxplot(outlier.alpha = 0.4) +
  theme_minimal() +
  labs(title = "Sale Price Distribution by Property Type",
       x = "Property Type",
       y = "Sale Price (£)") +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "none") +
  scale_fill_viridis_d(option = "cividis", end = 1)
#A lot of outliers with extreme high values are a confirmation
#of the right skewness of the data.

#COMMENT ABOUT BIMODALITY
#The bimodality observed in the overall price distribution likely reflects
#the presence of distinct submarkets.
#For instance flats and detached houses occupy different price ranges due
#to systematic differences in size, location etc... 
#When these groups are aggregated,their individual unimodal distributions 
#overlap and produce a bimodal pattern.


#########################Log of price sales#####################################

#Let's try to plot the histogram of the log of the sale prices instead:
ggplot(data = house, aes(x =log(price))) +
  geom_histogram(fill = "steelblue",bins=50,alpha=0.8) +
  theme_minimal() +
  labs(title = "Histogram of the log of Sale Prices",
       x = "log(Sale Price)",
       y = "Count") +
  scale_x_continuous(labels = comma)
#There still seems to be bimodality.


#Let's plot a density plot again to investigate this deeper:
ggplot(data = house, aes(x =log(price))) +
  geom_density(fill = "steelblue",col=NA,alpha=0.8) +
  theme_minimal() +
  labs(title = "Density-plot of the log of Sale Prices",
       x = "log(Sale Price)",
       y = "Density") +
  scale_x_continuous(labels = comma)
#Again here bimodality is even more apparent.


#Let's inspect now the distribution of the log of the prices controlling for 
#the property type:
ggplot(house, aes(x = log(price),fill =propertytype))+
  geom_density(alpha = 0.8,col=NA) +
  theme_minimal() +
  labs(title = "Distribution of log of the Sale Prices by Property Type",
       x = "log(Sale Price)",
       y = "Density") +
  scale_x_continuous(labels = comma) +
  facet_wrap(~propertytype) +
  scale_fill_viridis_d(option = "cividis", end = 1)




###############Univariate plots of the regressors of interest###################
################## 1) size of the house ########################
#tfarea is the regressor of interest here
#Histogram:
ggplot(house, aes(x = tfarea)) +
  geom_histogram(bins = 50, fill = "steelblue", color = NA,alpha=0.8) +
  theme_minimal() +
  labs(title = "Histogram of Total Floor Area (tfarea)",
       x = "Total floor area (m²)",
       y = "Count")

#Density plot:
ggplot(house, aes(x = tfarea)) +
  geom_density(fill = "steelblue", alpha = 0.8, color = NA) +
  theme_minimal() +
  labs(title = "Density of Total Floor Area (tfarea)",
       x = "Total floor area (m²)",
       y = "Density")



######################## 2) energy efficient homes##############################
#The regressor of interest here are:
#CURRENT_ENERGY_RATING 
#POTENTIAL_ENERGY_RATING
#CURRENT_ENERGY_EFFICIENCY
#POTENTIAL_ENERGY_EFFICIENCY


#Plot 1: barplot of CURRENT_ENERGY_RATING:
p1 <- ggplot(house, aes(x = CURRENT_ENERGY_RATING)) +
  geom_bar(fill = "steelblue", color=NA) +
  theme_minimal() +
  labs(title = "Current Energy Rating", x = "", y = "Count")

#Plot 2: barplot of POTENTIAL_ENERGY_RATING:
p2 <- ggplot(house, aes(x = POTENTIAL_ENERGY_RATING)) +
  geom_bar(fill = "steelblue", color=NA) +
  theme_minimal() +
  labs(title = "Potential Energy Rating", x = "", y = "Count")

#Plot 3: histogram of CURRENT_ENERGY_EFFICIENCY:
p3 <- ggplot(house, aes(x = CURRENT_ENERGY_EFFICIENCY)) +
  geom_histogram(fill = "darkorange", color=NA, bins = 30) +
  theme_minimal() +
  labs(title = "Current Energy Efficiency", x = "Efficiency Score", y = "Count")

#Plot 4: histogram of POTENTIAL_ENERGY_EFFICIENCY:
p4 <- ggplot(house, aes(x = POTENTIAL_ENERGY_EFFICIENCY)) +
  geom_histogram(fill = "darkorange", color =NA, bins = 30) +
  theme_minimal() +
  labs(title = "Potential Energy Efficiency", x = "Efficiency Score", y = "Count")

#Combine all four plots into a 2x2 grid:
grid.arrange(p1, p2, p3, p4, ncol = 2)

#CURRENT_ENERGY_RATING and CURRENT_ENERGY_EFFICIENCY show similar distibutions
#The same can be said for POTENTIAL_ENERGY_RATING and POTENTIAL_ENERGY_EFFICIENCY



#Let's inspect the correlation between those 4 variables:
energy_vars <- house[, c("CURRENT_ENERGY_RATING",
                         "POTENTIAL_ENERGY_RATING",
                         "CURRENT_ENERGY_EFFICIENCY",
                         "POTENTIAL_ENERGY_EFFICIENCY")]
#Convert ordered factors to numeric ranks, this is necessary to compute 
#Spearman correlation.
#Spearman correlation was chosen because it allows computing correlations
#with ordinal variables as well.
energy_numeric <- data.frame(
  CURRENT_ENERGY_RATING = as.numeric(energy_vars$CURRENT_ENERGY_RATING),
  POTENTIAL_ENERGY_RATING = as.numeric(energy_vars$POTENTIAL_ENERGY_RATING),
  CURRENT_ENERGY_EFFICIENCY = energy_vars$CURRENT_ENERGY_EFFICIENCY,
  POTENTIAL_ENERGY_EFFICIENCY = energy_vars$POTENTIAL_ENERGY_EFFICIENCY
)
#Let's compute the Spearman correlation matrix:
cor_matrix <- cor(energy_numeric, method = "spearman")
#Let's print it:
print(round(cor_matrix, 3))
#Let's plot it:
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 0,
         number.cex = 0.8,
         tl.cex = 0.4,
         cl.pos = "r",  
         cl.align.text = "l", 
         col = colorRampPalette(c("tomato3", "white", "steelblue"))(200))

#From the correlation matrix it's possible to observe that the correlation
#between CURRENTY_ENERGY_RATING and CURRENTY_ENERGY_EFFICIENCY, and the 
#correlation between POTENTIAL_ENERGY_RATING and POTENTIAL_ENERGY_EFFICIENCY
#are extremly high.
#Indeed, even if not explicitly stated in the data dictionary, the ratings 
#are computed directly from the numeric efficiency number. Official references 
#can be found at the following link:
#https://www.ons.gov.uk/peoplepopulationandcommunity/housing/articles/energyefficiencyofhousinginenglandandwales/2024


#Among the two set of variables (CURRENTY_ENERGY_RATING,POTENTIAL_ENERGY_RATING)
#and (CURRENTY_ENERGY_EFFICIENCY,POTENTIAL_ENERGY_EFFICIENCY), the latter 
#set will preferred in the modelling section since they are numeric and 
#more informative.
#The correlation and the "real world" relationship between the variables 
#already tell us that only one set of variables should be included in our models.

#Note instead that CURRENTY_ENERGY_RATING and POTENTIAL_ENERGY_RATING
#are not highly correlated.
#Analogously the correlation between CURRENTY_ENERGY_EFFICIENCY and
#POTENTIAL_ENERGY_EFFICIENCY is low.






##############3)areas of the country############################
#Here the regressor of interest is outward.
ggplot(data=house,aes(x=outward))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  labs(title="Histogram of Outward code")
#There are now enough observations in each category to ensure stable estimation.



##############Bivariate relationships#############
#####tfarea####
#price vs tfarea:
ggplot(house, aes(x = tfarea, y = price))+
  geom_point(size=0.3) +
  geom_smooth(method = "gam", se = FALSE) +
  theme_minimal() + 
  labs(title = "Price vs floor area",
       x="tfarea",
       y="price")
#Clear polynomial relationship between price and
#total factor area,this will be taken into account in the modelling section.

#Correlation between price and tfarea:
cor(house$price,house$tfarea,method="spearman")

#Let's inspect the interaction between tfarea and property_type:
ggplot(house, aes(x = tfarea, y = price, color = propertytype)) +
  geom_point(size=0.3) +
  geom_smooth(method = "gam",se=FALSE) +
  theme_minimal(base_size = 13) +
  labs(title = "Price vs floor area by Efficiency by property type",
       x = "tfarea",
       y = "price")+
  scale_color_viridis_d(option = "plasma",begin=0.05,end=0.9)

#alternative graph:
ggplot(house,aes(x=tfarea,y=price))+
  geom_point(size=0.3)+
  geom_smooth(method = "gam",se=FALSE)+
  theme_minimal()+
  labs(title = "Price vs floor area by property type",
       x = "tfarea",
       y = "Price") +
  facet_wrap(~propertytype)
#There is evidence of an interaction between tfarea and propertytype 
#in explaining price,that is the relationship between price and tfarea seems to 
#vary across different property types.
#This should be taken into account in the modelling section.

#interaction between tfarea and numberrooms:
ggplot(house,aes(x=tfarea,y=price))+
  geom_point(size=0.3)+
  geom_smooth(method = "gam",se=FALSE)+
  theme_minimal()+
  labs(title = "Price vs floor area by number of rooms ",
       x = "tfarea",
       y = "Price") +
  facet_wrap(~numberrooms)
#Slight evidence, this might be taken into consideration in the modelling section.

#interaction between tfarea and outward
ggplot(house,aes(x=tfarea,y=price))+
  geom_point(size=0.3)+
  geom_smooth(method = "gam",se=FALSE)+
  theme_minimal()+
  labs(title = "Price vs floor area by Outward code",
       x = "tfarea",
       y = "Price") +
  facet_wrap(~outward)
#No evidence of interaction, some of the smooth estimates
#bend in the end only for the presence of few outliers,but the core 
#relationship doesn't seem to vary across different Outward code areas.

#interaction between tfarea and CURRENT_ENERGY_RATING:
ggplot(house,aes(x=tfarea,y=price))+
  geom_point(size=0.3)+
  geom_smooth(method = "gam",se=FALSE)+
  theme_minimal()+
  labs(title = "Price vs floor area by Current Energy Rating",
       x = "tfarea",
       y = "Price") +
  facet_wrap(~CURRENT_ENERGY_RATING)
#Same story of the case of the interaction between tfarea and outward.

#interaction between tfarea and POTENTIAL_ENERGY_RATING:
ggplot(house,aes(x=tfarea,y=price))+
  geom_point(size=0.3)+
  geom_smooth(method = "gam",se=FALSE)+
  theme_minimal()+
  labs(title = "Price vs floor area by Potential Energy Rating",
       x = "tfarea",
       y = "Price") +
  facet_wrap(~POTENTIAL_ENERGY_RATING)
#Same story of the case of the interaction between tfarea and outward.



#CURRENT_ENERGY_EFFICIENCY & POTENTIAL_ENERGY_EFFICIENCY####
#Price vs CURRENT_ENERGY_EFFICIENCY: 
ggplot(house, aes(x = CURRENT_ENERGY_EFFICIENCY, y = price)) +
  geom_point(size=0.3, color = "steelblue") +
  geom_smooth(method = "gam", se = FALSE, color = "darkorange") +
  theme_minimal() +
  labs(title = "Price vs Current Energy Efficiency",
       x = "Current Energy Efficiency",
       y = "Price")
#Some evidence of polynomial relationship between price and current energy efficiency.

#price vs POTENTIAL_ENERGY_EFFICIENCY:
ggplot(house, aes(x = POTENTIAL_ENERGY_EFFICIENCY, y = price)) +
  geom_point(size=0.3, color = "steelblue") +
  geom_smooth(method = "gam", se = FALSE, color = "darkorange") +
  theme_minimal() +
  labs(title = "Price vs Potential Energy Efficiency",
       x = "Potential Energy Efficiency",
       y = "Price")
#Some evidence of a polynomial relationship between price and potential energy efficiency.


#Correlation between price and CURRENT_ENERGY_EFFICIENCY
#I use spearman correlation to ensure comparability with other correlations:
cor(house$price, house$CURRENT_ENERGY_EFFICIENCY,method="spearman")
#Correlation between price and POTENTIAL_ENERGY_EFFICIENCY
#I use spearman correlation to ensure comparability with other correlations:
cor(house$price, house$POTENTIAL_ENERGY_EFFICIENCY,method="spearman")

#This low correlations are likely due to the fact that smaller houses like flats
#which are less expensive tend to be more energy efficient. 
#After controlling for those factor we expect to observe a stronger relatonship.
#In other words tfarea and propertytype are confounding factors that we are not 
#taking into account,but are necessary to understand which is the
#"true" relationship between tfarea and energy efficiency.


#Price vs CURRENT_ENERGY_EFFICIENCY by propertytype:
ggplot(house, aes(x =CURRENT_ENERGY_EFFICIENCY, y = price, color = propertytype)) +
  geom_point(size=0.3) +
  geom_smooth(method = "gam", se = FALSE) +
  theme_minimal(base_size = 13) +
  labs(title = "Price vs Current Energy Efficiency by Property Type",
       x = "Current Energy Efficiency",
       y = "Price")+
  scale_color_viridis_d(option = "plasma",begin=0.05,end=0.9)


# Price vs POTENTIAL_ENERGY_EFFICIENCY by propertytype:
ggplot(house, aes(x = POTENTIAL_ENERGY_EFFICIENCY, y = price, color = propertytype)) +
  geom_point(size=0.3) +
  geom_smooth(method = "gam", se = FALSE) +
  theme_minimal(base_size = 13) +
  labs(title = "Price vs Potential Energy Efficiency by Property Type",
       x = "Potential Energy Efficiency",
       y = "Price")+
  scale_color_viridis_d(option = "plasma",begin=0.05,end=0.9)



#2 alternative graphs:
#Interaction between CURRENT_ENERGY_EFFICIENCY and propertytype:
ggplot(house,aes(x=CURRENT_ENERGY_EFFICIENCY,y=price))+
  geom_point(size=0.3)+
  geom_smooth(method = "gam", se = FALSE)+
  theme_minimal()+
  labs(title = "Price vs Current Energy Efficiency by property type",
       x = "CURRENT_ENERGY_EFFICIENCY",
       y = "Price") +
  facet_wrap(~propertytype)
#No evidence of an interaction between CURRENT_ENERGY_EFFICIENCY
#and propertytype in explaining price.

#Interaction between POTENTIAL_ENERGY_EFFICIENCY and propertytype:
ggplot(house,aes(x=POTENTIAL_ENERGY_EFFICIENCY,y=price))+
  geom_point(size=0.3)+
  geom_smooth(method = "gam", se = FALSE)+
  theme_minimal()+
  labs(title = "Price vs Potential Energy Efficiency by property type",
       x = "POTENTIAL_ENERGY_EFFICIENCY",
       y = "Price") +
  facet_wrap(~propertytype)
#Here there is more evidence of an interaction between 
#POTENTIAL_ENERGY_EFFICIENCY and propertytype in explaining price, in
#particular the relationship seems to be different for detached houses.
#This interaction will be tested in the modelling section.

#Interaction between CURRENT_ENERGY_EFFICIENCY and outward
ggplot(house,aes(x=CURRENT_ENERGY_EFFICIENCY,y=price))+
  geom_point(size=0.3)+
  geom_smooth(method = "gam", se = FALSE)+
  theme_minimal()+
  labs(title = "Price vs Current Energy Efficiency by Outward code",
       x = "CURRENT_ENERGY_EFFICIENCY",
       y = "Price") +
  facet_wrap(~outward)
#No evidence of interaction.

#Interaction between POTENTIAL_ENERGY_EFFICIENCY and outward
ggplot(house,aes(x=POTENTIAL_ENERGY_EFFICIENCY,y=price))+
  geom_point(size=0.3)+
  geom_smooth(method = "gam", se = FALSE)+
  theme_minimal()+
  labs(title = "Price vs Potential Energy Efficiency by Outward code",
       x = "POTENTIAL_ENERGY_EFFICIENCY",
       y = "Price") +
  facet_wrap(~outward)
#Some evidence of interaction.This should be tested at modelling level.
#The effect seems to be particularly different for level TS6-TS7
#and level TS8-TS9

#Interaction between CURRENT_ENERGY_EFFICIENCY and numberrooms
ggplot(house,aes(x=CURRENT_ENERGY_EFFICIENCY,y=price))+
  geom_point(size=0.3)+
  geom_smooth(method = "gam", se = FALSE)+
  theme_minimal()+
  labs(title = "Price vs Current Energy Efficiency by number of rooms",
       x = "CURRENT_ENERGY_EFFICIENCY",
       y = "Price") +
  facet_wrap(~numberrooms)
#No evidence of interaction.

#Iteraction between POTENTIAL_ENERGY_EFFICIENCY and numberrooms
ggplot(house,aes(x=POTENTIAL_ENERGY_EFFICIENCY,y=price))+
  geom_point(size=0.3)+
  geom_smooth(method = "gam", se = FALSE)+
  theme_minimal()+
  labs(title = "Price vs Potential Energy Efficiency by number of rooms",
       x = "POTENTIAL_ENERGY_EFFICIENCY",
       y = "Price") +
  facet_wrap(~numberrooms)
#No evidence of interaction.



#Interaction between CURRENT_ENERGY_EFFICIENCY and year
ggplot(house,aes(x=CURRENT_ENERGY_EFFICIENCY,y=price))+
  geom_point(size=0.3)+
  geom_smooth(method = "gam", se = FALSE)+
  theme_minimal()+
  labs(title = "Price vs Current Energy Efficiency by year",
       x = "CURRENT_ENERGY_EFFICIENCY",
       y = "Price") +
  facet_wrap(~year)
#No evidence of interaction.

#Interaction between POTENTIAL_ENERGY_EFFICIENCY and year
ggplot(house,aes(x=POTENTIAL_ENERGY_EFFICIENCY,y=price))+
  geom_point(size=0.3)+
  geom_smooth(method = "gam", se = FALSE)+
  theme_minimal()+
  labs(title = "Price vs Potential Energy Efficiency by year",
       x = "POTENTIAL_ENERGY_EFFICIENCY",
       y = "Price") +
  facet_wrap(~year)
#No evidence of interaction.




#####outward#####

#Boxplot: price vs outward
ggplot(house, aes(x =outward, y = price)) +
  geom_boxplot(fill = "steelblue") +
  theme_minimal(base_size = 13) +
  labs(title = "Price distribution by Outward code",
       x = "outward",
       y = "Price") 
#No Spearman correlation since outward is not an ordinal variable.
#Other possible interactions has already been inspected.

#year####
ggplot(house, aes(x =year, y = price)) +
  geom_boxplot(fill = "steelblue") +
  theme_minimal(base_size = 13) +
  labs(title = "Price distribution by year",
       x = "year",
       y = "Price")
#There seems to be a linear trend between price and year
#year might be included as a numeric variable in the 
#modelling section.

#numberrooms####
ggplot(house, aes(x =numberrooms, y = price)) +
  geom_boxplot(fill = "steelblue") +
  theme_minimal(base_size = 13) +
  labs(title = "Price distribution by number of rooms",
       x = "numberrooms",
       y = "Price")
#There seems to be a linear trend between price and numberrooms
#numberrooms might be included as a numeric variable in the 
#modelling section.



#################################Correlation####################################

#Let's drop non ordinal variables(propertytype,duration,outward) and
#dateoftransfer to compute the Spearman correlation matrix:
house.spearman<-select(house,-propertytype,-duration,-outward,-dateoftransfer)

#Convert ordered factors into ranks:
house.spearman$CURRENT_ENERGY_RATING   <- as.numeric(house.spearman$CURRENT_ENERGY_RATING)
house.spearman$POTENTIAL_ENERGY_RATING <- as.numeric(house.spearman$POTENTIAL_ENERGY_RATING)
house.spearman$numberrooms             <- as.numeric(house.spearman$numberrooms)
house.spearman$year                    <- as.numeric(house.spearman$year)

#Compute Spearman correlation matrix:
cor_spearman <- cor(house.spearman, method = "spearman")
#Let's plot it:
corrplot(
  cor_spearman,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  tl.cex = 0.9,      
  addCoef.col = "black",
  number.cex = 1,
  cl.pos = "r",
  cl.ratio=0.2,
  mar = c(0,0,1,0), 
  col = colorRampPalette(c("tomato3", "white", "steelblue"))(200))  
#We can observe that the correlation between tfarea and numberrooms
#is high (0.76),suggesting that numberrooms is hjust a proxy of tfarea
#and that tfarea ans numberrooms should not be included in the same model.

#However we can focus on the numerical variables only to compute the 
#Pearson correlation as well.
#Let's create the tibble house.pearson that contains only the numerical 
#variables only:
house.pearson <- tibble(
  price                       = house$price,
  tfarea                      = house$tfarea,
  numberrooms.num            = house$numberrooms.num,
  CURRENT_ENERGY_EFFICIENCY   = house$CURRENT_ENERGY_EFFICIENCY,
  POTENTIAL_ENERGY_EFFICIENCY = house$POTENTIAL_ENERGY_EFFICIENCY,
  year.num                    = house$year.num)

#Compute Pearson correlation matrix:
cor_pearson<- cor(house.pearson, method = "pearson")
#Let's plot it:
corrplot(
  cor_pearson,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  tl.cex = 0.9,
  addCoef.col = "black",
  number.cex = 1,
  cl.pos = "r",
  cl.ratio=0.2,
  mar = c(0,0,1,0), 
  col = colorRampPalette(c("tomato3", "white", "steelblue"))(200))  






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
################################MODELLING########################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#From the exploratory analysis emerged that:
#1) Strong evidence of a polynomial relationship between price and tfarea.
#2) Tentative evidence of a polynomial relationship between price and 
#   CURRENT_EFFICIENCY_RATING.
#3) Tentative evidence of a polynomial relationship between price and 
#   POTENTIAL_EFFICIENCY_RATING.
#4) Modest evidence of an interaction between tfarea and propertytype in explaining price.
#5) Tentative evidence of interaction between tfarea and numberrooms in explaining price.
#6) Tentative evidence of interaction between POTENTIAL_ENERGY_EFFICIENCY and 
#   propertytype in explaining price.
#7) Tentative evidence of interaction between POTENTIAL_ENERGY_EFFICIENCY and 
#   outward in explaining price.
#8) Correlation between CURRENT_ENERGY_RATING and CURRENT_ENERGY_EFFICIENCY is
#   really high.
#9) Correlation between POTENTIAL_ENERGY_RATING and POTENTIAL_ENERGY_EFFICIENCY is
#   really high.
#10) numberrooms and tfarea are highly correlated and shouldn't be 
#    included in the same model.
#11) year might be included in the models as a numeric variable as well.



#Now we don't need our factors to be ordered anymore, so:
house <-mutate(house,
               year =factor(year, ordered = FALSE),
               numberrooms =factor(numberrooms, ordered = FALSE),
               CURRENT_ENERGY_RATING =factor(CURRENT_ENERGY_RATING, ordered = FALSE),
               POTENTIAL_ENERGY_RATING =factor(POTENTIAL_ENERGY_RATING, ordered = FALSE))

#dateoftransfer is particularly difficult to handle so I will create now 2 tibbles:
#one that contains the time variable dateoftransfer and one that doesn't.
house1<-select(house,-dateoftransfer) #11 columns
house2<-house #12 columns

#Let's add as well the numeric versions of the ordinal categorical variables,
#that are year and numberrooms.
#So we add year.num and numberrooms.num :
house1<-mutate(house1,year.num=year.num,numberrooms.num=numberrooms.num)
house2<-mutate(house1,year.num=year.num,numberrooms.num=numberrooms.num)



#------------------------------------------------------------------------------#
# 1. Gamma GLM with log link
#------------------------------------------------------------------------------#
#The first model that is reasonable enough to fit is the Gamma GLM , with all
#the regressors present in the house1 tibble.
#I will fit all the models with the glm function..
model<- gam(price ~ tfarea +
              duration+
              propertytype +
              year+
              numberrooms+
              CURRENT_ENERGY_EFFICIENCY +
              POTENTIAL_ENERGY_EFFICIENCY+
              CURRENT_ENERGY_RATING +
              POTENTIAL_ENERGY_RATING+
              outward,
            data = house1, family = Gamma(link = "log"))
summary(model)

#CURRENT_ENERGY_RATING and POTENTIAL_ENERGY_RATING should be removed from the model
#since they are highly correlated with CURRENT_ENERGY_EFFICIENCY and 
#POTENTIAL_ENERGY_EFFICIENCY respectively.
#The same holds for numberrooms and tfarea, so we remove numberrooms from the model
#as it can be regarded as a proxy of tfarea.

#We try to fit a model without them:
model0 <- glm(price ~ tfarea +
                duration+
                propertytype +
                year+
                CURRENT_ENERGY_EFFICIENCY +
                POTENTIAL_ENERGY_EFFICIENCY+
                outward,
              data = house1, family = Gamma(link = "log"))
summary(model0)


#duration is as highly unsignificant so it should be removed from the model ,
#so we end  up with this model:
model1 <- glm(price ~ tfarea +
                propertytype +
                year+
                CURRENT_ENERGY_EFFICIENCY +
                POTENTIAL_ENERGY_EFFICIENCY+
                outward,
              data = house1, family = Gamma(link = "log"))
summary(model1)
#For performing diagnostic I will follow the approach described in chapter 8 of
#the book:
#Dunn, P. K., & Smyth, G. K. (2018). Generalized linear models with examples in R. Springer.
#I will cite the specific pages when necessary.

#DIAGNOSTIC for model1####
#Let's compute the Deviance residuals:
phi1<-summary(model1)$dispersion
dev.res1<-residuals(model1, type="deviance")/sqrt(phi1)

#Theory says that Pearson's residual/sqrt(phi) are approximately Normal(0,1) distirbuted
#if saddle point approximation holds, that is when the dispersion parameter<=1/5
#and n is large.(See page 299)
#here:
phi1<=1/5 #so we are good to go

#A rapid check confirms this:
#I create a tibble for plotting.
#I plot the histogram of the deviance residuals:
diagnostic.tib1 <- tibble(dev.res1= dev.res1)
x.vec<-seq(from=-5,to=+5,length.out=1000)
y.vec<-dnorm(x.vec)
norm.tib<- tibble(x=x.vec,y=y.vec)
ggplot()+
  geom_histogram(data=diagnostic.tib1, 
                 aes(x = dev.res1,y=..density..),
                 bins = 100,
                 fill = "steelblue",
                 color = "white",
                 alpha=0.8) +
  geom_line(data=norm.tib,
            aes(x=x,y=y),col="black",linewidth=1,linetype=3)
#Tails appear to be slightly too light.
#Indeed the standard deviance of the deviance residuals is
#slightly less than 1:
sd(dev.res1) #0.9778571

#I standardize the residuals to account for leverage:
dev.res1<- residuals(model1, type = "deviance")
h1<-hatvalues(model1)
dev.std1<-dev.res1/(sqrt(phi1)*sqrt(1-h1)) 


####Plots to check the systematic component####
#(p 322)

#Standardized Deviance residuals vs fitted values
#As suggested in the book I use the right fitted value transformation 
#to ease the interpretation of the graph (log for a gamma RV).

diagnostic.tib1 <- tibble(log.fitted = log(fitted(model1)),
                          dev.std1 = dev.std1)

ggplot(diagnostic.tib1, aes(x = log.fitted, y = dev.std1)) +
  geom_point(size = 0.3) +
  geom_smooth(method = "gam", se = FALSE,linewidth = 0.8) +
  labs(title = "Standardized deviance residuals vs Log fitted values",
       x = "log(Fitted values)",
       y = "Standardized deviance residuals")
#A negative pattern can be spotted in the graph,suggesting a systematic misfit
#of the data for units with an high expected value.

#Standardized deviance residuals vs numerical regressors
# 1. tfarea
diagnostic.tib1<-tibble(tfarea = house1$tfarea,
                        dev.std1 = dev.std1)

ggplot(diagnostic.tib1, aes(x = tfarea, y = dev.std1)) +
  geom_point( size = 0.3) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.8) +
  labs(title = "Standardized deviance residuals vs tfarea",
       x = "tfarea",
       y = "Standardized deviance residuals")
#Again residuals are negative for very large values 
#of tfarea corroborating the evidence that a polynomial term for tfarea is
#necessary.

# 2. CURRENT_ENERGY_EFFICIENCY
diagnostic.tib1 <- tibble(CURRENT_ENERGY_EFFICIENCY = house1$CURRENT_ENERGY_EFFICIENCY,
                          dev.std1 = dev.std1)

ggplot(diagnostic.tib1, aes(x =CURRENT_ENERGY_EFFICIENCY, y = dev.std1)) +
  geom_point( size = 0.3) +
  geom_smooth(method = "gam", se = FALSE, linewidth = 0.8) +
  labs(title = "Standardized deviance residuals vs CURRENT_ENERGY_EFFICIENCY",
       x = "CURRENT_ENERGY_EFFICIENCY",
       y = "Standardized deviance residuals")
#No trends here.

# 3. POTENTIAL_ENERGY_EFFICIENCY
diagnostic.tib1 <- tibble(POTENTIAL_ENERGY_EFFICIENCY = house1$POTENTIAL_ENERGY_EFFICIENCY,
                          dev.std1 = dev.std1)

ggplot(diagnostic.tib1, aes(x =POTENTIAL_ENERGY_EFFICIENCY, y = dev.std1)) +
  geom_point( size = 0.3) +
  geom_smooth(method = "gam", se = FALSE, linewidth = 0.8) +
  labs(title = "Standardized deviance residuals vs POTENTIAL_ENERGY_EFFICIENCY",
       x = "POTENTIAL_ENERGY_EFFICIENCY",
       y = "Standardized deviance residuals")
#Slight increaing trend here.


#####Check for the link function#######
#Linearity in the plot of Linear predictor vs Working responses
#suggests a right choice of the link function.
#(p 308)
z <- resid(model1, type="working") + model1$linear.predictor
plot( z ~ model1$linear.predictor,
      xlab="Working responses, z",
      ylab="Linear predictor",
      pch = 19,
      cex = 0.3,
      col = "black")
abline(0, 1)
#The link fucntion appears to be appropriate.



#####Plots to check the random component#####
#Compute quantile residuals for model1
quantile.res1<- qresid(model1)
qq.tib1<- tibble(quantile.res1 = quantile.res1)

ggplot(qq.tib1, aes(sample = quantile.res1)) +
  geom_qq(shape = 19, size = 0.4, alpha = 0.6) +
  geom_qq_line(color = "tomato1", linewidth = 0.8) +
  labs(title = "Q-Q Plot",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")
#The graph suggests that the tails of our response variable are too heavy
#to be handled by a Gamma-GLM, particularly for the right tail.

####Outliers####
#Standardized deviance residuals
sum(dev.std1>3) 
#Several units 42 have very high standardized deviance residuals,
#however this is not necessarily problematic since these observations are not necessarily
#influential.



#####Influential observations#####
#I compute the Cook's distances:
model1.cd<-cooks.distance(model1)
plot(model1.cd,type="h",ylab="Cook's distance",las=1)

#One of the observations have a cook's distance that is much larger than the 
#other ones.
infl<-which.max(model1.cd) #unit 3990

#Unit 3990 is an house with an high price(475000)  but very low tfarea(46),
#mediocre energy efficiency and only 2 rooms.
#This unit will have for sure an high leverage, as it will be far from the 
#other data points
#Let's compute the residual for this observation:
house1[infl,"price"]-predict(model1,newdata=as.data.frame(house1[infl,]),type="response")
#The residual is extremly high,the unit will likely be influent.

#Let's fit a new model without this unit (and call it model1.infl)
#to evaluate if unit 3990 is actually influential:
model1.infl <- update(model1, subset=(-infl))
#Let's compare the coefficients of the original and updated model:
data.frame(model1=coef(model1),model2=coef(model1.infl))
#the coefficients are not highly dissimilar actually.
#More formally we can compute the DFBETAS values for unit 3990 to check if any of the 
#coefficients change significantly
abs(dfbetas(model1)[infl,])>2/sqrt(nrow(house1))
#Some of the coefficients change significantly due to the inclusion of unit 3990.
#Although unit 3990 has a high Cook's distance and some DFBETAS exceed the threshold,
#removing it from the model does not drastically change the overall coefficients.
#This suggests that while the observation exerts some influence on certain parameters,
#it is not distorting the model in a way that justifies automatic exclusion.
#Instead, the observation highlights limitations of the model: 
#some important covariates that could explain this extreme house price are missing, 
#so the model cannot fully account for unusual combinations of predictors.


#####Collinearity#####
#From the correlation matrix only the Energy rating variables where highly correlated
#with the energy efficiency variables but they have already been removed.
#Sam hold for tfarea and numberofrooms.
vif(model1) #none of the values is concerning

#After completing the diagnostic of model1, we now understand its limitations.
#We will therefore rely on a simpler model comparison measure the AIC
#to quickly compare models and identify which ones deserve a more
#thorough diagnostic analysis afterwards.



#####Inverse Gaussian#####
#model2 inverse-gaussian
model2<- glm(price ~ tfarea +
               propertytype +
               year+
               CURRENT_ENERGY_EFFICIENCY +
               POTENTIAL_ENERGY_EFFICIENCY+
               outward,
             data = house1,
             family =inverse.gaussian(link = "log"))
#Quick graph to check the appropriateness of the link: 
z <- resid(model2, type="working") + model2$linear.predictor
plot( z ~ model2$linear.predictor, las=1,
      xlab="Working responses, z",
      ylab="Linear predictor",
      pch = 19,
      cex = 0.2,
      col = "black")
abline(0, 1)
#The link seems appropriate.

#model3 identity link
model3 <- glm(price ~ tfarea +
                propertytype +
                year +
                CURRENT_ENERGY_EFFICIENCY +
                POTENTIAL_ENERGY_EFFICIENCY +
                outward,
              data = house1,
              family = inverse.gaussian(link = "identity"))
z <- resid(model3, type="working") + model3$linear.predictor
plot( z ~ model3$linear.predictor, las=1,
      xlab="Working responses, z",
      ylab="Linear predictor",
      pch = 19,
      cex = 0.2,
      col = "black")
abline(0, 1)
#The link seems less appropriate here.

AIC(model1,model2,model3)
#AIC for model1 is drastically smaller than AIC of model 2 and 3
#even though the link appears to be appropriate (at least for model2), 
#definitely we should prefer a Gamma distributio over an Inverse-Gaussian 
#distibution.

#log-Normal GLM
#let's try to fit a log-Normal GLM
model4 <- glm(log(price) ~ tfarea +
                propertytype +
                year +
                CURRENT_ENERGY_EFFICIENCY +
                POTENTIAL_ENERGY_EFFICIENCY +
                outward,
              data = house1,
              family =gaussian(link = "identity"))
summary(model4)

#AIC of model1 and model4 are not directly comparable using AIC since they
#have response variable on different scales.
#A quick comparison shows that residual plots of model4 are somewhat worse:
par(mfrow = c(2, 4))
plot(model1)
plot(model4)
par(mfrow=c(1,1))

#Furthermore,Model 1 is preferred since it is preferable to work with the
#non-transformed response, to avoid issues like retransformation bias.

#Moreover, at the exploratory analysis stage, the normality assumption 
#for the distribution of the log of Sale Prices after controlling for Property Type,
#was less satisfactory than the gamma assumption for the distribution of Sale Prices 
#after controlling for Property Type.


####Polynomial effect######
#tfarea
#form the exploratory analysis:
#1) Strong evidence of a polynomial relationship between price and tfare

#Let's try to fit a model in which tfarea is included as a ploynomial of degree 2
model5 <- glm(price ~ poly(tfarea, 2) + 
                propertytype +
                year +
                CURRENT_ENERGY_EFFICIENCY +
                POTENTIAL_ENERGY_EFFICIENCY +
                outward,
              data = house1,
              family = Gamma(link = "log"))
summary(model5)
#The term of degree 2 is significant.
#Let's perform an additional likelihood ratio test to formally compare
#model1 and model5, to assess whether the more complex model (model5) 
#provides a significantly better fit than the simpler one (model1).
anova(model1,model5,test="LRT") #the test results to be significant.

#degree 3
model6 <- glm(price ~ poly(tfarea,3) + 
                propertytype +
                year +
                CURRENT_ENERGY_EFFICIENCY +
                POTENTIAL_ENERGY_EFFICIENCY +
                outward,
              data = house1,
              family = Gamma(link = "log"))
summary(model6)
#The term of degree 3 is significant as well.
#However, the exploratory analysis suggested that the relationship is more complex 
#than a third-degree polynomial. Therefore, we continue by increasing the degree 
#of the polynomial.

#degree 4
model7 <- glm(price ~ poly(tfarea,4) + 
                propertytype +
                year +
                CURRENT_ENERGY_EFFICIENCY +
                POTENTIAL_ENERGY_EFFICIENCY +
                outward,
              data = house1,
              family = Gamma(link = "log"))
summary(model7)
#The term of degree 4 is not significant.
#However let's perform an additional quick likelihood ratio test to the test:
anova(model6,model7,test="LRT") #the test results to be highly significant.
#So I believe it's sensible to increase the degree of the polynomial again.

#degree 5
model8 <- glm(price ~ poly(tfarea,5) + 
                propertytype +
                year +
                CURRENT_ENERGY_EFFICIENCY +
                POTENTIAL_ENERGY_EFFICIENCY +
                outward,
              data = house1,
              family = Gamma(link = "log"))
summary(model8)
#The term of degree 5 is significant as well.
#Let's perform an additional quick likelihood ratio test to the test:
anova(model7,model8,test="LRT") #the test results to be significant.

#degree 6
model9 <- glm(price ~ poly(tfarea,6) + 
                propertytype +
                year +
                CURRENT_ENERGY_EFFICIENCY +
                POTENTIAL_ENERGY_EFFICIENCY +
                outward,
              data = house1,
              family = Gamma(link = "log"))
summary(model9)
#The term of degree 6 is not significant.
#Let's perform an additional quick likelihood ratio test to the test:
anova(model8,model9,test="LRT") #the test results to be unsignificant.

#Now compare the AIC of the models:
AIC(model1,model5,model6,model7,model8,model9) 
#All those models are far better than model 1 according to AIC.
#Model 8 and 9 are the best ones for now, however model 8 is preferred
#since the last likelihood ratio test was not significant.

#A quick diagnostic check confirms that model8 is significantly better than
#our "reference" model, that is model1.
par(mfrow = c(2, 4))
plot(model1)
plot(model8)
par(mfrow=c(1,1))


#From the explanatory analysis:
#2) Tentative evidence of a polynomial relationship between price and 
#   CURRENT_EFFICIENCY_RATING.
#3) Tentative evidence of a polynomial relationship between price and 
#   POTENTIAL_EFFICIENCY_RATING.

#Let's include a polynomial term for CURRENT_ENERGY_EFFICIENCY:
model10 <- glm(price ~ poly(tfarea, 5) + 
                 propertytype +
                 year +
                 poly(CURRENT_ENERGY_EFFICIENCY,3) +
                 POTENTIAL_ENERGY_EFFICIENCY +
                 outward,
               data = house1,
               family = Gamma(link = "log"))
summary(model10)
#The term of degree 2 is not significant, but the one of degree 3 is.
#Therefore the polynomial terms for the regressor CURRENT_ENERGY_EFFICIENCY
#should be retained.
#Let's perform an additional quick likelihood ratio test to the test:
anova(model8,model10,test="LRT") #the test results to be significant.

#Let's include a plolynomial term for POTENTIAL_ENERGY_EFFICIENCY:
model11 <- glm(price ~ poly(tfarea, 5) + 
                 propertytype +
                 year +
                 CURRENT_ENERGY_EFFICIENCY +
                 poly(POTENTIAL_ENERGY_EFFICIENCY,3) +
                 outward,
               data = house1,
               family = Gamma(link = "log"))
summary(model11)
#The term of degree 2 is slightly significant, while the term of 
#degree 3 is highly significant.
#Therefore the polynomial terms for the regressor POTENTIAL_ENERGY_EFFICIENCY
#should be retained.
#Let's perform an additional quick likelihood ratio test to the test:
anova(model8,model11,test="LRT") #the test results to be significant.

#I will not proceed by increasing the degree of the polynomials for both 
#CURRENT_ENERGY_EFFICIENCY and POTENTIAL_ENERGY_EFFICIENCY , since from
#the exploratory analysis, the relationships did not appear to be complex.


#Let's include a polynomial term for both CURRENT_ENERGY_EFFICIENCY
#and POTENTIAL_ENERGY_EFFICIENCY:
model12<- glm(price ~ poly(tfarea, 5)+
                propertytype +
                year+
                poly(CURRENT_ENERGY_EFFICIENCY,3) +
                poly(POTENTIAL_ENERGY_EFFICIENCY,3) +
                outward,
              data = house1,
              family = Gamma(link = "log"))
summary(model12)
#Both the degree 3 terms are significant.
AIC(model10,model11,model12)
#model12 have an AIC that is lower than the other 2 models, but not drastically.
#We can perform 2 quick likelihood ratio tests:
anova(model10,model12,test="LRT") #This results to be significant.
anova(model11,model12,test="LRT") #This results to be significant.
#The result of the likelihood ratio tests suggest that the polynomial term should
#be retained for both current and CURRENT_ENERGY_EFFICIENCY
#and POTENTIAL_ENERGY_EFFICIENCY only.

#interactions#####
#From the exploratory analysis:
#4) Modest evidence of interaction between tfarea and propertytype in explaining price.
#So let's include an interaction term between tfarea and propertytype:
model13<- glm(price ~ poly(tfarea, 5) * propertytype +
                propertytype +
                year+
                poly(CURRENT_ENERGY_EFFICIENCY,3) +
                poly(POTENTIAL_ENERGY_EFFICIENCY,3) +
                outward,
              data = house1,
              family = Gamma(link = "log"))
summary(model13)
#Only the interaction terms for terraced houses are significant.
AIC(model12,model13) 
#However AIC of model13 is much lower.
#Let's perform an additional quick likelihood ratio test to the test:
anova(model12,model13,test="LRT") #the test results to be significant
#It's important to remember that at the exploratory level stage we expected the
#interaction terms to be significant for detached houses in particular:
ggplot(house,aes(x=tfarea,y=price))+
  geom_point(size=0.3)+
  geom_smooth(method = "gam", se = FALSE)+
  theme_minimal()+
  labs(title = "Price vs tfarea by property type",
       x = "tfarea",
       y = "Price") +
  facet_wrap(~propertytype)
#However, in the current model, Detached is the reference 
#category of propertytype. Let's change the reference category to Semi-Detached 
#to verify more clearly whether the model still indicates a different 
#relationship for Detached houses.
house1$propertytype <- relevel(house1$propertytype, ref = "Semi-Detached")

model13<- glm(price ~ poly(tfarea,5) * propertytype +
                propertytype +
                year +
                poly(CURRENT_ENERGY_EFFICIENCY,3)+
                poly(POTENTIAL_ENERGY_EFFICIENCY,3) +
                outward,
              data = house1,
              family = Gamma(link = "log"))
summary(model13)
# However, none of the interaction terms related to Detached houses are significant.
# Only the interaction term for Terraced houses appears to be significant, 
# which contradicts the exploratory analysis results. 
#Let's perform a quick diagnostic check to compare model12 and model13:
par(mfrow = c(2, 4))
plot(model12)
plot(model13)
par(mfrow=c(1,1))
#The first three diagnostic plots are identical for both models. 
#However, in the residuals-leverage plot of model13, we can identify the presence
#of potentially influential observations that may not be spotted in the 
#residuals-leverage plot of model12, suggesting signs of potential overfitting.
#Therefore, following the principle of parsimony, I have decided not to include 
#the interaction term in the final model and to stick with model12.


#From the exploratory analysis
#5) Tentative evidence of interaction between tfarea and numberrooms in
#explaining price:
model14<- glm(price ~ poly(tfarea,5) * numberrooms +
                propertytype +
                year+
                poly(CURRENT_ENERGY_EFFICIENCY,3)+
                poly(POTENTIAL_ENERGY_EFFICIENCY,3) +
                numberrooms+
                outward,
              data = house1,
              family = Gamma(link = "log"))
summary(model14)
#None of the interaction terms seem to be significant.
#This interaction will not be included in the final model, and model12 will still
#be preferred.


#6) Tentative evidence of interaction between POTENTIAL_ENERGY_EFFICIENCY and 
#   propertytype in explaining price.
#It was noted during the exploratory analysis stage that the effect seemed 
#to be particularly different for detached houses:
ggplot(house,aes(x=POTENTIAL_ENERGY_EFFICIENCY,y=price))+
  geom_point(size=0.3)+
  geom_smooth(method = "gam", se = FALSE)+
  theme_minimal()+
  labs(title = "Price vs POTENTIAL_ENERGY_EFFICIENCY by property type",
       x = "POTENTIAL_ENERGY_EFFICIENCY",
       y = "Price") +
  facet_wrap(~propertytype)

#Let's try to fit a model with an interaction between POTENTIAL_ENERGY_EFFICIENCY
#and propertytype:
model15<- glm(price ~ poly(tfarea,5)+
                propertytype +
                year+
                poly(CURRENT_ENERGY_EFFICIENCY,3)+
                poly(POTENTIAL_ENERGY_EFFICIENCY,3) * propertytype+
                outward,
              data = house1,
              family = Gamma(link = "log"))
summary(model15)
AIC(model12,model15)
#The AIC of model15 is lower; however, a reasoning similar to that used for 
#the interaction between tfarea and propertytype applies here as well.
#We expected the interaction terms for Detached houses to be significant; 
#however, this is not the case, and only a few among all the interaction terms 
#show significance. 

#Let's perform a quick diagnostic check to compare model11 and model15:
par(mfrow = c(2, 4))
plot(model12)
plot(model15)
par(mfrow=c(1,1))
#The diagnostic plots of the two models are equivalently good.
#Therefore, I decide not to include the interaction term in the final model.

#7) Tentative evidence of interaction between POTENTIAL_ENERGY_EFFICIENCY and 
#   outward in explaining price.
#In particular it was observed that the effect seems to be particularly 
#different for level TS6-TS7 and level TS8-TS9, this is evident from this graph:
ggplot(house1,aes(x=POTENTIAL_ENERGY_EFFICIENCY,y=price))+
  geom_point(size=0.3)+
  geom_smooth(method = "gam", se = FALSE)+
  labs(title = "Price vs Potential Energy Efficiency by Outward code",
       x = "POTENTIAL_ENERGY_EFFICIENCY",
       y = "Price") +
  facet_wrap(~outward)

#Let's try to fit a model with an interaction between POTENTIAL_ENERGY_EFFICIENCY 
#and outward:
model16<- glm(price ~ poly(tfarea,5)+
                propertytype +
                year+
                poly(CURRENT_ENERGY_EFFICIENCY,3) +
                poly(POTENTIAL_ENERGY_EFFICIENCY,3)*outward+
                outward,
              data = house1,
              family = Gamma(link = "log"))
summary(model16)
#Here as well only one among all the interaction terms for level TS6-TS7 and level
#TS8-TS9 is slightly significant.
#This suggests that the interaction term should not be included in the model.

AIC(model12,model16)
#AIC favours model16.
#However BIC doesn't:
BIC(model12,model16)
#BIC favours model12.

#The interaction add 13 parameters to the model, so by parsimony principle
#model12 is still preferred.


#From the exploratory analysis:
#11) year might be included in the models as a numeric variable as well.
#So we fit a model with year.num instead of year:
model17<- glm(price ~ poly(tfarea,5) +
                propertytype +
                year.num+
                poly(CURRENT_ENERGY_EFFICIENCY,3) +
                poly(POTENTIAL_ENERGY_EFFICIENCY,3)+
                outward,
              data = house1,
              family = Gamma(link = "log"))
summary(model17)
#The year.num term appears to be highly significant
AIC(model12,model17)
#Furthermore AIC of model17 is substantially similar to the one of  model 12.
#By the principle of parsimony model 17 is preferred since it has 7 parameters less.

ggplot(data=house1,aes(x=year.num,y=price))+
  geom_point()+
  geom_smooth(method="gam",se=FALSE)+
  labs(title="price vs year.num")
#Furthermore the  graph suggests that there is no evidence of a potential non linear
#relationship between price and year.num, for which including year as a
#categorical regressor is meaningful.

#10) numberrooms and tfarea are highly correlated and shouldn't be 
#    included in the same model.
#However, it is reasonable to believe that the number of rooms still plays
#a significant role in determining the final price of a house.
#The high correlation between the number of rooms and the floor area makes the 
#inclusion of both variables in the model problematic, as it can lead to
#multicollinearity, where the model cannot easily distinguish the individual 
#effects of each variable.

# The correlations are:
cor(house1$tfarea, house1$numberrooms.num, method = "pearson")  # 0.7644085

# And:
numberrooms.ordered <- factor(house1$numberrooms,
                              levels = c("1-2", "3", "4", "5", "6", "7", "8", "9", "10+"),
                              ordered = TRUE)
cor(house1$tfarea, as.numeric(numberrooms.ordered), method = "spearman")  # 0.7687774

# One approach that has been suggested in the literature to address the issue of 
#multicollinearity mean room size, defined as the ratio of floor area (tfarea)
#to the number of rooms (numberrooms.num).
#For instance see:
#van Vuuren, A., Kjellander, J. and Nilsson, V., 2019. Refugees and apartment prices: A case study to investigate the attitudes of home buyers. Regional Science and Urban Economics, 77, pp.20–37. doi:10.1016/j.regsciurbeco.2019.02.003

#This approach will help reduce multicollinearity while hopefully 
#still capturing the essential relationship between the number of rooms and the overall house size.

#The rationale for this approach is that by using the mean room size, we create
#a variable that reflects the "efficiency" of the house in terms of space utilization.
#In this sense a negative coefficient for the mean room size is expected, since
#house with an higher mean room size will have a less efficient utilization of the
#spaces.
#I will test whether using the ratio of floor area to number of rooms is sensible
#here or not.
#Let's create the mean.room.size variable and add it to house1:
house1$mean.room.size<-house1$tfarea/house1$numberrooms.num
#Let's compute the corrlation between mean.room.size and tfarea:
cor(house1$tfarea,house1$mean.room.size,method="pearson") #0.483122
#Which is not concerning.


#Let's fit a model with mean.room.size as regressor:
model18<- glm(price ~ poly(tfarea,5)+
                propertytype +
                year.num+
                poly(CURRENT_ENERGY_EFFICIENCY,3) +
                poly(POTENTIAL_ENERGY_EFFICIENCY,3)+
                mean.room.size+
                outward,
              data = house1,
              family = Gamma(link = "log"))
summary(model18) #The coefficient of mean.room.size is negative as expected.
AIC(model17,model18)
#The AIC of model 17 is higher.
#We can perform a quick likelihood ratio tests:
anova(model17,model18,test="LRT") #This results to be significant.

#Let's inspect this graph:
ggplot(house1,aes(x=mean.room.size,y=price))+
  geom_point(size=0.3)+
  geom_smooth(method = "gam", se = FALSE)+
  labs(x="mean room size",
       y="price",
       title="price vs mean room size")
#There is not great evidence of a polynomial relationship here.

#However we can still try to fit a model in which mean.room.size is included
#as a degree 3 polynomial.
model19<- glm(price ~ poly(tfarea,5)+
                propertytype +
                year.num+
                poly(CURRENT_ENERGY_EFFICIENCY,3) +
                poly(POTENTIAL_ENERGY_EFFICIENCY,3)+
                poly(mean.room.size,3)+
                outward,
              data = house1,
              family = Gamma(link = "log"))
summary(model19)
#The terms of degree 2 and 3 are not significant, so model18 should be preferred.











#DIAGNOSTIC to confront model17 and model18####
#I will now perform a more in-depth diagnostic to compare model17 and model18
#and assess whether the limitations observed in model1 persist in these models.
#Let's compute the Deviance residuals for model17 and model18:
#model17
phi17<-summary(model17)$dispersion
dev.res17<-residuals(model17, type="deviance")/sqrt(phi17)
#model18
phi18<-summary(model18)$dispersion
dev.res18<-residuals(model18, type="deviance")/sqrt(phi18)

#model17
phi17<=1/5 #so we are good to go for model17
#Model18
phi18<=1/5 #so we are gof to go for model18

#A rapid check confirms this:
#model17
#I create a tibble for plotting.
#I plot the histogram of the deviance residuals:
diagnostic.tib17<- tibble(dev.res17= dev.res17)
x.vec17<-seq(from=min(dev.res17),to=max(dev.res17),length.out = 1000)
y.vec17<-dnorm(x.vec17, mean = mean(dev.res17),sd=sd(dev.res17))
norm.tib17 <- tibble(x=x.vec17,y=y.vec17)

ggplot()+
  geom_histogram(data=diagnostic.tib17, 
                 aes(x = dev.res17,y=..density..),
                 bins = 100,
                 fill = "steelblue",
                 color = "white",
                 alpha=0.8) +
  geom_line(data=norm.tib17,
            aes(x=x,y=y),col="black",linewidth=1,linetype=3) +
  labs(y="count",
       title="deviance residuals of model17")
#Tails appear to be too light.
#Indeed the standaard devince of the deviance residuals is
#slightly less than 1:
sd(dev.res17)



#model18
#I create a tibble for plotting.
#I plot the histogram of the deviance residuals:
diagnostic.tib18<- tibble(dev.res18= dev.res18)
x.vec18<-seq(from = min(dev.res18), to = max(dev.res18), length.out = 1000)
y.vec18<-dnorm(x.vec18, mean = mean(dev.res18), sd = sd(dev.res18))
norm.tib18 <- tibble(x = x.vec18, y = y.vec18)

ggplot()+
  geom_histogram(data=diagnostic.tib18, 
                 aes(x = dev.res18,y=..density..),
                 bins = 100,
                 fill = "steelblue",
                 color = "white",
                 alpha=0.8) +
  geom_line(data=norm.tib18,
            aes(x=x,y=y),col="black",linewidth=1,linetype=3)+
  labs(y="count",
       title="deviance residuals of model18")
#Tails appear to be too light.
#Indeed the standaard devince of the deviance residuals is
#slightly less than 1:
sd(dev.res18)



#I standardize the residuals to take into account the leverage:
#model17
dev.res17<- residuals(model17, type = "deviance")
h17<-hatvalues(model17)
dev.std17<-dev.res17/(sqrt(phi17)*sqrt(1-h17))
#model18
dev.res18<- residuals(model18, type = "deviance")
h18<-hatvalues(model18)
dev.std18<-dev.res18/(sqrt(phi18)*sqrt(1-h18))



####Plots to check the systematic component regressors####
#(p 322)
#Standardized Deviance residuals vs fitted values
#As suggested in the book I use the right fitted value transformation 
#to ease the interpretation of the graph (log for a gamma RV).
#model17
diagnostic.plot.tib17<-tibble(log.fitted17= log(fitted(model17)),
                              dev.std17= dev.std17)

p17.1<-ggplot(diagnostic.plot.tib17,aes(x=log.fitted17,y=dev.std17)) +
  geom_point(size=0.3) +
  geom_smooth(method="loess",se=FALSE) +
  labs(y ="Standardized deviance residuals",
       x ="log(Fitted values)",
       title="model17")
p17.1
#The negative pattern that was spotted in model1 is not present here in
#model17,and the plot is actually very satisfactory.

#model18
diagnostic.plot.tib18<-tibble(log.fitted18=log(fitted(model18)),
                              dev.std18=dev.std18)

p18.1<-ggplot(diagnostic.plot.tib18,aes(x=log.fitted18,y=dev.std18)) +
  geom_point(size=0.3) +
  geom_smooth(method="loess",se=FALSE) +
  labs(y ="Standardized deviance residuals",
       x ="log(Fitted values)",
       title="model18")
p18.1
#The negative pattern that was spotted in model1 is not present here in
#model18, and the plot is actually very satisfactory.

#model1
diagnostic.tib1 <- tibble(log.fitted = log(fitted(model1)),
                          dev.std1 = dev.std1)

p1.1<-ggplot(diagnostic.tib1, aes(x = log.fitted, y = dev.std1)) +
  geom_point(size = 0.3) +
  geom_smooth(method = "gam", se = FALSE,linewidth = 0.8) +
  labs(title = "model1",
       x = "log(Fitted values)",
       y = "Standardized deviance residuals")

#We can plot a quick comparison of the 3 graphs:
p1.1+p17.1+p18.1
#We can observe that the graphs of model17 and model 18 look equally "good".



#Standardized pearson residuals vs numerical regressors
# 1. tfarea

#model17
#I create a tibble that is necessary for plotting:
diagnostic.tib17<- tibble(tfarea = house1$tfarea,
                          dev.std17 = dev.std17)

p17.2<-ggplot(diagnostic.tib17,aes(x=tfarea,y=dev.std17)) +
  geom_point(size = 0.3) +
  geom_smooth(method = "gam",se=FALSE,linewidth = 0.8) +
  labs(title = "model17",
       x = "tfarea",
       y = "Standardized deviance residuals")
p17.2
#No negative trend in the standardized deviance residuals can be observed here,
#unlike in model1, suggesting that the functional form used for tfarea
#is now appropriate.

#model18
diagnostic.tib18<- tibble(tfarea = house1$tfarea,
                          dev.std18 = dev.std18)
p18.2<-ggplot(diagnostic.tib18,aes(x=tfarea,y=dev.std18)) +
  geom_point(size = 0.3) +
  geom_smooth(method = "gam",se=FALSE,linewidth = 0.8) +
  labs(title = "model18",
       x = "tfarea",
       y = "Standardized deviance residuals")
p18.2
#Here as well no negative trend for the standardized deviance resiudals 
#can be spotted.

#model1
diagnostic.tib1<-tibble(tfarea = house1$tfarea,
                        dev.std1 = dev.std1)

p1.2<-ggplot(diagnostic.tib1, aes(x = tfarea, y = dev.std1)) +
  geom_point( size = 0.3) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.8) +
  labs(title = "model1",
       x = "tfarea",
       y = "Standardized deviance residuals")


#We can plot a quick comparison of the 3 graphs:
p1.2+p17.2+p18.2
#We can observe that the two graphs of model17 and model18 are equivalently "good".






#2. CURRENT ENERGY EFFICIENCY
#model17
diagnostic.tib17<-tibble(CURRENT_ENERGY_EFFICIENCY=house1$CURRENT_ENERGY_EFFICIENCY,
                         dev.std17=dev.std17)

p17.3<-ggplot(diagnostic.tib17,aes(x=CURRENT_ENERGY_EFFICIENCY,y=dev.std17)) +
  geom_point(shape = 19, size=0.3,alpha=0.7) +
  geom_smooth(method="loess",se=FALSE,linewidth=0.8) +
  labs(title = "model17",
       x = "CURRENT_ENERGY_EFFICIENCY",
       y = "Standardized deviance residuals")
p17.3
#Similar to model1, there are no evident trends in model17.



#model18
diagnostic.tib18<-tibble(CURRENT_ENERGY_EFFICIENCY=house1$CURRENT_ENERGY_EFFICIENCY,
                         dev.std18=dev.std18)

p18.3<-ggplot(diagnostic.tib18,aes(x=CURRENT_ENERGY_EFFICIENCY,y=dev.std18)) +
  geom_point(shape = 19, size=0.3,alpha=0.7) +
  geom_smooth(method="loess",se=FALSE,linewidth=0.8) +
  labs(title = "model18",
       x = "CURRENT_ENERGY_EFFICIENCY",
       y = "Standardized deviance residuals")
p18.3
#Similar to model1, there are no evident trends in model18.





# 3. POTENTIAL_ENERGY_EFFICIENCY
#model17
diagnostic.tib17<-tibble(POTENTIAL_ENERGY_EFFICIENCY=house1$CURRENT_ENERGY_EFFICIENCY,
                         dev.std17=dev.std17)

p17.4<-ggplot(diagnostic.tib17,aes(x=POTENTIAL_ENERGY_EFFICIENCY,y=dev.std17)) +
  geom_point(shape = 19, size=0.3,alpha=0.7) +
  geom_smooth(method="gam",se=FALSE,linewidth=0.8) +
  labs(title = "model18",
       x = "POTENTIAL_ENERGY_EFFICIENCY",
       y = "Standardized deviance residuals")
p17.4
#As in model1 no trends can be spotted here in model17.

#model18
diagnostic.tib18<-tibble(POTENTIAL_ENERGY_EFFICIENCY=house1$CURRENT_ENERGY_EFFICIENCY,
                         dev.std18=dev.std18)

p18.4<-ggplot(diagnostic.tib18,aes(x=POTENTIAL_ENERGY_EFFICIENCY,y=dev.std18)) +
  geom_point(shape = 19, size=0.3,alpha=0.7) +
  geom_smooth(method="gam",se=FALSE,linewidth=0.8) +
  labs(title = "model18",
       x = "POTENTIAL_ENERGY_EFFICIENCY",
       y = "Standardized deviance residuals")
p18.4
#As in model1 no trends can be spotted here in model18.




#####Check for the link function#######
#Linearity in the plot of Linear predictor vs Working responses
#suggests a right choice of the link function
#(p 308)


link.check.tib17 <- tibble(linear.predictor = model17$linear.predictors,
                           working.response = resid(model17, type = "working") + model17$linear.predictors)

p17.5 <- ggplot(link.check.tib17, aes(x = working.response, y = linear.predictor)) +
  geom_point(shape = 19, size = 0.3, color = "black", alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "steelblue", linewidth = 1) +
  labs(title = "model17",
       x = "Working responses, z",
       y = "Linear predictor")
p17.5



# Create tibble for model18
link.check.tib18 <- tibble(linear.predictor = model18$linear.predictors,
                           working.response = resid(model18, type = "working") + model18$linear.predictors)

p18.5 <- ggplot(link.check.tib18, aes(x = working.response, y = linear.predictor)) +
  geom_point(shape = 19, size = 0.3) +
  geom_abline(intercept = 0, slope = 1, color = "steelblue",linewidth = 1) +
  labs(title = "model18",
       x = "Working responses, z",
       y = "Linear predictor")
p18.5

p17.5+p18.5
#We can observe that the two graphs are equivalently "good".




#####Plots to check the random component#####
#model17
# Compute quantile residuals for model17
quantile.res17 <- qresid(model17)
qq.tib17 <- tibble(quantile.res17 = quantile.res17)

q17<-ggplot(qq.tib17, aes(sample = quantile.res17)) +
  geom_qq(shape = 19, size = 0.4, alpha = 0.6) +
  geom_qq_line(color = "tomato1", linewidth = 0.8) +
  labs(title = "Q-Q Plot for Model17",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")
#The graph suggests that the tails of our response variable are too heavy,for a 
#Gamma-GLM, the right tail in particular.

#model18
# Compute quantile residuals for model17
quantile.res18 <- qresid(model18)
qq.tib18 <- tibble(quantile.res18= quantile.res18)

q18<-ggplot(qq.tib18, aes(sample = quantile.res18)) +
  geom_qq(shape = 19, size = 0.4, alpha = 0.6) +
  geom_qq_line(color = "tomato1", linewidth = 0.8) +
  labs(title = "Q-Q Plot for Model18",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")

#model1:
q1<-ggplot(qq.tib1, aes(sample = quantile.res1)) +
  geom_qq(shape = 19, size = 0.4, alpha = 0.6) +
  geom_qq_line(color = "tomato1", linewidth = 0.8) +
  labs(title = "Q-Q Plot model1",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")

#We can plot a quick comparison of the 3 graphs:
q1+q17+q18
#We can observe that the 3 graphs share the same issues, that is the 
#tails of our response distribution are too heavy to be modelled by a Gamma-GLM.
#In this case qqplot ofmodel17 and model18 is not better than the one of model 1. 



####Outliers####
#model17
#Standardized deviance residuals
sum(dev.std17>3) 
#Several units 42 have very high standardized deviance residuals,
#however this is not necessarly problematic since those obs are not necessarly
#influential.

#model18
#Standardized deviance residuals
sum(dev.std18>3) 
#Several units 47 have very high standardized deviance residuals.



#####Influential observations#####
#model17
#I compute the Cook's distances:
model17.cd <- cooks.distance(model17)
plot( model17.cd, type="h", ylab="Cook's distance", las=1)

infl<-which.max(model17.cd) #unit 729
#Let's inspect this variable
as.data.frame(house1[infl,]) #it's a unit with an extremly high price and tfarea
#Let's compute the residual for this variable:
house1[infl,]$price - predict(model17,newdata=as.data.frame(house1[infl,]),type="response")
#The residual is high


#Let's fit a new model without this unit (and call it model17.infl)
#to evaluate if unit 2072 is actually influential:
model17.infl <- update(model17, subset=(-infl))

#Let's compare the coefficients of the original and updated model:
data.frame(model17=coef(model17),model17.no.infl=coef(model17.infl))
#the coefficients are very similar suggesting that none of the
#units in the dataset is influential.

#More formally we can compute the DFBETAS values for unit 729 to check if any of the 
#coefficients change significantly
abs(dfbetas(model17)[infl,])>2/sqrt(nrow(house1))
#Some of the regression coefficients are significantly affected by the presence 
#of this unit in the model.

#However there is no sign of an error at the measurement stage so the unit should
#not be remove form the dataset.



#model18
#I compute the Cook's distances:
model18.cd <- cooks.distance(model18)
plot( model18.cd, type="h", ylab="Cook's distance", las=1)

infl<-which.max(model18.cd) #unit 3990 (same unit for model1)
#Let's inspect this variable
as.data.frame(house1[infl,]) #it's a unit with an extremly high price and
#very low tfarea
#Let's compute the residual for this variable:
house1[infl,]$price - predict(model18,newdata=as.data.frame(house1[infl,]),type="response")
#The residual is extremly high.


#Let's fit a new model without this unit (and call it model18.infl)
#to evaluate if unit 2072 is actually influential:
model18.infl <- update(model18, subset=(-infl))

#Let's compare the coefficients of the original and updated model:
data.frame(model18=coef(model18),model18.no.infl=coef(model18.infl))
#Some of the coefficients change quite drastically due to the presence of this unit
#in the dataset.

#More formally we can compute the DFBETAS values for unit 3990 to check if any of the 
#coefficients change significantly
abs(dfbetas(model18)[infl,])>2/sqrt(nrow(house1))
#All the regression coefficients are significantly affected by the presence 
#of this unit in the model.

#However there is no sign of an error at the measurement stage so the unit should
#not be remove form the dataset.




#####Collinearity#####
#Highly correlated values has been excluded from the model.
vif(model17) #none of the values is concerning
vif(model18) #none of the values is concerning




#Cross validation#####
set.seed(12321)
train_control <- trainControl(method = "cv",number=50)
cv_model17 <- train(price ~ poly(tfarea,5) + 
                      propertytype + year.num +
                      poly(CURRENT_ENERGY_EFFICIENCY,3) +
                      poly(POTENTIAL_ENERGY_EFFICIENCY,3) +
                      outward,
                    data = house1,
                    method = "glm",
                    family = Gamma(link="log"),
                    trControl = train_control)

cv_model18 <- train(price ~ poly(tfarea,5)
                    + propertytype + 
                      year.num +
                      poly(CURRENT_ENERGY_EFFICIENCY,3) +
                      poly(POTENTIAL_ENERGY_EFFICIENCY,3) +
                      mean.room.size+
                      outward,
                    data = house1,
                    method = "glm",
                    family = Gamma(link="log"),
                    trControl = train_control)
cv_model17$results 
cv_model18$results #model18 has only slightlty better predictive performances




########Limitations#####
#Moore quality data
#no time considered
#Example of unit 3990
#Average partial effect , partial average effect!!!!!!!!!!!!

#final comments 
#In the model18 was preferred because it includes the mean.room.size variable, 
#which provides a meaningful measure and helps address 
#the high correlation between tfarea and numberrooms, and so reducing 
#multicollinearity concerns. 
#The variable is theoretically justified and highly significant, confirming 
#the presence of some explanatory power in determining property prices.



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#######################ANSWER TO THE RESEARCH QUESTIONS#########################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#RESEARCH QUESTION 1####
#How do house prices vary with the size of the house?
#House size is measured by tfarea in the dataset.
#In the model, 'tfarea' is included as a 5th-degree polynomial, therefore, the 
#usual "constant multiplicative effect" interpretation of Gamma-GLMs with a log
#link does not apply.
#To make the results accessible to practitioners with little statistical background,
#I will adopt a visual approach:
#1 I will visualize the relationship by computing average predicted prices across
#the range of tfarea values while holding other variables at their observed values.
#2 Compute approximate 95% confidence intervals using a bootstrap method.

#Gelman, A., & Pardoe, I. (2007). Average predictive comparisons for models with nonlinearity, interactions, and variance components. 
#Sociological Methodology, *37*(1), 23-51. https://doi.org/10.1111/j.1467-9531.2007.00181.x


set.seed(123)
#Let's create a sequence of equally spaced values of tfarea across its range:
tf.seq<-seq(min(house1$tfarea),max(house1$tfarea),length.out = 200)
#Function to compute average predicted price across different tfarea values
compute.avg.prediction<-function(model,data,tf.seq) {
  sapply(tf.seq, function(x) {
    newdata <- data
    newdata$tfarea <- x
    mean(predict(model, newdata = newdata, type = "response"))
  })
}

#Let's use the function that has just been defined to compute the  average predicted 
#price for each of the values in tfarea and to store them in the avg.prediction vector 
avg.prediction<-compute.avg.prediction(model18,house1,tf.seq)


#Bootstrap method to compute confidence intervals for the average predicted price
#estimates.
#1 Resample the dataset with replacement B times (each sample has the same number of rows as house1)
#2 For each bootstrap sample fit the Gamma GLM and compute average predicted prices across
#  the range of tfarea values
#3 Store the average predicted prices for each replication 
#4 Compute approximate 95% confidence intervals using the quantile function
#################!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!###########################
B<-200 #this procedure with B=200 is computationally intensive (around 7 min 
#compuation time on the university computer), put B=20 for a faster result.
#The graphs computed with B=200 are in the poster.
avg.prediction_boot<-replicate(B, {
  boot_data<-house1[sample(1:nrow(house1), replace = TRUE), ]
  boot_model<-glm(price ~ poly(tfarea,5) +
                    propertytype +
                    year.num +
                    poly(CURRENT_ENERGY_EFFICIENCY,3) +
                    poly(POTENTIAL_ENERGY_EFFICIENCY,3) +
                    mean.room.size +
                    outward,
                  data = boot_data,
                  family = Gamma(link="log"))
  compute.avg.prediction(boot_model,boot_data,tf.seq)
})

avg.prediction.lower<-apply(avg.prediction_boot,MARGIN=1,FUN=quantile, probs = 0.025)
avg.prediction.upper<-apply(avg.prediction_boot,MARGIN=1,FUN=quantile, probs = 0.975)

#Let's put everything into a datafranme to plot with ggplot:
avg.prediction.df<-data.frame(tfarea=tf.seq,
                              avg.prediction=avg.prediction,
                              lower=avg.prediction.lower,
                              upper=avg.prediction.upper)

ggplot(avg.prediction.df, aes(x = tfarea, y = avg.prediction)) +
  geom_ribbon(aes(ymin =lower, ymax = upper), fill = "skyblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  labs(x = "Total floor area",
       y = "Average predicted price",
       title = "Average predicted prices vs total floor area with 95% CI") +
  theme_minimal()


#A clear monotonic increasing trend can be visually observed in the data. 
#The confidence intervals become extremely wide for large values of tfarea, 
#reflecting high uncertainty in this range. This is expected, as the dataset 
#contains only a few observations with extremely large tfarea values, for this 
#reason, the apparent decreasing trend at the upper end is highly uncertain and
#should be interpreted with caution.
nrow(house1[house1$tfarea>=400,]) #6 units only have tfarea greateror equal than 400
hist(house1$tfarea,breaks=50)

#Answer:
#We have enough evidence to state that houses with a larger total floor area 
#tend to be more expensive in Middlesbrough during the time period of 
#interest, reflecting a clear positive relationship between property size and price. 
#This is consistent with economic intuition.
#The relationship appears roughly monotonic,however, the rate of increase may 
#diminish for extremely large properties, additional data are required to inspect
#more deeply the latter question.





#RESEARCH QUESTION 2####
#2. Do people pay more for energy efficient homes?
#I will inspect the effect of CURRENT_ENERGY_EFFCIENCY and  POTENTIAL_ENERGY_EFFCIENCY
#separately, in both cases the same visual approach adopted for tfarea will be 
#replicated.

#CURRRENT_ENERGY_EFFICIENCY####
set.seed(321123)
#Let's create a sequence of equally spaced values for CURRENT_ENERGY_EFFICIENCY
cee.seq <- seq(min(house1$CURRENT_ENERGY_EFFICIENCY),
               max(house1$CURRENT_ENERGY_EFFICIENCY),
               length.out = 200)

compute.avg.prediction<-function(model,data,cee.sq) {
  sapply(cee.seq, function(x) {
    newdata <- data
    newdata$CURRENT_ENERGY_EFFICIENCY<-x
    mean(predict(model, newdata = newdata, type = "response"))
  })
}

avg.prediction <- compute.avg.prediction(model18, house1, cee.seq)


B <-200 #this procedure with B=200 is computationally intensive (around 7 min 
#compuation time on the university computer), put B<-20 for a faster result.
avg.prediction_boot<- replicate(B, {
  boot_data<-house1[sample(1:nrow(house1),replace = TRUE), ]
  boot_model<-glm(price ~ poly(tfarea,5) +
                    propertytype +
                    year.num +
                    poly(CURRENT_ENERGY_EFFICIENCY,3) +
                    poly(POTENTIAL_ENERGY_EFFICIENCY,3) +
                    mean.room.size +
                    outward,
                  data = boot_data,
                  family = Gamma(link="log"))
  compute.avg.prediction(boot_model, boot_data, cee.seq)
})

avg.prediction.lower<-apply(avg.prediction_boot,MARGIN=1,FUN=quantile, probs = 0.025)
avg.prediction.upper<-apply(avg.prediction_boot,MARGIN=1,FUN=quantile, probs = 0.975)

avg.prediction.df<- data.frame(
  CURRENT_ENERGY_EFFICIENCY = cee.seq,
  avg.prediction = avg.prediction,
  lower = avg.prediction.lower,
  upper = avg.prediction.upper
)

ggplot(avg.prediction.df,aes(x = CURRENT_ENERGY_EFFICIENCY, y = avg.prediction)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "skyblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  labs(x = "Current Energy Efficiency",
       y = "Average predicted price",
       title = "Average predicted prices vs Current Energy Efficiency with 95% CI") +
  theme_minimal()
 
#Here again, a clear monotonic trend is also visible. The confidence intervals widen 
#at the extremes, reflecting the sparsity of observations in those regions. 
#Compared to the tfarea plot, the presence of less severe outliers results 
#in narrower confidence intervals at the extremes.
hist(house1$CURRENT_ENERGY_EFFICIENCY,breaks=50)











#POTENTIAL_ENERGY_EFFICIENCY####
set.seed(321)
#Let's create a sequence of equally spaced values for POTENTIAL_ENERGY_EFFICIENCY
pee.seq <- seq(min(house1$POTENTIAL_ENERGY_EFFICIENCY),
               max(house1$POTENTIAL_ENERGY_EFFICIENCY),
               length.out = 200)

compute.avg.prediction<-function(model,data,pee.sq) {
  sapply(cee.seq, function(x) {
    newdata <- data
    newdata$POTENTIAL_ENERGY_EFFICIENCY<-x
    mean(predict(model, newdata = newdata, type = "response"))
  })
}

avg.prediction <- compute.avg.prediction(model18, house1, pee.seq)


B <-200 #this procedure with B=200 is computationally intensive (around 7 min 
#compuation time on the university computer), put B<-20 for a faster result.
avg.prediction_boot<- replicate(B, {
  boot_data<-house1[sample(1:nrow(house1),replace = TRUE), ]
  boot_model<-glm(price ~ poly(tfarea,5) +
                    propertytype +
                    year.num +
                    poly(CURRENT_ENERGY_EFFICIENCY,3) +
                    poly(POTENTIAL_ENERGY_EFFICIENCY,3) +
                    mean.room.size +
                    outward,
                  data = boot_data,
                  family = Gamma(link="log"))
  compute.avg.prediction(boot_model, boot_data, cee.seq)
})

avg.prediction.lower<-apply(avg.prediction_boot,MARGIN=1,FUN=quantile, probs = 0.025)
avg.prediction.upper<-apply(avg.prediction_boot,MARGIN=1,FUN=quantile, probs = 0.975)

avg.prediction.df<- data.frame(
  POTENTIAL_ENERGY_EFFICIENCY = cee.seq,
  avg.prediction = avg.prediction,
  lower = avg.prediction.lower,
  upper = avg.prediction.upper)

ggplot(avg.prediction.df,aes(x = POTENTIAL_ENERGY_EFFICIENCY, y = avg.prediction)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "skyblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  labs(x = "Potential Energy Efficiency",
       y = "Average predicted price",
       title = "Average predicted prices vs Potential Energy Efficiency with 95% CI") +
  theme_minimal()


#The trend appears overall monotonically decreasing. The initial increase trend
#should be disregarded, as it is likely driven by the extreme left skewness of
#the POTENTIAL_ENERGY_EFFICIENCY distribution, as indicated by the extremely 
#wide confidence intervals. 
hist(house1$POTENTIAL_ENERGY_EFFICIENCY,breaks=50)
#It is important to further investigate the underlying meaning of this counterintuitive
#relationship as it could be the result of omitted variables bias or 
#it could reflect a real-world relationship between the two variables, further
#analysis are needed to inspect this in detail.




#ANSWER:
#There is strong evidence that houses with higher current energy efficiency 
#tend to be more expensive in Middlesbrough during the period under study.
#This is consistent with economic intuition, as energy-efficient homes 
#typically have lower running costs and higher desirability among buyers.

#It is instead less clear why a negative relationship between price and potential energy
#efficiency appears. 
#This counterintuitive trend could be driven by issues at modelling level 
#such as omittted variable bias or reflects a real-world phenomenon.



#RESEARCH QUESTION 3####
#What are the most affordable and most expensive areas of the county?

#To answer this question, the outward regressor (first three digits of the postcode)
#will be used as a location identifier.

#Let's inspect the values of the coefficients of model18:
summary(model18)
#TS1 is the reference category in this model (note that no observations with
#outward=TS2 are present in the dataset).
#All the coefficients are positive and significant, except for the one
#corresponding to outward TS3, which is significant but negative.
#This suggests that TS1 and TS3 are the less expensive areas of the town, 
#whereas TS6-TS7 and TS8-TS9 correspond to the most expensive areas(note that 
#these categories were merged at the data wrangling stage because too sparse).
#To facilitate interpretation, we will now change the reference category to a 
#"middle-price" area, such as TS4, so that the parameter estimates can be 
#directly compared to an intermediate location:
house1$outward <- relevel(house1$outward, ref = "TS4")
model18.ref.TS4 <- glm(price ~ poly(tfarea,5) +
                         propertytype +
                         year.num +
                         poly(CURRENT_ENERGY_EFFICIENCY,3) +
                         poly(POTENTIAL_ENERGY_EFFICIENCY,3) +
                         mean.room.size +
                         outward,
                       data = house1,
                       family = Gamma(link="log"))
summary(model18.ref.TS4)

#Coefficients can be explicitly interpreted:
location.coeff<-model18.ref.TS4$coefficients[c("outwardTS1","outwardTS3","outwardTS6-TS7","outwardTS8-TS9")]

#Least expensive areas
#TS1
exp(location.coeff[1])  #0.8276231 
1-exp(location.coeff[1]) #0.1723769 
#Houses in TS1 are estimated to have about 17.2% lower expected price than 
#houses in TS4, holding all other covariates in the model constant.

#TS3
exp(location.coeff[2])   #0.7884609
1-exp(location.coeff[2]) #0.2115391
#Houses in TS3 are estimated to have about 21.2% lower expected price than 
#houses in TS4, holding all other covariates in the model constant.

#Most expensive areas
#TS6-TS7
exp(location.coeff[3])  #1.547887 
exp(location.coeff[3])-1 #0.547887
#Houses in TS1 are estimated to have about 54.8% higher expected price than 
#houses in TS4, holding all other covariates in the model constant.

#TS7-TS8
exp(location.coeff[4])      #1.382453
exp(location.coeff[4])-1  #0.3824526 
#Houses in TS8–TS9 are estimated to have about 38.2% higher expected price than 
#houses in TS4, holding all other covariates in the model constant.


#Answer:
#These findings indicate that TS1 and TS3 are the most affordable areas of Middlesbrough,
#whereas TS6–TS7 and TS8–TS9 correspond to the most expensive areas.
#The use of the outward postcode as a location identifier provides a 
#useful but relatively broad spatial resolution,a more granular measure of
#location (e.g., full postcode or geographic coordinates) would allow for a 
#more detailed analysis of local housing price variation.


#Let's create a dataframe to summarise the results:
#Create a summary table of location effects
location.summary <- data.frame(
  Area= c("TS1", "TS3", "TS6–TS7", "TS8–TS9"),
  Exp.Beta = c(0.8276, 0.7885, 1.5479, 1.3825),
  Percent.Change = c(-17.2, -21.2, +54.8, +38.3),
  Interpretation = c("≈17.2% lower than TS4", "≈21.2% lower than TS4", "≈54.8% higher than TS4","≈38.3% higher than TS4"))

