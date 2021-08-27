#######################################
# Examples of loops and functions in R
# 2021-08-27
# Hallie Eilerts
# hallie.eilerts@lshtm.ac.uk
#######################################


# Loops  -----------------------------------------------

x <- c(2,5,3,9,8,11,6)
y <- c()

for(i in 1:length(x)){
  y[i] <- x[i] + x[i+1]
}
y


y <- c()
for(i in 1:(length(x)-1)){
  y[i] <- x[i] + x[i+1]
}
y

# Below example borrowed from:
# https://www.datamentor.io/r-programming/for-loop/
x <- c(2,5,3,9,8,11,6)
count <- 0
for(i in x) {
  if(i %% 2 == 0){ # check remainder. if 0, go to next step
    count = count+1
    }
}
count


x <- c(2,4,4,9,8,12,6)
count <- 0
for(i in x) {
  if(i %% 2 == 0){
    count = count+1
    }
}
count


x <- c(2,4,4,9,8,12,6)
total1 <- 0
total2 <- 0
for(j in x) {
  
  if(j %% 2 == 0){ 
    total1 = total1 + j
  }
  if(j %% 2 != 0){ 
    total2 = total2 + j
  }
  
}
total1
total2


# Functions ---------------------------------------------------------------

fn.whatever <- function(x,y){x+y}
fn.whatever(3,4)

fn.whatever <- function(x){x^2}
fn.whatever(5)

fn.function2 <- function(x){ x / sum(x)}
v.somenumbers <- c(2,4,4,9,8,12,6)
fn.function2(v.somenumbers)


# Count even numbers
fn.function3 <- function(x) {
  v.evennumbers <- subset(x, x %% 2 == 0)
  length(v.evennumbers)
}

v.somenumbers <- c(2,4,4,9,8,12,6)
fn.function3(v.somenumbers)


# Car data ----------------------------------------------------------------

head(mtcars)

# Create column for car name
mtcars$car <- row.names(mtcars)
row.names(mtcars) <- NULL

# Separate Valiant from other cars
valiant <- subset(mtcars, car == "Valiant")
othercars <- subset(mtcars, car != "Valiant")

# Compare valiant with other cars to find which is most similar
othercars$samegear <- NA
othercars$wtdif <- NA
for(i in 1:nrow(othercars)){
  
  othercars[i,"samegear"] <- othercars[i,"gear"] == valiant$gear
  othercars[i,"wtdif"] <- abs(othercars[i,"wt"] - valiant$wt)
}
subset(othercars, samegear == TRUE)
subset(othercars, samegear == TRUE & wtdif == min(othercars$wtdif))


# Create new columns named loop
othercars <- subset(mtcars, car != "Valiant")
for(i in (ncol(othercars)+1):(ncol(othercars)+5)){
  othercars <- cbind(othercars, i)
  names(othercars)[i] <- paste("loop",i, sep="")
}
head(othercars)


# Australia data -------------------------------------------------------


# quarterly time series of the number of Australian residents from 1971 to 1993
install.packages("astsa")
library(astsa)

# Prepare data
data <- matrix(c(NA,austres,NA),ncol = 4,byrow=TRUE )
df.data <- data.frame(data)
names(df.data) <- c("Qtr1","Qtr2","Qtr3","Qtr4")

# 3 different ways to calculate row totals

### By hand
df.data$AnnualTotal <- df.data$Qtr1 + df.data$Qtr2 + df.data$Qtr3 + df.data$Qtr4
df.data$AnnualTotal <- NULL

### Loop
for(i in 1:nrow(df.data)){
  df.data$AnnualTotal[i,] <- sum(df.data[i,], na.rm = T)
}
head(df.data)
df.data$AnnualTotal <- NULL

### Vectorize
df.data$AnnualTotal <- apply(df.data, 1, sum, na.rm=T) # the 1 is telling apply to perform the action on the rows. if you used 2, it would apply to columns.
df.data


# DHS data ---------------------------------------------------------------------

# Adapted from:
# https://github.com/mrc-ide/demogsurv/blob/master/vignettes/rdhs-integration.md


#devtools::install_github("ropensci/rdhs", ref = "issue33_path")
library(rdhs)
library(demogsurv)
library(ggplot2)
library(data.table)
library(haven)

# Download the DHS data

## First create an account at the DHS website

## Enter your credentials here
set_rdhs_config(email = "youremail@email.com",
                password_prompt =  TRUE,
                project = "Your DHS project name", 
                config_path = "rdhs.json",
                global=FALSE)

# we will give our permission here so that we don't have to provide a prompt within the README.
Sys.setenv("rdhs_RENVIRON_PERMISSION"=1)

## a little nugget to return API requests as data.table rather than data.frame.
Sys.setenv(rdhs_DATA_TABLE = "TRUE")

## Look at available countries
countries <- dhs_countries()
countries$DHS_CountryCode

## Make vector with countries of interest
cc <- c("MZ", "TZ", "UG")

# Use rdhs to retreive datasets, downloading them from DHS website if not already in the rdhs cache.
surveys <- dhs_surveys(countryIds = cc, surveyYearStart=1990, surveyType = "DHS")

# Identify births recode (BR) datasets corresponding to these surveys.
step <- dhs_datasets(fileType = "IR", fileFormat = "flat")
ird <- step[which(step$SurveyId %in% surveys$SurveyId),]

# Use rdhs to retreive datasets, downloading them from DHS website if not already in the rdhs cache.
Sys.setenv("rdhs_LOUD_DOWNLOAD" = TRUE)
ird$path <- unlist(get_datasets(ird$FileName)) #  clear_cache = TRUE )) 

# Load all of the datasets into R as a list.
ir <- list()
for(survid in ird$SurveyId){
  print(survid)
  dat <- readRDS(ird[which(ird$SurveyId == survid),]$path)
  dat <- dat[grep("caseid|^v0|^v1|^b", names(dat))]
  ir[[survid]] <- dat
}

ir <- lapply(ir, haven::as_factor)

## Add survey-level variables
ir <- Map(data.frame,
          SurveyId = surveys$SurveyId,
          CountryName = surveys$CountryName,
          SurveyYear = surveys$SurveyYear,
          ir)


#### Some functions


length(ir)
names(ir)
head(ir$MZ1997DHS)
lapply(ir, function(x) head(x))

ldply(ir, function(x){ x <- x[,c("CountryName","v024")]
                       x <- x[!duplicated(x),];
                       return(x)})

l.data <- lapply(ir, function(x){ x <- x[,c("SurveyId","CountryName","v024")]
                                  x <- x[!duplicated(x),];
                                  return(x)})
mynames <- c("Survey","Country","Region")
l.data <- lapply(l.data, setNames, mynames)
head(l.data$MZ1997DHS)

#### Loops

l.regioncount <- c()
for(i in 1:length(ir)){
  l.regioncount[[i]] <- setDT(ir[[i]])[,.N,by=v024]
}
l.regioncount
names(l.regioncount) <- names(ir)


