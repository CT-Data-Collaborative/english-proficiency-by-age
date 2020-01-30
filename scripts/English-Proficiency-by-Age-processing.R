library(dplyr)
library(datapkg)
library(acs)
library(stringr)
library(reshape2)
library(data.table)
library(plyr)
source('./scripts/acsHelpers.R')

##################################################################
#
# Processing Script for English Proficiency by Age
# Created by Jenna Daly
# On 11/27/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))
columns <- read.csv(paste0(path_to_raw, "/", "B16004-columns.csv"), stringsAsFactors = F, header=T, check.names=F)
columns

ctGeos <- getCTGeos()
yearList = c(2010:2018)
tn = "B16004"
acsdata <- getACSData(ctGeos, yearList=yearList, table = tn)

dataset <- data.frame(stringsAsFactors = FALSE)

for(data in acsdata) {
    year <- data@endyear
    print(paste("Processing: ", year))
    year <- paste(year-4, year, sep="-")
    geo <- data@geography
    geo$Geography <- NULL
    geo$Id <- NULL
    geo$Id2 <- gsub("^", "0", geo$Id2)
    
            universe = data[,1, "universe"]
            # Age == Total
            total.Native <- acsSum(data, c(3, 25, 47), "Total:Native")
            total.Very.Well <- acsSum(data, c(5, 10, 15, 20, 27, 32, 37, 42, 49, 54, 59, 64), "Total:Very Well")
            total.Well <- acsSum(data, c(6,11,16,21,28,33,38,43,50,55,60,65), "Total:Well")
            total.Not.Well <- acsSum(data, c(7,12,17,22,29,34,39,44,51,56,61,66), "Total:Not Well")
            total.Not.at.All <- acsSum(data, c(8,13,18,23,30,35,40,45,52,57,62,67), "Total:Not at All")

            #Age == 5 to 17
            x5to17Native <- data[,3, "5 to 17 Native"] # 5 to 17 Native
            x5to17years.Very.Well <- acsSum(data, c(5, 10, 15, 20), "5 to 17 years:Very Well")
            x5to17years.Well <- acsSum(data, c(6,11,16,21), "5 to 17 years:Well")
            x5to17years.Not.Well <- acsSum(data, c(7,12,17,22), "5 to 17 years:Not Well")
            x5to17years.Not.at.All <- acsSum(data, c(8,13,18,23), "5 to 17 years:Not at All")

            #Age == 18 to 64
            x18to64Native <- data[,25, "18 to 64 Native"] # 18 to 64 Native
            x18to64years.Very.Well <- acsSum(data, c(27, 32, 37, 42), "18 to 64 years:Very Well")
            x18to64years.Well <- acsSum(data, c(28,33,38,43), "18 to 64 years:Well")
            x18to64years.Not.Well <- acsSum(data, c(29,34,39,44), "18 to 64 years:Not Well")
            x18to64years.Not.at.All <- acsSum(data, c(30,35,40,45), "18 to 64 years:Not at All")

            #Age == 65+
            x65plusNative <- data[,47, "65+ Native"] # 65+ Native
            x65plusyears.and.over.Very.Well <- acsSum(data, c(49, 54, 59, 64), "65 years and over:Very Well")
            x65plusyears.and.over.Well <- acsSum(data, c(50,55,60,65), "65 years and over:Well")
            x65plusyears.and.over.Not.Well <- acsSum(data, c(51,56,61,66), "65 years and over:Not Well")
            x65plusyears.and.over.Not.at.All <- acsSum(data, c(52,57,62,67), "65 years and over:Not at All")

    # new data tables to build
    # Data tables must be instantiated with correct number of rows
    # and we need to add this extra data anyway
    numbers <- data.table(
            geo,
            estimate(universe),
            estimate(total.Native),
            estimate(total.Very.Well ),
            estimate(total.Well),
            estimate(total.Not.Well),
            estimate(total.Not.at.All),
            estimate(x5to17Native),
            estimate(x5to17years.Very.Well),
            estimate(x5to17years.Well),
            estimate(x5to17years.Not.Well),
            estimate(x5to17years.Not.at.All),
            estimate(x18to64Native),
            estimate(x18to64years.Very.Well),
            estimate(x18to64years.Well),
            estimate(x18to64years.Not.Well),
            estimate(x18to64years.Not.at.All),
            estimate(x65plusNative),
            estimate(x65plusyears.and.over.Very.Well),
            estimate(x65plusyears.and.over.Well),
            estimate(x65plusyears.and.over.Not.Well),
            estimate(x65plusyears.and.over.Not.at.All), 
            Year = rep_len(year, nrow(data)),
            `Measure Type` = rep_len("Number", nrow(data)),
            Variable = rep_len("English Proficiency", nrow(data))
        )
    numbers.moe <- data.table(
            geo,
            standard.error(universe) * 1.645,
            standard.error(total.Native) * 1.645,
            standard.error(total.Very.Well ) * 1.645,
            standard.error(total.Well) * 1.645,
            standard.error(total.Not.Well) * 1.645,
            standard.error(total.Not.at.All) * 1.645,
            standard.error(x5to17Native) * 1.645,
            standard.error(x5to17years.Very.Well) * 1.645,
            standard.error(x5to17years.Well) * 1.645,
            standard.error(x5to17years.Not.Well) * 1.645,
            standard.error(x5to17years.Not.at.All) * 1.645,
            standard.error(x18to64Native) * 1.645,
            standard.error(x18to64years.Very.Well) * 1.645,
            standard.error(x18to64years.Well) * 1.645,
            standard.error(x18to64years.Not.Well) * 1.645,
            standard.error(x18to64years.Not.at.All) * 1.645,
            standard.error(x65plusNative) * 1.645,
            standard.error(x65plusyears.and.over.Very.Well) * 1.645,
            standard.error(x65plusyears.and.over.Well) * 1.645,
            standard.error(x65plusyears.and.over.Not.Well) * 1.645,
            standard.error(x65plusyears.and.over.Not.at.All) * 1.645, 
            Year = rep_len(year, nrow(data)),
            `Measure Type` = rep_len("Number", nrow(data)),
            Variable = rep_len("Margins of Error", nrow(data))
        )
    


    
total.Native.p <- divide.acs(total.Native, universe, method = "proportion")
total.Very.Well.p <- divide.acs(total.Very.Well, universe, method = "proportion")
total.Well.p <- divide.acs(total.Well, universe, method = "proportion") 
total.Not.Well.p <- divide.acs(total.Not.Well, universe, method = "proportion") 
total.Not.at.All.p <- divide.acs(total.Not.at.All, universe, method = "proportion") 
x5to17Native.p <- divide.acs(x5to17Native, universe, method = "proportion")
x5to17years.Very.Well.p <- divide.acs(x5to17years.Very.Well, universe, method = "proportion")
x5to17years.Well.p <- divide.acs( x5to17years.Well, universe, method = "proportion") 
x5to17years.Not.Well.p <- divide.acs( x5to17years.Not.Well, universe, method = "proportion") 
x5to17years.Not.at.All.p <- divide.acs( x5to17years.Not.at.All, universe, method = "proportion") 
x18to64Native.p <- divide.acs( x18to64Native, universe, method = "proportion") 
x18to64years.Very.Well.p <- divide.acs( x18to64years.Very.Well, universe, method = "proportion") 
x18to64years.Well.p <- divide.acs(x18to64years.Well, universe, method = "proportion")
x18to64years.Not.Well.p <- divide.acs(x18to64years.Not.Well, universe, method = "proportion") 
x18to64years.Not.at.All.p <- divide.acs( x18to64years.Not.at.All, universe, method = "proportion") 
x65plusNative.p <- divide.acs( x65plusNative, universe, method = "proportion") 
x65plusyears.and.over.Very.Well.p <- divide.acs( x65plusyears.and.over.Very.Well, universe, method = "proportion") 
x65plusyears.and.over.Well.p <- divide.acs(x65plusyears.and.over.Well, universe, method = "proportion")
x65plusyears.and.over.Not.Well.p <- divide.acs(x65plusyears.and.over.Not.Well, universe, method = "proportion")
x65plusyears.and.over.Not.at.All.p <- divide.acs(x65plusyears.and.over.Not.at.All, universe, method = "proportion")

  if (any(is.nan(standard.error(total.Native.p)))) {
      total.Native.p <- divide.acs(total.Native, universe, method = "ratio")
  }  
  if (any(is.nan(standard.error(total.Very.Well.p)))) {
      total.Very.Well.p <- divide.acs(total.Very.Well, universe, method = "ratio")
  }  
  if (any(is.nan(standard.error(total.Well.p)))) {
    total.Well.p <- divide.acs(total.Well, universe, method = "ratio")
  }
  if (any(is.nan(standard.error(total.Not.Well.p)))) {
    total.Not.Well.p <- divide.acs(total.Not.Well, universe, method = "ratio")
  }
  if (any(is.nan(standard.error(total.Not.at.All.p)))) {
    total.Not.at.All.p <- divide.acs(total.Not.at.All, universe, method = "ratio")
  }
  if (any(is.nan(standard.error(x5to17Native.p)))) {
    x5to17Native.p <- divide.acs(x5to17Native, universe, method = "ratio")
  }
  if (any(is.nan(standard.error(x5to17years.Very.Well.p)))) {
    x5to17years.Very.Well.p <- divide.acs(x5to17years.Very.Well, universe, method = "ratio")
  }
  if (any(is.nan(standard.error(x5to17years.Well.p)))) {
    x5to17years.Well.p <- divide.acs(x5to17years.Well, universe, method = "ratio")
  }
  if (any(is.nan(standard.error(x5to17years.Not.Well.p)))) {
    x5to17years.Not.Well.p <- divide.acs(x5to17years.Not.Well, universe, method = "ratio")
  }
  if (any(is.nan(standard.error(x5to17years.Not.at.All.p)))) {
    x5to17years.Not.at.All.p <- divide.acs(x5to17years.Not.at.All, universe, method = "ratio")
  }
  if (any(is.nan(standard.error(x18to64Native.p)))) {
    x18to64Native.p <- divide.acs(x18to64Native, universe, method = "ratio")
  }
  if (any(is.nan(standard.error(x18to64years.Very.Well.p)))) {
    x18to64years.Very.Well.p <- divide.acs(x18to64years.Very.Well, universe, method = "ratio")
  }
  if (any(is.nan(standard.error(x18to64years.Well.p)))) {
    x18to64years.Well.p <- divide.acs(x18to64years.Well, universe, method = "ratio")
  }
  if (any(is.nan(standard.error(x18to64years.Not.Well.p)))) {
    x18to64years.Not.Well.p <- divide.acs(x18to64years.Not.Well, universe, method = "ratio")
  }
  if (any(is.nan(standard.error(x18to64years.Not.at.All.p)))) {
    x18to64years.Not.at.All.p <- divide.acs(x18to64years.Not.at.All, universe, method = "ratio")
  }
  if (any(is.nan(standard.error(x65plusNative.p)))) {
    x65plusNative.p <- divide.acs(x65plusNative, universe, method = "ratio")
  }
  if (any(is.nan(standard.error(x65plusyears.and.over.Very.Well.p)))) {
    x65plusyears.and.over.Very.Well.p <- divide.acs(x65plusyears.and.over.Very.Well, universe, method = "ratio")
  }
  if (any(is.nan(standard.error(x65plusyears.and.over.Well.p)))) {
    x65plusyears.and.over.Well.p <- divide.acs(x65plusyears.and.over.Well, universe, method = "ratio")
  }
  if (any(is.nan(standard.error(x65plusyears.and.over.Not.Well.p)))) {
    x65plusyears.and.over.Not.Well.p <- divide.acs(x65plusyears.and.over.Not.Well, universe, method = "ratio")
  }
  if (any(is.nan(standard.error(x65plusyears.and.over.Not.at.All.p)))) {
    x65plusyears.and.over.Not.at.All.p <- divide.acs(x65plusyears.and.over.Not.at.All, universe, method = "ratio")
  }
    
    props <- data.table(
        geo,
        estimate(total.Native.p), 
        estimate(total.Very.Well.p),
        estimate(total.Well.p),
        estimate(total.Not.Well.p),
        estimate(total.Not.at.All.p),
        estimate(x5to17Native.p),
        estimate(x5to17years.Very.Well.p),
        estimate(x5to17years.Well.p),
        estimate(x5to17years.Not.Well.p),
        estimate(x5to17years.Not.at.All.p),
        estimate(x18to64Native.p),
        estimate(x18to64years.Very.Well.p),
        estimate(x18to64years.Well.p),
        estimate(x18to64years.Not.Well.p),
        estimate(x18to64years.Not.at.All.p),
        estimate(x65plusNative.p),
        estimate(x65plusyears.and.over.Very.Well.p),
        estimate(x65plusyears.and.over.Well.p),
        estimate(x65plusyears.and.over.Not.Well.p),
        estimate(x65plusyears.and.over.Not.at.All.p),
        Year = rep_len(year, nrow(data)),
        `Measure Type` = rep_len("Percent", nrow(data)),
        Variable = rep_len("English Proficiency", nrow(data))
    )     
    
    props.moe <- data.table(
        geo,
        standard.error(total.Native.p) * 1.645, 
        standard.error(total.Very.Well.p) * 1.645, 
        standard.error(total.Well.p) * 1.645,
        standard.error(total.Not.Well.p) * 1.645,
        standard.error(total.Not.at.All.p) * 1.645,
        standard.error(x5to17Native.p) * 1.645,
        standard.error(x5to17years.Very.Well.p) * 1.645,
        standard.error(x5to17years.Well.p) * 1.645,
        standard.error(x5to17years.Not.Well.p) * 1.645,
        standard.error(x5to17years.Not.at.All.p) * 1.645,
        standard.error(x18to64Native.p) * 1.645,
        standard.error(x18to64years.Very.Well.p) * 1.645,
        standard.error(x18to64years.Well.p) * 1.645,
        standard.error(x18to64years.Not.Well.p) * 1.645,
        standard.error(x18to64years.Not.at.All.p) * 1.645,
        standard.error(x65plusNative.p) * 1.645,
        standard.error(x65plusyears.and.over.Very.Well.p) * 1.645,
        standard.error(x65plusyears.and.over.Well.p) * 1.645,
        standard.error(x65plusyears.and.over.Not.Well.p) * 1.645,
        standard.error(x65plusyears.and.over.Not.at.All.p) * 1.645,
        Year = rep_len(year, nrow(data)),
        `Measure Type` = rep_len("Percent", nrow(data)),
        Variable = rep_len("Margins of Error", nrow(data))
    )
    
    numbers_all <- rbind(numbers, numbers.moe)
    
    numbers_all$Column <- trimws(numbers_all$Column)
    #names(numbers_all)[grep( "HD01_VD01.Estimate; Total:", names(numbers_all))] <- "Total:Total"
    #names(numbers_all)[names(numbers_all) == "HD01_VD03.Estimate; 5 to 17 years: - Speak only English"] <- "5 to 17 years:Native"
    #names(numbers_all)[names(numbers_all) == "HD01_VD25.Estimate; 18 to 64 years: - Speak only English"] <- "18 to 64 years:Native"
    #names(numbers_all)[names(numbers_all) == "HD01_VD47.Estimate; 65 years and over: - Speak only English"] <- "65 years and over years:Native"
    
    names(numbers_all)[grep( "Estimate.*Total:*$", names(numbers_all))] <- "Total:Total"
    names(numbers_all)[grep( "Estimate.*5 to 17 years.*Speak only English$", names(numbers_all))] <- "5 to 17 years:Native"
    names(numbers_all)[grep( "Estimate.*18 to 64 years.*Speak only English$", names(numbers_all))] <- "18 to 64 years:Native"
    names(numbers_all)[grep( "Estimate.*65 years and over.*Speak only English$", names(numbers_all))] <- "65 years and over years:Native"

    numbers_all <- melt(
            numbers_all,
            id.vars=c("Id2", "Year", "Measure Type", "Variable"),
            variable.name="Column",
            variable.factor = F,
            value.name="Value",
            value.factor = F
         )
    
    names(numbers_all)[names(numbers_all) == "Id2"] <- "FIPS"
    
    props_all <- rbind(props, props.moe)
    
    colnames(props_all) <- c("FIPS", "Total:Native", "Total:Very Well", "Total:Well", "Total:Not Well", "Total:Not at All", 
                     "5 to 17 years:Native", "5 to 17 years:Very Well", 
                     "5 to 17 years:Well", "5 to 17 years:Not Well", "5 to 17 years:Not at All", "18 to 64 years:Native", 
                     "18 to 64 years:Very Well",
                     "18 to 64 years:Well", "18 to 64 years:Not Well", "18 to 64 years:Not at All", 
                     "65 years and over years:Native", "65 years and over:Very Well",
                     "65 years and over:Well", "65 years and over:Not Well", "65 years and over:Not at All",
                     "Year", "Measure Type", "Variable")
    
    props_all$"Total:Total" <- NA
    
    props_all <- melt(
            props_all,
            id.vars=c("FIPS", "Year", "Measure Type", "Variable"),
            variable.name="Column",
            variable.factor = F,
            value.name="Value",
            value.factor = F
         )
    
   numbers_all <- as.data.frame(numbers_all) 
   props_all <- as.data.frame(props_all) 
   
  
   dataset <- rbind(dataset, numbers_all, props_all)
    
}
options(scipen=9999)

#dataset$Column[dataset$Column == "HD01_VD01.Estimate;  Total: "] <- "Total:All"
#dataset$Column[dataset$Column == "HD01_VD03.Estimate;  5 to 17 years: Speak only English "] <- "5 to 17 years:Native"
#dataset$Column[dataset$Column == "HD01_VD25.Estimate;  18 to 64 years: Speak only English " ] <- "18 to 64 years:Native"
#dataset$Column[dataset$Column == "HD01_VD47.Estimate;  65 years and over: Speak only English "] <- "65 years and over:Native"

names(dataset$Column)[grep( "Estimate.*Total:*$", dataset$Column)] <- "Total:All"
names(dataset$Column)[grep( "Estimate.*5 to 17 years.*Speak only English$", dataset$Column)] <- "5 to 17 years:Native"
names(dataset$Column)[grep( "Estimate.*18 to 64 years.*Speak only English$", dataset$Column)] <- "18 to 64 years:Native"
names(dataset$Column)[grep( "Estimate.*65 years and over.*Speak only English$", dataset$Column)] <- "65 years and over years:Native"

dataset <- dataset[dataset$Column != "Column",]

#Final Additions, processing
# Split meaningful dimensions out of old column names
dataset <- as.data.table(dataset)
dataset[,c("Age", "English Proficiency"):=do.call(Map, c(f=c, strsplit(`Column`, ":", fixed=T)))]
dataset[,`Column` := NULL]

# Round Values according to type/variable
dataset$Value <- as.numeric(dataset$Value)
dataset[`Measure Type` == "Number", Value := round(Value, 0)]
dataset[`Measure Type` != "Number", Value := round(Value*100, 2)]


dataset$Age[dataset$Age == "65 years and over years"] <- "65 years and over"

dataset <- dataset[dataset$`English Proficiency` != "All",]

# Join town names by FIPS code
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
towns <- (town_fips_dp$data[[1]])

dataset <- merge(towns, dataset, by = "FIPS", all.x=T)

dataset <- dataset[dataset$`English Proficiency` != "Total",]

dataset$Age <- factor(dataset$Age, levels = c("Total", "5 to 17 years", "18 to 64 years", "65 years and over"))

#set final column order
dataset <- dataset %>% 
  select(Town, FIPS, Year, `English Proficiency`, Age, `Measure Type`, Variable, Value) %>% 
  arrange(Town, Year, `English Proficiency`, Age, `Measure Type`, Variable)

write.table(
    dataset,
    file.path("data", "english-proficiency-by-age-2018.csv"),
    sep = ",",
    row.names = F,
    na = "-9999"
)
