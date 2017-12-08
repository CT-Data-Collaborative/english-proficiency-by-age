library(acs)
library(datapkg)
source('./scripts/acsHelpers.R')

# ACS B16004
# Get geography object for CT and subcounty divisions
acsdata <- getACSData(
    getCTGeos("town"),
    yearList = 2010:2016,
    table = "B16004"
)

pops <- data.table()
for (data in acsdata) {
    year <- data@endyear
    Total.Native <- acsSum(data, c(3, 25, 47), "Total:Native")
    Total.Very.Well <- acsSum(data, c(5, 10, 15, 20, 27, 32, 37, 42, 49, 54, 59, 64), "Total:Very Well")
    Total.Well <- acsSum(data, c(6,11,16,21,28,33,38,43,50,55,60,65), "Total:Well")
    Total.Not.Well <- acsSum(data, c(7,12,17,22,29,34,39,44,51,56,61,66), "Total:Not Well")
    Total.Not.at.All <- acsSum(data, c(8,13,18,23,30,35,40,45,52,57,62,67), "Total:Not at All")
    #Age == 5 to 17
    x5.to.17.Native <- data[,3] # 5 to 17 Native
    x5.to.17.years.Very.Well<- acsSum(data, c(5, 10, 15, 20), "5 to 17 years:Very Well")
    x5.to.17.years.Well <- acsSum(data, c(6,11,16,21), "5 to 17 years:Well")
    x5.to.17.years.Not.Well <- acsSum(data, c(7,12,17,22), "5 to 17 years:Not Well")
    x5.to.17.years.Not.at.All <- acsSum(data, c(8,13,18,23), "5 to 17 years:Not at All")
    #Age == 18 to 64
    x18.to.64.Native <- data[,25] # 18 to 64 Native
    x18.to.64.years.Very.Well <- acsSum(data, c(27, 32, 37, 42), "18 to 64 years:Very Well")
    x18.to.64.years.Well <- acsSum(data, c(28,33,38,43), "18 to 64 years:Well")
    x18.to.64.years.Not.Well <- acsSum(data, c(29,34,39,44), "18 to 64 years:Not Well")
    x18.to.64.years.Not.at.All <- acsSum(data, c(30,35,40,45), "18 to 64 years:Not at All")
    #Age == 65+
    x65.plus.Native <- data[,47] # 65+ Native
    x65.plus.Very.Well <- acsSum(data, c(49, 54, 59, 64), "65 years and over:Very Well")
    x65.plus.Well  <- acsSum(data, c(50,55,60,65), "65 years and over:Well")
    x65.plus.Not.Well <- acsSum(data, c(51,56,61,66), "65 years and over:Not Well")
    x65.plus.Not.at.All <- acsSum(data, c(52,57,62,67), "65 years and over:Not at All")
    datafips <- data.table(fips = getACSFips(data))

    estimates <- data.table(
        FIPS = datafips$fips,
        Year = year,
        estimate(Total.Native),
        estimate(Total.Very.Well),
        estimate(Total.Well),
        estimate(Total.Not.Well),
        estimate(Total.Not.at.All), 
        estimate(x5.to.17.Native), 
        estimate(x5.to.17.years.Very.Well),
        estimate(x5.to.17.years.Well), 
        estimate(x5.to.17.years.Not.Well), 
        estimate(x5.to.17.years.Not.at.All),
        estimate(x18.to.64.Native ),
        estimate(x18.to.64.years.Very.Well),
        estimate(x18.to.64.years.Well ),
        estimate(x18.to.64.years.Not.Well ),
        estimate(x18.to.64.years.Not.at.All ),   
        estimate(x65.plus.Native), 
        estimate(x65.plus.Very.Well ),
        estimate(x65.plus.Well ), 
        estimate(x65.plus.Not.Well ),
        estimate(x65.plus.Not.at.All)   
    )
    
    names(estimates)[names(estimates) == "HD01_VD03.Estimate; 5 to 17 years: - Speak only English"] <- "Total 5 to 17 years Native"
    names(estimates)[names(estimates) == "HD01_VD25.Estimate; 18 to 64 years: - Speak only English"] <- "Total 18 to 64 years Native"
    names(estimates)[names(estimates) == "HD01_VD47.Estimate; 65 years and over: - Speak only English"] <- "Total 65 years and over Native"
    names(estimates)[names(estimates) == "HD01_VD03.Estimate; 5 to 17 years: Speak only English"] <- "Total 5 to 17 years Native"
    names(estimates)[names(estimates) == "HD01_VD25.Estimate; 18 to 64 years: Speak only English"] <- "Total 18 to 64 years Native"
    names(estimates)[names(estimates) == "HD01_VD47.Estimate; 65 years and over: Speak only English"] <- "Total 65 years and over Native"
    names(estimates)[names(estimates) == "HD01_VD03.Estimate;  5 to 17 years: Speak only English "] <- "Total 5 to 17 years Native"
    names(estimates)[names(estimates) == "HD01_VD25.Estimate;  18 to 64 years: Speak only English "] <- "Total 18 to 64 years Native"
    names(estimates)[names(estimates) == "HD01_VD47.Estimate;  65 years and over: Speak only English "] <- "Total 65 years and over Native"

    
    estimates <- melt(
        estimates,
        id.vars = c("FIPS", "Year"),
        variable.name = "Age",
        variable.factor = F,
        value.name = "Pop",
        value.factor = F
    )

    moes <- data.table(
        FIPS = datafips$fips,
        Year = year,
        standard.error(Total.Native) * 1.645,
        standard.error(Total.Very.Well) * 1.645,
        standard.error(Total.Well) * 1.645,
        standard.error(Total.Not.Well) * 1.645,
        standard.error(Total.Not.at.All) * 1.645, 
        standard.error(x5.to.17.Native) * 1.645, 
        standard.error(x5.to.17.years.Very.Well) * 1.645,
        standard.error(x5.to.17.years.Well) * 1.645, 
        standard.error(x5.to.17.years.Not.Well) * 1.645, 
        standard.error(x5.to.17.years.Not.at.All) * 1.645,
        standard.error(x18.to.64.Native ) * 1.645,
        standard.error(x18.to.64.years.Very.Well) * 1.645,
        standard.error(x18.to.64.years.Well ) * 1.645,
        standard.error(x18.to.64.years.Not.Well ) * 1.645,
        standard.error(x18.to.64.years.Not.at.All ) * 1.645,   
        standard.error(x65.plus.Native) * 1.645, 
        standard.error(x65.plus.Very.Well ) * 1.645,
        standard.error(x65.plus.Well ) * 1.645, 
        standard.error(x65.plus.Not.Well ) * 1.645,
        standard.error(x65.plus.Not.at.All) * 1.645  
    )
    
    names(moes)[names(moes) == "HD01_VD03.Estimate; 5 to 17 years: - Speak only English"] <- "Total 5 to 17 years Native"
    names(moes)[names(moes) == "HD01_VD25.Estimate; 18 to 64 years: - Speak only English"] <- "Total 18 to 64 years Native"
    names(moes)[names(moes) == "HD01_VD47.Estimate; 65 years and over: - Speak only English"] <- "Total 65 years and over Native"
    names(moes)[names(moes) == "HD01_VD03.Estimate; 5 to 17 years: Speak only English"] <- "Total 5 to 17 years Native"
    names(moes)[names(moes) == "HD01_VD25.Estimate; 18 to 64 years: Speak only English"] <- "Total 18 to 64 years Native"
    names(moes)[names(moes) == "HD01_VD47.Estimate; 65 years and over: Speak only English"] <- "Total 65 years and over Native"
    names(moes)[names(moes) == "HD01_VD03.Estimate;  5 to 17 years: Speak only English "] <- "Total 5 to 17 years Native"
    names(moes)[names(moes) == "HD01_VD25.Estimate;  18 to 64 years: Speak only English "] <- "Total 18 to 64 years Native"
    names(moes)[names(moes) == "HD01_VD47.Estimate;  65 years and over: Speak only English "] <- "Total 65 years and over Native"

    
    moes <- melt(
        moes,
        id.vars = c("FIPS", "Year"),
        variable.name = "Age",
        variable.factor = F,
        value.name = "MOE",
        value.factor = F
    )

    setkey(estimates, FIPS, Year, `Age`)
    setkey(moes, FIPS, Year, `Age`)

    pops <- rbind(pops, estimates[moes])
}

pops <- pops[pops$FIPS != "0900100000",]

# Write to File
write.table(
    pops,
    file.path(getwd(), "raw", "populations.csv"),
    sep = ",",
    row.names = F
)