# Clear work environment
rm(list = ls())
setwd(".../Duke Datathon")

library(plyr)
library(ggplot2)
library(dplyr) # easier data wrangling #Mutate
library(reshape)

# Combining jobs and companies data
df_jobs <- read.csv("jobs.csv", header = TRUE , sep = ",")
df_companies <- read.csv("companies.csv", header = TRUE , sep = ",")
df_real_estate <- read.csv("real_estate.csv", header = TRUE , sep = ",")

df_companies <- df_companies[,-c(3,4)]
df_companies <- df_companies[,-c(6:10)]
df_companies <- df_companies[as.character(df_companies$country) == "US",]

df_tot <- merge.data.frame(df_jobs, df_companies, by = "company_id")

rm(df_jobs)
rm(df_companies)

df_tot <- df_tot[,-c(2)]
df_tot <- df_tot[,-c(3,5)]
df_tot <- df_tot[, -c(7,8)]
df_tot <- na.omit(df_tot)
df_tot <- df_tot[complete.cases(df_tot),]

df_tot$created_date <- as.Date(df_tot$created_date, format = "%Y-%m-%d")

df_tot <- df_tot %>% mutate(year = year(created_date))

aggjobs <- count(df_tot, as.character(df_tot$year), as.character(df_tot$city.x), as.character(df_tot$state.x))
names(aggjobs) <- c("Year", "City", "State", "Num of Jobs")

# Manipulation and Processing of real estate data
md <- melt(df_real_estate, id = (c("regionID", "city", "state", "sizeRank")))
md$variable <- gsub("X","",md$variable)
md$variable[-length(md$variable)] <- paste0(md$variable[-length(md$variable)], '-01')
md$variable <- as.Date(md$variable, format = "%Y.%m-%d")
names(md) <- c("regionId", "City", "State", "sizeRank", "Date", "Home Value Index")
md <- md %>% mutate(year = year(Date),month = month(Date))
md <- na.omit(md)

agg_index <- as.data.frame(aggregate( md$`Home Value Index` ~ md$State + md$year + md$City + md$sizeRank, md , mean ))
names(agg_index) <- c("State", "Year", "City", "SizeRank", "Home Value Index")

# Combining real estate and jobs data
df_final1 <- merge(agg_index, aggjobs, by = c("City","State","Year"))

rm(df_real_estate)

#Integrating Demographics data into jobs and real estate data
df_demo <- read.csv("demographics.csv", header = TRUE , sep = ",")
df_demo[,"Ratio of population between 20-64"] <- sum(df_demo[8:13])/df_demo[3]
df_demo <- df_demo[,-c(4:16)]
df_demo <- df_demo[,-c(5:13)]

colnames(df_demo)[1] = "City"
colnames(df_demo)[2] = "State"

df_final2 <- merge(df_final1, df_demo, by = c("City","State"))

#Coordinates Data
df_geo <- read.csv("geographic.csv", header = TRUE , sep = ",")
colnames(df_geo)[1] = "City"
colnames(df_geo)[2] = "State"

df_final3 <- merge(df_final2, df_geo, by = c("City","State"))

