options(scipen=999)

# Western hemisphere countries (excluding CAN and USA)
library(countrycode)
wec <- 
  codelist$iso3c[codelist$region %in% 
                   c("Northern America", 
                     "Central America",
                     "Caribbean",
                     "South America")]
wec <- wec[-which(wec %in% c("CAN", "USA"))]

# Population data
library(wbstats)
pop_data <- 
  wbstats::wb(indicator = "SP.POP.TOTL", startdate = "1960", enddate = "2018",
              country = wec)
## Prep
pop_data$date <- as.numeric(as.character(pop_data$date))
pop_data <- pop_data[,1:3]
names(pop_data) <- c("iso3c", "year", "population")

# ODA data
## Source OECD
library(readxl)
oda_dat <- 
  read_excel("de201781-7c8c-44a5-bbea-0d0f58012a1c.xlsx", 
             skip = 7, na = "..")
## Prep
oda_dat <- oda_dat[-1,]
oda_dat$X__1 <- NULL
names(oda_dat)[1] <- 'recipient'
oda_dat <- oda_dat[-which(grepl("total|regional", oda_dat$recipient, ignore.case = T)),]
## Assign ISO3C
library(countrycode)
oda_dat$iso3c <- countrycode(oda_dat$recipient, "country.name", "iso3c")
## Melt
library(reshape2)
oda_dat <- melt(oda_dat, id.vars = c("recipient","iso3c"),
            variable.name = 'year',
            value.name = 'oda')
oda_dat <- oda_dat[!is.na(oda_dat$oda),]
oda_dat$oda <- as.numeric(oda_dat$oda)

# Merge
dat <- 
  merge(pop_data[,c('iso3c','population','year')], 
        oda_dat[,c("iso3c","year","oda")],
        by.x = c("iso3c","year"), by.y = c("iso3c","year"),
        all.x = TRUE)
## Prep
dat$oda[is.na(dat$oda)] <- 0
dat$oda_pp <- 
  dat$oda / dat$population
dat$region <- 
  countrycode(dat$iso3c, "iso3c", "region")

# Analysis
library(ggplot2)
library(scales)
dat %>%
  group_by(year) %>%
  summarize(oda_pp = sum(oda, na.rm = T) / sum(people, na.rm = T)) %>%
  ggplot() +
  geom_line(aes(x = year, y = oda_pp*1000)) + 
  scale_y_continuous(labels = scales::dollar) + 
  theme_bw() +
  labs(x = "ODA per person (World)", y=NULL, 
       caption = "Calculated by dividing the total net official development assistant (ODA)\nfrom the US by the total number of people living in recipient countries.")



dat %>%
  filter(recipient_region %in% c("Central America","South America")) %>% 
  ggplot() +
  geom_point(aes(x = year, y = oda_pp_diff*1000, colour = recipient_region)) + 
  scale_y_continuous(labels = scales::dollar) + 
  theme_bw() +
  labs(x = "ODA per person", y=NULL, 
       caption = "Calculated by dividing the total net official development assistant (ODA)\nfrom the US by the total number of people living in recipient countries.")


require(dplyr)
dat %>%
  group_by(year) %>%
  mutate(oda_tot_year = sum(oda)) %>%
  group_by(year, recipient_region) %>%
  mutate(oda_region_perc = sum(oda, na.rm = T) / oda_tot_year) %>%
  
  ggplot() +
  geom_line(aes(x=as.numeric(year), y=oda_region_perc, colour=recipient_region))
  
  
