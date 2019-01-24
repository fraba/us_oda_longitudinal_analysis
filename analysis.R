options(scipen=999)

# Western hemisphere countries (excluding CAN and USA)
library(countrycode)
wec <- 
  codelist$iso3c[codelist$region %in% 
                   c("Northern America", 
                     "Central America",
                     "Caribbean",
                     "South America")]
wec <- wec[-which(wec %in% c("CAN", "USA", "GRL"))]

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

dat_regional <-
  dat %>%
  dplyr::filter(region_plus %in% c("Central America", "South America")) %>%
  dplyr::group_by(year, region) %>%
  dplyr::summarize(oda_pp = sum(oda) / sum(population)) 

dat_regional <-
  dat_regional %>%
  dplyr::group_by(region) %>%
  dplyr::mutate(oda_pp_from_baseline = 
                  (oda_pp - oda_pp[year == 1961]) / )

ggsave(file = "us_oda_central_south_america.png", width = 10,
ggplot(dat_regional) +
  geom_line(aes(x = year, y = oda_pp*1000, colour=region)) + 
  scale_y_continuous(labels = scales::dollar_format(accuracy = .001)) + 
  scale_color_brewer(palette='Set1') +
  theme_bw() +
  annotate("text", x = 2001, y = 0.020, label = "Nicaraguan presidential\nelection\n(2001)\n↓",
           size = 3.6) +
  annotate("text", x = 1966, y = 0.001, label = "← Cuban revolution (1959)",
           size = 3.6) +
  annotate("text", x = 1990, y = 0.0245, label = "Reagan doctrine",
           size = 3.6) +
  annotate("rect", xmin=1981, xmax=1998, ymin=0, ymax=Inf, fill = 'red', alpha = .1) +
  labs(x = "ODA per person", y=NULL, 
       caption = "Calculated by dividing net official development assistant (ODA) from the US by total number of people living in the recipient regions\n@FrBailo | Source: World Bank, OECD | To replicate: git.io/fhwtW")
)

ggsave(file = "us_oda_central_south_america_perc.png", width = 10,
dat_regional %>% filter(year > 1960) %>%
ggplot() +
  geom_line(aes(x = year, y=oda_pp_from_baseline, colour=region)) + 
  scale_y_continuous(labels = percent, limits = c(0,1.4)) + 
  scale_color_brewer(palette='Set1') +
  theme_bw() +
  annotate("text", x = 2001, y = 1.15, label = "Nicaraguan presidential\nelection\n(2001)\n↓",
           size = 3.6) +
  # annotate("text", x = 1966, y = 0.001, label = "← Cuban revolution (1959)",
  #          size = 3.6) +
  annotate("text", x = 1990, y =1.35, label = "Reagan doctrine",
           size = 3.6) +
  annotate("rect", xmin=1981, xmax=1998, ymin=0, ymax=Inf, fill = 'red', alpha = .1) +
  labs(x = "ODA per person (as % of 1961 values)", y=NULL, 
       caption = "Calculated by dividing net official development assistant (ODA) from the US by total number of people living in the recipient regions\n@FrBailo | Source: World Bank, OECD | To replicate: git.io/fhwtW")
)
