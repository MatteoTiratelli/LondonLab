library(tidyverse)

setwd("/Users/matteo/Downloads/London Lab/")

#######################

LondonJobs <- read_csv("London sectoral employment.csv")

LondonJobs %>%
  mutate(across(-c(Year,Total),
                ~ . / Total)) -> temp

LondonJobs <- LondonJobs[,1]
LondonJobs$Primary <- temp$`Primary & utilities`
LondonJobs$`Manufacturing & Construction` <- temp$`Manufacturing & Construction` + temp$`Manufacturing & Construction_1`
LondonJobs$`Retail & Wholesale` <- temp$`Retail & Wholesale` + temp$`Retail & Wholesale_1`
LondonJobs$Transportation <- temp$`Transportation and Storage`
LondonJobs$Services <- temp$Services + temp$Services_1 + temp$Services_2 + temp$Services_3 + temp$Services_4 + temp$Services_5 + temp$Services_6 + temp$Services_7
LondonJobs$`Education & Health` <- temp$`Education & Health` + temp$`Education & Health_1`
rm(temp)

LondonJobs <- pivot_longer(LondonJobs, -Year)

ggplot(LondonJobs[LondonJobs$Year<2018,], aes(x = Year, y = value, colour = name)) +
  geom_line() + xlab(NULL) + ylab(NULL) +
  labs(title = 'Sectoral employment shares in London, 1971 - 2017 (GLA Economics)') +
  theme_base() +
  theme(legend.position = "bottom", legend.title = element_blank())

#######################

LondonPop <- read_csv("https://data.london.gov.uk/download/historic-census-population/2c7867e5-3682-4fdd-8b9d-c63e289b92a6/census-historic-population-borough.csv")
LondonPop <- LondonPop[LondonPop$`Area Code` %in% c('UKI1', 'UKI2', 'H'),]
names(LondonPop) <- gsub('Persons-', '', names(LondonPop))
LondonPop$`Area Code` <- NULL

LondonPop <- pivot_longer(LondonPop, -'Area Name')
LondonPop$name <- as.numeric(LondonPop$name)

#LondonPop <- bind_rows(LondonPop, tibble(`Area Name` = c("Inner London","Inner London","Inner London","Inner London"),
#                              name = c(1600, 1650, 1700, 1750),
#                              value = c(200000, 375000, 590000, 650000)))

ggplot(LondonPop, aes(x = name, y = value, colour = `Area Name`)) +
  geom_line() + xlab(NULL) + ylab(NULL) +
  labs(title = 'The population of London, 1600 - 2011') +
  scale_y_continuous(labels = scales::comma) +
  theme_base() +
  theme(legend.position = "bottom", legend.title = element_blank())

#######################

Homicide <- read_csv('England and Wales Homicides.csv')

ggplot(Homicide, aes(x = Year, y = Homicides)) +
  geom_col(colour = 'black', fill = 'grey80') + 
  xlab(NULL) + ylab(NULL) +
  labs(title = 'Homicides in England and Wales, 1969 - 2020') +
  theme_base()

HomicideRates <- tibble(City = c('London in 1340', 'London', 'New York', 'Los Angeles', 'Chicago'),
                        Order = c(5,1,2,3,4),
                        Rate = c(20, 1.6, 3.4, 7.0, 24.1))

ggplot(HomicideRates, aes(x = reorder(City, Order), y = Rate)) +
  geom_col(colour = 'black', fill = 'grey80') + 
  ylab(NULL) + xlab(NULL) +
  scale_y_continuous(breaks = c(0,5,10,15,20)) +
  labs(title = 'Homicide rates per 100,000 people') +
  theme_base()
  
#######################

data <- read_csv("CCTV.csv")

data$CCTVpermile <- na_if(data$CCTVpermile, "N/A")
data$CCTVpermile <- as.numeric(data$CCTVpermile)
data$London <- ifelse(data$City == "London", 'L', 0)

data %>%
  select(City, CCTVperperson, London) %>%
  arrange(desc(CCTVperperson)) %>%
  slice_max(CCTVperperson, n = 15) -> dataa

a <- rev(ifelse(dataa$London == 'L', "blue", "grey40"))

ggplot(dataa, aes(x = reorder(City, CCTVperperson), y = CCTVperperson)) +
  geom_col(aes(fill = London, alpha = London)) + 
  scale_fill_manual(values = c('L' = "blue", '0' = "grey40")) +
  scale_alpha_manual(values = c('L' = 1, '0' = 0.5)) +
  scale_y_continuous(limits = c(0, 118)) +
  ylab("# CCTV cameras per 1,000 people") + xlab(NULL) +
  coord_flip() + theme_base(panel_border = element_blank()) + 
  theme(legend.position = "none", axis.text.y = element_text(colour = a),
        axis.ticks = element_blank())


data %>%
  filter(!(Country %in% c('China','India'))) %>%
  select(City, CCTVperperson, London) %>%
  arrange(desc(CCTVperperson)) %>%
  slice_max(CCTVperperson, n = 15) -> datab

b <- c(rep('grey40', 14), 'blue')

ggplot(datab, aes(x = reorder(City, CCTVperperson), y = CCTVperperson)) +
  geom_col(aes(fill = London, alpha = London)) + 
  scale_fill_manual(values = c('L' = "blue", '0' = "grey40")) +
  scale_alpha_manual(values = c('L' = 1, '0' = 0.5)) +
  scale_y_continuous(limits = c(0, 118)) +
  ylab("# CCTV cameras per 1,000 people") + xlab(NULL) +
  coord_flip() + theme_base(panel_border = element_blank()) + 
  theme(legend.position = "none", axis.text.y = element_text(colour = b),
        axis.ticks = element_blank())
