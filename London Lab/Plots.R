
library(tidyverse)
library(viridis)

theme_base <- function (base_family = "sans", panel_border = element_rect(fill = NA, colour = "grey20")) {
  theme_bw(base_family = base_family) +
    theme(
      panel.border = panel_border,
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.caption = element_text(hjust = 0, vjust = -1),
      panel.background = element_blank(),
      plot.background = element_blank(),
      legend.background= element_blank(),
      legend.box.background = element_blank(),
      strip.background = element_blank())}

#######################

LondonJobs <- read_csv("https://raw.githubusercontent.com/MatteoTiratelli/LondonLab/main/London%20Lab/London%20sectoral%20employment.csv")

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

ggplot(LondonJobs[LondonJobs$Year<2018 & !(LondonJobs$name %in% c('Primary','Transportation')),], aes(x = Year, y = value, colour = name)) +
  geom_line() + xlab(NULL) + ylab(NULL) +
  labs(title = 'Sectoral employment shares in London, 1971 - 2017', caption = 'Source: GLA Economics Data') +
  scale_y_continuous(breaks = c(0.2, 0.4, 0.6), labels = c('20%','40%','60%')) +
  theme_base() +
  theme(legend.position = "bottom", legend.title = element_blank())

#######################

LondonPop <- read_csv("https://data.london.gov.uk/download/historic-census-population/2c7867e5-3682-4fdd-8b9d-c63e289b92a6/census-historic-population-borough.csv")
LondonPop <- LondonPop[LondonPop$`Area Code` %in% c('UKI1', 'UKI2', 'H'),]
names(LondonPop) <- gsub('Persons-', '', names(LondonPop))
LondonPop$`Area Code` <- NULL

LondonPop$`2021` <- c(3404300, 5395500,8799800)

LondonPop <- pivot_longer(LondonPop, -'Area Name')
LondonPop$name <- as.numeric(LondonPop$name)

#LondonPop <- bind_rows(LondonPop, tibble(`Area Name` = c("Inner London","Inner London","Inner London","Inner London"),
#                              name = c(1600, 1650, 1700, 1750),
#                              value = c(200000, 375000, 590000, 650000)))

ggplot(LondonPop, aes(x = name, y = value, colour = `Area Name`)) +
  geom_line() + xlab(NULL) + ylab(NULL) +
  labs(title = 'The population of London, 1800 - 2021', caption = 'Source: UK Census Data, accessed through https://data.london.gov.uk') +
  scale_y_continuous(labels = scales::comma) +
  theme_base() +
  theme(legend.position = "bottom", legend.title = element_blank())


#######################

ForeignBorn <- tibble(Year = c(1851, 1881, 1911, 1951, 1981, 1991, 2001, 2011, 2021),
                      count = c(9.558497847, 7.427176726, 10.412179132, 8, 24, 27, 27, 36, 37))

ggplot(ForeignBorn, aes(x = Year, y = count)) +
  geom_line() + xlab(NULL) + ylab(NULL) +
  labs(title = 'Percentage of Londoners who were born outside of Britain, 1851 - 2021', caption = 'Source: UK Census Data, accessed through I-CeM') +
  scale_y_continuous(limits = c(0,50), breaks = c(10,20,30,40,50), labels = c('10%','20%','30%', '40%', '50%')) +
  theme_base()

#######################

Homicide <- read_csv('https://raw.githubusercontent.com/MatteoTiratelli/LondonLab/main/London%20Lab/England%20and%20Wales%20Homicides.csv')

ggplot(Homicide, aes(x = Year, y = Homicides)) +
  geom_col(colour = 'black', fill = 'grey80') + 
  xlab(NULL) + ylab(NULL) +
  labs(title = 'Homicides in England and Wales, 1969 - 2020') +
  theme_base()

ggplot(Homicide, aes(x = Year, y = `Homicide rate`)) +
  geom_smooth(colour = 'blue', se = FALSE) + 
  xlab(NULL) + ylab(NULL) +
  labs(title = 'Homicides per 1,000,000 people in England and Wales (smoothed)') +
  theme_base()

HomicideRates <- tibble(City = c('London in 1340', 'Rome 2016', 'London 2020', 'Berlin 2016', 'Paris 2016', 'New York 2021', 'Chicago 2021'),
                        Order = c(7,1,2,3,4,5,6),
                        Rate = c(20, 0.7, 1.45, 1.4, 1.9, 5.91, 29.1))

ggplot(HomicideRates, aes(x = reorder(City, Order), y = Rate)) +
  geom_col(colour = 'black', fill = 'grey80') + 
  ylab(NULL) + xlab(NULL) +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30)) +
  labs(title = 'Homicide rates per 100,000 people') +
  theme_base()

#######################

data <- read_csv("https://raw.githubusercontent.com/MatteoTiratelli/LondonLab/main/London%20Lab/CCTV.csv")

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

#######################

PoliceUSA1 <- tibble(category = c('Responding to noncriminal calls','Traffic','Other crime','Property crime','Proactive','Medical or other', 'Violent crime'),
                     value = c(37,15,15,14,10,6,4))
PoliceUSA1$place <- 'New Orleans'
PoliceUSA2 <- tibble(category = c('Responding to noncriminal calls','Traffic','Other crime','Property crime','Proactive','Medical or other','Violent crime'),
                     value = c(38,13,19,12,7,8,4))
PoliceUSA2$place <- 'Montgomery County'
PoliceUSA3 <- tibble(category = c('Responding to noncriminal calls','Traffic','Other crime','Property crime','Proactive','Medical or other','Violent crime'),
                     value = c(32,19,7,12,18,9,4))
PoliceUSA3$place <- 'Sacramento'
PoliceUSA <- bind_rows(PoliceUSA1, PoliceUSA2, PoliceUSA3)
PoliceUSA$series <- 'NYT, 2020 (Police data)'

PoliceUSA$category <- fct_relevel(as.factor(PoliceUSA$category), 'Property crime','Violent crime','Other crime','Proactive','Traffic','Medical or other','Responding to noncriminal calls')

ggplot(PoliceUSA, aes(fill = category, x = place, y = value)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_viridis(discrete=TRUE) +
  xlab(NULL) + ylab(NULL) + theme_base() + labs(title = 'New York Times, 2020 (Police data)') +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave(filename = "/Users/matteo/Downloads/Figure_1.png", device='png',
       dpi = 300, bg = "transparent",
       width=6, height=3)

PoliceUK <- tibble(category = c('Community work', 'Dealing with incidents','Post incident work','Admin','Travel','Other activities','Briefing/meetings','Custody'),
                   value = c(20,18,6,27,9,9,5,3))
PoliceUK$place <- 'England and Wales'
PoliceUK$series <- 'UK, 2010 (Observation)'

PoliceUK$category <- fct_relevel(as.factor(PoliceUK$category), 'Dealing with incidents', 'Post incident work', 'Custody', 'Community work','Briefing/meetings','Admin','Travel','Other activities')

ggplot(PoliceUK, aes(fill = category, x = place, y = value)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_viridis(discrete=TRUE) +
  xlab(NULL) + ylab(NULL) + theme_base() + labs(title = 'NPIA, 2010 (Direct observation)') +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave(filename = "/Users/matteo/Downloads/Figure_2.png", device='png',
       dpi = 300, bg = "transparent",
       width=4, height=3)

PoliceAA1 <- tibble(category = c('Reducing crime','Investigating crime','Promoting public security','Providing assistance'),
                    value = c(7,44,25,24))
PoliceAA1$place <- 'Merseyside'
PoliceAA2 <- tibble(category = c('Reducing crime','Investigating crime','Promoting public security','Providing assistance'),
                    value = c(5,49,16,30))
PoliceAA2$place <- 'West Yorshire'
PoliceAA3 <- tibble(category = c('Reducing crime','Investigating crime','Promoting public security','Providing assistance'),
                    value = c(4,40,32,24))
PoliceAA3$place <- 'Met Police'
PoliceAA <- bind_rows(PoliceAA1, PoliceAA2, PoliceAA3)
PoliceAA$series <- 'Home Office, 2007 (Activity analysis survey)'

PoliceAA$category <- fct_relevel(as.factor(PoliceAA$category), 'Investigating crime','Reducing crime','Providing assistance','Promoting public security')

ggplot(PoliceAA, aes(fill = category, x = place, y = value)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_viridis(discrete=TRUE) +
  xlab(NULL) + ylab(NULL) + theme_base() + labs(title = 'Home Office, 2007 (Activity analysis survey)') +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave(filename = "/Users/matteo/Downloads/Figure_3.png", device='png',
       dpi = 300, bg = "transparent",
       width=6, height=3)


################

Prisons <- read_csv("https://raw.githubusercontent.com/MatteoTiratelli/LondonLab/main/London%20Lab/Prison%20pop.csv", skip=1)

ggplot(Prisons, aes(x = Year, y = Total)) +
  geom_line(colour = 'blue') +
  scale_x_continuous(limits = c(1900,2020), breaks = seq(1900,2020,20)) +
  xlab(NULL) + ylab(NULL) + theme_base() + labs(title = 'Total prison population in England and Wales')
ggsave(filename = "/Users/matteo/Downloads/Figure_4.png", device='png',
       dpi = 300, bg = "transparent",
       width=6, height=3)

ggplot(Prisons[!is.na(Prisons$Per100000),], aes(x = Year, y = Per100000)) +
  geom_line(colour = 'blue') +
  scale_x_continuous(limits = c(1900,2020), breaks = seq(1900,2020,20)) +
  xlab(NULL) + ylab(NULL) + theme_base() + labs(title = 'Prisoners per 100,000 people in England and Wales')
ggsave(filename = "/Users/matteo/Downloads/Figure_5.png", device='png',
       dpi = 300, bg = "transparent",
       width=6, height=3)

Police <- read_csv("https://raw.githubusercontent.com/MatteoTiratelli/LondonLab/main/London%20Lab/Police%20numbers.csv")
labels <- tibble(x = c(1949,2013),
                 y = c(74000,90000),
                 label = c("<= # Officers","Officers per 100,000 =>"),
                 colour = c('blue','black'))

ggplot(Police[Police$Year>1900,]) +
  geom_line(aes(x = Year, y = Police), colour = 'blue') +
  geom_line(aes(x = Year, y = Rate*500), colour = 'black') +
  scale_y_continuous(labels = scales::comma, name = NULL,
                     sec.axis = sec_axis( trans=~./500, name=NULL)) +
  geom_text(data = labels, aes(x = x, y = y, label = label), colour = labels$colour, size = 3) +
  xlab(NULL) + ylab(NULL) + theme_base() + labs(title = 'Police officers in England and Wales')
ggsave(filename = "/Users/matteo/Downloads/Figure_5.png", device='png',
       dpi = 300, bg = "transparent",
       width=6, height=3)

Crime <- read_csv("https://raw.githubusercontent.com/MatteoTiratelli/LondonLab/main/London%20Lab/Crime.csv")

ggplot(Crime, aes(x = Year, y = CSEW)) +
  geom_line(colour = 'blue') +
  scale_x_continuous(limits = c(1969,2020)) +
  scale_y_continuous(labels = scales::comma) +
  xlab(NULL) + ylab(NULL) + theme_base() + labs(title = 'Estimated number of crimes (CSEW)')
ggsave(filename = "/Users/matteo/Downloads/Figure_5.png", device='png',
       dpi = 300, bg = "transparent",
       width=6, height=3)
