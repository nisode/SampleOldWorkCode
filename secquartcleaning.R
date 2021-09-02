#Relevant Package Installations
install.packages('tm')
install.packages('dplyr')
install.packages('stringr')
devtools::install_github("hadley/ggplot2")
install.packages('zipcode')
install.packages('xlsx')
#Library Relevant Packages
library(dplyr)
library(stringr)
library(tm)
library(ggplot2)
library(zipcode)
library(xlsx)

#Date Restructuring to fit R Formatting
#Pulling apart individual date charateristics
merged_df$filing_date<-paste(substr(merged_df$filing_date,6,7), substr(merged_df$filing_date,9,10), substr(merged_df$filing_date,1,4),sep="/")
#Putting Date characteristics into R Format
merged_df$filing_date<-as.Date(merged_df$filing_date, format="%m/%d/%Y")
summary(merged_df$filing_date)

merged_df$dateIncorporation<-paste(substr(merged_df$dateIncorporation,6,7), substr(merged_df$dateIncorporation,9,10), substr(merged_df$dateIncorporation,1,4),sep="/")
merged_df$dateIncorporation<-as.Date(merged_df$dateIncorporation, format="%m/%d/%Y")
summary(merged_df$dateIncorporation)

merged_df$deadlineDate<-paste(substr(merged_df$deadlineDate,6,7), substr(merged_df$deadlineDate,9,10), substr(merged_df$deadlineDate,1,4),sep="/")
merged_df$deadlineDate<-as.Date(merged_df$deadlineDate, format="%m/%d/%Y")
summary(merged_df$deadlineDate)

#Portal Data Cleaning
#removing punctuation
merged_df$companyName <- gsub("[[:punct:][:blank:]]+", " ", merged_df$companyName)
#turn all names to lower case
merged_df$companyName <- sapply(merged_df$companyName, tolower)
#removing certain word differences in names
stopwords = c("llc", "portal", "inc", "securities", "funding", "dba", "republic", "quot")
merged_df$companyName <- removeWords(merged_df$companyName,stopwords)
#replacing some remaining names
merged_df <- merged_df %>% mutate(companyName = str_replace(companyName, "seedinvest", "si"))
#removing remaining spaces
merged_df$companyName <- gsub(" ", "", merged_df$companyName, fixed = TRUE)
#postname corrections
merged_df <- merged_df %>% mutate(companyName = str_replace(companyName, "gothamballers", "startenginecapital"))
merged_df <- merged_df %>% mutate(companyName = str_replace(companyName, "alkanetruckcompany", "startenginecapital"))
merged_df <- merged_df %>% mutate(companyName = str_replace(companyName, "viosnutrition", "startenginecapital"))
merged_df <- merged_df %>% mutate(companyName = str_replace(companyName, "bargible", "startenginecapital"))
merged_df <- merged_df %>% mutate(companyName = str_replace(companyName, "betterworldspirits", "startenginecapital"))
merged_df <- merged_df %>% mutate(companyName = str_replace(companyName, "anjedafitness", "startenginecapital"))
merged_df <- merged_df %>% mutate(companyName = str_replace(companyName, "choresrus", "startenginecapital"))
merged_df <- merged_df %>% mutate(companyName = str_replace(companyName, "marciemcgovern", "fundingwonder"))
merged_df <- merged_df %>% mutate(companyName = str_replace(companyName, "seriesone", "fundingwonder"))
merged_df <- merged_df %>% mutate(companyName = str_replace(companyName, "glnholdings", "startenginecapital"))
merged_df <- merged_df %>% mutate(companyName = str_replace(companyName, "hylete", "startenginecapital"))
merged_df <- merged_df %>% mutate(companyName = str_replace(companyName, "hypersubplatformtechnologies", "startenginecapital"))
merged_df <- merged_df %>% mutate(companyName = str_replace(companyName, "ingendynamics", "startenginecapital"))
merged_df <- merged_df %>% mutate(companyName = str_replace(companyName, "itft", "startenginecapital"))
merged_df <- merged_df %>% mutate(companyName = str_replace(companyName, "jauvtisengineering", "startenginecapital"))
merged_df <- merged_df %>% mutate(companyName = str_replace(companyName, "ksdaq", "mrcrowd"))
merged_df <- merged_df %>% mutate(companyName = str_replace(companyName, "jetpackaviation", "startenginecapital"))
merged_df <- merged_df %>% mutate(companyName = str_replace(companyName, "lincolngrant", "startenginecapital"))
merged_df <- merged_df %>% mutate(companyName = str_replace(companyName, "solboards", "startenginecapital"))
merged_df <- merged_df %>% mutate(companyName = str_replace(companyName, "vanguardmoto", "startenginecapital"))
merged_df <- merged_df %>% mutate(companyName = str_replace(companyName, "zoi", "startenginecapital"))

#Dataframe of Portal Distribution
portaldistribution_df <- as.data.frame(table(merged_df$companyName))
portaldistribution_df
#Number of Unique Portals in Merged length comparisons
length(unique(merged_df$companyName))
length(unique(merged_df$commissionCik))
length(unique(merged_df$crdNumber))

#UniquePortals vs Quarters
#Requires SECdfMerge.R to be run beforehand
#gather all the previous partially merged data per quarter to a single list
quarter_list <- list(sec2016_q2, sec2016_q3, sec2016_q4, sec2017_q1, sec2017_q2, sec2017_q3, sec2017_q4, sec2018_q1, sec2018_q2, sec2018_q3)
#apply previous text triming techniques to loop through entire list
newquarter_list <- lapply(quarter_list, function(x) {
  x$companyName <- gsub("[[:punct:][:blank:]]+", " ", x$companyName)
  x$companyName <- sapply(x$companyName, tolower)
  stopwords = c("llc", "portal", "inc", "securities", "funding", "dba", "republic", "quot")
  x$companyName <- removeWords(x$companyName,stopwords)
  x <- x %>% mutate(companyName = str_replace(companyName, "seedinvest", "si"))
  x$companyName <- gsub(" ", "", x$companyName, fixed = TRUE)
  x <- x %>% mutate(companyName = str_replace(companyName, "gothamballers", "startenginecapital"))
  x <- x %>% mutate(companyName = str_replace(companyName, "alkanetruckcompany", "startenginecapital"))
  x <- x %>% mutate(companyName = str_replace(companyName, "viosnutrition", "startenginecapital"))
  x <- x %>% mutate(companyName = str_replace(companyName, "bargible", "startenginecapital"))
  x <- x %>% mutate(companyName = str_replace(companyName, "betterworldspirits", "startenginecapital"))
  x <- x %>% mutate(companyName = str_replace(companyName, "anjedafitness", "startenginecapital"))
  x <- x %>% mutate(companyName = str_replace(companyName, "choresrus", "startenginecapital"))
  x <- x %>% mutate(companyName = str_replace(companyName, "marciemcgovern", "fundingwonder"))
  x <- x %>% mutate(companyName = str_replace(companyName, "seriesone", "fundingwonder"))
  x <- x %>% mutate(companyName = str_replace(companyName, "glnholdings", "startenginecapital"))
  x <- x %>% mutate(companyName = str_replace(companyName, "hylete", "startenginecapital"))
  x <- x %>% mutate(companyName = str_replace(companyName, "hypersubplatformtechnologies", "startenginecapital"))
  x <- x %>% mutate(companyName = str_replace(companyName, "ingendynamics", "startenginecapital"))
  x <- x %>% mutate(companyName = str_replace(companyName, "itft", "startenginecapital"))
  x <- x %>% mutate(companyName = str_replace(companyName, "jauvtisengineering", "startenginecapital"))
  x <- x %>% mutate(companyName = str_replace(companyName, "ksdaq", "mrcrowd"))
  x <- x %>% mutate(companyName = str_replace(companyName, "jetpackaviation", "startenginecapital"))
  x <- x %>% mutate(companyName = str_replace(companyName, "lincolngrant", "startenginecapital"))
  x <- x %>% mutate(companyName = str_replace(companyName, "solboards", "startenginecapital"))
  x <- x %>% mutate(companyName = str_replace(companyName, "vanguardmoto", "startenginecapital"))
  x <- x %>% mutate(companyName = str_replace(companyName, "zoi", "startenginecapital"))
  return(x)
})
#Creating UniquePortal vs Quarter Data Frame
SECQuart <- c("2016 Q2", "2016 Q3", "2016 Q4", "2017 Q1", "2017 Q2", "2017 Q3", "2017 Q4", "2018 Q1", "2018 Q2", "2018 Q3")
SECQuart
UniquePortals <- c(length(unique(newquarter_list[[1]]$companyName)), length(unique(newquarter_list[[2]]$companyName)), length(unique(newquarter_list[[3]]$companyName)), length(unique(newquarter_list[[4]]$companyName)), length(unique(newquarter_list[[5]]$companyName)), length(unique(newquarter_list[[6]]$companyName)), length(unique(newquarter_list[[7]]$companyName)), length(unique(newquarter_list[[8]]$companyName)), length(unique(newquarter_list[[9]]$companyName)), length(unique(newquarter_list[[10]]$companyName)))
UniquePortals
PortalsvsQuart_df <- data.frame(SECQuart, UniquePortals)
#Plotting Unique portal distribution over Quarters of Time
UniquePortalplot <- ggplot(PortalsvsQuart_df, aes(SECQuart, UniquePortals))
UniquePortalplot <- UniquePortalplot + geom_bar(stat = "identity", fill = "steelblue")
UniquePortalplot <- UniquePortalplot + ggtitle("Unique Portals vs Quarter")
UniquePortalplot <- UniquePortalplot + xlab("Quarter")
UniquePortalplot <- UniquePortalplot + ylab("Unique Portals")
print(UniquePortalplot)

uniquecik <- c(length(unique(newquarter_list[[1]]$cik)), length(unique(newquarter_list[[2]]$cik)), length(unique(newquarter_list[[3]]$cik)), length(unique(newquarter_list[[4]]$cik)), length(unique(newquarter_list[[5]]$cik)), length(unique(newquarter_list[[6]]$cik)), length(unique(newquarter_list[[7]]$cik)), length(unique(newquarter_list[[8]]$cik)), length(unique(newquarter_list[[9]]$cik)), length(unique(newquarter_list[[10]]$cik)))
cikvsQuart_df <- data.frame(SECQuart, Uniquecik)
Uniquecikplot <- ggplot(cikvsQuart_df, aes(SECQuart, Uniquecik))
Uniquecikplot <- Uniquecikplot + geom_bar(stat = "identity", fill = "steelblue")
Uniquecikplot <- Uniquecikplot + ggtitle("Unique cik vs Quarter")
Uniquecikplot <- Uniquecikplot + xlab("Quarter")
Uniquecikplot <- Uniquecikplot + ylab("Unique cik")
print(Uniquecikplot)

uniquecommissionCik <- c(length(unique(newquarter_list[[1]]$commissionCik)), length(unique(newquarter_list[[2]]$commissionCik)), length(unique(newquarter_list[[3]]$commissionCik)), length(unique(newquarter_list[[4]]$commissionCik)), length(unique(newquarter_list[[5]]$commissionCik)), length(unique(newquarter_list[[6]]$commissionCik)), length(unique(newquarter_list[[7]]$commissionCik)), length(unique(newquarter_list[[8]]$commissionCik)), length(unique(newquarter_list[[9]]$commissionCik)), length(unique(newquarter_list[[10]]$commissionCik)))
commissionCikvsQuart_df <- data.frame(SECQuart, uniquecommissionCik)
UniquecommissionCikplot <- ggplot(commissionCikvsQuart_df, aes(SECQuart, uniquecommissionCik))
UniquecommissionCikplot <- UniquecommissionCikplot + geom_bar(stat = "identity", fill = "steelblue")
UniquecommissionCikplot <- UniquecommissionCikplot + ggtitle("Unique commissionCik vs Quarter")
UniquecommissionCikplot <- UniquecommissionCikplot + xlab("Quarter")
UniquecommissionCikplot <- UniquecommissionCikplot + ylab("Unique commissionCik")
print(UniquecommissionCikplot)

#Zipcode cleaning
merged_df$zipCode <- clean.zipcodes(merged_df$zipCode)
merged_df$zipCode <- as.character(merged_df$zipCode)
merged_df$zipCode <- paste("",merged_df$zipCode)

merged_df1.01 <- merged_df

#Single Zipcode Correction from '1'
merged_df1.01[287, 46] = 92101
merged_df1.01[379, 46] = 92101

write.csv(merged_df1.01, file = "merged_df1.01.csv")

merged_df1.02 <- merged_df1.01[merged_df1.01$submission_type == "C",]
length(merged_df1.02$accession_number)
portaldistribution_df2 <- as.data.frame(table(merged_df1.02$companyName))
portaldistribution_df2

portaltotal3 <- portaldistribution_df2$Freq[portaldistribution_df2$Var1 %in% c('wefunder','startenginecapital','opendeal')]
sum(portaltotal3)/(sum(portaldistribution_df2$Freq))

write.csv(merged_df1.02, file = "merged_df1.02.csv")

write.xlsx(merged_df1.02, 'merged_df1.02.xlsx')

merged_df1.03<- merged_df1.02[!duplicated(merged_df1.02[c('companyName', 'filing_date', 'nameOfIssuer', 'securityOfferedType')]),]
write.csv(merged_df1.03, file = "merged_df1.03.txt")
write.xlsx(merged_df1.03, 'merged_df1.03.xlsx')

length(merged_df1.03$accession_number)
portaldistribution_df3 <- as.data.frame(table(merged_df1.03$companyName))
portaldistribution_df3

as.data.frame(table(merged_df1.03$nameOfIssuer))

length(unique(merged_df1.03$cik))

merged_df1.04 <- merged_df1.01

#nameOfIssuer Data Cleaning
#removing punctuation
merged_df1.04$nameOfIssuer <- gsub("[[:punct:][:blank:]]+", " ", merged_df1.04$nameOfIssuer)
#turn all names to lower case
merged_df1.04$nameOfIssuer <- sapply(merged_df1.04$nameOfIssuer, tolower)
#removing certain word differences in names
stopwords2 = c("llc", "portal", "inc", "securities", "funding", "dba", "holdings", "quot", "co", "company", "the", "and", "incorporated", "corp", "corporation", "l l c", "waterblocks")
merged_df1.04$nameOfIssuer <- removeWords(merged_df1.04$nameOfIssuer,stopwords2)
#replacing some remaining names
merged_df1.04 <- merged_df1.04 %>% mutate(nameOfIssuer = str_replace(nameOfIssuer, "f k a epec biofuels", ""))
merged_df1.04 <- merged_df1.04 %>% mutate(nameOfIssuer = str_replace(nameOfIssuer, "power hero", "ijuze"))
merged_df1.04 <- merged_df1.04 %>% mutate(nameOfIssuer = str_replace(nameOfIssuer, "indeco financial syndicate", "indeco"))
merged_df1.04 <- merged_df1.04 %>% mutate(nameOfIssuer = str_replace(nameOfIssuer, "indeco union", "indeco"))
merged_df1.04 <- merged_df1.04 %>% mutate(nameOfIssuer = str_replace(nameOfIssuer, "great game of real estate", ""))
merged_df1.04 <- merged_df1.04 %>% mutate(nameOfIssuer = str_replace(nameOfIssuer, "james william spencer", "axe a i technologies"))
#removing remaining spaces
merged_df1.04$nameOfIssuer <- gsub(" ", "", merged_df1.04$nameOfIssuer, fixed = TRUE)
#further name cleaning
merged_df1.04 <- merged_df1.04 %>% mutate(nameOfIssuer = str_replace(nameOfIssuer, "serviceunavailabletryagainlater", "gothamballers"))
merged_df1.04 <- merged_df1.04 %>% mutate(nameOfIssuer = str_replace(nameOfIssuer, "jrcenterprisesdentalfixrx", "jrcenterprises"))
merged_df1.04 <- merged_df1.04 %>% mutate(nameOfIssuer = str_replace(nameOfIssuer, "patricksherwin", "gosun"))
merged_df1.04 <- merged_df1.04 %>% mutate(nameOfIssuer = str_replace(nameOfIssuer, "gabal", "gabai"))
merged_df1.04 <- merged_df1.04 %>% mutate(nameOfIssuer = str_replace(nameOfIssuer, "greasebox", "lizzyhae"))
merged_df1.04 <- merged_df1.04 %>% mutate(nameOfIssuer = str_replace(nameOfIssuer, "richardgarriottdecayeux", "portalarium"))
merged_df1.04 <- merged_df1.04 %>% mutate(nameOfIssuer = str_replace(nameOfIssuer, "dermatechbrands", "dermatech"))
merged_df1.04 <- merged_df1.04 %>% mutate(nameOfIssuer = str_replace(nameOfIssuer, "greengearcyclingbikefriday", "greengearcycling"))
merged_df1.04 <- merged_df1.04 %>% mutate(nameOfIssuer = str_replace(nameOfIssuer, "voicevoicemaestroconference", "voicevoice"))
merged_df1.04 <- merged_df1.04 %>% mutate(nameOfIssuer = str_replace(nameOfIssuer, "maestroconference", "voicevoice"))
merged_df1.04 <- merged_df1.04 %>% mutate(nameOfIssuer = str_replace(nameOfIssuer, "startenginecapital", "plantsnap"))
merged_df1.04 <- merged_df1.04 %>% mutate(nameOfIssuer = str_replace(nameOfIssuer, "121c", "spacedivision"))
merged_df1.04 <- merged_df1.04 %>% mutate(nameOfIssuer = str_replace(nameOfIssuer, "digitalcurrencygrowth", "digitalfrontiermarketing"))
merged_df1.04 <- merged_df1.04 %>% mutate(nameOfIssuer = str_replace(nameOfIssuer, "phoenixsettlementsemergentsolarsolutions", "phoenixsettlements"))
merged_df1.04 <- merged_df1.04 %>% mutate(nameOfIssuer = str_replace(nameOfIssuer, "harvestfreshfoodspbc", "everytablepbc"))
merged_df1.04 <- merged_df1.04 %>% mutate(nameOfIssuer = str_replace(nameOfIssuer, "mobilespiketechnologies", "mobilespike"))
merged_df1.04 <- merged_df1.04 %>% mutate(nameOfIssuer = str_replace(nameOfIssuer, "techotel", "bellhoptechnologies"))
merged_df1.04 <- merged_df1.04 %>% mutate(nameOfIssuer = str_replace(nameOfIssuer, "wisepower", "wise"))
merged_df1.04 <- merged_df1.04 %>% mutate(nameOfIssuer = str_replace(nameOfIssuer, "novaaerotechnology", "novoaerotechnology"))
merged_df1.04 <- merged_df1.04 %>% mutate(nameOfIssuer = str_replace(nameOfIssuer, "geopulseexplorations", "geopulseexploration"))
merged_df1.04 <- merged_df1.04 %>% mutate(nameOfIssuer = str_replace(nameOfIssuer, "unclebillystaproom", "ubtaproom"))
merged_df1.04 <- merged_df1.04 %>% mutate(nameOfIssuer = str_replace(nameOfIssuer, "calroo", "famterra"))
merged_df1.04 <- merged_df1.04 %>% mutate(nameOfIssuer = str_replace(nameOfIssuer, "agardmidtown", "gardmidtown"))
merged_df1.04 <- merged_df1.04 %>% mutate(nameOfIssuer = str_replace(nameOfIssuer, "lockinglinebarrierswaterblockswaterblocks", "lockinglinebarrierswaterblocks"))
#df of non duplicated names
merged_df1.05 <- merged_df1.04[!duplicated(merged_df1.04$nameOfIssuer), ]
#df of duplicated cik to check for duplicated names
merged_df1.06 <- merged_df1.05[duplicated(merged_df1.05$cik), ]
#moving changes back to main
merged_df1.01 <- merged_df1.04

write.csv(merged_df1.01, file = "merged_df1.01.csv")

write.xlsx(merged_df1.01, 'merged_df1.01.xlsx')

#zipcode plotting
install.packages('tidyverse')
install.packages('maps')
install.packages('viridis')
install.packages('ggthemes')
install.packages('devtools')
install.packages('Rcpp')
install.packages('digest')
install.packages('rlang')
library(Rcpp)
library(devtools)
devtools::install_github("hrbrmstr/albersusa", force = TRUE)
devtools::install_github("hadley/ggplot2")
install.packages('DT')
install.packages('ggmap')
install.packages('usmap')
devtools::install_github("wmurphyrd/fiftystater")
library(zipcode)
library(tidyverse)
library(maps)
library(viridis)
library(ggthemes)
library(albersusa)
library(DT)
library(knitr)
library(ggmap)
library(usmap)
library(ggplot2)
library(fiftystater)
library(sf)



#Company Distribution by State
#creating DF for company state distribution
statedistribution_df <- as.data.frame(table(merged_df1.05$stateOrCountry))
#removing non US states
statedistribution_df2 <- filter(statedistribution_df, !Var1 %in% c("","Z4","U3","PR","M3","1B"))
colnames(statedistribution_df2) <- c("state", "Freq")
statedistribution_df2
#'Heat' Mapping of Company Distribution by State
heatmapbystate <- plot_usmap(data = statedistribution_df2, values = "Freq", lines = "red") + 
  scale_fill_continuous(low="white", high="blue", name = "Company Distribution (State)") + 
  theme(legend.position = "right") 
heatmapbystate

#Creating a locational variable for Geocoding
merged_df1.07 <- merged_df1.01
merged_df1.07$street1 <- gsub("[[:punct:][:blank:]]+", " ", merged_df1.07$street1)
merged_df1.07$street1 <- as.character(merged_df1.07$street1)
merged_df1.07$city <- as.character(merged_df1.07$city)
merged_df1.07$stateOrCountry <- as.character(merged_df1.07$stateOrCountry) 
merged_df1.07$location <- paste0(merged_df1.07$street1, ", ", merged_df1.07$city, merged_df1.07$stateOrCountry, merged_df1.07$zipCode)

register_google(key = "AIzaSyD2_43O9jK2A6u44CLmqe6XEYUtx2R48HE", second_limit = "50")

#Geocoding
geo <- geocode(location = merged_df1.07$location, output="latlon", source="google")
secgeo1 <- geo
merged_df1.07$lon <- secgeo1$lon
merged_df1.07$lat <- secgeo1$lat

write.xlsx(merged_df1.07, 'merged_df1.07.xlsx')

write.csv(merged_df1.07, file = "merged_df1.07.txt")

merged_df1.08 <- merged_df1.07[merged_df1.07$submission_type=='C', ]

write.xlsx(merged_df1.08, 'merged_df1.08.xlsx')

write.csv(merged_df1.08, file = "merged_df1.08.txt")

#google API map
usamap<-get_map(location=c(-135,20,-60,50), zoom=4, maptype = "roadmap",
             source='google',color='bw')

#Heatmap Geoplot
ggmap(usamap) +
  stat_density2d(data = merged_df1.08 , aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
                 geom = "polygon", size = 0.01, bins = 16) +
  scale_fill_gradient(low = "red", high = "blue") +
  scale_alpha(range = c(0.1, 0.9), guide = FALSE) 

#Individual Point Geoplot
ggmap(usamap) + geom_point(
  aes(x=lon, y=lat, show_guide = TRUE), 
  data=merged_df1.08, alpha=.5, na.rm = T)  + 
  scale_color_gradient(low="beige", high="blue")

#Attempting alternative plotting
install.packages('choroplethr')
library(choroplethr)
devtools::install_github('arilamstein/choroplethrZip@v1.3.0')

epsg <- 102003
US <- st_as_sf(fifty_states, coords =c("long", "lat"), crs = 4326) %>%
  st_transform(epsg) %>%
  cbind(st_coordinates(.))

p <- ggplot() + 
  geom_polygon(data = US, aes(x=X, y = Y, group = group), fill="grey", alpha=0.3) +
  geom_point(data = merged_df1.08, aes(x=merged_df1.08$lon, y=merged_df1.08$lat, size=)) +
  scale_size_continuous(name="Premium",  range=c(0.1,4)) +
  scale_alpha_continuous(range=c(0.1, 0.9)) +
  scale_color_viridis(name = "nameOfIssuer", option="magma") +
  theme_void() +
  coord_sf(crs = epsg, datum = NA) +
  ggtitle("Scores") +
  theme(
    legend.position = c(0.85, 0.8),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm"))
  )

p



zipcodeheatmap <- ggplot()
zipcodeheatmap <- zipcodeheatmap + stat_density2d(data=merged_df1.07, show.legend=FALSE, aes(x=lon, y=lat, fill=..level.., alpha=..level..), geom="polygon", size=2, bins=10)
zipcodeheatmap <- zipcodeheatmap + geom_map(aes(fill = Assault), map = fifty_states)
zipcodeheatmap
sessionInfo()
rmarkdown::render('secquartcleaning.R', output_format = 'word_document')
