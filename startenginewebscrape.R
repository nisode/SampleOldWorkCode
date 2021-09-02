library(rvest)
library(XML)
library(plyr)
library(dplyr)
library(stringr)
library(tidyverse)
library(xlsx)
library(httr)
library(RCurl)

missing <- c("bloomeryinvestment",
                "gigmor",
                "gametreepbc",
                "nextrx",
                "graphicarmor",
                "turtlewise",
                "pipelinesportsnetwork",
                "seedvolagetech",
                "primoconnect",
                "greensensefarms",
                "rockmanialive",
                "coaldiva",
                "organiponic",
                "starringclarabow",
                "campagnamotors",
                "lincolngrant",
                "vanguardmoto",
                "alkanetruck",
                "hypersubplatformtechnologies",
                "gln",
                "panexkenzamltd",
                "vyllage",
                "dnx7foods",
                "exploride",
                "caleighclover",
                "klawyers",
                "yonduur",
                "vibranttv",
                "filmio",
                "laradasciences",
                "nutripy",
                "sparecs",
                "gofishmarketplace",
                "printfuture",
                "boxx",
                "mumo",
                "bigpictureartists",
                "ezcoco",
                "obronrestaurantwaiter",
                "wami",
                "tetonsgroup",
                "newageautomotiveconceptsdbaperfectpriceauto",
                "perserbid",
                "dermatech",
                "4scored",
                "teachu",
                "karmies",
                "wanglespace",
                "kerringtonhome",
                "vernucokordas",
                "bikekininorthamerica",
                "aro",
                "spacedivision",
                "globaliquidselyqdcom",
                "wolfieservices",
                "atarigamepartners",
                "revohloo",
                "gulfstaraircharters",
                "caknowtechnology",
                "screendoorgreenhouse",
                "boroughfiveslateone",
                "cannakorp",
                "cblworldwideii",
                "threeflipstudios",
                "marketanalyst",
                "fidotvchannel",
                "fullmeta",
                "frtoken",
                "ar2find",
                "sharebert",
                "etelligent",
                "eventjoin",
                "groveworks",
                "livingway",
                "escoboss",
                "timetoken",
                "farrhotelsresorts",
                "vinividivici",
                "shielddevices",
                "starrtupcom",
                "supportmarket",
                "wwwnewtubevideocom",
                "zoptiks",
                "netstairscom",
                "trait",
                "orbislaw",
                "naturaldynamicsystems",
                "edso",
                "molafoods",
                "genlinescom",
                "overlandats",
                "stryd",
                "whitecrossprojects",
                "memberscarclub",
                "usestateplanners",
                "isinamusic",
                "kokuatoken",
                "betterpath",
                "integratedcapitalglobalwineexport",
                "ellisonrains",
                "alignme",
                "creaticsenterprises",
                "symbiotries",
                "wireless1apps",
                "moviesshowtimestickets",
                "yogabody",
                "anniestacks",
                "patientdirected",
                "karecall",
                "diyev",
                "idiva",
                "evarians",
                "yummi",
                "bondfilmplatform",
                "consideredthoughtfully",
                "gigvines",
                "xascale",
                "shiftmobility",
                "concsteel",
                "sentigraph",
                "meetmenow",
                "fullpartner",
                "leapwithalice",
                "335soli",
                "moduleproject")
startenginemergeddf <- merged_df1.08[merged_df1.08$companyName == 'startenginecapital',]
cik <- startenginemergeddf$cik[startenginemergeddf$nameOfIssuer %in% c(missing)]
startenginemergeddf2 <- merged_df[merged_df$companyName == 'startenginecapital',]
startenginemergeddf2 <- startenginemergeddf2[startenginemergeddf2$submission_type == 'C',]
missingse_df <- data.frame(missing, cik)
mergedmissingse_df<- merge(missingse_df, startenginemergeddf2, by="cik")
mergedmissingse_df<- mergedmissingse_df[!duplicated(mergedmissingse_df$missing), ]
mergedmissingse_df <- data.frame(mergedmissingse_df$nameOfIssuer, mergedmissingse_df$cik, mergedmissingse_df$issuerWebsite)
write.xlsx(mergedmissingse_df, "mergedmissingse_df.xlsx")

urls <- list("https://www.startengine.com/qazing",
             "https://www.startengine.com/snapwire-media-inc",
             "https://www.startengine.com/little-red-feather",
             "https://www.startengine.com/sondors-electric-car",
             "https://www.startengine.com/baqua-inc",
             "https://www.startengine.com/jetpack-aviation",
             "https://www.startengine.com/golfboard",
             "https://www.startengine.com/better-world-spirits",
             "https://www.startengine.com/aido",
             "https://www.startengine.com/gothamballers",
             "https://www.startengine.com/itft",
             "https://www.startengine.com/elliptigo",
             "https://www.startengine.com/fat-to-finish",
             "https://www.startengine.com/hylete",
             "https://www.startengine.com/monday-motorbikes",
             "https://www.startengine.com/bargible",
             "https://www.startengine.com/vovioslife",
             "https://www.startengine.com/chorerelief",
             "https://www.startengine.com/zoi",
             "https://www.startengine.com/zoisleep",
             "https://www.startengine.com/magnabid",
             "https://www.startengine.com/slidebelts",
             "https://www.startengine.com/x-craft",
             "https://www.startengine.com/thatchristmasmovie",
             "https://www.startengine.com/instaslim",
             "https://www.startengine.com/next-future-transportation",
             "https://www.startengine.com/storiad,-inc",
             "https://www.startengine.com/miola",
             "https://www.startengine.com/windowmirror",
             "https://www.startengine.com/la-superstars",
             "https://www.startengine.com/geoorbital",
             "https://www.startengine.com/gracedbygrit",
             "https://www.startengine.com/famacash",
             "https://www.startengine.com/baja-united-group",
             "https://www.startengine.com/sharkwheel",
             "https://www.startengine.com/seatxchange",
             "https://www.startengine.com/freespeech",
             "https://www.startengine.com/oper",
             "https://www.startengine.com/ovum-medical-inc",
             "https://www.startengine.com/n-gen-technologies",
             "https://www.startengine.com/keezel",
             "https://www.startengine.com/spendwith",
             "https://www.startengine.com/judobaby",
             "https://www.startengine.com/mockout",
             "https://www.startengine.com/bikefriday",
             "https://www.startengine.com/garageskins",
             "https://www.startengine.com/bringpro",
             "https://www.startengine.com/smash-global",
             "https://www.startengine.com/symmpl",
             "https://www.startengine.com/vitaperk",
             "https://www.startengine.com/quadmarine",
             "https://www.startengine.com/brightlocker",
             "https://www.startengine.com/ovanova",
             "https://www.startengine.com/atmos",
             "https://www.startengine.com/the-real-bloody-mary-co-llc",
             "https://www.startengine.com/care2",
             "https://www.startengine.com/witfoo",
             "https://www.startengine.com/martell-broadcasting-systems",
             "https://www.startengine.com/icfb-productions-llc",
             "https://www.startengine.com/jasperate-inc",
             "https://www.startengine.com/lifebridge-10000-llc",
             "https://www.startengine.com/pax",
             "https://www.startengine.com/wetboard",
             "https://www.startengine.com/ijuze-corporation",
             "https://www.startengine.com/kindkatie",
             "https://www.startengine.com/samsara",
             "https://www.startengine.com/helpp",
             "https://www.startengine.com/paygevity",
             "https://www.startengine.com/affordablecommunityenergyservices",
             "https://www.startengine.com/horror-equity-fund",
             "https://www.startengine.com/stayblcam",
             "https://www.startengine.com/style-station",
             "https://www.startengine.com/plum",
             "https://www.startengine.com/ceres-greens-llc",
             "https://www.startengine.com/yecup",
             "https://www.startengine.com/biohealthonomics",
             "https://www.startengine.com/indeco",
             "https://www.startengine.com/lovebug-probiotics",
             "https://www.startengine.com/waverlylabsinc",
             "https://www.startengine.com/hirejoe",
             "https://www.startengine.com/15thround",
             "https://www.startengine.com/sun-fund-dc",
             "https://www.startengine.com/farm-one",
             "https://www.startengine.com/little-red-feather",
             "https://www.startengine.com/biotech-restorations",
             "https://www.startengine.com/emergentsolarsolutions",
             "https://www.startengine.com/cryptid",
             "https://www.startengine.com/zen",
             "https://www.startengine.com/jinglz",
             "https://www.startengine.com/thegreatgameofrealestate",
             "https://www.startengine.com/solar-direct",
             "https://www.startengine.com/perfict",
             "https://www.startengine.com/totalsource-solutions-inc",
             "https://www.startengine.com/medchain",
             "https://www.startengine.com/storen-technologies-inc",
             "https://www.startengine.com/nexus-e-water",
             "https://www.startengine.com/abstract-tube",
             "https://www.startengine.com/seam-tech-inc",
             "https://www.startengine.com/netobjexinc",
             "https://www.startengine.com/aurora",
             "https://www.startengine.com/anyone-media-network-inc--yayway",
             "https://www.startengine.com/ossic",
             "https://www.startengine.com/firebot",
             "https://www.startengine.com/firebot-suppression",
             "https://www.startengine.com/eliport",
             "https://www.startengine.com/ncshowcase",
             "https://www.startengine.com/litescape",
             "https://www.startengine.com/eclipse-diagnostics",
             "https://www.startengine.com/dlyted",
             "https://www.startengine.com/artichoke",
             "https://www.startengine.com/contendersclothing",
             "https://www.startengine.com/electronixiq",
             "https://www.startengine.com/gyomo",
             "https://www.startengine.com/poundwishes",
             "https://www.startengine.com/life-recovery-systems",
             "https://www.startengine.com/atlis-motor-vehicles",
             "https://www.startengine.com/epigencare",
             "https://www.startengine.com/rentah",
             "https://www.startengine.com/apparent-energy",
             "https://www.startengine.com/win",
             "https://www.startengine.com/unicoin-blockchain",
             "https://www.startengine.com/lisaicelandmyaiexpert",
             "https://www.startengine.com/supporter/",
             "https://www.startengine.com/minthealth",
             "https://www.startengine.com/crowd-coverage",
             "https://www.startengine.com/onesphera",
             "https://www.startengine.com/cannco",
             "https://www.startengine.com/Erndo",
             "https://www.startengine.com/senclo",
             "https://www.startengine.com/citizenhealth",
             "https://www.startengine.com/palmetto-traditions",
             "https://www.startengine.com/wise-llc",
             "https://www.startengine.com/titomirov-vodka-llc",
             "https://www.startengine.com/jwlcoin",
             "https://www.startengine.com/cyrus-the-great-production-company-inc",
             "https://www.startengine.com/cen-inc",
             "https://www.startengine.com/soarin-indoors",
             "https://www.startengine.com/audl",
             "https://www.startengine.com/lgbt-media-inc",
             "https://www.startengine.com/llbusa",
             "https://www.startengine.com/little-starship-productions",
             "https://www.startengine.com/junto-bikes",
             "https://www.startengine.com/novo-aero-technology-inc",
             "https://www.startengine.com/trustabit1",
             "https://www.startengine.com/makeamericalaughagain",
             "https://www.startengine.com/ampere-electric-cars-inc",
             "https://www.startengine.com/dashing",
             "https://www.startengine.com/curaite",
             "https://www.startengine.com/crooru",
             "https://www.startengine.com/mycroft-ai",
             "https://www.startengine.com/dstld",
             "https://www.startengine.com/security-biometrics-corp",
             "https://www.startengine.com/rhino-hide",
             "https://www.startengine.com/flower-turbines",
             "https://www.startengine.com/microgenvet",
             "https://www.startengine.com/flont",
             "https://www.startengine.com/sagoon",
             "https://www.startengine.com/worthyfinancial",
             "https://www.startengine.com/anatomic-global-inc",
             "https://www.startengine.com/ono-3d-inc",
             "https://www.startengine.com/smac",
             "https://www.startengine.com/compete",
             "https://www.startengine.com/dablr",
             "https://www.startengine.com/ecxtech",
             "https://www.startengine.com/gift-jeenie-corp",
             "https://www.startengine.com/diviniawater",
             "https://www.startengine.com/chella",
             "https://www.startengine.com/power2peer",
             "https://www.startengine.com/wcb-productions-llc",
             "https://www.startengine.com/tribal-rides",
             "https://www.startengine.com/virtual-qe",
             "https://www.startengine.com/new-haven-community-solar",
             "https://www.startengine.com/1upgolf",
             "https://www.startengine.com/fuller-real-estate-solutions",
             "https://www.startengine.com/sober-network-inc",
             "https://www.startengine.com/liveshopbuy",
             "https://www.startengine.com/freedom-motors",
             "https://www.startengine.com/anchor-digital",
             "https://www.startengine.com/nowketo",
             "https://www.startengine.com/cityfreighter",
             "https://www.startengine.com/kill-giggles",
             "https://www.startengine.com/magnus-rewards",
             "https://www.startengine.com/world-cycling-league-llc",
             "https://www.startengine.com/timeburst",
             "https://www.startengine.com/rhode-island-organics",
             "https://www.startengine.com/duby")

startenginedata_dflist <- lapply(urls,function(url){
  
webpage <- read_html(url)

portaldata_namehtml <- html_nodes(webpage, 'div.splash-info.text-left.no-percent h3')
portaldata_name <- html_text(portaldata_namehtml)
portaldata_name <- gsub("[\r\n]", " ", portaldata_name)
portaldata_name <- gsub("\\s+", " ", portaldata_name)
portaldata_name <- lapply(portaldata_name, tolower)
portaldata_name <- gsub("the", "", portaldata_name)
portaldata_name <- gsub("inc", "", portaldata_name)
portaldata_name <- gsub("llc.", "", portaldata_name)
portaldata_name <- gsub("[[:punct:]]", "", portaldata_name)
portaldata_name <- gsub(" ", "", portaldata_name)
portaldata_name

portaldata_deschtml <- html_nodes(webpage, 'div.splash-info.text-left.no-percent h4.desc')
portaldata_desc <- html_text(portaldata_deschtml)
portaldata_desc <- gsub("[\r\n]", "", portaldata_desc)
portaldata_desc <- gsub("\\s+", " ", portaldata_desc)
portaldata_desc <- ifelse(length(portaldata_desc) == 0||portaldata_desc == "", NA, portaldata_desc)
portaldata_desc

portaldata_tagshtml <-  html_nodes(webpage,xpath='//*[@id="the-profile"]/div[2]/div[2]/div[2]/h6/div[3]')
portaldata_tags <- html_text(portaldata_tagshtml)
portaldata_tags <- gsub("[\r\n]", " ", portaldata_tags)
portaldata_tags <- gsub("\\s+", " ", portaldata_tags)
portaldata_tags <- ifelse(length(portaldata_tags) == 0||portaldata_tags == "", NA, portaldata_tags)
portaldata_tags

portaldata_lochtml <- html_nodes(webpage,xpath='//*[@id="the-profile"]/div[2]/div[2]/div[2]/h6/div[2]')
portaldata_loc <- html_text(portaldata_lochtml)
portaldata_loc <- gsub("[\r\n]", "", portaldata_loc)
portaldata_loc <- gsub("\\s+", " ", portaldata_loc)
portaldata_loc <- ifelse(length(portaldata_loc) == 0||portaldata_loc == "", NA, portaldata_loc)
portaldata_loc

portaldata_pitchvhtml <- html_nodes(webpage, xpath='/html/body/script[8]')
portaldata_pitchv <- str_split(portaldata_pitchvhtml, "video_url", simplify=TRUE)
portaldata_pitchv <- portaldata_pitchv[2]
portaldata_pitchv <- str_split(portaldata_pitchv, "video_transcript", simplify=TRUE)
portaldata_pitchv <- portaldata_pitchv[1]
portaldata_pitchv <- gsub('"', "", portaldata_pitchv)
portaldata_pitchv <- gsub(':', "", portaldata_pitchv)
portaldata_pitchv <- gsub(',', "", portaldata_pitchv)
portaldata_pitchv <- ifelse(length(portaldata_pitchv) == 0||portaldata_pitchv == "", NA, portaldata_pitchv)
portaldata_pitchv

portaldata_pitchthtml <- html_nodes(webpage, 'div.campaign-story')
portaldata_pitcht <- html_text(portaldata_pitchthtml)
portaldata_pitcht <- gsub("[\r\n]", "", portaldata_pitcht)
portaldata_pitcht <- gsub("\\s+", " ", portaldata_pitcht)
portaldata_pitcht <- gsub("\\\\", "", portaldata_pitcht, fixed=TRUE)
portaldata_pitcht <- str_split(portaldata_pitcht, "@media screen", simplify=TRUE)
portaldata_pitcht <- portaldata_pitcht[1]
portaldata_pitcht <- ifelse(length(portaldata_pitcht) == 0||portaldata_pitcht == "", NA, portaldata_pitcht)

portaldata_docshtml <- html_nodes(webpage,'li.jumbotron.financial-document a')
portaldata_docs <- html_attr(portaldata_docshtml, "href")
portaldata_docs <- paste(portaldata_docs, collapse = " ")
portaldata_docs <- ifelse(length(portaldata_docs) == 0||portaldata_docs == "", NA, portaldata_docs)

portaldata_cikhtml <- html_nodes(webpage,'li.jumbotron.financial-document a')
portaldata_cik <- html_attr(portaldata_cikhtml, "href")
portaldata_cik <- grep("www.sec.gov/cgi-bin/browse-edgar", portaldata_cik, value=TRUE)
portaldata_cik <- str_split(portaldata_cik, "CIK=", simplify=TRUE)
portaldata_cik <- str_split(portaldata_cik, "&owner=", simplify=TRUE)
portaldata_cik <- grep("[[:digit:]]",portaldata_cik, value=TRUE)
portaldata_cik <- portaldata_cik[1]
portaldata_cik <- gsub("(^|[^0-9])0+", "\\1", portaldata_cik, perl = TRUE)

portaldata_managhtml <- html_nodes(webpage,xpath='/html/body/script[8]')
portaldata_managhtml <- gsub('"',"", portaldata_managhtml)
portaldata_manag <- str_split(portaldata_managhtml, "(full_name)", simplify=TRUE)
portaldata_manag <- str_split(portaldata_manag, "(twitter_url)", simplify=TRUE)
portaldata_manag <- grep("bio:", portaldata_manag, value=TRUE)
portaldata_manag <- paste(portaldata_manag, collapse = " ")
portaldata_manag <- gsub("\\", "", portaldata_manag, fixed=TRUE)
portaldata_manag <- gsub("rnrn", "", portaldata_manag)
portaldata_manag <- gsub("title:", " ", portaldata_manag)
portaldata_manag <- gsub("bio:", " ", portaldata_manag)
portaldata_manag <- gsub(":", "", portaldata_manag)
portaldata_manag <- gsub("u0026", "", portaldata_manag)
portaldata_manag <- gsub("\\s+", " ", portaldata_manag)
portaldata_manag <- ifelse(length(portaldata_manag) == 0||portaldata_manag == "", NA, portaldata_manag)

portaldata_inveshtml <- html_nodes(webpage, 'div.banner-stat.col-xs-12')
portaldata_inveshtml <- html_text(portaldata_inveshtml)
portaldata_inveshtml <- grep("Investors", portaldata_inveshtml, value=TRUE)
portaldata_inves <- portaldata_inveshtml[1]
portaldata_inves <- gsub("[\r\n]", "", portaldata_inves)
portaldata_inves <- gsub("Investors", "", portaldata_inves)
portaldata_inves <- gsub(" ", "", portaldata_inves)
portaldata_inves <- ifelse(length(portaldata_inves) == 0||portaldata_inves == "", NA, portaldata_inves)

portaldata_raiseshtml <- html_nodes(webpage, 'div.banner-stat.col-xs-12 h2')
portaldata_raises <- html_text(portaldata_raiseshtml)
portaldata_raises <- grep("\\$", portaldata_raises, value=TRUE)
portaldata_raises <- portaldata_raises[1]
portaldata_raises <- gsub("[\r\n]", " ", portaldata_raises)
portaldata_raises <- gsub(" ", "", portaldata_raises)
portaldata_raises <- ifelse(length(portaldata_raises) == 0||portaldata_raises == "", NA, portaldata_raises)

raisinghtml <- html_nodes(webpage, 'div.action-tabs.clearfix.show-ng')
raising <- html_text(raisinghtml)
raising <- grep("Invest Now", raising, value=TRUE)
raising <- ifelse(length(raising) == 0||raising == "", 0, 1)

portaldata_df = data.frame(nameonportal = portaldata_name, desc = portaldata_desc,
                           tags = portaldata_tags, loc = portaldata_loc,
                           pitchv = portaldata_pitchv, pitcht = portaldata_pitcht,
                           docs = portaldata_docs, cik = portaldata_cik,
                           manag = portaldata_manag,
                           inves = portaldata_inves,
                           raises = portaldata_raises,
                           raising = raising)

})

startenginedata_df <- bind_rows(startenginedata_dflist)
View(startenginedata_df)

startenginedata_df[162,"cik"] = 1741757
startenginedata_df[144,"cik"] = 1740993
startenginedata_df[6,"cik"] = 1688786

startenginedata_df$uniqueid <- c(2001:2186)
startenginedata_df <- startenginedata_df %>% select(uniqueid, everything())

startenginedata_df$raises <- gsub("[\\$,\\+,]", "", startenginedata_df$raises)
startenginedata_df$raises <- as.numeric(startenginedata_df$raises)

write.xlsx(startenginedata_df, 'startenginedata_df.xlsx')

write.csv(startenginedata_df, file = "startenginedata_df.csv")

merged_dfsew <- merged_df1.07[merged_df1.07$companyName == 'startenginecapital',]
merged_dfse <- merge(merged_dfsew, startenginedata_df, by="cik")
merged_dfse <- merged_dfse[, !(names(merged_dfse) %in% "nameOfIssuer.y")]
merged_dfse <- merged_dfse %>%
  select(uniqueid, nameOfIssuer, cik, everything())
View(merged_dfse)

write.xlsx(merged_dfse, 'merged_dfse.xlsx')

merged_dfse2 <- merged_dfse
merged_dfse2$Platform <- 'StartEngine'
merged_dfse2$Platform_Country <- 'US'
merged_dfse2$Successful_Exit <- ifelse(merged_dfse2$offeringAmount <= merged_dfse2$raises, 1, 0)
merged_dfse2$Successful_Exit <- ifelse(is.na(merged_dfse2$Successful_Exit), 0, merged_dfse2$Successful_Exit)
merged_dfse2$Campaign_Duration <- NA
merged_dfse2$numberOfRounds <- NA
merged_dfse2$issuerWebsite <- as.character(merged_dfse2$issuerWebsite)
merged_dfse2$issuerWebsite <- gsub("https://", "" ,merged_dfse2$issuerWebsite)
merged_dfse2$issuerWebsite <- gsub("http://", "" ,merged_dfse2$issuerWebsite)
merged_dfse2$issuerWebsite[merged_dfse2$issuerWebsite == "None"] <- NA
merged_dfse2$Survival <- sapply((merged_dfse2$issuerWebsite), function(x){
  tryCatch(
    {
      !http_error(x, config(followlocation = 0L))
    },
    # ... but if an error occurs, tell me what happened: 
    error=function(error_message) {
      message("")
      message("")
      message(error_message)
      return(0)
    }
  )
})
merged_dfse2$Appointments <- NA
merged_dfse2$PercentEquityAvail <- NA
merged_dfse2$Valuation <- NA
merged_dfse2$TaxRelief <- NA
merged_dfse2$percent <- NA
merged_dfse2$sm <- NA 


merged_dfse2 <- merged_dfse2 %>%
  select(Platform, 
         Platform_Country,
         nameOfIssuer,
         cik, 
         stateOrCountry, 
         dateIncorporation, 
         securityOfferedType, 
         securityOfferedOtherDesc, 
         inves,
         raises,
         Campaign_Duration,
         deadlineDate,
         tags,
         numberOfRounds,
         zipCode,
         Survival,
         Successful_Exit,
         issuerWebsite,
         sm,
         Appointments,
         docs,
         pitchv,
         manag,
         offeringAmount,
         percent,
         PercentEquityAvail,
         Valuation,
         desc,
         TaxRelief,
         everything())

write.xlsx(merged_dfse2, 'StartEngine_Data.xlsx')

ZIP_CBSA <- ZIP_CBSA_122019

ZIP_CBSA <- ZIP_CBSA_122019 %>% group_by(ZIP) %>% top_n(1, TOT_RATIO)

merged_dfse2$zipCode <- as.numeric(merged_dfse2$zipCode)
merged_dfse5 <- merge(merged_dfse2, ZIP_CBSA, by.x="zipCode", by.y="ZIP", all.x=TRUE)
merged_dfse6 <- merge(merged_dfse5, StartEngine_SIC, by.x="cik", by.y="cik", all.x=TRUE)

merged_dfse6$firmAge <- (difftime(merged_dfse6$filing_date, merged_dfse6$dateIncorporation))/365

merged_dfse6$overfunded <- ifelse(merged_dfse6$raises > merged_dfse6$offeringAmount , 1, 0)

merged_dfse6$Campaign_Duration <- difftime(merged_dfse6$deadlineDate, merged_dfse6$filing_date, units="days")

merged_dfse6$inves <- gsub("[[:punct:]]", "", merged_dfse6$inves)

merged_dfse6 <- merged_dfse6 %>%
  group_by(cik) %>%
  arrange(cik, filing_date)

merged_dfse6 <- transform(merged_dfse6, uniqueid=match(cik, unique(cik)))

merged_dfse7 <- merged_dfse6[merged_dfse6$submission_type=='C',]

merged_dfse8 <- merged_dfse7 %>%
  group_by(uniqueid) %>%
  mutate(uniqueid_count=paste(as.character(uniqueid), as.character(seq_len(n())), sep="_"))

merged_dfse8 <- merged_dfse8 %>%
  group_by(cik) %>%
  mutate(totalraise = sum(raises))

merged_dfse8 <- merged_dfse8 %>%
  group_by(cik) %>%
  mutate(totalinves = sum(as.numeric(inves)))

merged_dfse8 <- as.data.frame(merged_dfse8)

write.xlsx(merged_dfse8, 'SE_Data2.xlsx')

merged_dfse3 <- merged_dfse2[merged_dfse2$submission_type=='C',]
merged_dfse3<- merged_dfse3[!duplicated(merged_dfse3$cik),]
merged_dfse3$inves <- gsub("[[:punct:]]", "", merged_dfse3$inves)
merged_dfse3$inves <- as.numeric(merged_dfse3$inves)
View(merged_dfre3)
write.xlsx(merged_dfse3, 'StartEngine_Data1.xlsx')

aggregate(Successful_Exit ~ cut(deadlineDate, "1 year"), merged_dfse3, sum, na.rm=TRUE)
aggregate(raises ~ cut(deadlineDate, "1 year"), merged_dfse3, sum, na.rm=TRUE)
aggregate(raises ~ cut(deadlineDate, "1 year"), merged_dfse3, mean, na.rm=TRUE)
aggregate(inves ~ cut(deadlineDate, "1 year"), merged_dfse3, mean, na.rm=TRUE)
aggregate(Survival ~ cut(deadlineDate, "1 year"), merged_dfse3, mean, na.rm=TRUE)
