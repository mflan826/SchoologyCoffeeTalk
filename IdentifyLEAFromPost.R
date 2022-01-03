library(tidyverse)
library(stringr)

#objective 1: Create a new column that identifies which LEA each post represents so that consultants could filter posts for a specific LEA.
#Objective 2" Within the new column that identifies which LEA a post represents, also identify the "unidentified".
#Specifications: Leave the "subject" and "body" fields so that cross referencing and identifying the unidentified. 

LEAlistWest <- c(
  "Brownsville",
  "Carmichael",
  "Central Greene",
  "Connellsville",
  "Morgan",
  "Southeastern Greene",
  "West Greene",
  "City High",
  "Environmental",
  "Duquesne City",
  "Elizabeth Forward",
  "Fox_Chapel",
  "Hampton Township",
  "Keystone Oaks",
  "Quaker Valley",
  "South Allegheny",
  "West Jefferson Hills",
  "Ellwood City",
  "Jamestown",
  "Keystone",
  "Shenango",
  "Erie City",
  "North East",
  "Northwestern",
  "Warren County",
  "Dubois",
  "DuBois",
  "Redbank Valley",
  "PPS")

LEAlistCentral <- c(
  "Bedford",
  "Berlin",
  "Bradford Area",
  "Coudersport",
  "Galeton",
  "Smethport",
  "Bald Eagle",
  "Keystone Central",
  "Central Fulton",
  "Forbes Road",
  "Juniata Valley",
  "Northeastern York",
  "Boyertown",
  "Brandywine Heights",
  "East Pennsboro",
  "South Middleton",
  "West_Shore",
  "Benton",
  "Line Mountain",
  "Millville",
  "Mount Carmel",
  "Montgomery",
  "Milton Area",
  "Northeast Bradford"
)

LEAlistEast <- c(
  "Hazleton",
  "Wyoming",
  "Abington",
  "Mountain View",
  "Susquehanna Community",
  "Executive",
  "Lincoln Leadership",
  "21st Century Cyber",
  "Achievement House",
  "Interboro",
  "Antonia Pantoja Community",
  "Belmont",
  "Community Academy",
  "Discovery",
  "First Phila Preparatory",
  "Bradford",
  "Franklin Towne",
  "Green Woods",
  "KIPP Dubois",
  "KIPP West Philadelphia",
  "MaST Community",
  "Pan American Academy",
  "Philadelphia Electrical",
  "Russell Byers",
  "Sankofa Freedom",
  "Tacony Academy",
  "Youth Build",
  "Wissahickon",
  "AMY Northwest",
  "Feltonville",
  "Russell_Conwell",
  "Motivation",
  "Parkway West",
  "Paul Robeson",
  "Science Leadership",
  "Vaux Big Picture",
  "Walter B. Saul",
  "William W. Bodine"
)


#Get the data. Change the file within the "" to match the name of the csv export from Padlet.
df <- read.csv("Padlet - Indicator 13 MAGs  Progress Monitoring Coffee Talk.csv")
#Remove unneeded columns
df <- df %>% select(-Attachment,-Author,-Created.At)
#make a list of LEAs to check against and fill values from.

#identify which LEA is represented in each post. Could not get grepl to loop and fill from a list so....violate do not repeat...
#look in subject and body
df$LEA1 <- ifelse (grepl("Northeast Bradford",df$Subject)==TRUE | (grepl("Northeast Bradford",df$Body)==TRUE),"Northeast Bradford","No")
df$LEA2 <- ifelse (grepl("Bradford Area",df$Subject)==TRUE | (grepl("Bradford Area",df$Body)==TRUE),"Bradford Area","No")
df$LEA3 <- ifelse (grepl("Bedford",df$Subject)==TRUE | (grepl("Bedford",df$Body)==TRUE),"Bedford","No")
df$LEA4 <- ifelse (grepl("Berlin",df$Subject)==TRUE | (grepl("Berlin",df$Body)==TRUE),"Berlin","No")
df$LEA5 <- ifelse (grepl("Coudersport",df$Subject)==TRUE | (grepl("Coudersport",df$Body)==TRUE),"Coudersport","No")
df$LEA6 <- ifelse (grepl("Galeton",df$Subject)==TRUE | (grepl("Galeton",df$Body)==TRUE),"Galeton","No")
df$LEA7 <- ifelse (grepl("Smethport",df$Subject)==TRUE | (grepl("Smethport",df$Body)==TRUE),"Smethport","No")
df$LEA8 <- ifelse (grepl("Bald Eagle",df$Subject)==TRUE | (grepl("Bald Eagle",df$Body)==TRUE),"Bald Eagle","No")
df$LEA9 <- ifelse (grepl("Keystone Central",df$Subject)==TRUE | (grepl("Keystone Central",df$Body)==TRUE),"Keystone Central","No")
df$LEA10 <- ifelse (grepl("Central Fulton",df$Subject)==TRUE | (grepl("Central Fulton",df$Body)==TRUE),"Central Fulton","No")
df$LEA11 <- ifelse (grepl("Forbes Road",df$Subject)==TRUE | (grepl("Fordes Road",df$Body)==TRUE),"Forbes Road","No")
df$LEA12 <- ifelse (grepl("Juniata Valley",df$Subject)==TRUE | (grepl("Juniata",df$Body)==TRUE),"Juniata Valley","No")
df$LEA13 <- ifelse (grepl("Northeastern York",df$Subject)==TRUE | (grepl("Northeastern York",df$Body)==TRUE),"Northeastern York","No")
df$LEA14 <- ifelse (grepl("Boyertown",df$Subject)==TRUE | (grepl("Boyertown",df$Body)==TRUE),"Boyertown","No")
df$LEA15 <- ifelse (grepl("East Pennsboro",df$Subject)==TRUE | (grepl("East Pennsboro",df$Body)==TRUE),"East Pennsboro","No")
df$LEA16 <- ifelse (grepl("South Middleton",df$Subject)==TRUE | (grepl("South Middleton",df$Body)==TRUE),"South Middleton","No")
df$LEA17 <- ifelse (grepl("West_Shore",df$Subject)==TRUE | (grepl("West_Shore",df$Body)==TRUE),"West_Shore","No")
df$LEA18 <- ifelse (grepl("Benton",df$Subject)==TRUE | (grepl("Benton",df$Body)==TRUE),"Benton","No")
df$LEA19 <- ifelse (grepl("Line Mountain",df$Subject)==TRUE | (grepl("Line Mountain",df$Body)==TRUE),"Line Mountain","No")
df$LEA20 <- ifelse (grepl("Millville",df$Subject)==TRUE | (grepl("Millville",df$Body)==TRUE),"Millville","No")
df$LEA21 <- ifelse (grepl("Mount Carmel",df$Subject)==TRUE | (grepl("Mount Carmel",df$Body)==TRUE),"Mount Carmel","No")
df$LEA22 <- ifelse (grepl("Montgomery",df$Subject)==TRUE | (grepl("Montgomery",df$Body)==TRUE),"Montgomery","No")
df$LEA24 <- ifelse (grepl("Hazleton",df$Subject)==TRUE | (grepl("Hazelton",df$Body)==TRUE),"Hazleton","No")
df$LEA25 <- ifelse (grepl("Wyoming",df$Subject)==TRUE | (grepl("Wyoming",df$Body)==TRUE),"Wyoming","No")
df$LEA26 <- ifelse (grepl("Abington",df$Subject)==TRUE | (grepl("Abington",df$Body)==TRUE),"Abington","No")
df$LEA27 <- ifelse (grepl("Mountain View",df$Subject)==TRUE | (grepl("Mountain View",df$Body)==TRUE),"Mountain View","No")
df$LEA28 <- ifelse (grepl("Susquehanna Community",df$Subject)==TRUE | (grepl("Susquehanna Community",df$Body)==TRUE),"Susquehanna Community","No")
df$LEA29 <- ifelse (grepl("Executive",df$Subject)==TRUE | (grepl("Executive",df$Body)==TRUE),"Executive","No")
df$LEA30 <- ifelse (grepl("Lincoln Leadership",df$Subject)==TRUE | (grepl("Lincoln Leadership",df$Body)==TRUE),"Lincoln Leadership","No")
df$LEA31 <- ifelse (grepl("21st Century Cyber",df$Subject)==TRUE | (grepl("21st Century Cyber",df$Body)==TRUE),"21st Century Cyber","No")
df$LEA32 <- ifelse (grepl("Achievement House",df$Subject)==TRUE | (grepl("Achievement House",df$Body)==TRUE),"Achievement House","No")
df$LEA33 <- ifelse (grepl("Interboro",df$Subject)==TRUE | (grepl("Interboro",df$Body)==TRUE),"Interboro","No")
df$LEA34 <- ifelse (grepl("Antonia Pantoja",df$Subject)==TRUE | (grepl("Antonia Pantoja",df$Body)==TRUE),"Antonia Pantoja Community","No")
df$LEA35 <- ifelse (grepl("Belmont",df$Subject)==TRUE | (grepl("Belmont",df$Body)==TRUE),"Belmont","No")
df$LEA36 <- ifelse (grepl("Community Academy",df$Subject)==TRUE | (grepl("Community Academy",df$Body)==TRUE),"Community Academy","No")
df$LEA37 <- ifelse (grepl("Discovery",df$Subject)==TRUE | (grepl("Discovery",df$Body)==TRUE),"Discovery","No")
df$LEA38 <- ifelse (grepl("First Phila Preparatory",df$Subject)==TRUE | (grepl("First Phila Preparatory",df$Body)==TRUE),"First Phila Preparatory","No")
df$LEA39 <- ifelse (df$Subject=="Bradford" | df$Subject=="Bradford","Bradford","No")
df$LEA40 <- ifelse (grepl("Franklin Towne",df$Subject)==TRUE | (grepl("Franklin Towne",df$Body)==TRUE),"Franklin Towne","No")
df$LEA41 <- ifelse (grepl("Green Woods",df$Subject)==TRUE | (grepl("Green Woods",df$Body)==TRUE),"Green Woods","No")
df$LEA42 <- ifelse (grepl("KIPP Dubois",df$Subject)==TRUE | (grepl("KIPP Dubois",df$Body)==TRUE),"KIPP Dubois","No")
df$LEA43 <- ifelse (grepl("KIPP West Philadelphia",df$Subject)==TRUE | (grepl("KIPP West Philadelphia",df$Body)==TRUE),"KIPP West Philadelphia","No")
df$LEA44 <- ifelse (grepl("MaST Community",df$Subject)==TRUE | (grepl("MaST Community",df$Body)==TRUE),"MaST Community","No")
df$LEA45 <- ifelse (grepl("Pan American Academy",df$Subject)==TRUE | (grepl("Pan American Academy",df$Body)==TRUE),"Pan American Academy","No")
df$LEA46 <- ifelse (grepl("Philadelphia Electrical",df$Subject)==TRUE | (grepl("Philadelphia Electrical",df$Body)==TRUE),"Philadelphia Electrical","No")
df$LEA47 <- ifelse (grepl("Russell Byers",df$Subject)==TRUE | (grepl("Russel Byers",df$Body)==TRUE),"Russell Byers","No")
df$LEA48 <- ifelse (grepl("Sankofa Freedom",df$Subject)==TRUE | (grepl("Sankofa Freedom",df$Body)==TRUE),"Sankofa Freedom","No")
df$LEA49 <- ifelse (grepl("Tacony Academy",df$Subject)==TRUE | (grepl("Talcony Academy",df$Body)==TRUE),"Tacony Academy","No")
df$LEA50 <- ifelse (grepl("Youth Build",df$Subject)==TRUE | (grepl("Youth Build",df$Body)==TRUE),"Youth Build","No")
df$LEA51 <- ifelse (grepl("Wissahickon",df$Subject)==TRUE | (grepl("Wissahickon",df$Body)==TRUE),"Wissahickon","No")
df$LEA52 <- ifelse (grepl("AMY Northwest",df$Subject)==TRUE | (grepl("AMY Northwest",df$Body)==TRUE),"AMY Northwest","No")
df$LEA53 <- ifelse (grepl("Feltonville",df$Subject)==TRUE | (grepl("Feltonville",df$Body)==TRUE),"Feltonville","No")
df$LEA54 <- ifelse (grepl("Russell_Conwell",df$Subject)==TRUE | (grepl("Russel_Conwell",df$Body)==TRUE),"Russell_Conwell","No")
df$LEA55 <- ifelse (grepl("Motivation",df$Subject)==TRUE | (grepl("Motivation",df$Body)==TRUE),"Motivation","No")
df$LEA56 <- ifelse (grepl("Parkway West",df$Subject)==TRUE | (grepl("Parkway West",df$Body)==TRUE),"Parkway West","No")
df$LEA57 <- ifelse (grepl("Paul Robeson",df$Subject)==TRUE | (grepl("Paul Robeson",df$Body)==TRUE),"Paul Robeson","No")
df$LEA58 <- ifelse (grepl("Science Leadership",df$Subject)==TRUE | (grepl("Science Leadership",df$Body)==TRUE),"Science Leadership","No")
df$LEA59 <- ifelse (grepl("Vaux Big Picture",df$Subject)==TRUE | (grepl("Vaux Big Picture",df$Body)==TRUE),"Vaux Big Picture","No")
df$LEA60 <- ifelse (grepl("Walter B. Saul",df$Subject)==TRUE | (grepl("Walter B. Saul",df$Body)==TRUE),"Walter B. Saul","No")
df$LEA61 <- ifelse (grepl("William W. Bodine",df$Subject)==TRUE | (grepl("William w. Bodine",df$Body)==TRUE),"William W. Bodine","No")
df$LEA62 <- ifelse (grepl("Brownsville",df$Subject)==TRUE | (grepl("Brownsville",df$Body)==TRUE),"Brownsville","No")
df$LEA63 <- ifelse (grepl("Carmichael",df$Subject)==TRUE | (grepl("Carmichael",df$Body)==TRUE),"Carmichael","No")
df$LEA64 <- ifelse (grepl("Central Greene",df$Subject)==TRUE | (grepl("Central Greene",df$Body)==TRUE),"Central Greene","No")
df$LEA65 <- ifelse (grepl("Connellsville",df$Subject)==TRUE | (grepl("Connellsville",df$Body)==TRUE),"Connellsville","No")
df$LEA66 <- ifelse (grepl("Morgan",df$Subject)==TRUE | (grepl("Morgan",df$Body)==TRUE),"Morgan","No")
df$LEA67 <- ifelse (grepl("Southeastern Greene",df$Subject)==TRUE | (grepl("Southeastern Greene",df$Body)==TRUE),"Southeastern Greene","No")
df$LEA68 <- ifelse (grepl("West Greene",df$Subject)==TRUE | (grepl("West Greene",df$Body)==TRUE),"West Greene","No")
df$LEA69 <- ifelse (grepl("City High",df$Subject)==TRUE | (grepl("City High",df$Body)==TRUE),"City High","No")
df$LEA70 <- ifelse (grepl("Environmental",df$Subject)==TRUE | (grepl("Environmental",df$Body)==TRUE),"Environmental","No")
df$LEA71 <- ifelse (grepl("Duquesne City",df$Subject)==TRUE | (grepl("Duquesne City",df$Body)==TRUE),"Duquesne City","No")
df$LEA72 <- ifelse (grepl("Elizabeth Forward",df$Subject)==TRUE | (grepl("Elizabeth Forward",df$Body)==TRUE),"Elizabeth Forward","No")
df$LEA73 <- ifelse (grepl("Fox_Chapel",df$Subject)==TRUE | (grepl("Fox Chapel",df$Body)==TRUE),"Fox_Chapel","No")
df$LEA74 <- ifelse (grepl("Hampton Township",df$Subject)==TRUE | (grepl("Hampton Township",df$Body)==TRUE),"Hampton Township","No")
df$LEA75 <- ifelse (grepl("Keystone Oaks",df$Subject)==TRUE | (grepl("Keystone Oaks",df$Body)==TRUE),"Keystone Oaks","No")
df$LEA76 <- ifelse (grepl("Quaker Valley",df$Subject)==TRUE | (grepl("Quaker Valley",df$Body)==TRUE),"Quaker Valley","No")
df$LEA77 <- ifelse (grepl("South Allegheny",df$Subject)==TRUE | (grepl("South Allegheny",df$Body)==TRUE),"South Allegheny","No")
df$LEA78 <- ifelse (grepl("West Jefferson Hills",df$Subject)==TRUE | (grepl("West Jefferson Hills",df$Body)==TRUE),"West Jefferson Hills","No")
df$LEA79 <- ifelse (grepl("Ellwood City",df$Subject)==TRUE | (grepl("Ellewood City",df$Body)==TRUE),"Ellwood City","No")
df$LEA80 <- ifelse (grepl("Jamestown",df$Subject)==TRUE | (grepl("Jamestown",df$Body)==TRUE),"Jamestown","No")
df$LEA81 <- ifelse (df$Subject=="Keystone" | df$Body=="Keystone" | df$Subject=="keystone" | df$Body=="keystone","Keystone","No")
df$LEA82 <- ifelse (grepl("Shenango",df$Subject)==TRUE | (grepl("Shenango",df$Body)==TRUE),"Shenango","No")
df$LEA83 <- ifelse (grepl("Erie City",df$Subject)==TRUE | (grepl("Erie City",df$Body)==TRUE),"Erie City","No")
df$LEA84 <- ifelse (grepl("North East",df$Subject)==TRUE | (grepl("North East",df$Body)==TRUE),"North East","No")
df$LEA85 <- ifelse (grepl("Northwestern",df$Subject)==TRUE | (grepl("Northwestern",df$Body)==TRUE),"Northwestern","No")
df$LEA86 <- ifelse (grepl("Warren County",df$Subject)==TRUE | (grepl("Warren County",df$Body)==TRUE),"Warren County","No")
df$LEA87 <- ifelse (grepl("Dubois",df$Subject)==TRUE | (grepl("Dubois",df$Body)==TRUE),"Dubois","No")
df$LEA88 <- ifelse (grepl("Redbank Valley",df$Subject)==TRUE | (grepl("Redbank Valley",df$Body)==TRUE),"Redbank Valley","No")
df$LEA89 <- ifelse (grepl("PPS",df$Subject)==TRUE | (grepl("PPS",df$Body)==TRUE),"PPS","No")
df$LEA90 <- ifelse (grepl("Pittsburgh",df$Subject)==TRUE | (grepl("Pittsburgh",df$Body)==TRUE),"PPS","No")
df$LEA91 <- ifelse (grepl("BASD",df$Subject)==TRUE | (grepl("BASD",df$Body)==TRUE),"Bradford Area","No")
df$LEA92 <- ifelse (grepl("MASD",df$Subject)==TRUE | (grepl("MASD",df$Body)==TRUE),"Milton Area","No")
df$LEA93 <- ifelse (grepl("DuBois",df$Subject)==TRUE | (grepl("DuBois",df$Body)==TRUE),"DuBois","No")
df$LEA94 <- ifelse (grepl("Philadelphia Public",df$Subject)==TRUE | (grepl("Philadelphia Public",df$Body)==TRUE),"Philadelphia Public Schools","No")
df$LEA95 <- ifelse (df$Subject=="Mountain View" | df$Body=="Mountain View" | df$Body=="Mt.View" |df$Body=="Mt.View","Mountain View","No")
df$LEA96 <- ifelse (grepl("Brandywine Height",df$Subject)==TRUE | (grepl("Brandywine Height",df$Body)==TRUE),"Brandywine Heights","No")
df$LEA97 <- ifelse (grepl("Milton",df$Subject)==TRUE | (grepl("Milton",df$Body)==TRUE),"Milton Area","No")

#convert no to NA in 7:LEAi
df2 <- df %>%
  as_tibble %>%
  mutate(across(c(4:LEA97),~na_if(.,"No")))

#merge columns
df2 <- df2 %>% unite("LEA",4:LEA97,na.rm = TRUE)

#identify region
df2$region <- ifelse(df2$LEA %in% LEAlistEast,"East","Null")
df2$region2 <- ifelse(df2$LEA %in% LEAlistWest,"West","Null")
df2$region3 <- ifelse(df2$LEA %in% LEAlistCentral,"Central","Null")

#Convert null to na in region columns
df2 <- df2 %>%
  as_tibble %>%
  mutate(across(c(region,region2,region3),~na_if(.,"Null")))

#merge columns
df2 <- df2 %>% unite("Region",region:region3,na.rm=TRUE)

#Export as csv. Change file name to match the course activity.
write.csv(df2,"I13_Schoology_Coffetalk_.csv")
