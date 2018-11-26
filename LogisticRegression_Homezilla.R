library(readxl)
Homezilla_Dataset <- read_excel("C:/Users/Sowmya/Desktop/fall2018/IDS515/Homezilla/Homezilla Dataset.xlsx", 
                                       sheet = "62 Listing Properties")
View(Homezilla_Dataset)

listing_properties<-Homezilla_Dataset
str(listing_properties)
is.na(listing_properties)
colnames(listing_properties) <- c("Web_ID", "type", "subtype", "sqfoot", "bedrooms", "bathrooms",
                                  "half_baths", "price",	"status", "last_update")

library(readxl)
Homezilla_Dataset <- read_excel("C:/Users/Sowmya/Desktop/fall2018/IDS515/Homezilla/Homezilla Dataset.xlsx", 
                                       sheet = "Browsing Data")

browsing_data<-Homezilla_Dataset
colnames(browsing_data) <- c("Web_ID",	"Time_Viewed",	"Timestamp",	"Direction",	"Photo_ID",
                             "PhotoTag1",	"PhotoTag2",	"PhotoTag3",	"PhotoTag4",	"PhotoTag5",	"PhotoTag6", "PhotoTag7",	
                             "PhotoTag8",	"UserAgent","CustomerID")

#boxplot(browsing_data$Time_Viewed)
boxplot(browsing_data$Time_Viewed)
outliersBrowse = boxplot.stats(browsing_data$Time_Viewed, coef = 2)$out
browsing_data$Time_Viewed = ifelse(browsing_data$Time_Viewed %in% outliersBrowse, NA, browsing_data$Time_Viewed)
boxplot(browsing_data$Time_Viewed)
head(browsing_data)
ncol(browsing_data)

browsing_data$Direction <- as.factor(browsing_data$Direction)
browsing_data$Direction[is.na(browsing_data$Direction)] <- "First"
levels(browsing_data$Direction)

browsing_data$Time_Viewed[is.na(browsing_data$Time_Viewed)] = mean(browsing_data$Time_Viewed, na.rm=TRUE)
browsing_data$Time_Viewed <- as.factor(ifelse(browsing_data$Time_Viewed >=3, 1,0))
levels(browsing_data$Time_Viewed)

browsing_data$PhotoTag3[is.na(browsing_data$PhotoTag3)] <- "Null"
browsing_data$PhotoTag3<- as.factor(browsing_data$PhotoTag3)

browsing_data$PhotoTag4[is.na(browsing_data$PhotoTag4)] <- "Null"
browsing_data$PhotoTag4<- as.factor(browsing_data$PhotoTag4)

browsing_data$PhotoTag5[is.na(browsing_data$PhotoTag5)] <- "Null"
browsing_data$PhotoTag5<- as.factor(browsing_data$PhotoTag5)

browsing_data$PhotoTag6[is.na(browsing_data$PhotoTag6)] <- "Null"
browsing_data$PhotoTag6<- as.factor(browsing_data$PhotoTag6)

browsing_data$PhotoTag7[is.na(browsing_data$PhotoTag7)] <- "Null"
browsing_data$PhotoTag7<- as.factor(browsing_data$PhotoTag7)

browsing_data$PhotoTag8[is.na(browsing_data$PhotoTag8)] <- "Null"
browsing_data$PhotoTag8<- as.factor(browsing_data$PhotoTag8)

#filter data frame based on subtype
condo_webid<-as.data.frame(listing_properties[ listing_properties$subtype == "Condo Apartment", 1:3])
is.na(condo_webid)
condo_webid<-na.omit(condo_webid)
head(condo_webid)

townhouse<-as.data.frame(listing_properties[ listing_properties$subtype == "Townhouse", 1:3])
is.na(townhouse)
townhouse<-na.omit(townhouse)
head(townhouse)

single_family <- as.data.frame(listing_properties[ 
  listing_properties$subtype == "Single Family Detached", 1:3 ])
is.na(single_family)
single_family<-na.omit(single_family)
head(single_family)

#join with browsing data set
head(browsing_data)
is.na(browsing_data$Web_ID) #no rows without webid
single_web<-merge(single_family, browsing_data, by="Web_ID")
head(single_web)
nrow(single_web)

condo_web<-merge(condo_webid, browsing_data, by="Web_ID")
head(condo_web)
nrow(condo_web)

townHouse_web<-merge(townhouse, browsing_data, by="Web_ID")
head(townHouse_web)
nrow(townHouse_web)

#Model for condo apartment
myFormula<- Time_Viewed ~ Direction  + PhotoTag2 + PhotoTag3 + PhotoTag4 + PhotoTag5 + PhotoTag6 + PhotoTag7 
condo_regression<-glm(myFormula, data = condo_web, family = binomial(logit))
summary(condo_regression)

#modify based on NA
levels(condo_web$PhotoTag4)[levels(condo_web$PhotoTag4)=="machines-visible"] <- "Null"
levels(condo_web$PhotoTag6)[levels(condo_web$PhotoTag6)=="living-window"] <- "Null"
levels(condo_web$PhotoTag3)[levels(condo_web$PhotoTag3)=="tableview"] <- "Null"
levels(condo_web$PhotoTag5)[levels(condo_web$PhotoTag5)=="living-window"] <- "Null"
levels(condo_web$PhotoTag5)[levels(condo_web$PhotoTag5)=="machines-not-visible"] <- "Null"
levels(condo_web$PhotoTag6)[levels(condo_web$PhotoTag6)=="bath-sink"] <- "Null"
levels(condo_web$PhotoTag3)[levels(condo_web$PhotoTag3)=="bath-tub"] <- "Null"
levels(condo_web$PhotoTag3)[levels(condo_web$PhotoTag3)=="machines-visible"] <- "Null"

myFormula<- Time_Viewed ~ Direction  + PhotoTag2 + PhotoTag2 + PhotoTag3     
condo_regression<-glm(myFormula, data = condo_web, family = binomial(logit))
summary(condo_regression)


#Predict the factors affecting single_family house types
myFormula<- Time_Viewed ~ Direction  + PhotoTag2 + PhotoTag3 + PhotoTag4 + PhotoTag5 
single_regression<-glm(myFormula, data = single_web, family =binomial(logit))
summary(single_regression)


#Model for TownHouse
myFormula<- Time_Viewed ~ Direction + PhotoTag2 + PhotoTag3 + PhotoTag4   
town_regression<-glm(myFormula, data = townHouse_web, family = binomial(logit))
summary(town_regression)

#modify based on NA
levels(townHouse_web$PhotoTag3)[levels(townHouse_web$PhotoTag3)=="machines-visible"] <- "Null"
