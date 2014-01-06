setwd("C:/Users/Mohan/Desktop/Intel/")
library(data.table)
library(plyr)
library(gdata)
library(zoo)
# save.image("C:/Users/Mohan/Desktop/Intel/Intel_workspace.RData")

##### declare all variables in small letters
##### use dot instead of underscore

##### Notes  
# Tier 1 are big customers
# Influencer -> cutting edge technology like i7, i8
# embeded -> Existing products
# our model should be built on online influencer and embeded for tier3 and tier4 customers
# online through webinars, events, online ads and generate leads
# covered means whether a sales person is assigned to that particular customer
# OSC - Online Sales Centre (Influencer)
#####



options(stringsAsFactors=FALSE)
accounts <- read.csv("Accounts.csv", header=T, stringsAsFactors=FALSE)
dim(accounts)
accounts$Account.Updated.Date <- as.Date(accounts$Account.Updated.Date, "%b %d, %Y %H:%M:%S %p")
dim(accounts)
accounts <- accounts[order(accounts$Account.Updated.Date, decreasing=T),]
dim(accounts)
accounts <- accounts[!(duplicated(accounts$Account.ID)),]
dim(accounts)

test <- data.frame(table(accounts$Account.ID))
test_s <- subset(test, Freq>2)
dim(test_s)

influencer.account.opp.mapping <- read.csv("influencer.account.opp.mapping.csv", header=T, stringsAsFactors=FALSE)
dim(influencer.account.opp.mapping)
influencer.account.opp.mapping <- influencer.account.opp.mapping[!(is.na(influencer.account.opp.mapping$Opportunity.ID)),]
dim(influencer.account.opp.mapping)

test <- data.frame(table(influencer.account.opp.mapping$Opportunity.ID))
test_s <- subset(test, Freq>1)

infuencer.accounts_mapped <- join(influencer.account.opp.mapping, accounts, type="left")
dim(infuencer.accounts_mapped)

##### Check
mapping_ids <- influencer.account.opp.mapping$Opportunity.ID[which(!(influencer.account.opp.mapping$Account.ID %in% accounts$Account.ID))]


####


emb.account.opp.mapping <- read.csv("emb.account.opp.mapping.csv", header=T, stringsAsFactors=FALSE)
emb.accounts_mapped <- join(accounts, emb.account.opp.mapping, type="inner")


###### Influence
influencer.opportunities.tiger.extract <- read.csv("Influencer Opportunities Tiger Extract.csv", header=T, stringsAsFactors=FALSE)
influencer.opportunities.tiger.extract_dup <- influencer.opportunities.tiger.extract
#check <- subset(influencer.opportunities.tiger.extract_dup, Opportunity.Category=="OSC Influencer")
#table(check$Opportunity.Status)
dim(influencer.opportunities.tiger.extract)
influencer.product.tiger.extract <- read.csv("Influencer Product Tiger Extract.csv", header=T, stringsAsFactors=FALSE)
dim(influencer.product.tiger.extract)

#### Remove Duplicated opportunities
influencer.opportunities.tiger.extract <- influencer.opportunities.tiger.extract[!(duplicated(influencer.opportunities.tiger.extract$Opportunity.ID)),]
#check <- subset(influencer.opportunities.tiger.extract, Opportunity.Category=="OSC Influencer")
#table(check$Opportunity.Status)
dim(influencer.opportunities.tiger.extract)
####################################

#### Further check on opportunity ids missing in product ids
missing_ids <- influencer.opportunities.tiger.extract$Opportunity.ID[!(influencer.opportunities.tiger.extract$Opportunity.ID %in% influencer.product.tiger.extract$Opportunity.ID)]
missing_df <- influencer.opportunities.tiger.extract[which(influencer.opportunities.tiger.extract$Opportunity.ID%in%missing_ids),]
missing_df_osc <- subset(missing_df, Opportunity.Category=="OSC Influencer")
table(missing_df_osc$Opportunity.Status)
other_missing <-  influencer.product.tiger.extract$Opportunity.ID[!(influencer.product.tiger.extract$Opportunity.ID %in% influencer.opportunities.tiger.extract$Opportunity.ID)]


influencer.interm <- join(influencer.product.tiger.extract, influencer.opportunities.tiger.extract, by="Opportunity.ID", type="left")
dim(influencer.interm)
#check <- subset(influencer.interm, Opportunity.Category=="OSC Influencer")
influencer.final <- join(influencer.interm, infuencer.accounts_mapped, by="Opportunity.ID" ,type="left")
dim(influencer.final)
influencer.osc <- subset(influencer.final, Opportunity.Category=="OSC Influencer")
dim(influencer.osc)
write.csv(influencer.osc, "Influencer_OSC.csv")

######## Recoding variables 
influencer.osc$Opportunity.Created.Intel.Quarter <- as.yearqtr(influencer.osc$Opportunity.Created.Intel.Quarter)
influencer.osc$Opportunity.Decision.Intel.Quarter <- as.yearqtr(influencer.osc$Opportunity.Decision.Intel.Quarter)
influencer.osc$lag <- influencer.osc$Opportunity.Decision.Intel.Quarter - influencer.osc$Opportunity.Created.Intel.Quarter
Lost <- c("Canceled", "Loss Approved", "Loss Submitted", "Lost")
Won <- c("Win Approved")
Pending <- c("Pending", "Pending Red", "Win Disapproved", "Win Submitted")
influencer.osc$Opportunity.Status.recoded <- influencer.osc$Opportunity.Status
influencer.osc$Opportunity.Status.recoded[influencer.osc$Opportunity.Status %in% Lost] <- "Lost" 
influencer.osc$Opportunity.Status.recoded[influencer.osc$Opportunity.Status %in% Won] <- "Won"
influencer.osc$Opportunity.Status.recoded[influencer.osc$Opportunity.Status %in% Pending] <- "Pending"


influencer.osc$Opportunity.Status.levels <- "Level 4"
influencer.osc$Opportunity.Status.levels[which(influencer.osc$lag==0 & influencer.osc$Opportunity.Status.recoded=="Won")] <- "Level 1"
influencer.osc$Opportunity.Status.levels[which(influencer.osc$lag>0 & influencer.osc$Opportunity.Status.recoded=="Won")] <- "Level 2"
influencer.osc$Opportunity.Status.levels[which(influencer.osc$Opportunity.Status.recoded=="Lost")] <- "Level 3"

########### lag distribution
lag_df <- data.frame(Variable=c("Win", "Loss", "Pending"))
lag_df$start <- 0
lag_df$lag1 <- 0
lag_df$lag2 <- 0
lag_df$lag3 <- 0
lag_df$lag4 <- 0

##### Date Distribution for Q4 2011 to Q4 2012
influencer.osc.modified <- subset(influencer.osc, Opportunity.Created.Intel.Quarter == "2011 Q4" | Opportunity.Created.Intel.Quarter == "2012 Q1" |
                                      Opportunity.Created.Intel.Quarter == "2012 Q2" | Opportunity.Created.Intel.Quarter == "2012 Q3" |
                                      Opportunity.Created.Intel.Quarter == "2012 Q4",
                                select=c(Opportunity.Created.Intel.Quarter, Opportunity.Decision.Intel.Quarter, Opportunity.Status.recoded))
influencer.osc.modified$diff <- influencer.osc.modified$Opportunity.Decision.Intel.Quarter - influencer.osc.modified$Opportunity.Created.Intel.Quarter
date_lags <- table(influencer.osc.modified$Opportunity.Status.recoded, influencer.osc.modified$diff)
write.csv(date_lags, "date distribution.csv")

##### Date Distribution for Q1 2013
influencer.osc.modified.2013Q1 <- subset(influencer.osc, Opportunity.Created.Intel.Quarter == "2013 Q1",
                                select=c(Opportunity.Created.Intel.Quarter, Opportunity.Decision.Intel.Quarter, Opportunity.Status.recoded))
influencer.osc.modified.2013Q1$diff <- influencer.osc.modified.2013Q1$Opportunity.Decision.Intel.Quarter - influencer.osc.modified.2013Q1$Opportunity.Created.Intel.Quarter
date_lags_2013Q1 <- table(influencer.osc.modified.2013Q1$Opportunity.Status.recoded, influencer.osc.modified.2013Q1$diff)


##### Date Distribution for Q2 2013
influencer.osc.modified.2013Q2 <- subset(influencer.osc, Opportunity.Created.Intel.Quarter == "2013 Q2",
                                select=c(Opportunity.Created.Intel.Quarter, Opportunity.Decision.Intel.Quarter, Opportunity.Status.recoded))
influencer.osc.modified.2013Q2$diff <- influencer.osc.modified.2013Q2$Opportunity.Decision.Intel.Quarter - influencer.osc.modified.2013Q2$Opportunity.Created.Intel.Quarter
date_lags_2013Q2 <- table(influencer.osc.modified.2013Q2$Opportunity.Status.recoded, influencer.osc.modified.2013Q2$diff)


##### Date Distribution for Q3 2013
influencer.osc.modified.2013Q3 <- subset(influencer.osc, Opportunity.Created.Intel.Quarter == "2013 Q3",
                                select=c(Opportunity.Created.Intel.Quarter, Opportunity.Decision.Intel.Quarter, Opportunity.Status.recoded))
influencer.osc.modified.2013Q3$diff <- influencer.osc.modified.2013Q3$Opportunity.Decision.Intel.Quarter - influencer.osc.modified.2013Q3$Opportunity.Created.Intel.Quarter
date_lags_2013Q3 <- table(influencer.osc.modified.2013Q3$Opportunity.Status.recoded, influencer.osc.modified.2013Q3$diff)


##### Date Distribution for Q4 2013
influencer.osc.modified.2013Q4 <- subset(influencer.osc, Opportunity.Created.Intel.Quarter == "2013 Q4",
                                select=c(Opportunity.Created.Intel.Quarter, Opportunity.Decision.Intel.Quarter, Opportunity.Status.recoded))
influencer.osc.modified.2013Q4$diff <- influencer.osc.modified.2013Q4$Opportunity.Decision.Intel.Quarter - influencer.osc.modified.2013Q4$Opportunity.Created.Intel.Quarter
date_lags_2013Q4 <- table(influencer.osc.modified.2013Q4$Opportunity.Status.recoded, influencer.osc.modified.2013Q4$diff)

write.table(date_lags_2013Q1, "date lags for 2013 data.csv", sep=",")
write.table(date_lags_2013Q2, "date lags for 2013 data.csv", sep=",", append=T)
write.table(date_lags_2013Q3, "date lags for 2013 data.csv", sep=",", append=T)
write.table(date_lags_2013Q4, "date lags for 2013 data.csv", sep=",", append=T)
#####################################################################################################################################################################


############### Revenue recoded
influencer.osc$Est.Rev..k.recoded <- influencer.osc$Est.Rev..k
influencer.osc$Est.Rev..k.recoded[which(influencer.osc$Est.Rev..k <= 0.3)] <- "Level 1"
influencer.osc$Est.Rev..k.recoded[which(influencer.osc$Est.Rev..k > 0.3 & influencer.osc$Est.Rev..k <= 1)] <- "Level 2"
influencer.osc$Est.Rev..k.recoded[which(influencer.osc$Est.Rev..k > 1 & influencer.osc$Est.Rev..k <= 2)] <- "Level 3"
influencer.osc$Est.Rev..k.recoded[which(influencer.osc$Est.Rev..k > 2 & influencer.osc$Est.Rev..k <= 10)] <- "Level 4"
influencer.osc$Est.Rev..k.recoded[which(influencer.osc$Est.Rev..k > 10)] <- "Level 5"

############### Account Geo Analysis
influencer.osc$Account.Geo.recoded <- influencer.osc$Account.Geo
influencer.osc$Account.Geo.recoded[is.na(influencer.osc$Account.Geo.recoded)] <- "EMPTY"
table(influencer.osc$Account.Geo.recoded)
sum(table(influencer.osc$Account.Geo.recoded))
table(influencer.osc$Account.Geo.recoded, influencer.osc$Opportunity.Status.levels)
sum(table(influencer.osc$Opportunity.Status.levels, influencer.osc$Account.Geo.recoded))

############### Oppportunity Emerging Market
table(influencer.osc$Oppportunity.Emerging.Market, influencer.osc$Opportunity.Status.levels)
sum(table(influencer.osc$Oppportunity.Emerging.Market, influencer.osc$Opportunity.Status.levels))

##############  Competitive OEM
comp.oem <- table(influencer.osc$Competitive.OEM, influencer.osc$Opportunity.Status.levels)
sum(table(influencer.osc$Competitive.OEM, influencer.osc$Opportunity.Status.levels))

############## Competitive OEM date lags

influencer.osc$Competitive.OEM.recoded <- influencer.osc$Competitive.OEM
influencer.osc$Competitive.OEM.recoded[which(influencer.osc$Competitive.OEM.recoded=="")] <- "Not Specified"
influencer.osc$Competitive.OEM.recoded[which(influencer.osc$Competitive.OEM.recoded!="Not Specified" & influencer.osc$Competitive.OEM.recoded!="No Competitor")] <- "Competitor"

table(influencer.osc$Competitive.OEM.recoded, influencer.osc$Opportunity.Status.levels)

oem.comp.lags <- subset(influencer.osc, Competitive.OEM.recoded == "No Competitor",
                                select=c(Opportunity.Created.Intel.Quarter, Opportunity.Decision.Intel.Quarter, Opportunity.Status.recoded))
oem.comp.lags$diff <- oem.comp.lags$Opportunity.Decision.Intel.Quarter - oem.comp.lags$Opportunity.Created.Intel.Quarter
date_lags_oem <- table(oem.comp.lags$Opportunity.Status.recoded, oem.comp.lags$diff)

table(influencer.osc$Opportunity.Status.recoded, influencer.osc$lag, influencer.osc$Competitive.OEM.recoded)


############## Vertical Segment
table(influencer.osc$Vertical.Segment, influencer.osc$Opportunity.Status.levels)
sum(table(influencer.osc$Vertical.Segment, influencer.osc$Opportunity.Status.levels))

table(influencer.osc$Division.Long.Name, influencer.osc$Opportunity.Status.levels)
sum(table(influencer.osc$Division.Long.Name, influencer.osc$Opportunity.Status.levels))

############# Influencer Product Family
table(influencer.osc$Influencer.Product.Family, influencer.osc$Opportunity.Status.levels)
sum(table(influencer.osc$Influencer.Product.Family, influencer.osc$Opportunity.Status.levels))

table(influencer.osc$Influencer.Product.Class, influencer.osc$Opportunity.Status.levels)
sum(table(influencer.osc$Influencer.Product.Class, influencer.osc$Opportunity.Status.levels))

############ Opportunity Approving Manager Name
table(influencer.osc$Opportunity.Approving.Manager.Name, influencer.osc$Opportunity.Status.levels)
sum(table(influencer.osc$Opportunity.Approving.Manager.Name, influencer.osc$Opportunity.Status.levels))


############ Opportunity Sales Org No Overlap
table(influencer.osc$Opportunity.Sales.Org..No.Overlap., influencer.osc$Opportunity.Status.levels)
sum(table(influencer.osc$Opportunity.Sales.Org..No.Overlap., influencer.osc$Opportunity.Status.levels))

############ Revenue Type
table(influencer.osc$Revenue.Type, influencer.osc$Opportunity.Status.levels)
sum(table(influencer.osc$Revenue.Type, influencer.osc$Opportunity.Status.levels))

############ Source
table(influencer.osc$Source, influencer.osc$Opportunity.Status.levels)
sum(table(influencer.osc$Source, influencer.osc$Opportunity.Status.levels))

############ Awarness of needs
table(influencer.osc$Stage, influencer.osc$Opportunity.Status.levels)
sum(table(influencer.osc$Stage, influencer.osc$Opportunity.Status.levels))





###### Embedded #####################################################################################################################################################
emb.oppty.tiger.extract <- read.csv("emb oppty tiger extract.csv", header=T, stringsAsFactors=FALSE)
emb.product.tiger.extract <- read.csv("emb product tiger extract.csv", header=T, stringsAsFactors=FALSE)

#### Remove Duplicated opportunities
emb.oppty.tiger.extract <- emb.oppty.tiger.extract[!(duplicated(emb.oppty.tiger.extract$Opportunity.ID)),]

####################################

emb.interm <- join(emb.oppty.tiger.extract, emb.product.tiger.extract,  by="Opportunity.ID", type="inner")
emb.final <- join(emb.interm, emb.accounts_mapped,  by="Opportunity.ID", type="inner")










#### Find number of missing values for each column
vars <- colnames(inf.osc)
vars_na <- data.frame(variable=vars, Missing=0)

for (v in vars){
    vars_na$Missing[vars_na$variable==v] <- length(which(is.na(inf.osc[,v])))
}
