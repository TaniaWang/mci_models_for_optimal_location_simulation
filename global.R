huffNewDistances<-NULL
error <- FALSE
clusteringData <- NULL
mergclusterPASubzone <- NULL
paPALib <- NULL
newLibData <- NULL   #add/remove new location
huffMapLat <- NULL
huffMapLng <- NULL
huffMapName <- NULL
huffMapCode <- NULL
huffMapSize <- NULL
huffMapType <- NULL
huffMapArea <- NULL
totalLng <- NULL
totalLat <- NULL
totalCollectionSize <- NULL
totalName <- NULL
totalCode <- NULL
totalBranchType <- NULL
totalFloorArea <- NULL
totalMRTno <- NULL
totalMallno <- NULL
totalTCno <- NULL
huffMapLatChange <- NULL
huffMapLngChange <- NULL
libLocList <- NULL  #list of lib changed location. change existing lib location
ijmatrix_Full_Txn_Data <- NULL
libMatrix <- NULL
subzone_map <- NULL
huffMap <- NULL
actualMap <- NULL
diffMap <- NULL

subzoneCentroids <- readRDS("data/subzoneCentroids.rds", refhook = NULL) 
subzonePA <- readRDS("data/subzonePA.rds", refhook = NULL) 
lib <- readRDS("data/lib.rds", refhook = NULL) 
mrt <- readRDS("data/mrt.rds", refhook = NULL)
mall <- readRDS("data/mall.rds", refhook = NULL)
tuition <- readRDS("data/tuition.rds", refhook = NULL)
patronLib <- readRDS("data/patronLib.rds", refhook = NULL)
libData <- readRDS("data/libData.rds", refhook = NULL)

cluster <- readRDS("data/cluster.rds", refhook = NULL)
colVariables <- c("Total TXN" = 4, "Books Borrowed" = 5, "Avg Books Borrowed" = 6,"Day from Last TXN" = 7)
mergclusterPASubzone <- merge(cluster, subzonePA, by.x="Locale.Planning.ADZID", by.y="Locale.Planning.ADZID",all.x=TRUE)

#read in a kml file for choropleth
data.list<-list(mrt, mall, tuition)
name.list<-list("MRT", "Mall", "Tuition Centre")

lib$X[1]

libName <- paste(lib$NAME)

libLng <- paste(lib$X)
libLat <- paste(lib$Y)

#lib loc
libLocList <<- lib
libLocLibList <- lib$NAME


#----------------------------------------------------------------------------------------------------------------------------#

# {PART 0: Read and clean data for MCI Model}

# Load distance data - between each subzone and each library

library(MCI)
library(rgdal)

Subzone_Distance <- readRDS("data/Subzone_Distance.rds", refhook = NULL)
subzone_map <- readOGR(dsn="data/Subzone.shp", layer="Subzone", encoding="UTF-8")

# Load library information
Lib_Info <- readRDS("data/Lib_Info.rds", refhook = NULL)
# transform banch type to dummy variable
Lib_Info <- transform(Lib_Info, type_if_mall = ifelse(Branch.Type == "Mall",1,0))
Lib_Info <- transform(Lib_Info, type_if_standalone = ifelse(Branch.Type == "Stand-Alone",1,0))

# Load patron data
Patron_Dataset_FY13_Subzone <- readRDS("data/Patron_Dataset_FY13_Subzone.rds", refhook = NULL)

# remove "Bad Value" & "Missing Value" under Locale Planning ADZID
# remove invalid subzone "CKSZ07" and invalid patrons with birth year = 1900
Patron_Dataset_FY13_Subzone <- Patron_Dataset_FY13_Subzone[(Patron_Dataset_FY13_Subzone$Locale.Planning.ADZID != "Bad Value")&
                                                             (Patron_Dataset_FY13_Subzone$Locale.Planning.ADZID != "Missing Value")&
                                                             (Patron_Dataset_FY13_Subzone$Subzone != "CKSZ07")&
                                                             (Patron_Dataset_FY13_Subzone$Patron.Birthyear != 1900) ,]

# read population by subzone data
Subzone_Pop_simple <- readRDS("data/Subzone_Pop_simple.rds", refhook = NULL)
# To exclude the "total" row in population data, to make sure the row number matches 323
Subzone_Pop_simple <- Subzone_Pop_simple[-324, ] 

# Delete patrons from those subzones where population is 0 - invalid patrons
Patron_Dataset_FY13_Subzone <- merge(Patron_Dataset_FY13_Subzone, Subzone_Pop_simple, 
                                     by.x="Subzone", by.y="Subzone", stringsAsFactors = FALSE)
Patron_Dataset_FY13_Subzone <- Patron_Dataset_FY13_Subzone[(Patron_Dataset_FY13_Subzone$Population != 0),]


# Import transaction data
TXN_FY13 <- readRDS("data/TXN_FY13.rds", refhook = NULL)

# Merge transaction data and patron data to get the "Subzone" of each transaction
# Use inner join to make sure invalid data from transaction records are not passed
TXN_FY13_wPatron <- merge(TXN_FY13, Patron_Dataset_FY13_Subzone, by.x="Patron.UID", by.y="Patron.UID", stringsAsFactors = FALSE)

TXN_FY13_wPatron$interaction <- paste(TXN_FY13_wPatron$Subzone, TXN_FY13_wPatron$Branch.Code, sep="-")

TXN_FY13_wPatron_dist <- merge(Subzone_Distance, TXN_FY13_wPatron, by.x="route", by.y="interaction", 
                               all.x = TRUE, stringsAsFactors = FALSE)

# Convert all columns from factor to character
# TXN_FY13_wPatron_dist[] <- lapply(TXN_FY13_wPatron_dist, as.character)

# Replace all NA with string "NA" to generate 8075 rows
# TXN_FY13_wPatron_dist[is.na(TXN_FY13_wPatron_dist)] <- "NA"

# Create matrix of 8075 rows
ijmatrix_TXN_FY13 <- ijmatrix.create(TXN_FY13_wPatron_dist, "from_Subzone","to_Branch.Code",
                                     correctVar = TRUE, correctVar.val = 1)

ijmatrix_Full_Txn_Data <- merge(ijmatrix_TXN_FY13, Lib_Info, by.x="to_Branch.Code", by.y="Branch.Code", all.x = TRUE)
ijmatrix_Full_Txn_Data <- merge(ijmatrix_Full_Txn_Data, Subzone_Distance, by.x="interaction", by.y="route", all.x = TRUE)

# Rename columns for convenience
names(ijmatrix_Full_Txn_Data)[2] <- "Branch.Code"
names(ijmatrix_Full_Txn_Data)[3] <- "Subzone"

ijmatrix_Full_Txn_Data$to_Branch.Code.y <- NULL
ijmatrix_Full_Txn_Data$from_Subzone.y <- NULL