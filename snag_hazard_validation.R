# snag hazard validation
# written by Karin Riley and Jessica Haas, 6/22/2021


library(RSQLite)
library(sqldf)
library(DBI)
library(data.table)
library(foreign)

options(scipen = 999)

# read tree table for multicondition validation plots
trees <- read.csv("D:\\tree_table.csv")
trees <- trees[order(trees$tl_id),]

# calculate number and median height of snags
plotlist <- sort(unique(trees$tl_id))

snagnum <- as.numeric()
snaght <- as.numeric()

for (j in 1:length(plotlist))
{
  curmat <- trees[(trees$tl_id==plotlist[j]),]
  curmatsnag <- curmat[(curmat$STATUSCD==2),]
  truevec <- which(curmatsnag$TPA_UNADJ>0)
  curmatsnag <- curmatsnag[(truevec),]
  curmatsnag <- curmatsnag[(curmatsnag$DIA>=7.87),]
  snagnum[j] <- sum(curmatsnag$TPA_UNADJ, na.rm = TRUE)
  # weight the median by the # TPA
  tparound <- round(curmatsnag$TPA_UNADJ, digits=2)
  tpanum <- tparound * 100
  treevec <- as.numeric()
  if (snagnum[j]>0) {
    for (k in 1:length(tpanum))
    {
      treevectemp <- rep(curmatsnag$ACTUALHT[k], tpanum[k])
      treevec <- append(treevec, treevectemp)
    }
    snaght[j] <- median(treevec, na.rm=TRUE)
  }
  if (snagnum[j]==0) { 
    snaght[j] <- 0  }
}

outmat <- cbind(plotlist, snagnum, snaght)




# assign snag hazard class based on TPA and median height

outmat <- data.frame(outmat)
ht_meters = outmat$snaght * 0.3048
tp_hect = outmat$snagnum * 2.47

haz = as.vector(matrix(0,nrow=length(tp_hect)))

haz[tp_hect < 10] = 1 
haz[(tp_hect >= 10 & tp_hect < 30) & ht_meters < 20] = 1
haz[(tp_hect >= 30 & tp_hect < 50) & ht_meters < 5] = 1

haz[(tp_hect >= 10 & tp_hect < 30) & ht_meters >= 20] = 2
haz[(tp_hect >= 30 & tp_hect < 50) & ht_meters >=5 &  ht_meters < 20] = 2
haz[(tp_hect >= 50 & tp_hect < 100) & ht_meters < 5] = 2

haz[(tp_hect >= 30 & tp_hect < 50) & ht_meters >= 20] = 3
haz[(tp_hect >= 50 & tp_hect < 100) & ht_meters >=5 &  ht_meters< 20] = 3
haz[(tp_hect >= 100) & ht_meters < 5] = 3

haz[tp_hect >= 50 & tp_hect < 100 & ht_meters >= 20] = 4
haz[tp_hect >= 100 & ht_meters >= 5] = 4

sum(haz == 1) 
sum(haz == 2)  
sum(haz == 3)  
sum(haz == 4) 

haz_out = data.frame(cbind(outmat, haz))
head(haz_out)
colnames(haz_out) <- c("tm_id", "snagTPA", "snaght", "haz_class")

write.csv(haz_out, 'G:\\TreeMap2016\\snag_haz.csv')



# compare number of live & dead trees in multicondition FIA plots and imputed dataset
combine = read.dbf("D:\\TreeMap2016\\combine_table.dbf")
# lookup for number of trees and height for multicond plots = haz_out
# lookup for number of trees and height for SnagHazard product
treemap <- read.dbf("D:\\TreeMap2016\\SnagHazard2016.tif.vat.dbf")

val1 <- merge(combine, treemap, by.x="TreeMap201", by.y="Value")
val2 <- merge(val1, haz_out, by.x="FIA_MultiC", by.y="tm_id")

val3 <- val2[order(val2$FIA_MultiC),]

# check to see how often values matched
plots <- unique(val3$FIA_MultiC)
classmatch <- as.numeric()
htmatch <- as.numeric()
tpamatch <- as.numeric()
sort(unique(val3$snagTPA.x))
for (j in 1:length(plots))
{
  curmat <- val3[(val3$FIA_MultiC==plots[j]),]
  # how often did hazard class match anywhere within plot radius
  hc_plot <- curmat$haz_class.y[1]
  checkvec <- as.logical()
  for (k in 1:dim(curmat)[[1]])
  {
    checkvec[k] <- curmat$haz_class.x[k]==hc_plot
  }
  if (sum(checkvec)==0) {  classmatch[j] <- 0 }
  if (sum(checkvec)>0) {  classmatch[j] <- 1 }
  
}
sum(classmatch) # 2497 out of 2889 = 0.8643129



## compare number of live & dead trees in multicondition FIA plots and imputed dataset
combine = read.dbf("D:\\TreeMap2016\\combine_table.dbf")
# lookup for number of trees and height for multicond plots (also now classified) = validmat
# lookup for number of trees and height for SnagHazard product (also now classified) =  tmmat

val1 <- merge(combine, tmmat, by.x="TreeMap201", by.y="Value")
val2 <- merge(val1, validmat, by.x="FIA_MultiC", by.y="tm_id")

val3 <- val2[order(val2$FIA_MultiC),]

# check to see how often classified values matched
plots <- unique(val3$FIA_MultiC)
classmatch <- as.numeric()
htmatch <- as.numeric()
tpamatch <- as.numeric()
sort(unique(val3$snagTPA.x))
for (j in 1:length(plots))
{
  curmat <- val3[(val3$FIA_MultiC==plots[j]),]
  # how often did hazard class match anywhere within plot radius?
  hc_plot <- curmat$haz_class.y[1]
  checkvec <- as.logical()
  for (k in 1:dim(curmat)[[1]])
  {
    checkvec[k] <- curmat$haz_class.x[k]==hc_plot
  }
  if (sum(checkvec)==0) {  classmatch[j] <- 0 }
  if (sum(checkvec)>0) {  classmatch[j] <- 1 }
  # how often did snag height class match?
  checkht <- sum(curmat$ht_class2==curmat$ht_class)
  if (checkht>=1) { htmatch[j] <- 1 }
  if (checkht==0) {  htmatch[j] <- 0 }

  # how often did TPA class match anywhere within plot radius?
  checktpa <- sum(curmat$tpa_class2==curmat$tpa_class)
  if (checktpa>=1) { tpamatch[j] <- 1 }
  if (checktpa==0) {  tpamatch[j] <- 0 }
}
sum(classmatch) # 2497 out of 2889 = 0.8643129
sum(htmatch)  #2211 out of 2889 = 0.7653167
sum(tpamatch) # 2150 out of 2889 = 0.7442021
rightmat <- cbind(plots, classmatch, htmatch, tpamatch)


# did Snag Hazard perform more accurately inside or outside disturbed areas?-----------------------------------------
library(RSQLite)
library(sqldf)
library(DBI)
library(data.table)
library(foreign)

# merge CN onto snag hazard class matrix
plots2 <- read.dbf("D:\\TreeMap2016\\FIA_MultiCond.dbf")
snaghaz <- read.csv('D:\\TreeMap2016\\snag_haz_2016.csv')

snaghaz2 <- merge(plots2, snaghaz, by.x = "OBJECTID", by.y = "tm_id") 
snaghaz3 <- snaghaz2[,c(1,2,11,12,13)]

# was classification right?
snaghaz4 <- merge(snaghaz3, rightmat, by.x="OBJECTID", by.y="plots")

# was plot disturbed?
con = dbConnect(RSQLite::SQLite(), dbname="D:\\FIA\\FIADB_USA.db")

# get a list of all tables
alltables = dbListTables(con)
alltables

# make table with needed fields from FIA data (all stands)
valid <- as.data.table(dbGetQuery(con, 'select PLT_CN, DSTRBCD1, DSTRBCD2, DSTRBCD3 from COND'))
valid_filtered <- merge(valid, snaghaz4, by.x="PLT_CN", by.y="PLT_CN")
length(unique(valid_filtered$PLT_CN)) #2889, so all were in national database
sort(unique(valid_filtered$DSTRBCD1))
sort(unique(valid_filtered$DSTRBCD2))
sort(unique(valid_filtered$DSTRBCD3))
curplots <- unique(valid_filtered$PLT_CN)
distcodes <- c(10, 12, 20, 22, 30, 31, 32)
# determine which validation plots were disturbed
yesvec <- as.numeric()
for (j in 1:length(snaghaz4$OBJECTID))
{
  curmat <- valid_filtered[(valid_filtered$PLT_CN==curplots[j]),]
  # relevant disturbance codes for insect/disease and fire = 10, 12, 20, 22, 30, 31, 32 (all are present in DSTRBCD1, some in DSTRBCD2 and DSTRBCD3)
  check1 <- distcodes %in% curmat$DSTRBCD1
  check2 <- distcodes %in% curmat$DSTRBCD2
  check3 <- distcodes %in% curmat$DSTRBCD3
  yes <- sum(check1, check2, check3)
  if (yes>0) { yesvec[j] <- 1}
  if (yes==0) { yesvec[j] <- 0}
}
sum(yesvec) # 442 out of 2889 were disturbed or 15%
yesmat <- cbind(curplots, yesvec)
finalmat <- merge(snaghaz4, yesmat, by.x="PLT_CN", by.y="curplots")

distmat <- finalmat[finalmat$yesvec==1,]
distrightmat <- distmat[distmat$classmatch==1,] # 350 out of 442 disturbed plots (79.2%) had the correct snag hazard class assigned
nodistmat <- finalmat[finalmat$yesvec==0,]
nodistrightmat <- nodistmat[nodistmat$classmatch==1,] # 2147 out of 2447 (87.7%) non-disturbed plots had the correct classification


