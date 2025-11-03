# Inter-rater reliability (IRR) for BORIS coding of detailed tool use
# author : Zoë Goldsborough, MPI-AB

# IRR for article "Development and social dynamics of stone tool use in white-faced  capuchin monkeys"
# Comparing coders LR and ZG
# LR recoded all data done by MC, so LR and ZG are the only two coders in the dataset at this point (October 2025)
# sample of ~125 videos

library(stringr)
library(dplyr)
library(irr)
library(psych)

######## PREP ##########

# double-coding was done in separate BORIS project, with original coding imported to allow for comparison
# load CSV with output
IRR_raw <- read.csv("detailedtools/InterObserverReliability_LR_withoriginalscompare.csv")
head(IRR_raw)

# rename columns and exclude unnecessary ones
IRR_raw <- data.frame("videoID" = IRR_raw$Observation.id, "codingdate" = IRR_raw$Observation.date,
                      "medianame" = IRR_raw$Media.file.name, "videolength" = IRR_raw$Media.duration..s., "coder" = 
                        IRR_raw$Coder.ID, "subjectID" = IRR_raw$Subject, "behavior" = IRR_raw$Behavior,
                      "modifier1" = IRR_raw$Modifier..1,  "modifier2" = IRR_raw$Modifier..2,  "modifier3" = IRR_raw$Modifier..3,  "modifier4" = IRR_raw$Modifier..4, 
                      "starttime" = IRR_raw$Start..s., "comment" = IRR_raw$Comment.start)

# create flag for original vs IRR
IRR_raw$original <- ifelse(IRR_raw$coder == "LR", 0, 1)

# filter out the ones by MC coder
MCcoded <- unique(IRR_raw$videoID[which(IRR_raw$coder %in% c("MC", "MKWC"))])
MCcoded <- str_remove(MCcoded, "( \\(imported at .+\\)|_original)$")

IRR_filter <- IRR_raw[which(!IRR_raw$videoID %in% MCcoded & !IRR_raw$coder %in% c("MC", "MKWC")) ,]
ftable(IRR_filter$coder)

# filter into sequences, once for LR dataframe and once for ZG dataframe
originalcoding <- IRR_filter[which(IRR_filter$coder == "ZG"),]
originalcoding$videoID <- str_remove(originalcoding$videoID," \\(imported at .+\\)$")
IRRcoding <- IRR_filter[which(IRR_filter$coder == "LR"),]

## Sequences can span several videos, and there can be several sequences in a video
## For the IRR coding, LR was instructed to only code a sequence spanning several videos if the next video was in the sample
## This means that in these cases only, the seqend coding has to be changed to seqcont 
# videos that are in the sample and have a seqcont (found these manually through looking through)
sample <- c("CEBUS-02-R11__2022-03-10__06-50-25", "CEBUS-02-R11__2022-05-14__13-11-05", "CEBUS-02-R11__2022-05-14__14-56-52", "CEBUS-02-R11__2022-06-02__09-17-55", "CEBUS-02-R11__2022-06-02__09-19-09", 	
            "CEBUS-02-R11__2022-06-02__12-43-59", "CEBUS-02-R11__2022-06-14__16-01-26", "CEBUS-02-R12__2022-09-17__10-51-12", "CEBUS-02-R12__2022-09-20__09-01-58",
            "CEBUS-02-R12__2022-09-25__11-49-33", "CEBUS-02-R12__2022-09-25__13-16-36", "CEBUS-02-R12__2022-09-25__13-17-47", "CEBUS-02-R12__2022-09-25__13-18-57", "CEBUS-02-R12__2022-09-25__13-40-12",
            "CEBUS-02-R12__2022-09-26__18-05-45", "CEBUS-02-R12__2022-09-27__10-56-01", "EXP-ANV-01-R11__2022-04-25__12-42-49")
# change in both originalcoding and IRRcoding
originalcoding$behavior[which(originalcoding$videoID %in% sample & str_detect(originalcoding$modifier1, "cont") == TRUE)] <- "seqcont"
IRRcoding$behavior[which(IRRcoding$videoID %in% sample & str_detect(IRRcoding$modifier1, "cont") == TRUE)] <- "seqcont"

## If a sequence was a continuation from a previous video which was not in the IRR sample, the IRR coder will have coded "seqstart" while the originalcoding did not
# since the IRR coder did not know the previous video existed
# found these manually, and then add seq_start at the beginning of the video (just at 0 seconds)
addrows <- data.frame(bind_rows(originalcoding[which(originalcoding$videoID == "CEBUS-02-R11__2022-06-09__16-21-28"),][1,], 
                                originalcoding[which(originalcoding$videoID == "CEBUS-02-R12__2022-09-21__07-23-39"),][1,],
                                originalcoding[which(originalcoding$videoID == "CEBUS-02-R12__2022-09-25__11-49-33"),][1,],
                                originalcoding[which(originalcoding$videoID == "CEBUS-02-R12__2022-09-25__13-40-12"),][1,],
                                originalcoding[which(originalcoding$videoID == "CEBUS-02-R12__2022-09-26__18-05-45"),][1,],
                                originalcoding[which(originalcoding$videoID == "CEBUS-02-R12__2022-09-28__16-33-52"),][1,]))
addrows[,c("behavior", "modifier1", "modifier2", "modifier3", "modifier4", "starttime")] <- c(rep("seqstart", nrow(addrows)), rep("added",nrow(addrows)), rep("None", 3*nrow(addrows)), rep(0, nrow(addrows)))
addrows$starttime <- as.numeric(addrows$starttime)
originalcoding <- bind_rows(originalcoding, addrows)
# fix peel now being before seq_start in one sequence
originalcoding$starttime[which(originalcoding$videoID == "CEBUS-02-R12__2022-09-25__11-49-33" & originalcoding$modifier1 == "peel")] <- 0.001

# in IRR coding this sometimes also happened
addrows2 <- data.frame(bind_rows(IRRcoding[which(IRRcoding$videoID == "CEBUS-02-R11__2022-05-12__13-49-55"),][1,], 
                                 IRRcoding[which(IRRcoding$videoID == "CEBUS-02-R11__2022-06-09__16-21-28"),][1,], 
                                 IRRcoding[which(IRRcoding$videoID == "CEBUS-02-R12__2022-09-25__11-49-33"),][1,],
                                 IRRcoding[which(IRRcoding$videoID == "CEBUS-02-R12__2022-09-25__13-40-12"),][1,],
                                 IRRcoding[which(IRRcoding$videoID == "CEBUS-02-R12__2022-09-26__18-05-45"),][1,],
                                 IRRcoding[which(IRRcoding$videoID == "CEBUS-02-R12__2022-09-28__16-33-52"),][1,]))
addrows2[,c("behavior", "modifier1", "modifier2", "modifier3", "modifier4", "starttime")] <- c(rep("seqstart", nrow(addrows2)), rep("added",nrow(addrows2)), rep("None", 3*nrow(addrows2)), rep(0, nrow(addrows2)))
addrows2$starttime <- as.numeric(addrows2$starttime)
IRRcoding <- bind_rows(IRRcoding, addrows2)

## Remove uncodeable ones
# # to make the two dataframes actually comparable so the sequences can be matched and we compare the coding within the sequences
## e.g. if the sequence continued from a previous video that wasn't in sample, and ended instantly, or if the sequence continued in a video not in sample
originalcoding <- originalcoding[!(originalcoding$videoID == "CEBUS-02-R12__2022-09-27__15-22-35" & originalcoding$subjectID == "TER"),]
originalcoding <- originalcoding[!(originalcoding$videoID == "EXP-ANV-01-R11__2022-04-28__17-24-39" & originalcoding$starttime == 0),]
originalcoding <- originalcoding[!((originalcoding$videoID == "CEBUS-02-R11__2022-05-14__13-08-58" | originalcoding$videoID == "CEBUS-02-R11__2022-06-02__12-38-37") & originalcoding$starttime < 20),]
originalcoding <- originalcoding[!(originalcoding$videoID == "CEBUS-02-R11__2022-06-02__09-03-29" & originalcoding$subjectID == "TER"),]
originalcoding <- originalcoding[!(originalcoding$videoID == "CEBUS-02-R12__2022-08-30__16-06-35" & originalcoding$starttime > 60),]
originalcoding <- originalcoding[!(originalcoding$videoID == "CEBUS-02-R12__2022-09-16__08-39-54" & originalcoding$subjectID == "LAR"),]
originalcoding <- originalcoding[!((originalcoding$videoID == "CEBUS-02-R12__2022-09-19__14-11-52" | originalcoding$videoID == "CEBUS-02-R12__2022-09-26__18-06-58" | 
                                      originalcoding$videoID == "CEBUS-02-R12__2022-09-28__16-33-52") & originalcoding$starttime > 40),]
originalcoding <- originalcoding[!(originalcoding$videoID == "CEBUS-02-R12__2022-09-22__06-38-49" & originalcoding$starttime < 20),]

# exclude one sequence that spanned 3 videos, and wasn't used in analyses, but is hard to compare due to it running across so many videos
megalongvideo <- c("CEBUS-02-R12__2022-09-20__07-02-01", "CEBUS-02-R12__2022-09-20__07-00-51", "CEBUS-02-R12__2022-09-20__06-59-42")
originalcoding <- originalcoding[!(originalcoding$videoID %in% megalongvideo),]
IRRcoding <- IRRcoding[!(IRRcoding$videoID %in% megalongvideo),]

# sort
originalcoding <- originalcoding[order(originalcoding$videoID, originalcoding$starttime),]
IRRcoding <- IRRcoding[order(IRRcoding$videoID, IRRcoding$starttime),]

# create ascending number for each sequence
curseq <- 1
cache <- 0
originalcoding$seqnumber <- NA
IRRcoding$seqnumber <- NA

for (i in 1:nrow(originalcoding)) {
  originalcoding$seqnumber[i] <- curseq
  
  if(originalcoding$behavior[i] == "seqend") {
    cache <- curseq
    originalcoding$seqnumber[i] <- curseq
    curseq <- NA
  }
  if(originalcoding$behavior[i] == "seqstart") {
    curseq <- cache + 1
    originalcoding$seqnumber[i] <- curseq
    cache <- NA
  }
  
}

curseq <- 1
cache <- 0

for (i in 1:nrow(IRRcoding)) {
  IRRcoding$seqnumber[i] <- curseq
  
  if(IRRcoding$behavior[i] == "seqend") {
    cache <- curseq
    IRRcoding$seqnumber[i] <- curseq
    curseq <- NA
  }
  if(IRRcoding$behavior[i] == "seqstart") {
    curseq <- cache + 1
    IRRcoding$seqnumber[i] <- curseq
    cache <- NA
  }
  
}

originalcoding$location <- ifelse(str_detect(originalcoding$videoID, "EXP-ANV") == TRUE, "EXP-ANV-01", "CEBUS-02")
originalcoding$mediadate <- sapply(str_split(originalcoding$videoID, "__"), '[', 2)
originalcoding$sequenceID <- paste(originalcoding$location, originalcoding$mediadate, originalcoding$seqnumber, sep = "_" )

IRRcoding$location <- ifelse(str_detect(IRRcoding$videoID, "EXP-ANV") == TRUE, "EXP-ANV-01", "CEBUS-02")
IRRcoding$mediadate <- sapply(str_split(IRRcoding$videoID, "__"), '[', 2)
IRRcoding$sequenceID <- paste(IRRcoding$location, IRRcoding$mediadate, IRRcoding$seqnumber, sep = "_" )

# check that all sequences and videos match between the two samples
setdiff(originalcoding$sequenceID, IRRcoding$sequenceID)

### final sample size
length(unique(originalcoding$videoID)) # 121 videos
length(unique(originalcoding$seqnumber)) # 203 sequences

########### IRR ############
## create dataframe to store the outputs of the IRR comparison for all the measures we are interested in
# Per SEQUENCE
IRR_results <- data.frame(sequenceID = unique(originalcoding$sequenceID), 
                          coder_original = "ZG", coder_IRR = "LR")

#### 1. information per sequence (item, displacement, scrounging, etc) ####
## sequence end information
seqendings <- originalcoding[originalcoding$behavior == "seqend" | originalcoding$behavior == "seqcont",]
seqendings2 <- IRRcoding[IRRcoding$behavior == "seqend" | IRRcoding$behavior == "seqcont",]
# outcome
seqendings$outcome_ZG <- seqendings$modifier1
seqendings2$outcome_LR <- seqendings2$modifier1
ftable(seqendings$outcome_ZG)
ftable(seqendings2$outcome_LR)
# displacement
seqendings$disp_ZG <- seqendings$modifier3
seqendings2$disp_LR <- seqendings2$modifier3
ftable(seqendings$disp_ZG)
ftable(seqendings2$disp_LR)
# social attention
seqendings$socatt_ZG <- seqendings$modifier4
seqendings2$socatt_LR <- seqendings2$modifier4
ftable(seqendings$socatt_ZG)
ftable(seqendings2$socatt_LR)
# scrounging
seqendings$scrounging_ZG <- seqendings$modifier2
seqendings2$scrounging_LR <- seqendings2$modifier2
ftable(seqendings$scrounging_ZG)
ftable(seqendings2$scrounging_LR)

# filter out seqcont ones after making sure their information is included
seqendings <- seqendings[!seqendings$behavior == "seqcont", c("sequenceID", "outcome_ZG", "disp_ZG", "socatt_ZG", "scrounging_ZG")]
seqendings2 <- seqendings2[!seqendings2$behavior == "seqcont", c("sequenceID", "outcome_LR", "disp_LR", "socatt_LR", "scrounging_LR")]

IRR_results <- left_join(IRR_results, seqendings, "sequenceID")
IRR_results <- left_join(IRR_results, seqendings2, "sequenceID")

head(IRR_results)
# convert to factors
IRR_results[4:11] <- lapply(IRR_results[4:11], as.factor)
str(IRR_results)

## 1a. agreement on outcome
ftable(IRR_results$outcome_ZG, IRR_results$outcome_LR)
cohen.kappa(IRR_results[,c("outcome_ZG", "outcome_LR")])
## 1b. agreement on displacement
cohen.kappa(IRR_results[,c("disp_ZG", "disp_LR")])
## 1c. agreement on socatt
cohen.kappa(IRR_results[,c("socatt_ZG", "socatt_LR")])
## 1d. agreement on scrounging
cohen.kappa(IRR_results[,c("scrounging_ZG", "scrounging_LR")])

# all above 0.8 - high

### Item type ###
items <- left_join(originalcoding[originalcoding$behavior == "seqstart", c("sequenceID", "modifier1")], 
                   IRRcoding[IRRcoding$behavior == "seqstart", c("sequenceID", "modifier1")], by = "sequenceID")
# filter out the ones that were "added" 
colnames(items) <- c("sequenceID", "item_ZG", "item_LR")

IRR_results <- left_join(IRR_results, items, by = "sequenceID")

# first just if we agreed on the overall itemtype
ftable(IRR_results$item_LR)
IRR_results$bigitem_ZG <- factor(ifelse(str_detect(IRR_results$item_ZG, "almendra") == TRUE, "almendra", 
                                        ifelse(IRR_results$item_ZG == "coconut", "coconut", 
                                               ifelse(IRR_results$item_ZG == "fruit", "fruit", "other"))), levels = c("almendra", "coconut", "fruit", "other"))
IRR_results$bigitem_LR <- factor(ifelse(str_detect(IRR_results$item_LR, "almendra") == TRUE, "almendra", 
                                        ifelse(IRR_results$item_LR == "coconut", "coconut", 
                                               ifelse(IRR_results$item_LR == "fruit", "fruit", "other"))), levels = c("almendra", "coconut", "fruit", "other"))

str(IRR_results)

cohen.kappa(IRR_results[!(IRR_results$item_LR == "added" | IRR_results$item_ZG == "added"),c("bigitem_ZG", "bigitem_LR")])
# good (0.83) 
# was just a lot of almendras which we agreed on
ftable(IRR_results[!(IRR_results$item_LR == "added" | IRR_results$item_ZG == "added"),c("bigitem_ZG", "bigitem_LR")])

# if sea almond, agreement on the color
IRR_results$color_ZG <- str_remove(IRR_results$item_ZG, "^almendra")
IRR_results$color_LR <- str_remove(IRR_results$item_LR, "^almendra")
IRR_results$color_ZG[!(IRR_results$bigitem_ZG == "almendra" & IRR_results$bigitem_LR == "almendra")] <- NA
IRR_results$color_LR[!(IRR_results$bigitem_LR == "almendra" & IRR_results$bigitem_ZG == "almendra")] <- NA
IRR_results$color_ZG <- factor(IRR_results$color_ZG, levels = c("brown", "green", "red", "unknown"))
IRR_results$color_LR <- factor(IRR_results$color_LR, levels = c("brown", "green", "red", "unknown"))

# how confident were we both in assigning a color?
ftable(IRR_results$color_LR, IRR_results$color_ZG)
# if we both assigned a color, did we agree?
cohen.kappa(IRR_results[(IRR_results$bigitem_LR == "almendra" & IRR_results$bigitem_ZG == "almendra") & !(IRR_results$color_LR == "unknown" | IRR_results$color_ZG == "unknown"), c("color_ZG", "color_LR")])
# cohen.kappa (0.7) 

### 2. age and identity of tool user ####
### 2a. Age
IRR_results <- left_join(IRR_results, originalcoding[!duplicated(originalcoding$sequenceID), c("sequenceID", "subjectID")], by = "sequenceID")
IRR_results <- left_join(IRR_results, IRRcoding[!duplicated(IRRcoding$sequenceID), c("sequenceID", "subjectID")], by = "sequenceID")

# Add ages to identities
adults <- c("ABE", "SMG", "TOM", "INK")
subadults <- c("SPT", "LAR", "MIC")
juveniles <- c("PEA", "ZIM", "TER", "JOE", "BLO", "MIN", "BAL")
ftable(IRR_results$subjectID.x)
IRR_results$age_ZG <- ifelse((IRR_results$subjectID.x == "juvenileunknown" | IRR_results$subjectID.x %in% juveniles), "Juvenile", 
                             ifelse((IRR_results$subjectID.x == "subadultmale" | IRR_results$subjectID.x %in% subadults), "Subadult",
                                    ifelse((IRR_results$subjectID.x == "adultmale" | IRR_results$subjectID.x %in% adults), "Adult", NA)))
IRR_results$age_LR <- ifelse((IRR_results$subjectID.y == "juvenileunknown" | IRR_results$subjectID.y %in% juveniles), "Juvenile", 
                             ifelse((IRR_results$subjectID.y == "subadultmale" | IRR_results$subjectID.y %in% subadults), "Subadult",
                                    ifelse((IRR_results$subjectID.y == "adultmale" | IRR_results$subjectID.y %in% adults), "Adult", NA)))

IRR_results[12:13] <- lapply(IRR_results[12:13], as.factor)
str(IRR_results)

cohen.kappa(IRR_results[,c("age_ZG", "age_LR")])
# kappa of 0.98 on age, incredibly high

### 2b. Identity
IRR_results$ID_ZG <- ifelse(IRR_results$subjectID.x %in% c(adults, subadults, juveniles), 1, 0)
IRR_results$ID_LR <- ifelse(IRR_results$subjectID.y %in% c(adults, subadults, juveniles), 1, 0)

ftable(IRR_results[,c("ID_ZG", "ID_LR")])

# ID agreement if both ID
# coerce ID to factor, with all possible levels equal across both
allIDlevels <- union(unique(IRR_results$subjectID.x), unique(IRR_results$subjectID.y))
IRR_results$IDfactor_ZG <- factor(IRR_results$subjectID.x, levels = allIDlevels)
IRR_results$IDfactor_LR <- factor(IRR_results$subjectID.y, levels = allIDlevels)

# agreement if both identified (180 cases)
cohen.kappa(IRR_results[(IRR_results$ID_LR == 1 & IRR_results$ID_ZG == 1),c("IDfactor_ZG", "IDfactor_LR")])
# cohen's kappa 0.96 - very high

### 3. number of pounds per sequence ####
n_pounds_ZG <- as.data.frame(as.matrix(aggregate(originalcoding$sequenceID[originalcoding$behavior == "pound"], by = list(sequenceID = originalcoding$sequenceID[originalcoding$behavior == "pound"]), FUN = length)))
colnames(n_pounds_ZG) <- c("sequenceID", "npounds_ZG")
IRR_results <- left_join(IRR_results, n_pounds_ZG, by = c("sequenceID"))
n_pounds_LR <- as.data.frame(as.matrix(aggregate(IRRcoding$sequenceID[IRRcoding$behavior == "pound"], by = list(sequenceID = IRRcoding$sequenceID[IRRcoding$behavior == "pound"]), FUN = length)))
colnames(n_pounds_LR) <- c("sequenceID", "npounds_LR")
IRR_results <- left_join(IRR_results, n_pounds_LR, by = c("sequenceID"))
IRR_results[is.na(IRR_results)] <- 0
IRR_results$npounds_LR <- as.numeric(IRR_results$npounds_LR)
IRR_results$npounds_ZG <- as.numeric(IRR_results$npounds_ZG)

icc(IRR_results[,c("npounds_ZG", "npounds_LR")], model = "twoway", type = "agreement")

# VERY HIGH (0.98)

### 4. number of repositions per sequence ####
# 4a. nr of repositions (item)
n_repos_ZG <- as.data.frame(as.matrix(aggregate(originalcoding$sequenceID[originalcoding$behavior == "reposit" & originalcoding$modifier1 == "item"], 
                                                by = list(sequenceID = originalcoding$sequenceID[originalcoding$behavior == "reposit" & originalcoding$modifier1 == "item"]), FUN = length)))
colnames(n_repos_ZG) <- c("sequenceID", "nrepos_ZG")
IRR_results <- left_join(IRR_results, n_repos_ZG, by = c("sequenceID"))
n_repos_LR <- as.data.frame(as.matrix(aggregate(IRRcoding$sequenceID[IRRcoding$behavior == "reposit" & IRRcoding$modifier1 == "item"], 
                                                by = list(sequenceID = IRRcoding$sequenceID[IRRcoding$behavior == "reposit" & IRRcoding$modifier1 == "item"]), FUN = length)))
colnames(n_repos_LR) <- c("sequenceID", "nrepos_LR")
IRR_results <- left_join(IRR_results, n_repos_LR, by = c("sequenceID"))
IRR_results[is.na(IRR_results)] <- 0
IRR_results$nrepos_LR <- as.numeric(IRR_results$nrepos_LR)
IRR_results$nrepos_ZG <- as.numeric(IRR_results$nrepos_ZG)

icc(IRR_results[,c("nrepos_ZG", "nrepos_LR")], model = "twoway", type = "agreement")
# high (>0.9)

# 4b. nr of peels
n_peel_ZG <- as.data.frame(as.matrix(aggregate(originalcoding$sequenceID[originalcoding$behavior == "reposit" & originalcoding$modifier1 == "peel"], 
                                               by = list(sequenceID = originalcoding$sequenceID[originalcoding$behavior == "reposit" & originalcoding$modifier1 == "peel"]), FUN = length)))
colnames(n_peel_ZG) <- c("sequenceID", "npeel_ZG")
IRR_results <- left_join(IRR_results, n_peel_ZG, by = c("sequenceID"))
n_peel_LR <- as.data.frame(as.matrix(aggregate(IRRcoding$sequenceID[IRRcoding$behavior == "reposit" & IRRcoding$modifier1 == "peel"], 
                                               by = list(sequenceID = IRRcoding$sequenceID[IRRcoding$behavior == "reposit" & IRRcoding$modifier1 == "peel"]), FUN = length)))
colnames(n_peel_LR) <- c("sequenceID", "npeel_LR")
IRR_results <- left_join(IRR_results, n_peel_LR, by = c("sequenceID"))
IRR_results[is.na(IRR_results)] <- 0
IRR_results$npeel_LR <- as.numeric(IRR_results$npeel_LR)
IRR_results$npeel_ZG <- as.numeric(IRR_results$npeel_ZG)

icc(IRR_results[,c("npeel_ZG", "npeel_LR")], model = "twoway", type = "agreement")
# high (>0.9)

### 5. number of mistakes per sequence ####
# 5a. nr of misstrikes
n_missZG <- as.data.frame(as.matrix(aggregate(originalcoding$sequenceID[originalcoding$behavior == "misstrike" & originalcoding$modifier1 %in% c("None", "other")], 
                                              by = list(sequenceID = originalcoding$sequenceID[originalcoding$behavior == "misstrike" & originalcoding$modifier1 %in% c("None", "other")]), FUN = length)))
colnames(n_missZG) <- c("sequenceID", "nmissZG")
IRR_results <- left_join(IRR_results, n_missZG, by = c("sequenceID"))
n_missLR <- as.data.frame(as.matrix(aggregate(IRRcoding$sequenceID[IRRcoding$behavior == "misstrike" & IRRcoding$modifier1 %in% c("None", "other")], 
                                              by = list(sequenceID = IRRcoding$sequenceID[IRRcoding$behavior == "misstrike" & IRRcoding$modifier1 %in% c("None", "other")]), FUN = length)))
colnames(n_missLR) <- c("sequenceID", "nmissLR")
IRR_results <- left_join(IRR_results, n_missLR, by = c("sequenceID"))
IRR_results[is.na(IRR_results)] <- 0
IRR_results$nmissLR <- as.numeric(IRR_results$nmissLR)
IRR_results$nmissZG <- as.numeric(IRR_results$nmissZG)

icc(IRR_results[,c("nmissZG", "nmissLR")], model = "twoway", type = "agreement")
# ok (0.73)

# 5b. nr of itemflies
n_fliesZG <- as.data.frame(as.matrix(aggregate(originalcoding$sequenceID[originalcoding$behavior == "misstrike" & str_detect(originalcoding$modifier1, "itemflies") == TRUE], 
                                               by = list(sequenceID = originalcoding$sequenceID[originalcoding$behavior == "misstrike" & str_detect(originalcoding$modifier1, "itemflies") == TRUE]), FUN = length)))
colnames(n_fliesZG) <- c("sequenceID", "nfliesZG")
IRR_results <- left_join(IRR_results, n_fliesZG, by = c("sequenceID"))
n_fliesLR <- as.data.frame(as.matrix(aggregate(IRRcoding$sequenceID[IRRcoding$behavior == "misstrike" & str_detect(IRRcoding$modifier1, "itemflies") == TRUE], 
                                               by = list(sequenceID = IRRcoding$sequenceID[IRRcoding$behavior == "misstrike" & str_detect(IRRcoding$modifier1, "itemflies") == TRUE]), FUN = length)))
colnames(n_fliesLR) <- c("sequenceID", "nfliesLR")
IRR_results <- left_join(IRR_results, n_fliesLR, by = c("sequenceID"))
IRR_results[is.na(IRR_results)] <- 0
IRR_results$nfliesLR <- as.numeric(IRR_results$nfliesLR)
IRR_results$nfliesZG <- as.numeric(IRR_results$nfliesZG)

icc(IRR_results[,c("nfliesZG", "nfliesLR")], model = "twoway", type = "agreement")
# ok (0.68)

### 6. duration of sequence and timing of seq_start/seq_end ####
## seq_start
IRR_results <- left_join(IRR_results, originalcoding[originalcoding$behavior == "seqstart", c("sequenceID", "starttime")], by = "sequenceID")
IRR_results <- left_join(IRR_results, IRRcoding[IRRcoding$behavior == "seqstart", c("sequenceID", "starttime")], by = "sequenceID")
colnames(IRR_results)[36:37] <- c("seqstart_ZG", "seqstart_LR")
str(IRR_results)
IRR_results$seqstartdif <- abs(IRR_results$seqstart_LR - IRR_results$seqstart_ZG)
mean(IRR_results$seqstartdif)
sd(IRR_results$seqstartdif)

# prop agreeing within 1 seconds
mean(IRR_results$seqstartdif <= 1) # 88%

## seq_end
IRR_results <- left_join(IRR_results, originalcoding[originalcoding$behavior == "seqend", c("sequenceID", "starttime")], by = "sequenceID")
IRR_results <- left_join(IRR_results, IRRcoding[IRRcoding$behavior == "seqend", c("sequenceID", "starttime")], by = "sequenceID")
colnames(IRR_results)[39:40] <- c("seqend_ZG", "seqend_LR")
IRR_results$seqenddif <- abs(IRR_results$seqend_LR - IRR_results$seqend_ZG)
mean(IRR_results$seqenddif)
sd(IRR_results$seqenddif)

# prop agreeing within 1 seconds
mean(IRR_results$seqenddif <= 1) # 79%

## duration of sequence
IRR_results$seqdur_ZG <- IRR_results$seqend_ZG - IRR_results$seqstart_ZG
IRR_results$seqdur_LR <- IRR_results$seqend_LR - IRR_results$seqstart_LR

icc(IRR_results[,c("seqdur_ZG", "seqdur_LR")], model = "twoway", type = "agreement")
# 0.99 agreement
