## Bodo Winter
## April 27, 2015; Amended May 15-17, 2015; May 19, 2015
## Adapted new style and cleaned code: Jan 17, 2016
## Loading in RT data and merging it with the perception data frame


##------------------------------------------------------------------
## Load in data:
##------------------------------------------------------------------

## Libraries:

library(stringr)
library(dplyr)

## Set to main analysis directory (everything else is relative to that):

mainDir <- '/Users/teeniematlock/Desktop/research/kettig_canadian_vowel_shift/analysis'

## Sub-directory that contains by-participant perception data:

subDir <- 'raw_data/perception_individual_data/'
setwd(file.path(mainDir, subDir))

## Create data frame with empty column (to be filled in loop) and the subject identifiers taken from the file names:

pp <- data.frame(Subject = rep(gsub('.txt', '', list.files()),
	each = 102))
pp$Info <- character(nrow(pp))

## Loop through files and put into dataframe:

filenames <- list.files()
subject_name <- gsub('.txt', '', filenames)
for(i in 1:length(filenames)) {
	this_file <- filenames[i]
	suppressWarnings(pp[pp$Subject %in% subject_name[i],]$Info <- readLines(this_file)[-1])
		# warnings are due to incomplete final line and can be ignored
	}



##------------------------------------------------------------------
## Preprocessing:
##------------------------------------------------------------------

## Separate the string information:

xlist <- strsplit(pp$Info, split = '-')
pp$Trial <- sapply(xlist, FUN = function(x)x[[1]])
pp$Response <- sapply(xlist, FUN = function(x)x[[2]])
pp$RT <- sapply(xlist, FUN = function(x)x[[3]])

## Load in the perception data:

setwd('..')
xdata <- read.csv(file = 'perception.txt', sep = '\t')

## Sort perception data by subject, trial:

xdata <- xdata[order(xdata$Subject, xdata$Stimulus),]
pp <- mutate(pp, Trial = as.numeric(Trial))
pp <- pp[order(pp$Subject, pp$Trial),]

## Get rid of first six practice trials to match the lengths of the frames:

pp <- filter(pp, !(Trial %in% 1:6))

## Create a column with vowel information:

pp$ChosenVowel <- 'ae'
pp[pp$Response == 'BET',]$ChosenVowel <- 'e'
pp[pp$Response == 'BUT',]$ChosenVowel <- 'u'
pp[pp$Response == 'BOUGHT',]$ChosenVowel <- 'o'

## Merge synthetic formant values of the stimuli into the new 'pp' data frame:

pp <- cbind(pp,
	xdata[match(pp$Trial, xdata$Stimulus), c('F1', 'F2')])

## Log-transform and center:

pp <- mutate(pp,
	RT = as.numeric(RT),
	LogRT = log(RT),
	RT_c = RT - mean(RT),
	LogRT_c = LogRT - mean(LogRT),
	Trial_c = Trial - mean(Trial),
	F1_c = F1 - mean(F1),
	F2_c = F2 - mean(F2))

## Add gender plus continuous and categorical age information:

pp$Gender <- str_extract(as.character(pp$Subject), '[A-Z]')
pp$BirthYear <- as.numeric(str_extract(as.character(pp$Subject), '[0-9]+'))
pp$Age <- ifelse(pp$BirthYear < 1984, 'old', 'young')

## Create a 'Labov Hierarchy' variable:

pp$LabovHierarchy <- 1
pp[pp$Gender == 'M' & pp$Age == 'young',]$LabovHierarchy <- 2
pp[pp$Gender == 'F' & pp$Age == 'old',]$LabovHierarchy <- 3
pp[pp$Gender == 'M' & pp$Age == 'old',]$LabovHierarchy <- 4

## Re-order columns and get rid of the INFO column:

pp <- select(pp, Subject, Gender, Age, BirthYear, LabovHierarchy, Trial, Trial_c,
	F1, F2, F1_c, F2_c, RT, RT_c, LogRT, LogRT_c, Response, ChosenVowel)



##------------------------------------------------------------------
## Saving:
##------------------------------------------------------------------

## Set working directory:

subDir <- 'processed_data'
setwd(file.path(mainDir, subDir))

## Write table:

write.table(pp, file = 'perception_processed.csv', sep = ',', row.names = F)



