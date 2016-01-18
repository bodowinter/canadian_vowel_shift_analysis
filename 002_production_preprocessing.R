## Bodo Winter
## April 24, 2015; Amended May 15-17, 2015
## Adapted new style and cleaned code: Jan 17, 2016
## Canadian Vowel Shift Production Preprocessing


##------------------------------------------------------------------
## Load in data:
##------------------------------------------------------------------

## Load in libraries:

library(utils)
library(gdata)
library(dplyr)

## Set to main analysis directory (everything else is relative to that):

mainDir <- '/Users/teeniematlock/Desktop/research/kettig_canadian_vowel_shift/analysis'

## Sub-directory that contains productin data:

subDir <- 'raw_data'
setwd(file.path(mainDir, subDir))

## Load in normalized production data:

ae <- read.csv(file = 'ae.txt', sep = '\t', stringsAsFactors = F)
u <- read.csv(file = 'u.txt', sep = '\t', stringsAsFactors = F)
e <- read.csv(file = 'e.txt', sep = '\t', stringsAsFactors = F)
o <- read.csv(file = 'o.txt', sep = '\t', stringsAsFactors = F)

## Load in raw data (non-normalized):

raw <- read.csv('kettig_raw_vowels.csv', stringsAsFactors = F)

## Download frequency data from SUBTLEX-US (link accessed May 15, 2015):

download.file(url = 'http://www.ugent.be/pp/experimentele-psychologie/en/research/documents/subtlexus/subtlexus3.zip/at_download/file',
	destfile = 'SUBTLEXusExcel2007.zip')
unzip('SUBTLEXusExcel2007.zip')
SUBTL <- read.xls('SUBTLEXusExcel2007.xlsx', sheet = 1, header = T)			# takes several minutes

## Make 'Word' column of SUBTLEX into lower case (since, e.g. 'valley' is represented as 'Valley'):

SUBTL <- mutate(SUBTL,
	Word = as.character(Word),
	Word = tolower(Word))



##------------------------------------------------------------------
## Preprocessing normalized production data:
##------------------------------------------------------------------

## Merge all production data into one file:

vowels <- rbind(ae, u, e, o)

## Exclude '(not)' => This was data that Thomas Kettig used when people mispronounced 'Holly'. However, there are two few cases to be considered:

vowels <- filter(vowels, Context != '(not)')

## Exclude 'calm' (only 2 instances):

vowels <- filter(vowels, Context != 'calm')

## Add frequency data:

vowels <- cbind(vowels,
	SUBTL[match(vowels$Context, SUBTL$Word), c('FREQcount', 'Lg10WF')])

## Re-extract the age variable (we might want to look at this continuously later):

vowels$BirthYear <- as.numeric(gsub('[A-Za-z]', '', as.character(vowels$Speaker)))

## Center frequency:

vowels <- mutate(vowels,
	Lg10WF_c = Lg10WF - mean(Lg10WF, na.rm = T))

## Create a 'Labov Hierarchy' variable:

vowels$LabovHierarchy <- 1
vowels[vowels$Gender == 'M' & vowels$Age == 'Y',]$LabovHierarchy <- 2
vowels[vowels$Gender == 'F' & vowels$Age == 'O',]$LabovHierarchy <- 3
vowels[vowels$Gender == 'M' & vowels$Age == 'O',]$LabovHierarchy <- 4



##------------------------------------------------------------------
## Split up post-vowel context:
##------------------------------------------------------------------

## Retrieve a vector of the unique words:

these_words <- unique(vowels$Context)

## Extract alveolars based on final letter, words with 'y' are alveolar because they are of the general structure 'holly':

alveolars <- substr(these_words, nchar(these_words), nchar(these_words)) %in% c('s', 'z', 'd', 'n', 't', 'l', 'y')
alveolars <- these_words[alveolars]

## Add 'gone' and 'cause' which also end in an alveolar (silent 'e'):

alveolars <- c(alveolars, 'gone', 'cause')
alveolars <- alveolars[alveolars != 'soft']

## Extract velars based on final letter:

velars <- substr(these_words, nchar(these_words), nchar(these_words)) %in% c('k', 'g')
velars <- these_words[velars]

## Extract bilabials based on final letter:

bilabials <- substr(these_words, nchar(these_words), nchar(these_words)) %in% c('b', 'm')
bilabials <- these_words[bilabials]

## Extract labiodental based on final letter ('h' corresponds to 'rough'):

labiodentals <- substr(these_words, nchar(these_words), nchar(these_words)) %in% c('f', 'h')
labiodentals <- these_words[labiodentals]
labiodentals <- c(labiodentals, 'soft')

## Extract vowels followed by voiced consonants (all 'y' correspond to words like 'gully'):

voiced <- substr(these_words, nchar(these_words), nchar(these_words)) %in% c('m', 'b', 'y', 'z', 'd', 'n', 'l', 'e')
voiced <- these_words[voiced]

## Additionally, Thomas Kettig classified 'says' as voiced:

voiced <- c(voiced, 'says')

## Extract words ending in a nasal:

nasal <- substr(these_words, nchar(these_words), nchar(these_words)) %in% c('m', 'n')
nasal <- these_words[nasal]
nasal <- c(nasal, 'gone')

## Extract vowels followed by laterals:

lateral <- substr(these_words, nchar(these_words), nchar(these_words)) %in% c('l', 'y')
lateral <- these_words[lateral]

## Extract vowels followed by stops (stop <- full oral closure, including nasals):

stops <- substr(these_words, nchar(these_words), nchar(these_words)) %in% c('k', 'b', 'd', 'n', 't', 'g', 'm')
stops <- these_words[stops]
stops <- stops[stops != 'soft']

## Extract vowels followed by fricatives:

fricatives <- substr(these_words, nchar(these_words), nchar(these_words)) %in% c('f', 's', 'z', 'h')
fricatives <- these_words[fricatives]
fricatives <- c(fricatives, 'cause', 'soft')

## Extract vowels followed by sonorants:

sonorants <- substr(these_words, nchar(these_words), nchar(these_words)) %in% c('m', 'n', 'l', 'y')
sonorants <- these_words[sonorants]
sonorants <- c(sonorants, 'gone')

## Put these all into a table, first create an empty table:

wordfactors <- data.frame(Word = these_words, AlveolarStatus = 'not_alveolar', VelarStatus = 'not_velar',
	BilabialStatus = 'not_bilabial', LabiodentalStatus = 'not_labiodental', Voicing = 'voiceless', OralStopStatus = 'not_stop',
	Nasality = 'not_nasal', LateralStatus = 'not_lateral', Frication = 'not_fricative', Sonority = 'obstruent',
	stringsAsFactors = F)

## Fill the table with the information based on the above extracted information:

wordfactors[wordfactors$Word %in% alveolars, ]$AlveolarStatus <- 'alveolar'
wordfactors[wordfactors$Word %in% velars, ]$VelarStatus <- 'velar'
wordfactors[wordfactors$Word %in% bilabials, ]$BilabialStatus <- 'bilabial'
wordfactors[wordfactors$Word %in% labiodentals, ]$LabiodentalStatus <- 'labiodental'
wordfactors[wordfactors$Word %in% voiced, ]$Voicing <- 'voiced'
wordfactors[wordfactors$Word %in% stops, ]$OralStopStatus <- 'stop'
wordfactors[wordfactors$Word %in% nasal, ]$Nasality <- 'nasal'
wordfactors[wordfactors$Word %in% lateral, ]$LateralStatus <- 'lateral'
wordfactors[wordfactors$Word %in% fricatives, ]$Frication <- 'fricative'
wordfactors[wordfactors$Word %in% sonorants, ]$Sonority <- 'sonorant'

## Produce Manner of Articulation and Place of Articulation columns, first seed empty ones:

wordfactors <- cbind(wordfactors,
	data.frame(PlaceOfArticulation = '', MannerOfArticulation = '', stringsAsFactors = F))

## Fill with information, for Manner of Articulation:

wordfactors[wordfactors$OralStopStatus == 'stop' & wordfactors$Nasality != 'nasal', ]$MannerOfArticulation <- 'stop'
wordfactors[wordfactors$Nasality == 'nasal', ]$MannerOfArticulation <- 'nasal'
wordfactors[wordfactors$Frication == 'fricative', ]$MannerOfArticulation <- 'fricative'
wordfactors[wordfactors$LateralStatus == 'lateral', ]$MannerOfArticulation <- 'lateral'

## Fill with information, for Place of Articulation:

wordfactors[wordfactors$AlveolarStatus == 'alveolar', ]$PlaceOfArticulation <- 'alveolar'
wordfactors[wordfactors$VelarStatus == 'velar', ]$PlaceOfArticulation <- 'velar'
wordfactors[wordfactors$BilabialStatus == 'bilabial', ]$PlaceOfArticulation <- 'bilabial'
wordfactors[wordfactors$LabiodentalStatus == 'labiodental', ]$PlaceOfArticulation <- 'labiodental'

## Finally, add syllable information:

wordfactors$Syllables <- 'monosyllabic'
wordfactors[wordfactors$Word %in% c('gully', 'holly', 'belly', 'valley'),]$Syllables <- 'disyllabic'

## Add 'wordfactors' to the main dataset:

vowels <- cbind(vowels, wordfactors[match(vowels$Context, wordfactors$Word), -1])



##------------------------------------------------------------------
## Merge raw F1/F2 data into this:
##------------------------------------------------------------------

## Rename columns so they don't clash with the main dataset to be merged into:

colnames(raw)[colnames(raw) %in% c('F1','F2')] <- c('F1_raw','F2_raw')

## Create unique identifier columns for matching:

vowels <- cbind(vowels,
	raw[match(paste(vowels$Speaker, vowels$Context), paste(raw$speaker, raw$context)), c('F1_raw', 'F2_raw')])



##------------------------------------------------------------------
## Save data:
##------------------------------------------------------------------

## Re-order:

vowels <- select(vowels, Speaker, Gender, Age, BirthYear, LabovHierarchy,
	AlveolarStatus, VelarStatus, BilabialStatus, LabiodentalStatus,
	Voicing, OralStopStatus, Nasality, LateralStatus, Frication,
	Sonority, PlaceOfArticulation, MannerOfArticulation, Syllables,
	Context, Vowel, FREQcount, Lg10WF, Lg10WF_c, F1_raw, F2_raw, F1, F2)

## Set working directory:

subDir <- 'processed_data'
setwd(file.path(mainDir, subDir))

## Write table for normalized production data:

write.table(vowels, 'production_processed.csv', sep = ',', row.names = F)



