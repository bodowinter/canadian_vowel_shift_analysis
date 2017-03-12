Statistical analysis of Canadian Vowel Shift Analysis
=============

-	**Study design & data collection:** Thomas Kettig
-	**Statistical Analysis:** Bodo Winter

## Libraries required for this analysis:

-	stringr
-	library
-	utisl
-	gdata
-	car
-	plyr
-	mgcv
-	party

## Script files contained in this analysis:

1.	**001_perception_preprocessing.R**<br>
	Preprocessing of the perception data (vowel categorization).
2.	**002_production_preprocessing.R**<br>
	Preprocessing of the production data (vowel formants).
3.	**003_production_analysis.R**<br>
	Analysis of the vowel production data (Lobanov-normalized).
4.	**004_perception_analysis.R**<br>
	Analysis of the perception data including production/perception correlation.

## Data files contained in this analysis:

-	raw_data/perception_individual_data folder<br>
	By-participant perception data processed by script 001
-	a/o/u/ae.txt<br>
	Lobanov-normalized production data
-	perception.txt<br>
	Perception stimulus F1/F2 values
-	kettig_raw_vowels.csv<br>
	Non-normalized vowel formants
-	processed_data folder<br>
	Processed data resulting from scripts 001 and 002

