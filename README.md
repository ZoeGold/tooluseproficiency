# tooluseproficiency
Data and code for "Development and social dynamics of stone tool use in
white-faced capuchin monkeys" by Zoë Goldsborough, Meredith Carlson, Leonie Reetz, Margaret C. Crofoot, and Brendan J. Barrett (2025). 

This repository contains all the data and code required to replicate the main analyses reported in the manuscript. The raw data is present in the form of four CSV files, one per coder for the tool use proficiency analyses ("CEBUS-02-R11_R12_2022_EXP-ANV-01_R12_LRdetailedtoolscoding.csv", "EXP-ANV-01-R11_MC_2025-03-03_correctionbyLRZG.csv", and "ZGdetailedtoolscoding.csv") and one for the social attention analyses ("socialattentioncoding.csv"). 
These files are raw BORIS output csvs, which were cleaned using the R script included in this repository ("BORIScleaningscript.R). The cleaning script also requires the "capuchinIDs.csv", which is a file with age and sex information for each capuchin in our sample.
All statistical analyses using the cleaned data were done using the R script ("proficiencyanalyses.R"). 

Furthermore, this repository contains the Coding Guide that was used to train all coders, which also includes our ethogram (a video version of the ethogram can be found here: https://keeper.mpdl.mpg.de/d/0c1b9853f3f342d8b3da/
