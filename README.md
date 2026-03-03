# tooluseproficiency
Data and code for "Development and social dynamics of stone tool use in
white-faced capuchin monkeys" by Zoë Goldsborough, Meredith Carlson, Leonie S. Reetz, Evelyn Del Rosario-Vargas, Margaret C. Crofoot, and Brendan J. Barrett (2026). 

This repository contains all the data and code required to replicate the main analyses reported in the manuscript. The raw data is present in the form of five CSV files, four for the tool use proficiency analyses ("2025-10-27_CrackingCapuchins_LRcoding.csv", "2025-10-27_CrackingCapuchins_MCcodingEXP-ANV_correctedbyLR.csv", "2025-10-27_CrackingCapuchins_MCcodingCEBUS-02_correctedbyLR.csv", and "2025-10-27_CrackingCapuchins_ZGcoding.csv") and one for the social attention analyses ("2025-10-27_CrackingCapuchins_SocialAttention.csv"). 
These files are raw BORIS output csvs, which were cleaned using the R script included in this repository ("BORIScleaningscript.R). The cleaning script also requires the "capuchinIDs.csv", which is a file with age and sex information for each capuchin in our sample.
All statistical analyses using the cleaned data were done using the R script ("proficiencyanalyses.R"). 
Interrater reliability was established by double coding a sample of sequences. A csv containing both coders' coding of this sample is present ("InterObserverReliability_LR_withoriginalscompare.csv") as well as the code we used to calculate inter-observer agreement ("detailedtools_IRR.R"). 

Furthermore, this repository contains the Coding Guide that was used to train all coders, which also includes our ethogram (a video version of the ethogram can be found here: https://keeper.mpdl.mpg.de/d/0c1b9853f3f342d8b3da/)
