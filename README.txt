Rise time task – C1 pre-test



Data collection


Rise time data were collected during the C1 pre-test going from December 16th 2018 to March 5th 2019.
All children except 1 performed both test and retest, mostly with a short break in between both. 
Specific instructions provided to the children during testing can be found in the C1 pre-test manual (J:\GBW-0416_DYSCO_OnGoing\c1_intervention\3_Testmaterial_Stimuli\ASSR\1. Pre-test\Manual\Handleiding_C1_ASSR). 
Details about data collection and reliability of thresholds can be found in the individual participants’ PDF files containing all data collection information (e.g. K:\GBW-0071_DYSCO_Project\CohortI_c1project\4_Raw_data\ASSR\i002\pre\i002_20190102 ). Every subjects’ folder contains one PDF with I-code and date of testing.  



Data management


Raw data can be found in the following location (J:\GBW-0416_DYSCO_OnGoing\c1_intervention\5_WorkInProgress\ASSR\Analysis\RT_task\RT_results_pre), as well as in the individual subjects’ behavioral folder (e.g. K:\GBW-0071_DYSCO_Project\CohortI_c1project\4_Raw_data\ASSR\i002\pre\beh). One test is not included in the analyses, since this test had to be re-started and the re-started test is included in the analysis. 



Data analysis


Firstly, raw data (1 test and 1 retest file per child) are combined into one file containing I-code, group, mean of 4 last trials and mean of 4 last reversals for both test and retest. To this end, the RT RMD file is run,  which provides us with the RT_data_pre file and the RT_mean_pre file. To this last file, the group of the child, as well as mean of 4 last reversals is added manually (the last can’t be read from the .apr files). The final file on which analysis is done, is saved as RT_mean_pre_withmeanreversals. 

Secondly, after finishing the file on which analysis is done. The RT_analysis R file is run. In this script, the final file is created, in which mean of last 4 reversals is translated into the actual thresholds. 
Descriptive statistics are performed, as well as more elaborate analyses looking into group and test-retest differences, and finally some descriptive figures are being plotted. 

