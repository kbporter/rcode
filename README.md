# rcode
- code for analyzing behavioral error rates of an enumeration task  

### linregR.R
- performs linear regresson on error rates and calculates R2
 
###### Inputs:
- error rates in .csv files (rows = subjects, columns = stimulus condition) 
- can take multiple experiments/csv files in conditions vector (line 4)

###### Outputs:
*For each experiment*
- csv file with the fitline points for each subject
- csv with r2 values for each subject

*For each subject + experiment*
- png of the raw data, with the fitline and r2 value displayed
