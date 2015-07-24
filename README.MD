For this script 'run_analysis.R', the following information is relevant:

1) Required packages
- dplyr

2) Units of measurement
- Units of measurement are unchanged from the original dataset, for more information look at the codeBook

3) Required Files
- You should have downloaded and unzipped the following file into your working directory: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
- Do not change the names of any of the folders

3) What the script does
- First, it reads each individual dataset into various dataframes such as XTrain, YTrain, subTrain etc. Then we do abit of initial tidying by combining the X, Y and subject files into one dataframe for both the test and the train (i.e. we have 2 useful dataframes now)
- Then we merge the test and training dataset using rowbind, and for readability, we sort the merged dataset by ID
- We then extract only the measurements on the mean and standard deviation for each measurement. Here, we do not include meanfreq() as a measurement
- Then, we convert the activity to a factor class, using the activity labels as a mapping function. The activityLabels object was read from the activity_labels.txt in the original data folder
- Then we rename each column to give it a more intuitive and human readable name. Specifically, t and f prefixes stand for time and frequency domains respectively. For more information, look at the codeBook.
- Lastly, we using the dplyr package to group the dataframe by ID and activity, then we use summarise_each in order to get the column mean for each specific ID and activity pair. This gives us our final tidy dataset, described more in detail in the codeBook. The final file is saved as a csv file to the working directory as "newTidyData.csv"
