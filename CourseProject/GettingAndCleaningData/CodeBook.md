# Codebook

## Project Walk-through
This Section describes all the work that has been done step by step.

### Step 1
Merges the training and the test sets to create one data set.
1. Read X_train.txt, using read.table(..., header=FALSE), assgined as variable **X_train**. The dimension of X_train is 7352 x 561.
2. Read y_train.txt, using read.table(..., header=FALSE), assgined as variable **y_train**. The dimension of y_train is 7352 x 1.
3. Read X_test.txt,  using read.table(..., header=FALSE), assgined as variable **X_test**. The dimension of X_test is 2947 x 561.
4. Read y_test.txt,  using read.table(..., header=FALSE), assgined as variable **y_test**. The dimension of y_test is 2947 x 1.
5. Read subject_train.txt,  using read.table(..., header=FALSE), assgined as variable **subject_train**. The dimension of y_test is 7352 x 1.
6. Read subject_test.txt,  using read.table(..., header=FALSE), assgined as variable **subject_test**. The dimension of y_test is 2947 x 1.

7. Column-bind X_train, y_train and subject_train using cbind(X_train, y_train), assigned as variable as **train_data**. The dimension of train_data is 7352 x 563.
8. Column-bind X_test and y_test by using cbind(X_test, y_test), assigned as variable **test_data**. The dimension of test_data is 2947 x 563.

9. Row-bind train_data and test_data by using rbind(train_data, test_data), assigned as variable **data**. The dimension of data is 10299 x 563.

### Step 2
Extracts only the measurements on the mean and standard deviation for each measurement.
1. Read feature.txt, using read.table(..., header=FALSE), assgined as variable **feature_names**. The dimension of feature_names is 561 x 2.
2. According to README.txt, the 2nd column should contain all the column names to **data** except last column, which should be the label. Since there's no header configured in **feature_names**, we fetched the 2nd column as **feature_names$V2** and then append an additional column called "**Activity**". I used the append() function as append(feature_names, "Label") and assign it to names(data).
3. Since the last column records subject information. So I renamed the last column to be **Subject**.
4. Using grep() function, selecting the columns that only have keyword "mean" or "std" or exactly "Label". The regular expression I used is "(.\*-mean.\*)|(.\*-std.\*)|Label". The result character vector is stored as variable **valid_feature_names**.
5. Finally, subsetting **data** by only select the columns that in **valid_feature_names**. The result DataFrame is stored as variable **data_trimmed**. The dimension of data_trimmed is 10299 x 80.

### Step 3
Uses descriptive activity names to name the activities in the data set.
1. The naming convention follows the pattern "**ILoveDataScience**".
2. Using the function gsub() with the following order:
	a. Replace all the string "Gyro" to "AngularVelocity", as suggested by the following link  under section "Attribute Information":http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
	b. Replace all the string "Acc" to "Acceleration".
	c. Replace all the string "fBody" to "FrequencyBody".
	d. Replace all the string "tGravity" to "TimeGravity".
	e. Replace all the string "tBody" to "TimeBody".
3. The full set of feature names will be listed in the section **Feature Names with Explanations**

### Step 4
Appropriately labels the data set with descriptive variable names.
1. Read activity_labels.txt, using read.table(..., header=FALSE), assgined as variable **activity_labels_data**. The dimension of activity_labels_data is 6 x 2.
2. Create a list such that the value is the 2nd column of activity_labels_data, a.k.a the descriptive label. The name of this list is the label in integer. The list is called *activity_labels_list*.
3. Using sapply() on column *Activity* such that it convert the label in integer to the descriptive factor based on the mapping between names of *activity_labels_list* and its values.
5. Using the function gsub() with the following order:
	a. Replace all the string "std" to "StandardDeviation".
	b. Replace all the string "meanFreq" to "WeightedAverage".
	c. Replace all the string "mean" to "Mean".
	d. Replace all the string "()" to "".
	e. Replace all the string "Mag" to Magnitude".
	f. Replace all the string "BodyBody" to "Body".
	g. Replace all the string "-" to "".
6. After these transformations, the variable name is supposed to be descriptive, because all of the abbreviations are well spelled out. For example, the variable name "fBodyBodyGyroJerkMag-std()" is being renamed as "FrequencyBodyAngularVelocityJerkMagnitudeStandardDeviation".


### Step 5
From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
1. Using group_by() and summarise_all() from the package **dplyr**, I split the dataframe by each **Activity** and each **Subject** and then generate the column mean for each column of the group. I add na.rm=TRUE to avoid generate NAs. The result dataframe is assigned as **tidy_data**.
2. Rename all the columns by appending "AverageOf" on top. e.g. "TimeBodyAccelerationMeanX" will be "AverageOfTimeBodyAccelerationMeanX", except column *Activity* and *Subject*.
3. The full set of feature names will be listed in the section **Feature Name List**
4. Save the **tidy_data** data frame by using write.table(, row.names=FALSE).

## Feature Name List

	AverageOfTimeBodyAccelerationMeanX                                 
	AverageOfTimeBodyAccelerationMeanY                                 
	AverageOfTimeBodyAccelerationMeanZ                                 
	AverageOfTimeBodyAccelerationStandardDeviationX                    
	AverageOfTimeBodyAccelerationStandardDeviationY                    
	AverageOfTimeBodyAccelerationStandardDeviationZ                    
	AverageOfTimeGravityAccelerationMeanX                              
	AverageOfTimeGravityAccelerationMeanY                              
	AverageOfTimeGravityAccelerationMeanZ                              
	AverageOfTimeGravityAccelerationStandardDeviationX                 
	AverageOfTimeGravityAccelerationStandardDeviationY                 
	AverageOfTimeGravityAccelerationStandardDeviationZ                 
	AverageOfTimeBodyAccelerationJerkMeanX                             
	AverageOfTimeBodyAccelerationJerkMeanY                             
	AverageOfTimeBodyAccelerationJerkMeanZ                             
	AverageOfTimeBodyAccelerationJerkStandardDeviationX                
	AverageOfTimeBodyAccelerationJerkStandardDeviationY                
	AverageOfTimeBodyAccelerationJerkStandardDeviationZ                
	AverageOfTimeBodyAngularVelocityMeanX                              
	AverageOfTimeBodyAngularVelocityMeanY                              
	AverageOfTimeBodyAngularVelocityMeanZ                              
	AverageOfTimeBodyAngularVelocityStandardDeviationX                 
	AverageOfTimeBodyAngularVelocityStandardDeviationY                 
	AverageOfTimeBodyAngularVelocityStandardDeviationZ                 
	AverageOfTimeBodyAngularVelocityJerkMeanX                          
	AverageOfTimeBodyAngularVelocityJerkMeanY                          
	AverageOfTimeBodyAngularVelocityJerkMeanZ                          
	AverageOfTimeBodyAngularVelocityJerkStandardDeviationX             
	AverageOfTimeBodyAngularVelocityJerkStandardDeviationY             
	AverageOfTimeBodyAngularVelocityJerkStandardDeviationZ             
	AverageOfTimeBodyAccelerationMagnitudeMean                         
	AverageOfTimeBodyAccelerationMagnitudeStandardDeviation            
	AverageOfTimeGravityAccelerationMagnitudeMean                      
	AverageOfTimeGravityAccelerationMagnitudeStandardDeviation         
	AverageOfTimeBodyAccelerationJerkMagnitudeMean                     
	AverageOfTimeBodyAccelerationJerkMagnitudeStandardDeviation        
	AverageOfTimeBodyAngularVelocityMagnitudeMean                      
	AverageOfTimeBodyAngularVelocityMagnitudeStandardDeviation         
	AverageOfTimeBodyAngularVelocityJerkMagnitudeMean                  
	AverageOfTimeBodyAngularVelocityJerkMagnitudeStandardDeviation     
	AverageOfFrequencyBodyAccelerationMeanX                            
	AverageOfFrequencyBodyAccelerationMeanY                            
	AverageOfFrequencyBodyAccelerationMeanZ                            
	AverageOfFrequencyBodyAccelerationStandardDeviationX               
	AverageOfFrequencyBodyAccelerationStandardDeviationY               
	AverageOfFrequencyBodyAccelerationStandardDeviationZ               
	AverageOfFrequencyBodyAccelerationWeightedAverageX                 
	AverageOfFrequencyBodyAccelerationWeightedAverageY                 
	AverageOfFrequencyBodyAccelerationWeightedAverageZ                 
	AverageOfFrequencyBodyAccelerationJerkMeanX                        
	AverageOfFrequencyBodyAccelerationJerkMeanY                        
	AverageOfFrequencyBodyAccelerationJerkMeanZ                        
	AverageOfFrequencyBodyAccelerationJerkStandardDeviationX           
	AverageOfFrequencyBodyAccelerationJerkStandardDeviationY           
	AverageOfFrequencyBodyAccelerationJerkStandardDeviationZ           
	AverageOfFrequencyBodyAccelerationJerkWeightedAverageX             
	AverageOfFrequencyBodyAccelerationJerkWeightedAverageY             
	AverageOfFrequencyBodyAccelerationJerkWeightedAverageZ             
	AverageOfFrequencyBodyAngularVelocityMeanX                         
	AverageOfFrequencyBodyAngularVelocityMeanY                         
	AverageOfFrequencyBodyAngularVelocityMeanZ                         
	AverageOfFrequencyBodyAngularVelocityStandardDeviationX            
	AverageOfFrequencyBodyAngularVelocityStandardDeviationY            
	AverageOfFrequencyBodyAngularVelocityStandardDeviationZ            
	AverageOfFrequencyBodyAngularVelocityWeightedAverageX              
	AverageOfFrequencyBodyAngularVelocityWeightedAverageY              
	AverageOfFrequencyBodyAngularVelocityWeightedAverageZ              
	AverageOfFrequencyBodyAccelerationMagnitudeMean                    
	AverageOfFrequencyBodyAccelerationMagnitudeStandardDeviation       
	AverageOfFrequencyBodyAccelerationMagnitudeWeightedAverage         
	AverageOfFrequencyBodyAccelerationJerkMagnitudeMean                
	AverageOfFrequencyBodyAccelerationJerkMagnitudeStandardDeviation   
	AverageOfFrequencyBodyAccelerationJerkMagnitudeWeightedAverage     
	AverageOfFrequencyBodyAngularVelocityMagnitudeMean                 
	AverageOfFrequencyBodyAngularVelocityMagnitudeStandardDeviation    
	AverageOfFrequencyBodyAngularVelocityMagnitudeWeightedAverage      
	AverageOfFrequencyBodyAngularVelocityJerkMagnitudeMean             
	AverageOfFrequencyBodyAngularVelocityJerkMagnitudeStandardDeviation
	AverageOfFrequencyBodyAngularVelocityJerkMagnitudeWeightedAverage 
