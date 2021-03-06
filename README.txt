==================================================================
Human Activity Recognition Using Smartphones Dataset
by Neetika Mittal

The dataset included the following files:
=========================================

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

- 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 

- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 

- 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 

=========================================

About run_analysis.R

The script is running in following steps:

1. Firstly it reads features data from X_train.txt and X_test.txt. It does row binding or union of these two files.
2. It reads Subject data for both training and test. Again it does row binding or union of these two files.
3. It reads Activity data for both training and test. Again it does row binding or union of these two files.
4. It reads features.txt to get the labels of the different columns in features data file created in step1. 

5. Then it assigns the names of these columns fetched in step 4 to the features data and also assigns column name "activity" to activity data and "subject to Subject data.
6. Then it merges all the data created in step 1,2,3 with the header created in step5.
7. Then the code Extracts only the measurements on the mean and standard deviation for each measurement using grep. 

8. Also, the column names are renamed by replacing Acc with Accelerometer, Gyro with Gyroscope, Mag with Magnitude and BodyBody with Body.

9. Lastly, with the help of the "aggregate" , "order" and write.table functions the data is converted into a table containing mean values of all the included features, ordered by the activity name and the subject id, and the data is written to the "tidydata.txt" file.