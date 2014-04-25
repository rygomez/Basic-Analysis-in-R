# Read in the data from the working directory assuming the files have been saved as downloaded.
# Memory is an issue so subset the data early.

readData <- function(fname_suffix, path_prefix) {
    fpath <- file.path(path_prefix, paste0("y_", fname_suffix, ".txt"))
    y_data <- read.table(fpath, header=F, col.names=c("ActivityID"))    
    fpath <- file.path(path_prefix, paste0("subject_", fname_suffix, ".txt"))
    subject_data <- read.table(fpath, header=F, col.names=c("SubjectID"))  
    data_cols <- read.table("features.txt", header=F, as.is=T, col.names=c("MeasureID", "MeasureName"))
    fpath <- file.path(path_prefix, paste0("X_", fname_suffix, ".txt"))
    data <- read.table(fpath, header=F, col.names=data_cols$MeasureName)
    subset_data_cols <- grep(".*mean\\(\\)|.*std\\(\\)", data_cols$MeasureName)
    data <- data[,subset_data_cols]
    data$ActivityID <- y_data$ActivityID
    data$SubjectID <- subject_data$SubjectID
    data
}

# Read in the test data.
readTestData <- function() {
    readData("test", "test")
}

# Read in the train data.
readTrainData <- function() {
    readData("train", "train")
}

# Merge the data sets
mergeData <- function() {
    data <- rbind(readTestData(), readTrainData())
    cnames <- colnames(data)
    cnames <- gsub("\\.+mean\\.+", cnames, replacement="Mean")
    cnames <- gsub("\\.+std\\.+",  cnames, replacement="Std")
    colnames(data) <- cnames
    data
}

# Add activity and labels.
applyActivityLabel <- function(data) {
    activity_labels <- read.table("activity_labels.txt", header=F, as.is=T, col.names=c("ActivityID", "ActivityName"))
    activity_labels$ActivityName <- as.factor(activity_labels$ActivityName)
    data_labeled <- merge(data, activity_labels)
    data_labeled
}

# Combine data sets and add the activity label as another column.
getMergedLabeledData <- function() {
    applyActivityLabel(mergeData())
}

# Create a tidy data set that and add the average of each variable for each activity and each subject.
getTidyData <- function(merged_labeled_data) {
    library(reshape2)    
    id_vars = c("ActivityID", "ActivityName", "SubjectID")
    measure_vars = setdiff(colnames(merged_labeled_data), id_vars)
    melted_data <- melt(merged_labeled_data, id=id_vars, measure.vars=measure_vars)
    dcast(melted_data, ActivityName + SubjectID ~ variable, mean)    
}

# Tidy the data and create a specified file name that is passed to the function.
createTidyData <- function(fname) {
    tidy_data <- getTidyData(getMergedLabeledData())
    write.table(tidy_data, fname)
}

print("Tidying up the data, please stand by.")
createTidyData("TidyData.txt")
print("Done.")