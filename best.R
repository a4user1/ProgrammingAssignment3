best <- function(state, outcome) {
        #read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        #check if the state and outcomes are valid
        Postal_code <- data[ , 7]
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        if ((state %in% Postal_code) == FALSE) {
                stop(print("invalid state"))
        }
        else if ((outcome %in% outcomes) == FALSE) {
                stop(print("invalid outcome"))
        }
        
        #get the subset of the data with the specified state
        new_data <- subset(data, State == state)
        
        #get the specified outcome column from the data file
        if (outcome == "heart attack") {
                outcome_column <- 11
        }
        else if (outcome == "heart failure") {
                outcome_column <- 17
        }
        else {
                outcome_column <- 23
        }
        
        #Omit the NA's in the desired outcome column
        required_columns <- as.numeric(new_data[,outcome_column])
        OMITE <- is.na(required_columns)
        desired_data <- new_data[!OMITE, ]
        
        #find the hospitals in the rows with the minimum outcome value
        columns_considered <- as.numeric(desired_data[, outcome_column])
        desired_rows <- which(columns_considered == min(columns_considered))
        desired_hospitals <- desired_data[desired_rows, 2]
        
        #if there are multiple hospitals with the minimum outcome value, then
        #return the first hospital name from the alphabetically ordered list
        if (length(desired_hospitals) > 1) {
                hospitals_sorted <- sort(desired_hospitals)
                hospitals_sorted[1]
        }
        else {
                desired_hospitals
        }
}