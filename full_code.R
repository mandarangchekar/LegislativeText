# Initialize the data frame
df <- data.frame(
  File = character(),
  Bill_Number = character(),
  Introduced_By_Members = character(),
  Introduced_By_Date = character(),
  Approved_By_Members = character(),
  Approved_By_Date = character(),
  All_Text = character(),
  stringsAsFactors = FALSE
)

# Define the directory containing the .txt files
dir_path <- "/Users/mandarangchekar/Syracuse/RA_Prof._Braton_Prof.Siddiki/2001-2002"

# List of files in the directory with .txt extension
file_paths <- list.files(dir_path, pattern = "\\.txt$", full.names = TRUE)

# Pattern for the date
pattern_date <- "(January|February|March|April|May|June|July|August|September|October|November|December)\\W*\\d{1,2}\\W*,\\W*\\d{4}"

# Loop through each file and extract the details
for (file_path in file_paths) {
  # Read the content of the file into a variable
  bill_text <- readLines(file_path)
  
  # Convert the content to a single string
  text <- paste(bill_text, collapse = "\n")
  
  # Extract bill_number
  bill_number_match <- regexpr("No\\. ([0-9]+)", text)
  if (bill_number_match != -1) {
    bill_number <- regmatches(text, bill_number_match)[[1]][1]
  } else {
    bill_number <- NA
  }
  
  # Extract Introduced By details
  splits_intro <- strsplit(text, "Introduced[[:space:]\u00A0]+by")[[1]]
  if(length(splits_intro) > 1) {
    date_start_pos_intro <- regexpr(pattern_date, splits_intro[2])[1] 
    introduced_by_members <- substr(splits_intro[2], 1, date_start_pos_intro - 1)
    introduced_by_date <- regmatches(splits_intro[2], regexpr(pattern_date, splits_intro[2]))
  } else {
    introduced_by_members <- NA
    introduced_by_date <- NA
  }
  
  # Extract Approved By details
  splits_approved <- strsplit(text, "Approved\\s+by")[[1]]
  if(length(splits_approved) > 1) {
    date_start_pos_approved <- regexpr(pattern_date, splits_approved[2])[1] 
    approved_by_members <- substr(splits_approved[2], 1, date_start_pos_approved - 1)
    approved_by_date <- regmatches(splits_approved[2], regexpr(pattern_date, splits_approved[2]))
  } else {
    approved_by_members <- NA
    approved_by_date <- NA
  }
  
  # Add the details to the data frame
  df <- rbind(df, data.frame(
    File = basename(file_path),
    Bill_Number = bill_number,
    Introduced_By_Members = introduced_by_members,
    Introduced_By_Date = introduced_by_date,
    Approved_By_Members = approved_by_members,
    Approved_By_Date = approved_by_date,
    All_Text = text
  ))
}

# Print the resulting data frame
View(df)
write.csv(df, "/Users/mandarangchekar/Syracuse/RA_Prof._Braton_Prof.Siddiki/SPAM/sample1.csv", row.names = FALSE)

