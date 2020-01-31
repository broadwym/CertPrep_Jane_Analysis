library(dplyr)
library(fuzzyjoin)

all_assessments <- read.csv("~/Desktop/Subset_Asessments.csv")
jane <- read.csv("~/Desktop/JANE.csv")

all_assessments <- sapply(all_assessments, function(x) gsub("A Cert Prep Program:", "", x)) # cleaning 
all_assessments <- as.data.frame(all_assessments)
all_assessments <- sapply(all_assessments, function(x) gsub("Assessment -", "", x)) # cleaning
all_assessments <- as.data.frame(all_assessments)
all_assessments <- sapply(all_assessments, function(x) gsub("Cert Prep:", "", x)) # cleaning
all_assessments <- as.data.frame(all_assessments)

jane <- jane[-3] 
jane <- as.data.frame(sapply(jane, function(x) gsub("RN:", "", x))) # cleaning
jane <- as.data.frame(sapply(jane, function(x) gsub("Assessment-No Timer", "", x))) # cleaning
jane <- as.data.frame(sapply(jane, function(x) gsub("Assessment", "", x))) # cleaning

object_size(all_assessments)
object_size(jane)

df <- stringdist_join(all_assessments, jane, 
                by = "course_name",
                mode = "left",
                ignore_case = FALSE, 
                method = "jw", 
                max_dist = .5, 
                distance_col = "dist") %>%
  group_by(course_name.x) %>%
  top_n(1, -dist)

df2 <- df %>%
  select(2, 3, 6, 10) # grab specific columns from df 

write.csv(df2, "~/Desktop/certprep_jane.csv") # export as .csv


## choose another methodology 
df3 <- stringdist_join(all_assessments, jane, 
                       by = "course_name",
                       mode = "full",
                       ignore_case = FALSE, 
                       method = "lv", 
                       max_dist = 90, 
                       distance_col = "dist") %>%
  group_by(course_name.x) %>%
  top_n(1, -dist)

df4 <- df3 %>%
  select(2, 3, 6, 10)


## join gold standard list and .xls of all Cert Preps
names(All_Asessments)[4] <- "certprep_asessment"
merged <- merge(gold_standard, All_Asessments, by = "certprep_asessment", all = TRUE)
merged <- merged[complete.cases(merged), ]


# export as .csv
write.csv(merged, "~/Desktop/merged.csv")
