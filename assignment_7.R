#--------------------------------------#
#      HOMEWORK 7: PDF EXTRACTING      #
#--------------------------------------#

# Load packages 
library(tidyverse)
library(pdftools)
library(dplyr)
library(lubridate)

# Part 1.a

# Load in the data
crime_log <- pdftools::pdf_text("1-20-16.pdf")

# Part 2

## splitting the data by the newline character
crime_log_text <- crime_log %>%
  str_split("\n")


## unlisting the data
crime_log_text <- crime_log_text %>%
  unlist()

crime_log_text %>% head(10)

## converting all the data to lowercase
crime_log_text <- crime_log_text %>%
  str_to_lower() %>%
  str_trim()

crime_log_text %>% head(10)

# Part 3.a

## extracting data into new object that begin with "date reported"
date_reported_indices <- crime_log_text %>%
                         str_detect("^date reported") %>%
                         which

# Part 3.b 
## extracting data into new object that begin with "general location"
location_indices <- crime_log_text %>%
                    str_detect("^general location") %>%
                    which

# Part 3.c 
## extracting data into new object that begin with "date occurred from"
date_occurred_from_indices <- crime_log_text %>%
                              str_detect("^date occurred from") %>%
                              which

# Part 3.d 
## extracting data into new object that begin with "date occurred to"
date_occurred_to_indices <- crime_log_text %>%
                            str_detect("^date occurred to") %>%
                            which

# Part 3.e 
## extracting data into new object that begin with "incident"
incident_indices <- crime_log_text %>%
                    str_detect("^incident") %>%
                    which
# Part 3.f 
## extracting data into new object that begin with "disposition"
disposition_indices <- crime_log_text %>%
                       str_detect("^disposition") %>%
                       which
# Part 3.g 
## extracting data into new object that begin with "modified"
modified_indices <- crime_log_text %>%
                    str_detect("^modified") %>%
                    which


# Part 4.a
disposition <- crime_log_text[disposition_indices] %>%
               as_tibble() %>%
               extract(value, into = "disposition", "^disposition:\\s(.{1,})"  )

# Part 4.b
location <- crime_log_text[location_indices] %>%
  as_tibble() %>%
  extract(value, into = "location", "^general location:\\s\\s\\s(.{1,})"  ) %>%
  mutate(location = str_trim(location))

# Part 4.c
incident <- crime_log_text[incident_indices] %>%
  as_tibble() %>%
  extract(value, into = "incident", "^incident/offenses:\\s\\s(.{1,})"  ) %>%
  mutate(incident = str_trim(incident))

# Part 4.d
modified <- crime_log_text[modified_indices] %>%
  as_tibble() %>%
  extract(value, into = "modified_time", ".{1,}-\\s.{3}\\sat\\s(\\d{1,2}:\\d{1,2})", remove = F  ) %>%
  extract(value, into = "modified_date", "^modified date:\\s(\\d{1,2}/\\d{1,2}/\\d{2})" )

# Part 4.e
date_occurred_from <- crime_log_text[date_occurred_from_indices] %>%
  as_tibble() %>%
  extract(value, into = "time_occurred_from", ".{1,}-\\s.{3}\\sat\\s(\\d{1,2}:\\d{1,2})", remove = F) %>%
  extract(value, into = "date_occurred_from", "^date occurred from:\\s(\\d{1,2}/\\d{1,2}/\\d{2})")

# Part 4.f
date_occurred_to <- crime_log_text[date_occurred_to_indices] %>%
  as_tibble() %>%
  extract(value, into = "time_occurred_to", ".{1,}-\\s.{3}\\sat\\s(\\d{1,2}:\\d{1,2})", remove = F) %>%
  extract(value, into = "date_occurred_to", ".{1,}(\\d{1,2}/\\d{1,2}/\\d{2})")

# Part 4.g
date_reported <- crime_log_text[date_reported_indices] %>%
  as_tibble() %>%
  extract(value, into = "report_number", ".{1,}(\\d{6})", remove = F) %>%
  extract(value, into = "time_reported", ".{1,}\\s(\\d{1,2}:\\d{1,2}).{1,}", remove = F) %>%
  extract(value, into = "date_reported", ".{1,}\\s(\\d{1,2}/\\d{1,2}/\\d{2})") 


# Part 5
# The aoutograder doesn't like this order
#final_crime_log <- bind_cols(disposition, location, incident, modified, 
#                             date_occurred_from, date_occurred_to, date_reported)

final_crime_log <- bind_cols(date_reported,location,date_occurred_from,date_occurred_to,incident,disposition,modified)

#final_crime_log <- final_crime_log %>%
 # mutate(across(where(is.character), str_trim) )

# Part 6
# Recall that myd is the input not the output, the output is always YYYY-MM-DD
final_crime_log_cleaned <- final_crime_log %>%
                           mutate(across(c(date_reported, date_occurred_from, date_occurred_to), ~mdy(.) ))

