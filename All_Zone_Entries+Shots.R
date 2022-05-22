install.packages('tidyverse')
install.packages('tidymodels')
install.packages('lubridate')
install.packages('writexl')
library(tidyverse)
library(tidymodels)
library(lubridate)
library(writexl)

# Filtering so only events are zone entries and shots
entries_shots <- pxpdata %>%
  filter((Event == 'Shot')|(Event == 'Zone Entry')|(Event == 'Goal'))

# This was a copy data set so I could compare and make sure the follwing filtering steps were being performed as I wanted
E_S <- pxpdata %>%
  filter((Event == 'Shot')|(Event == 'Zone Entry')|(Event == 'Goal'))

# This classified each zone entry as either left, middle left, middle, middle right or right
for (i in 1:nrow(entries_shots)) {
  if (entries_shots[i,'Event'] == 'Zone Entry') {
    if ((0 <= entries_shots[i,'Y.Coordinate']) & (entries_shots[i,'Y.Coordinate'] < 17)) {
      entries_shots[i,'entry_loc'] = 'Left'
    }
    if ((17 <= entries_shots[i,'Y.Coordinate']) & (entries_shots[i,'Y.Coordinate'] < 34)) {
      entries_shots[i,'entry_loc'] = 'Middle Left'
    }
    if ((34 <= entries_shots[i,'Y.Coordinate']) & (entries_shots[i,'Y.Coordinate'] <= 51)) {
      entries_shots[i,'entry_loc'] = 'Middle'
    }
    if ((51 < entries_shots[i,'Y.Coordinate']) & (entries_shots[i,'Y.Coordinate'] < 68)) {
      entries_shots[i,'entry_loc'] = 'Middle Right'
    }
    if ((68 <= entries_shots[i,'Y.Coordinate']) & (entries_shots[i,'Y.Coordinate'] <= 85)) {
      entries_shots[i,'entry_loc'] = 'Right'
    }
  }
}

# Filtering data set to only include shots taken within 10 seconds of an entry to isolate rush offense
for (i in 1:(nrow(entries_shots)-1)) {
  stop = FALSE  
    if (entries_shots[i,'Event'] == 'Zone Entry') {
      for (j in 1:5) {
        if ((entries_shots[i+j,'Event'] == 'Shot')|(entries_shots[i+j,'Event'] == 'Goal')) {
          time1 = strptime(entries_shots[i,'Clock'],format = '%M:%S')
          time2 = strptime(entries_shots[i+j,'Clock'],format = '%M:%S')
          timediff = as.numeric(time1)-as.numeric(time2)
          if (timediff > 10) {
            entries_shots <- entries_shots[-c(i+j),]
          } }
        if (entries_shots[i+j,'Event']=='Zone Entry') {
          stop = TRUE
          }
        if (stop == TRUE) {break}
  } } }
# The above code needs to be looped for some reason.
# There should be 4387 obs.

# Giving each player their handedness (off eliteprospects)
for (i in 1:nrow(entries_shots)) {
  name = entries_shots[i,'Player']
  entries_shots[i,'Handedness'] = players$Shoots[players$Player == as.character(name)]
}

# Pairing each zone entry with the shots that occured during that entry
temp = 1
for (i in 1:(nrow(entries_shots)-1)) {
  if (entries_shots[i,'Event'] == 'Zone Entry') {
    entries_shots[i,'ze_num'] = temp
    if ((entries_shots[i+1,'Event'] == 'Shot')|(entries_shots[i+1,'Event'] == 'Goal')) {
      entries_shots[i+1,'ze_num'] = temp
      entries_shots[i+1,'entry_loc'] = entries_shots[i,'entry_loc']
    }
    if ((entries_shots[i+2,'Event'] == 'Shot')|(entries_shots[i+2,'Event'] == 'Goal')) {
      entries_shots[i+2,'ze_num'] = temp
      entries_shots[i+2,'entry_loc'] = entries_shots[i,'entry_loc']
    }
    if ((entries_shots[i+3,'Event'] == 'Shot')|(entries_shots[i+3,'Event'] == 'Goal')) {
      entries_shots[i+3,'ze_num'] = temp
      entries_shots[i+3,'entry_loc'] = entries_shots[i,'entry_loc']
    }
    if ((entries_shots[i+4,'Event'] == 'Shot')|(entries_shots[i+4,'Event'] == 'Goal')) {
      entries_shots[i+4,'ze_num'] = temp
      entries_shots[i+4,'entry_loc'] = entries_shots[i,'entry_loc']
    }
    temp = temp+1
  }
}

# These indices allowed to to separate the big data set to three smaller ones focusing on carried, played and dumped entries
carried_index <- c()
for (i in 1:nrow(entries_shots[,'ze_num'])) {
  if (entries_shots[i,'Detail.1'] == 'Carried') {
    newelement <- entries_shots[i,'ze_num']
    carried_index <- c(carried_index,newelement)
    }
}
played_index <- c()
for (i in 1:nrow(entries_shots[,'ze_num'])) {
  if (entries_shots[i,'Detail.1'] == 'Played') {
    newelement <- entries_shots[i,'ze_num']
    played_index <- c(played_index,newelement)
  }
}
dumped_index <- c()
for (i in 1:nrow(entries_shots[,'ze_num'])) {
  if (entries_shots[i,'Detail.1'] == 'Dumped') {
    newelement <- entries_shots[i,'ze_num']
    dumped_index <- c(dumped_index,newelement)
  }
}

carried_entries = entries_shots
dumped_entries = entries_shots
played_entries = entries_shots

# Creating the sub data sets
for (i in 1:nrow(carried_entries)) {
  if (carried_entries[i,'ze_num'] %in% carried_index == FALSE) {
    carried_entries <- carried_entries[-c(i),]
  }
}
# Above code needs to be repeated for some reason to get correct number of entries (2906)

for (i in 1:nrow(dumped_entries)) {
  if (dumped_entries[i,'ze_num'] %in% dumped_index == FALSE) {
    dumped_entries <- dumped_entries[-c(i),]
  }
}
# Same for this (1163)

for (i in 1:nrow(played_entries)) {
  if (played_entries[i,'ze_num'] %in% played_index == FALSE) {
    played_entries <- played_entries[-c(i),]
  }
}
# And this (318)

sum(carried_entries$xG,na.rm = TRUE)/length(which(carried_entries$Event == 'Zone Entry'))
# Average xG per carried zone entry
sum(played_entries$xG,na.rm = TRUE)/length(which(played_entries$Event == 'Zone Entry'))
# Average xG per played zone entry
sum(dumped_entries$xG,na.rm = TRUE)/length(which(dumped_entries$Event == 'Zone Entry'))
# Average xG per dumped zone entry

# Carried zone entries by location
c_l_entry <- carried_entries %>%
  filter(entry_loc == "Left")
c_ml_entry <- carried_entries %>%
  filter(entry_loc == 'Middle Left')
c_m_entry <- carried_entries %>%
  filter(entry_loc == 'Middle')
c_mr_entry <- carried_entries %>%
  filter(entry_loc == 'Middle Right')
c_r_entry <- carried_entries %>%
  filter(entry_loc == 'Right')

carriedentrysummary <- function() {
  average = sum(c_l_entry$xG,na.rm = TRUE)/(length(which(c_l_entry$Event == 'Zone Entry')))
  message('Average xG per carried left entry is: ',average)
  average = sum(c_ml_entry$xG,na.rm = TRUE)/(length(which(c_ml_entry$Event == 'Zone Entry')))
  message('Average xG per carried middle left entry is: ',average)
  average = sum(c_m_entry$xG,na.rm = TRUE)/(length(which(c_m_entry$Event == 'Zone Entry')))
  message('Average xG per carried middle entry is: ',average)
  average = sum(c_mr_entry$xG,na.rm = TRUE)/(length(which(c_mr_entry$Event == 'Zone Entry')))
  message('Average xG per carried middle right entry is: ',average)
  average = sum(c_r_entry$xG,na.rm = TRUE)/(length(which(c_r_entry$Event == 'Zone Entry')))
  message('Average xG per carried right entry is: ',average)
  }  

# Played zone entries by location
p_l_entry <- played_entries %>%
  filter(entry_loc == "Left")
p_ml_entry <- played_entries %>%
  filter(entry_loc == 'Middle Left')
p_m_entry <- played_entries %>%
  filter(entry_loc == 'Middle')
p_mr_entry <- played_entries %>%
  filter(entry_loc == 'Middle Right')
p_r_entry <- played_entries %>%
  filter(entry_loc == 'Right')

playedentrysummary <- function() {
  average = sum(p_l_entry$xG,na.rm = TRUE)/(length(which(p_l_entry$Event == 'Zone Entry')))
  message('Average xG per played left entry is: ',average)
  average = sum(p_ml_entry$xG,na.rm = TRUE)/(length(which(p_ml_entry$Event == 'Zone Entry')))
  message('Average xG per played middle left entry is: ',average)
  average = sum(p_m_entry$xG,na.rm = TRUE)/(length(which(p_m_entry$Event == 'Zone Entry')))
  message('Average xG per played middle entry is: ',average)
  average = sum(p_mr_entry$xG,na.rm = TRUE)/(length(which(p_mr_entry$Event == 'Zone Entry')))
  message('Average xG per played middle right entry is: ',average)
  average = sum(p_r_entry$xG,na.rm = TRUE)/(length(which(p_r_entry$Event == 'Zone Entry')))
  message('Average xG per played right entry is: ',average)
}  

# Dumped Zone Entries by location
d_l_entry <- dumped_entries %>%
  filter(entry_loc == "Left")
d_ml_entry <- dumped_entries %>%
  filter(entry_loc == 'Middle Left')
d_m_entry <- dumped_entries %>%
  filter(entry_loc == 'Middle')
d_mr_entry <- dumped_entries %>%
  filter(entry_loc == 'Middle Right')
d_r_entry <- dumped_entries %>%
  filter(entry_loc == 'Right')

dumpedentrysummary <- function() {
  average = sum(d_l_entry$xG,na.rm = TRUE)/(length(which(d_l_entry$Event == 'Zone Entry')))
  message('Average xG per dumped left entry is: ',average)
  average = sum(d_ml_entry$xG,na.rm = TRUE)/(length(which(d_ml_entry$Event == 'Zone Entry')))
  message('Average xG per dumped middle left entry is: ',average)
  average = sum(d_m_entry$xG,na.rm = TRUE)/(length(which(d_m_entry$Event == 'Zone Entry')))
  message('Average xG per dumped middle entry is: ',average)
  average = sum(d_mr_entry$xG,na.rm = TRUE)/(length(which(d_mr_entry$Event == 'Zone Entry')))
  message('Average xG per dumped middle right entry is: ',average)
  average = sum(d_r_entry$xG,na.rm = TRUE)/(length(which(d_r_entry$Event == 'Zone Entry')))
  message('Average xG per dumped right entry is: ',average)
}  

# Summary of zone entry xG expectations by location and entry type
carriedentrysummary()
playedentrysummary()
dumpedentrysummary()