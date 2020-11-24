## DS 303 Final Project
# Authors: Jessie Bustin and Ben Litterer
#
# Data Cleaning and Merge
#
# Data collected from:
# 2012 & 2016 American Community Survey US Census Bureau
# https://www.census.gov/acs/www/data/data-tables-and-tools/
# MIT Election + Data Lab
# https://electionlab.mit.edu/research/voter-turnout


# Load Libraries
library(tidyverse)


# Import Datasets
turnout <- read.csv("countypres_2000-2016.csv")
race2012 <- read.csv("ACSDT5Y2012.B02001_data_with_overlays_2020-10-23T225800.csv")
race2016 <- read.csv("ACSDT5Y2016.B02001_data_with_overlays_2020-10-23T225800.csv")
age2012 <- read.csv("ACSST5Y2012.S0101_data_with_overlays_2020-10-24T100122.csv")
age2016 <- read.csv("ACSST5Y2016.S0101_data_with_overlays_2020-10-24T100122.csv")
edu2012 <- read.csv("ACSST5Y2012.S1501_data_with_overlays_2020-10-24T100826.csv")
edu2016 <- read.csv("ACSST5Y2016.S1501_data_with_overlays_2020-10-24T100826.csv")
income2012 <- read.csv("ACSST5Y2012.S1901_data_with_overlays_2020-10-24T100516.csv")
income2016 <- read.csv("ACSST5Y2016.S1901_data_with_overlays_2020-10-24T100516.csv")

# Clean Turnout Data
turnout <- turnout %>%
  filter(year %in% c(2012, 2016)) %>%
  select(-version, -office, -candidate)

# Pivot Turnout Wide
turnout <- turnout %>%
  pivot_wider(names_from = party, values_from = candidatevotes) %>%
  select(-"NA")

## Clean, Select, and Prep Age Data for Merge
#2012
age2012 <- age2012[-1,]

age2012 <- age2012 %>%
  select(NAME, S0101_C01_001E, S0101_C01_025E, S0101_C01_022E, S0101_C01_028E, 
         S0101_C01_030E, S0101_C01_031E) %>%
  rename(over18percent = S0101_C01_025E, 
         over65 = S0101_C01_028E,
         under24 = S0101_C01_022E,
         medianAge = S0101_C01_030E,
         sexRatio = S0101_C01_031E,
         totalPop = S0101_C01_001E)

age2012$year <- 2012

age2012 <- age2012 %>%
  separate(NAME, c("county", "state"), sep = "([,])") %>%
  mutate(county = str_trim(str_remove(county, "County")))

age2012$state <- trimws(age2012$state, which= c("left"))

#2016
age2016 <- age2016[-1,]

age2016 <- age2016 %>%
  select(NAME, S0101_C01_001E, S0101_C01_025E, S0101_C01_022E, S0101_C01_028E, 
         S0101_C01_030E, S0101_C01_031E) %>%
  rename(over18percent = S0101_C01_025E, 
         over65 = S0101_C01_028E,
         under24 = S0101_C01_022E,
         medianAge = S0101_C01_030E,
         sexRatio = S0101_C01_031E,
         totalPop = S0101_C01_001E)

age2016$year <- 2016

age2016 <- age2016 %>%
  separate(NAME, c("county", "state"), sep = "([,])") %>%
  mutate(county = str_trim(str_remove(county, "County")))

age2016$state <- trimws(age2016$state, which= c("left"))

# Merge 2012 and 2016
age <- rbind(age2012, age2016)

# Data Merge Fixes
age <- age %>%
  mutate(county = str_trim(str_remove(county, "Parish"))) %>%
  mutate(county = str_trim(str_remove(county, "city")))

age[2163, 1] <- "Dona Ana"
age[5144, 1] <- "Dona Ana"

turnout[2556, 4] <- "DeWitt"
turnout[5673, 4] <- "DeWitt"

turnout[304, 4] <- "DeSoto"
turnout[3421, 4] <- "DeSoto"

turnout[1113, 4] <- "LaSalle"
turnout[4230, 4] <- "LaSalle"

turnout[1321, 4] <- "Lac qui Parle"
turnout[4438, 4] <- "Lac qui Parle"

turnout[1353, 4] <- "St. Louis"
turnout[4470, 4] <- "St. Louis"

turnout <- turnout %>%
  filter(state != "Alaska") %>%
  filter(county != "Statewide writein") %>%
  filter(county != "Maine UOCAVA") %>%
  filter(county != "Federal Precinct") %>%
  filter(county != "St. Louis City") %>%
  mutate(county = str_trim(str_remove(county, "County")))

# Merge Turnout and Age Data
full <- left_join(turnout, age, by = c("state", "county", "year"))

# The following is used to check the success of the merge
# Rows in the fail dataframe were checked for reason of fail,
# reason was fixed, and merge reran.
fails <- full %>%
  filter(is.na(totalPop))

# After fixes, there were 5 rows in the voter turnout data that
# did not have census data so they are being dropped.  The Alaska
# data was also removed as the districts in 1 dataset did not
# line up with the counties in the other.
full <- full %>%
  drop_na()

## Create Turnout rate column
full$turnout <- full$totalvotes/(as.numeric(full$totalPop) * (as.numeric(full$over18percent) / 100)) * 100

# These are places where a county vs city were confused,
# they are not being removed because they are outliers 
# but because they are incorrect data points
tooHigh <- full %>%
  filter(turnout > 100)

tooLow <- full %>%
  filter(turnout < 10)

full <- anti_join(full, tooHigh)
full <- anti_join(full, tooLow)

# Checking distribution of turnout rate
hist(full$turnout)

## Race Clean, Subset, and merge
# 2012
race2012 <- race2012[-1,]

race2012 <- race2012 %>%
  select(NAME, B02001_001E, B02001_002E) %>%
  mutate(whitePercent = as.numeric(B02001_002E)/as.numeric(B02001_001E) * 100) %>%
  select(-B02001_001E, -B02001_002E) %>%
  separate(NAME, c("county", "state"), sep = "([,])") %>%
  mutate(county = str_trim(str_remove(county, "County"))) %>%
  mutate(county = str_trim(str_remove(county, "Parish"))) %>%
  mutate(county = str_trim(str_remove(county, "city")))

race2012$year <- 2012

# 2016
race2016 <- race2016[-1,]

race2016 <- race2016 %>%
  select(NAME, B02001_001E, B02001_002E) %>%
  mutate(whitePercent = as.numeric(B02001_002E)/as.numeric(B02001_001E) * 100) %>%
  select(-B02001_001E, -B02001_002E) %>%
  separate(NAME, c("county", "state"), sep = "([,])") %>%
  mutate(county = str_trim(str_remove(county, "County"))) %>%
  mutate(county = str_trim(str_remove(county, "Parish"))) %>%
  mutate(county = str_trim(str_remove(county, "city")))

race2016$year <- 2016

# Merge 2012 with 2016 Race
race <- rbind(race2012, race2016)

race$state <- trimws(race$state, which= c("left"))

race[1389, 1] <- "Dona Ana"
race[5302, 1] <- "Dona Ana"

# Merge race with full data
full <- left_join(full, race, by = c("state", "county", "year"))

# Test merge, 0 fails, yay!!!!
fails <- full %>%
  filter(is.na(whitePercent))

## Clean, subset, and merge edu data
#2012
edu2012 <- edu2012[-1,]

edu2012 <- edu2012 %>%
  select(NAME, S1501_C01_009E, S1501_C01_012E, S1501_C01_013E) %>%
  rename(highschool = S1501_C01_009E, bachelor = S1501_C01_012E, grad = S1501_C01_013E) %>%
  separate(NAME, c("county", "state"), sep = "([,])") %>%
  mutate(county = str_trim(str_remove(county, "County"))) %>%
  mutate(county = str_trim(str_remove(county, "Parish"))) %>%
  mutate(county = str_trim(str_remove(county, "city")))

edu2012$year <- 2012

#2016
edu2016 <- edu2016[-1,]

edu2016 <- edu2016 %>%
  select(NAME, S1501_C01_009E, S1501_C01_012E, S1501_C01_013E) %>%
  rename(highschool = S1501_C01_009E, bachelor = S1501_C01_012E, grad = S1501_C01_013E) %>%
  separate(NAME, c("county", "state"), sep = "([,])") %>%
  mutate(county = str_trim(str_remove(county, "County"))) %>%
  mutate(county = str_trim(str_remove(county, "Parish"))) %>%
  mutate(county = str_trim(str_remove(county, "city")))

edu2016$year <- 2016

# Merge edu data
edu <- rbind(edu2012, edu2016)

edu$state <- trimws(edu$state, which= c("left"))

edu[2163, 1] <- "Dona Ana"
edu[5144, 1] <- "Dona Ana"

# Merge edu with full
full <- left_join(full, edu, by = c("state", "county", "year"))

# Test merge, 0 fails, yay!!!!
fails <- full %>%
  filter(is.na(highschool))

## Clean, subset, and merge income data
# 2012
income2012 <- income2012[-1,]

income2012 <- income2012 %>%
  select(NAME, S1901_C01_012E, S1901_C01_013E) %>%
  rename(medianIncome = S1901_C01_012E, meanIncome = S1901_C01_013E)%>%
  separate(NAME, c("county", "state"), sep = "([,])") %>%
  mutate(county = str_trim(str_remove(county, "County"))) %>%
  mutate(county = str_trim(str_remove(county, "Parish"))) %>%
  mutate(county = str_trim(str_remove(county, "city")))

income2012$year <- 2012

# 2016
income2016 <- income2016[-1,]

income2016 <- income2016 %>%
  select(NAME, S1901_C01_012E, S1901_C01_013E) %>%
  rename(medianIncome = S1901_C01_012E, meanIncome = S1901_C01_013E)%>%
  separate(NAME, c("county", "state"), sep = "([,])") %>%
  mutate(county = str_trim(str_remove(county, "County"))) %>%
  mutate(county = str_trim(str_remove(county, "Parish"))) %>%
  mutate(county = str_trim(str_remove(county, "city")))

income2016$year <- 2016

# Merge edu data
income <- rbind(income2012, income2016)

income$state <- trimws(income$state, which= c("left"))

income[2163, 1] <- "Dona Ana"
income[5024, 1] <- "Dona Ana"

# Merge edu with full
full <- left_join(full, income, by = c("state", "county", "year"))

# Test merge, 0 fails, yay!!!!
fails <- full %>%
  filter(is.na(medianIncome))

# De-duplication of Virginia Counties
full <- full %>%
  distinct(state, county, year, .keep_all = TRUE)

# Save full dataset
write.csv(full, "Turnout_Demos_County.csv")

full <- full %>%
  mutate(totalPop = as.integer(totalPop),
         over18percent = as.numeric(over18percent),
         under24 = as.numeric(under24),
         over65 = as.numeric(over65),
         medianAge = as.numeric(medianAge),
         sexRatio = as.numeric(sexRatio),
         highschool = as.numeric(highschool),
         bachelor = as.numeric(bachelor),
         grad = as.numeric(grad),
         medianIncome = as.numeric(medianIncome),
         meanIncome = as.numeric(meanIncome))

full$highschool <- ifelse(full$year == 2016, full$highschool / (full$totalPop - full$under24) * 100, full$highschool)
full$bachelor <- ifelse(full$year == 2016, full$bachelor / (full$totalPop - full$under24) * 100, full$bachelor)
full$grad <- ifelse(full$year == 2016, full$grad / (full$totalPop - full$under24) * 100, full$grad)

fails <- full %>%
  filter(is.na(medianIncome))

full <- drop_na(full)

write.csv(full, "Turnout_Demos_County.csv")

## Just playing below
n = dim(full)[1]
train_index = sample(1:n,n/2,rep=FALSE)

train = full[train_index,]
test = full[-train_index,]

model_train = lm(turnout~totalPop+under24+over65+medianAge+sexRatio+whitePercent+
                   highschool+bachelor+medianIncome, data = train)

summary(model_train)
