#### PREPARE WORKSPACE ####

# Import libraries
library(tidyr)
library(dplyr)
library(MASS)
library(TRAMPR)
library(glm.predict)
library(pscl)
library(stringr)
library(randomForest)
library(caret)

# Set random seed
set.seed(1234)


#### CLEAN COMPLIANCE DATA ####

# Read in compliance data
df_compliance <- read.csv("./data/compliance.csv") %>%
  # Edit columns
  mutate(
    # Make commitment year numeric
    year = as.numeric(year)) %>%
  # Convert data to long format
  gather(country, score, argentina:european.union, factor_key = T) %>%
  # Edit columns
  mutate(
    # Replace incorrectly written scores and convert to integer
    score = as.factor(case_when(
      score == "" ~ as.character(NA),
      score == "-" ~ "-1",
      score == "−1" ~ "-1",
      score == "+" ~ "1",
      TRUE ~ score))) %>%
  # Merge summit dates with compliance data
  merge(read.csv("./data/summit_dates.csv"))


#### A) ANALYZE COMPLIANCE BY MINISTERIAL MEETING ####

# Read in ministerial dates data
df_ministerial.dates <- read.csv(
  "./data/ministerial_dates.csv", na.strings = c("","NA")) %>%
  # Merge summit dates with ministerial dates
  merge(read.csv("./data/summit_dates.csv")) %>%
  # Edit columns
  mutate(
    # Count number of ministerial for a given year-subject
    min.number = rowSums(!is.na(.[3:11])))

# Calculate number of days between ministerial meetings and summit
for (i in colnames(df_ministerial.dates[3:11])) {
  name <- gsub(" ", "", paste(i, ".dist"))
  df_ministerial.dates[[name]] <- as.integer(
    as.Date(df_ministerial.dates[[i]]) - as.Date(df_ministerial.dates$summit.date))
}

# Create column for any ministerial before
df_ministerial.dates$min.before <- ifelse(is.na(apply(
  df_ministerial.dates[15:23], 1, function(row) any(row < 0))), 0, 1)

# Create column for any ministerial after
df_ministerial.dates$min.after <- ifelse(is.na(apply(
  df_ministerial.dates[15:23], 1, function(row) any(row > 0))), 0, 1)

# Create column for closest ministerial
df_ministerial.dates$min.closest <-
  apply(abs(df_ministerial.dates[15:23]), 1, absolute.min)

# Merge closest ministerials with compliance data
cols <- c("year", "area", "min.number", "min.before", "min.after", "min.closest")
df_compliance <- left_join(df_compliance, df_ministerial.dates[cols])

# Format missing values in ministerial columns
cols <- c("min.number", "min.before", "min.after")
df_compliance[cols][is.na(df_compliance[cols])] <- 0

# Add column for any ministerial
df_compliance$min.any <- ifelse(is.na(df_compliance$min.closest), 0, 1)

# Test effect of number of ministerials on compliance
fit <- polr(score ~ min.number + year, data = df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))

# Test effect of before-summit ministerial on compliance
fit <- polr(score ~ min.before + year, data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))

# Test effect of after-summit ministerial on compliance
fit <- polr(score ~ min.after + year, data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))

# Test effect of closest ministerial on compliance
fit <- polr(score ~ min.closest + year, data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))

# Test effect of any ministerial on compliance
fit <- polr(score ~ min.any + year, data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))

#### B) ANALYZE COMPLIANCE BY SAME-SUBJECT COMMITMENTS ####

# Read in commitment data
df_commitments <- read.csv("./data/total_commitments.csv") %>%
  # Convert data to long format
  gather(year, commitments, X2008:X2021, factor_key=TRUE) %>%
  # Edit columns
  mutate(
    year = as.numeric(substring(year, 2, 5)),
    # Replace missing values with zeroes
    commitments = ifelse(is.na(commitments), 0, commitments))

# Merge with compliance data
df_compliance <- left_join(df_compliance, df_commitments)

# Make column for commitments squared
df_compliance$commitments2 <- df_compliance$commitments^2

# Test effect of total commitments on compliance
fit <- polr(score ~ total + year, data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))

# Test effect of same-subject commitments on compliance
fit <- polr(score ~ commitments + year, data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))


has #### C) ANALYZE COMPLIANCE BY BINDING LEVEL ####

# Set high binding level
high = c('commit','agree','endorse','pledge','promise','must renew','seek',
         'reaffirm commitment','intend to','create','will','are determined to',
         'continue to','insist on','remain determined','affirm our intention',
         'ensure','complete','reaffirm the need','reaffirmed the importance',
         'remain focussed','we request','ratify','publish','implement','look to',
         'strongly advocate','more ambitious','we shall','pursue','immediate action',
         'investment','decide to','plan of action','resolve to','hold a','mobilize',
         'determination to','undertake to mobilize','mandate','preventing',
         'confirmed additional contributions','further efforts','raising',
         'take sustainable measures', 'strive to')

# Set medium binding level
mid = c('encourage','promote','support','stand ready to','shall ourselves object',
        'need to','must ensure','necessary','need to address','ought to',
        'we emphasize the need','foster','help','strengthen','working to',
        'making available', 'facilitate','strive for','cooperate','should',
        'action is required','stimulate','aim to','necessity','also working')

# Set low binding level
low = c('welcome','urge','call on','reflected upon','discussed','are aware',
        'look forward to','emphasize','call for','recognize the importance',
        'we gave particular emphasis to','united in determination','wishes to',
        'should stand ready to','express confidence in', 'consider',
        'reaffirm the need for advocate','call for','ready to','ask')

# Remove blank and single word commitment text
df_compliance$text <-
  ifelse(df_compliance$text == "" | df_compliance$text == "Trade",
         NA, df_compliance$text)

# Code commitment text by binding level
df_compliance$binding.level <- NA
df_compliance$binding.level <-
  ifelse(str_detect(tolower(df_compliance$text), paste(high, collapse = '|')),
         "High", df_compliance$binding.level)
df_compliance$binding.level <-
  ifelse(str_detect(tolower(df_compliance$text), paste(mid, collapse = '|')),
         "Mid", df_compliance$binding.level)
df_compliance$binding.level <-
  ifelse(str_detect(tolower(df_compliance$text), paste(low, collapse = '|')),
         "Low", df_compliance$binding.level)

# Combine medium and low biding levels
df_compliance$binding.level <- ifelse(df_compliance$binding.level != "Low", 1, 0)

# Test effect of binding level on compliance
fit <- polr(score ~ binding.level + year, data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))


#### D) ANALYZE COMPLIANCE BY REFERENCE TO ANOTHER SUMMIT ####

# Set terms to exclude
exclude = c('world summit', 'johannesburg summit', 'observation summit',
            'anti-corruption summit', 'nato lisbon summit', 'un lcd summit')
detect = c('summit','osaka blue ocean')

# Identify commitments that reference a summit
df_compliance$summit.ref <- NA
df_compliance$summit.ref <-
  ifelse(str_detect(
    tolower(df_compliance$text), paste(detect, collapse = '|')), 1, 0)

# Exclude terms that do not reference 20 summit
df_compliance$summit.ref <-
  ifelse(str_detect(tolower(df_compliance$text), paste(exclude, collapse = '|')),
         0, df_compliance$summit.ref)

# Test effect of summit reference on compliance
fit <- polr(score ~ summit.ref + year, data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))


#### E) ANALYZE COMPLIANCE BY REFERENCE TO INTERNATIONAL ORGANIZATION ####

# Set international organization terms
orgs = c('united nations', 'oecd', ' un ', 'g20', 'world health organization',
         'unaids', 'who plan', 'with who', 'global action plan', 'the who',
         'global health security agenda')

# Identify commitments that reference an international organization
df_compliance$org.ref <-
  ifelse(str_detect(tolower(df_compliance$text), paste(orgs, collapse = '|')), 1, 0)

# Test effect of international organization reference on compliance
fit <- polr(score ~ org.ref + year, data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))


#### G) ANALYZE COMPLIANCE BY REFERENCE TO TIMETABLE ####

# Identify commitments that mention single year timetable
match <- c("19|20\\d{2}", "\\d+/\\d+/\\d+", "next year",
           "end of the year", "this year")
df_compliance$mentions.date <-
  ifelse(grepl(paste(match, collapse = "|"), df_compliance$text), 1, 0)

# Identify commitments that mention multi-year timetable
match <- c("four year", "19|20\\d{2}-19|20\\d{2}", "5 years", "five years",
           "ten years", "three years", "four-year")
df_compliance$mentions.multi.year <-
  ifelse(grepl(paste(match, collapse = "|"), df_compliance$text), 1, 0)

# Test effect of date reference on compliance
fit <- polr(score ~ mentions.date + year, data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))

# Test effect of multiyear reference on compliance
fit <- polr(score ~ mentions.multi.year + year, data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))


#### H) ANALYZE COMPLIANCE BY REFERENCE TO MONEY MOBILIZED ####

# Identify commitments that mention money
match <- c("\\$", "billion", "million")
df_compliance$mentions.money <-
  ifelse(grepl(paste(match, collapse = "|"), df_compliance$text), 1, 0)

# Test effect of money reference on compliance
fit <- polr(score ~ mentions.money + year, data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))


#### I) ANALYZE COMPLIANCE BY INTERNATIONAL ORGANIZATION MEMBERSHIP ####

# Merge with organization membership data
df_compliance <- merge(df_compliance, read.csv("./data/org_membership.csv")) %>%
  # Encode columns
  mutate(
    p5 = ifelse(p5 == "yes", 1, 0),
    g7 = ifelse(g7 == "yes", 1, 0),
    brics = ifelse(brics == "yes", 1, 0),
    oecd = ifelse(oecd == "yes", 1, 0),
    imf_economy = ifelse(imf_economy == "advanced", 1, 0))

# Test effect of organization membership on compliance
fit <- polr(score ~ p5 + g7 + brics + oecd + imf_economy + year,
            data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))


#### J) ANALYZE COMPLIANCE BY HOST NATION STATUS ####

# Create column for whether or not the country was the host
df_compliance$is.host <- ifelse(df_compliance$country == df_compliance$host, 1, 0)

# Test effect of host nation status on compliance
fit <- polr(score ~ is.host + year,
            data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))


#### K) ANALYZE COMPLIANCE BY GDP ####

# Read in GDP data
df_gdp.per.cap <- read.csv("./data/gdp_per_capita.csv")
df_gdp.growth <- read.csv("./data/gdp_per_capita_growth.csv")

# Convert data to long format
df_gdp.per.cap <-
  gather(df_gdp.per.cap, year, gdp.per.cap, X2008:X2021, factor_key=TRUE)
df_gdp.growth <-
  gather(df_gdp.growth, year, gdp.growth, X2008:X2021, factor_key=TRUE)

# Format year column
df_gdp.per.cap$year <- as.numeric(substring(df_gdp.per.cap$year, 2))
df_gdp.growth$year <- as.numeric(substring(df_gdp.growth$year, 2))

# Merge with compliance data
df_compliance <- left_join(df_compliance, df_gdp.per.cap)
df_compliance <- left_join(df_compliance, df_gdp.growth)

# Convert GDP per capita into thousands
df_compliance$gdp.per.cap <- df_compliance$gdp.per.cap / 10000

# Test effect of GDP on compliance
fit <- polr(score ~ gdp.per.cap + gdp.growth + year,
            data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))


#### L) ANALYZE COMPLIANCE BY MEMBER NATION ####

# Create new columns for countries of interest
df_compliance$is.argentina <- ifelse(df_compliance$country == "argentina", 1, 0)
df_compliance$is.australia <- ifelse(df_compliance$country == "australia", 1, 0)
df_compliance$is.brazil <- ifelse(df_compliance$country == "brazil", 1, 0)
df_compliance$is.canada <- ifelse(df_compliance$country == "canada", 1, 0)
df_compliance$is.china <- ifelse(df_compliance$country == "china", 1, 0)
df_compliance$is.eu <- ifelse(df_compliance$country == "european.union", 1, 0)
df_compliance$is.france <- ifelse(df_compliance$country == "france", 1, 0)
df_compliance$is.germany <- ifelse(df_compliance$country == "germany", 1, 0)
df_compliance$is.india <- ifelse(df_compliance$country == "india", 1, 0)
df_compliance$is.indonesia <- ifelse(df_compliance$country == "indonesia", 1, 0)
df_compliance$is.italy <- ifelse(df_compliance$country == "italy", 1, 0)
df_compliance$is.japan <- ifelse(df_compliance$country == "japan", 1, 0)
df_compliance$is.korea <- ifelse(df_compliance$country == "korea", 1, 0)
df_compliance$is.mexico <- ifelse(df_compliance$country == "mexico", 1, 0)
df_compliance$is.russia <- ifelse(df_compliance$country == "russia", 1, 0)
df_compliance$is.saudi <- ifelse(df_compliance$country == "saudi.arabia", 1, 0)
df_compliance$is.safrica <- ifelse(df_compliance$country == "south.africa", 1, 0)
df_compliance$is.turkey <- ifelse(df_compliance$country == "turkey", 1, 0)
df_compliance$is.uk <- ifelse(df_compliance$country == "united.kingdom", 1, 0)
df_compliance$is.us <- ifelse(df_compliance$country == "united.states", 1, 0)

# Test effect of member nation on compliance
fit <- polr(score ~ is.argentina + is.australia + is.brazil + is.canada
            + is.china + is.eu + is.france + is.germany + is.india
            + is.indonesia + is.italy + is.japan + is.korea + is.mexico
            + is.russia + is.saudi + is.safrica + is.turkey + is.uk + year,
            data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))


#### M) ANALYZE COMPLIANCE BY COMMITMENT ISSUE-AREA ####

# Create new columns for areas of interest
df_compliance$is.climate <- ifelse(df_compliance$area == "Climate Change", 1, 0)
df_compliance$is.crime <- ifelse(df_compliance$area == "Crime and Corruption", 1, 0)
df_compliance$is.dev <- ifelse(df_compliance$area == "Development", 1, 0)
df_compliance$is.energy <- ifelse(df_compliance$area == "Energy", 1, 0)
df_compliance$is.enviro <- ifelse(df_compliance$area == "Environment", 1, 0)
df_compliance$is.finreg <- ifelse(df_compliance$area == "Financial Regulation", 1, 0)
df_compliance$is.food <- ifelse(df_compliance$area == "Food and Agriculture", 1, 0)
df_compliance$is.gender <- ifelse(df_compliance$area == "Gender", 1, 0)
df_compliance$is.health <- ifelse(df_compliance$area == "Health", 1, 0)
df_compliance$is.ict <- ifelse(df_compliance$area == "ICT and Digitization", 1, 0)
df_compliance$is.ifi <- ifelse(df_compliance$area == "IFI Reform", 1, 0)
df_compliance$is.infra <- ifelse(df_compliance$area == "Infrastructure", 1, 0)
df_compliance$is.coop <- ifelse(df_compliance$area == "International Cooperation", 1, 0)
df_compliance$is.tax <- ifelse(df_compliance$area == "International Taxation", 1, 0)
df_compliance$is.labor <- ifelse(df_compliance$area == "Labour and Employment", 1, 0)
df_compliance$is.macro <- ifelse(df_compliance$area == "Macroeconomic Policy", 1, 0)
df_compliance$is.micro <- ifelse(df_compliance$area == "Microeconomic Policy", 1, 0)
df_compliance$is.ref <- ifelse(df_compliance$area == "Migration and Refugees", 1, 0)
df_compliance$is.terror <- ifelse(df_compliance$area == "Terrorism", 1, 0)
df_compliance$is.trade <- ifelse(df_compliance$area == "Trade", 1, 0)

# Test effect of commitment issue-area on compliance
fit <- polr(score ~ is.climate + is.crime + is.energy + is.enviro + is.finreg 
            + is.food + is.gender + is.health + is.ict + is.ifi + is.infra
            + is.coop + is.tax + is.labor + is.macro + is.micro + is.ref +
              is.terror + is.trade + year,
            data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))


#### BUILD MULTIVARIABLE MODEL TO EXPLAIN COMPLIANCE ####

# Build model with potentially significant variables
fit.olr <- polr(score ~ year + min.number + min.before + min.after + min.any + total 
                + binding.level + summit.ref + org.ref + mentions.date
                + mentions.multi.year + mentions.money + g7 + brics + oecd
                + imf_economy + is.host + gdp.per.cap + is.argentina + is.australia
                + is.brazil + is.canada + is.china + is.france + is.germany
                + is.india + is.indonesia + is.italy + is.japan + is.mexico
                + is.russia + is.saudi + is.uk + is.crime + is.dev + is.energy
                + is.enviro + is.finreg + is.health + is.ict + is.ifi + is.infra
                + is.coop + is.tax + is.labor + is.macro + is.micro + is.ref
                + is.terror, data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit.olr))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit.olr))
pR2(fit.olr)

fit.rf <- randomForest(score ~ year + min.number + min.before + min.after
                      + min.any + total + binding.level + summit.ref + org.ref
                      + mentions.date + mentions.multi.year + mentions.money
                      + g7 + brics + oecd + imf_economy + is.host + gdp.per.cap
                      + is.argentina + is.australia + is.brazil + is.canada
                      + is.china + is.france + is.germany + is.india
                      + is.indonesia + is.italy + is.japan + is.mexico
                      + is.russia + is.saudi + is.uk + is.crime + is.dev
                      + is.energy + is.enviro + is.finreg + is.health + is.ict
                      + is.ifi + is.infra + is.coop + is.tax + is.labor
                      + is.macro + is.micro + is.ref + is.terror,
                      data=df_compliance, na.action=na.omit)
fit.rf


#### BUILD FORECASTING MODEL ####

# Create training and test sets
dt <- sort(sample(nrow(drop_na(df_compliance)), nrow(drop_na(df_compliance)) * 0.9))
train <- drop_na(df_compliance)[dt,]
test <- drop_na(df_compliance)[-dt,]

# Create predictions
pred.olr <- predict(fit.olr, test)
pred.rf <- predict(fit.rf, test)

# Create confusion matrices
confusionMatrix(pred.olr, test$score)
confusionMatrix(pred.rf, test$score)


#### EXPORT DATA AS CSV ####

# Write CSV with full features included
write.csv(df_compliance, "G20 Dataset with Full Features.csv")