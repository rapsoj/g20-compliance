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
library(ggplot2)

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
  gather(country, score, argentina:eu, factor_key = T) %>%
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
  left_join(read.csv("./data/summit_dates.csv"), by = c("year", "code"))


#### A) ANALYZE COMPLIANCE BY MINISTERIAL MEETING ####

# Read in long form ministerial dates data
df_ministerial.dates.long <- read.csv(
  "./data/ministerial_dates_long.csv", na.strings = c("","NA")) %>%
  # Merge summit dates with ministerial dates
  merge(read.csv("./data/summit_dates.csv")) %>%
  # Edit columns
  mutate(
    # Count number of ministerial for a given year-subject
    min.number = rowSums(!is.na(.[3:11])))

# Calculate number of days between ministerial meetings and summit
for (i in colnames(df_ministerial.dates.long[3:11])) {
  name <- gsub(" ", "", paste(i, ".dist"))
  df_ministerial.dates.long[[name]] <- as.integer(
    as.Date(df_ministerial.dates.long[[i]])
    - as.Date(df_ministerial.dates.long$summit.date))
}

# Create column for any ministerial before summit
df_ministerial.dates.long$min.before <- ifelse(is.na(apply(
  df_ministerial.dates.long[16:24], 1, function(row) any(row <= 0))), 0, 1)

# Create column for any ministerial after summit
df_ministerial.dates.long$min.after <- ifelse(is.na(apply(
  df_ministerial.dates.long[16:24], 1, function(row) any(row >= 0))), 0, 1)

# Create column for closest ministerial
df_ministerial.dates.long$min.closest <-
  apply(abs(df_ministerial.dates.long[16:24]), 1, absolute.min)

# Merge closest ministerials with compliance data
cols <- c("year", "area", "host", "min.number",
          "min.closest",  "min.before", "min.after")
df_compliance <- left_join(df_compliance, df_ministerial.dates.long[cols],
                           by = c("year", "area", "host"))

# Format missing values in ministerial columns
cols <- c("min.number", "min.before", "min.after")
df_compliance[cols][is.na(df_compliance[cols])] <- 0

# Add column for any ministerial
df_compliance$min.any <- ifelse(is.na(df_compliance$min.closest), 0, 1)

# Read in wide form ministerial dates data
df_ministerial.dates.wide <- read.csv(
  "./data/ministerial_dates_wide.csv", na.strings = c("","NA")) %>%
  # Merge summit dates with ministerial dates
  merge(read.csv("./data/summit_dates.csv")) 

# Calculate number of days between ministerial meetings and summit
for (i in colnames(df_ministerial.dates.wide[2:75])) {
  name <- gsub(" ", "", paste(i, ".dist"))
  df_ministerial.dates.wide[[name]] <- as.integer(
    as.Date(df_ministerial.dates.wide[[i]])
    - as.Date(df_ministerial.dates.wide$summit.date))
}

# Create column for closest overall ministerial
df_ministerial.dates.wide$min.closest.overall <-
  apply(abs(df_ministerial.dates.wide[81:153]), 1, absolute.min)

# Merge closest ministerials with compliance data
cols <- c("year", "area", "host", "min.closest.overall")
df_compliance <- left_join(df_compliance, df_ministerial.dates.wide[cols],
                           by = c("year", "area", "host"))

# Replace missing closest ministerials with large numbers
cols <- c("min.closest", "min.closest.overall")
df_compliance[cols][is.na(df_compliance[cols])] <- 10000

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

# Test effect of closest overall ministerial on compliance
fit <- polr(score ~ min.closest.overall + year, data=df_compliance, Hess=TRUE)
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
  gather(id, commitments, X2008:X2022, factor_key=TRUE) %>%
  # Edit columns
  mutate(
    year = as.numeric(substring(id, 2, 5)),
    code = ifelse(
      year %in% c(2009, 2010), str_sub(id, -1), ""),
    # Replace missing values with zeroes
    commitments = ifelse(is.na(commitments), 0, commitments)) %>%
  # Remove id columns
  dplyr::select(-id)

# Merge with compliance data
df_compliance <- left_join(df_compliance, df_commitments,
                           by = c("area", "year", "code"))

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


#### C) ANALYZE COMPLIANCE BY BINDING LEVEL ####

# Set high binding level
high = c('commit','agree','endorse','pledge','promise','must renew','seek',
         'reaffirm commitment','intend to','create','will','are determined',
         'continue to','insist on','remain determined','affirm our intention',
         'ensure','complete','reaffirm the need','reaffirmed the importance',
         'remain focussed','we request','ratify','publish','implement','look to',
         'strongly advocate','more ambitious','we shall','pursue','immediate action',
         'investment','decide to','plan of action','resolve to','hold a','mobilize',
         'determination to','undertake to mobilize','mandate','preventing',
         'confirmed additional contributions','further efforts','raising',
         'take sustainable measures', 'strive to', 'championing', 'collaborate')

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
detect = c('summit', 'osaka', 'pittsburgh', 'toronto', 'london', 'washington',
           'cannes', 'cabos', 'brisbane', 'hamburg', 'buenos aires', 'riyadh')

# Identify commitments that reference a summit
df_compliance$summit.ref <- NA
df_compliance$summit.ref <-
  ifelse(str_detect(
    tolower(df_compliance$text), paste(detect, collapse = '|')), 1, 0)

# Exclude terms that do not reference a summit
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
         'global health security agenda', 'connectivity alliance', 'fatf')

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
df_compliance <- left_join(
  df_compliance, read.csv("./data/org_membership.csv"),
  by = c("country", "year")) %>%
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


#### K) ANALYZE COMPLIANCE BY HOST NATION ####

# Create column for whether or not the country was the host
df_compliance$is.host <- ifelse(
  df_compliance$country == df_compliance$host, 1, 0)

# Create new columns for countries of interest
for(i in unique(df_compliance$host)){
  df_compliance[[paste0("host.", i)]] <- ifelse(df_compliance$host == i, 1, 0)
}

# Test effect of host nation status on compliance
fit <- polr(score ~ is.host + year,
            data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))

# Test effect of host nation on compliance
fit <- polr(score ~ host.uk + host.canada + host.korea + host.france + 
              host.mexico + host.russia + host.australia + host.turkey + 
              host.china + host.germany + host.argentina + host.japan + 
              host.saudi + host.italy + year,
            data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))


#### L) ANALYZE COMPLIANCE BY GDP ####

# Read in GDP data
df_gdp.per.cap <- read.csv("./data/gdp_per_capita.csv") %>%
  # Convert data to long format
  gather(year, gdp.per.cap, X2007:X2022, factor_key=TRUE) %>%
  # Format year column
  mutate(year = as.numeric(substring(year, 2))) %>%
  # Group by country
  group_by(country) %>%
  # Create a column for lagged GDP
  mutate(gdp.per.cap.lag = lag(gdp.per.cap)) %>%
  # Ungroup data
  ungroup()

# Read in GDP growth data
df_gdp.growth <- read.csv("./data/gdp_per_capita_growth.csv") %>%
  # Convert data to long format
  gather(year, gdp.growth, X2007:X2022, factor_key=TRUE) %>%
  # Format year column
  mutate(year = as.numeric(substring(year, 2))) %>%
  # Group by country
  group_by(country) %>%
  # Create a column for lagged GDP
  mutate(gdp.growth.lag = lag(gdp.growth)) %>%
  # Ungroup data
  ungroup()

# Merge with compliance data
df_compliance <- left_join(df_compliance, df_gdp.per.cap)
df_compliance <- left_join(df_compliance, df_gdp.growth)

# Convert GDP per capita into ten thousands
df_compliance$gdp.per.cap <- df_compliance$gdp.per.cap / 10000
df_compliance$gdp.per.cap.lag <- df_compliance$gdp.per.cap / 10000

# Test effect of GDP on compliance
fit <- polr(score ~ gdp.per.cap + gdp.growth + year,
            data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))

# Test effect of lagged GDP on compliance
fit <- polr(score ~ gdp.per.cap.lag + gdp.growth.lag + year,
            data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))


#### M) ANALYZE COMPLIANCE BY MEMBER NATION ####

# Create new columns for countries of interest
for(i in unique(df_compliance$country)){
  df_compliance[[paste0("is.", i)]] <- ifelse(df_compliance$country == i, 1, 0)
}

# Test effect of member nation on compliance
fit <- polr(score ~ is.argentina + is.australia + is.brazil + is.canada + 
              is.china + is.france + is.germany + is.india + is.indonesia + 
              is.italy + is.japan + is.korea + is.mexico + is.russia + 
              is.saudi + is.safrica + is.turkey + is.uk + is.eu + year,
            data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))


#### N) ANALYZE COMPLIANCE BY COMMITMENT ISSUE-AREA ####

# Create new columns for areas of interest
df_compliance$is.climate <- ifelse(df_compliance$area == "Climate Change", 1, 0)
df_compliance$is.crime <- ifelse(df_compliance$area == "Crime and Corruption", 1, 0)
df_compliance$is.dev <- ifelse(df_compliance$area == "Development", 1, 0)
df_compliance$is.energy <- ifelse(df_compliance$area == "Energy", 1, 0)
df_compliance$is.envi <- ifelse(df_compliance$area == "Environment", 1, 0)
df_compliance$is.finreg <- ifelse(df_compliance$area == "Financial Regulation", 1, 0)
df_compliance$is.food <- ifelse(df_compliance$area == "Food and Agriculture", 1, 0)
df_compliance$is.gender <- ifelse(df_compliance$area == "Gender", 1, 0)
df_compliance$is.health <- ifelse(df_compliance$area == "Health", 1, 0)
df_compliance$is.ict <- ifelse(df_compliance$area == "ICT and Digitization", 1, 0)
df_compliance$is.ifi <- ifelse(df_compliance$area == "IFI Reform", 1, 0)
df_compliance$is.infra <- ifelse(df_compliance$area == "Infrastructure", 1, 0)
df_compliance$is.int <- ifelse(df_compliance$area == "International Cooperation", 1, 0)
df_compliance$is.tax <- ifelse(df_compliance$area == "International Taxation", 1, 0)
df_compliance$is.labor <- ifelse(df_compliance$area == "Labour and Employment", 1, 0)
df_compliance$is.macro <- ifelse(df_compliance$area == "Macroeconomic Policy", 1, 0)
df_compliance$is.migr <- ifelse(df_compliance$area == "Migration and Refugees", 1, 0)
df_compliance$is.terror <- ifelse(df_compliance$area == "Terrorism", 1, 0)
df_compliance$is.trade <- ifelse(df_compliance$area == "Trade", 1, 0)

# Test effect of commitment issue-area on compliance
fit <- polr(score ~ is.climate + is.crime + is.energy + is.envi + is.finreg
            + is.food + is.gender + is.health + is.ict + is.ifi + is.infra
            + is.int + is.tax + is.labor + is.macro + is.migr + is.terror
            + is.trade + year,
            data=df_compliance, Hess=TRUE)
ctable <- coef(summary(fit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
exp(coef(fit))


#### BUILD MULTIVARIABLE MODEL TO EXPLAIN COMPLIANCE ####

# Modify score to be binary
df_compliance$score_mod <- as.factor(ifelse(df_compliance$score == 1, 1, 0))

# Select variables for model
features <- c(
  "score_mod", "year", "min.number", "min.closest", "min.before",
  "min.after", "min.any", "min.closest.overall", "total", "commitments",
  "commitments2", "binding.level", "summit.ref", "org.ref", "mentions.date",
  "mentions.multi.year", "mentions.money", "p5", "g7", "brics", "oecd",
  "imf_economy", "is.host", "host.us", "host.uk", "host.canada", "host.korea",
  "host.france", "host.mexico", "host.russia", "host.australia",
  "host.turkey", "host.china", "host.germany", "host.argentina",
  "host.japan", "host.saudi", "host.italy", "gdp.per.cap", "gdp.per.cap.lag",
  "gdp.growth", "gdp.growth.lag", "is.argentina", "is.australia", "is.brazil",
  "is.canada", "is.china", "is.france", "is.germany", "is.india",
  "is.indonesia", "is.italy", "is.japan", "is.korea", "is.mexico",
  "is.russia", "is.saudi", "is.safrica", "is.turkey", "is.uk", "is.eu",
  "is.climate", "is.crime", "is.energy", "is.envi", "is.finreg", "is.food",
  "is.gender", "is.health", "is.ict", "is.ifi", "is.infra", "is.int",
  "is.tax", "is.macro", "is.migr", "is.terror", "is.trade", "is.labor")

# Create training and test sets
dt <- sort(sample(nrow(drop_na(df_compliance)), nrow(drop_na(df_compliance)) * 0.95))
train <- drop_na(df_compliance)[dt,] %>% dplyr::select(features)
test <- drop_na(df_compliance)[-dt,] %>% dplyr::select(features)

# # Perform principal component analysis (PCA) on scaled data
# pca <- prcomp(train %>% dplyr::select(!score_mod), scale. = TRUE)
# 
# # Project training data onto significant components
# train_pca <- as.data.frame(predict(pca, newdata = train)[, 1:49])
# train_pca$score_mod <- train$score_mod

# Build binomial model with potentially significant variables
fit.glm <- glm(score_mod ~ .,
               data = train, family = "binomial")
summary(fit.glm)
exp(coef(fit.glm))
pR2(fit.glm)

# Build random forest model with potentially significant variables
fit.rf <- randomForest(
  score_mod ~ .,
  data = df_compliance %>% dplyr::select(features), na.action = na.omit)
fit.rf


#### BUILD FORECASTING MODEL ####

# # Project test data onto significant components
# test_pca <- as.data.frame(predict(pca, newdata = test)[, 1:49])
# test_pca$score_mod <- test$score_mod

# Create predictions
pred.glm <- predict(fit.glm, test, type = "response")
pred.rf <- predict(fit.rf, test, type = "prob")[,2]

# Create confusion matrices
confusionMatrix(as.factor(ifelse(pred.glm >= 0.5, 1, 0)), test$score_mod)
confusionMatrix(as.factor(ifelse(pred.rf >= 0.5, 1, 0)), test$score_mod)


#### EXPORT DATA AND MODEL ####

# Write CSV with full features included
write.csv(df_compliance, "G20 Dataset with Full Features.csv", row.names = F)

# Export binomial model
saveRDS(fit.rf, file = "model.rds")
