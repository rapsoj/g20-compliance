#### PREPARE WORKSPACE ####

# Load in data manipulation libraries
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)

# Load in data visualization libraries
library(ggplot2)
library(gridExtra)
library(extrafont)
library(scales)
loadfonts()

# Load data
df <- read.csv("G20 Dataset with Full Features.csv")

# Set colours
red <- "#d96a5d"
blue <- "#91b7db"
gray <- "#a8a5ad"


#### CLEAN DATA ####

# Read in copy-pasted summit features data
summit <- read.table(text = "Summit and Commitment Characteristics	Value	Label
Commitment Mentions Democracy or Human Rights	2.04976	+1.05
Ministerial Meeting Held Before Summit	1.302804	+0.30
Same-Subject Official-Level Body Formed	1.224491	+0.22
Country is Host	1.188899	+0.19", sep = "\t", header = T)

# Read in copy-pasted host data
host <- read.table(text = "Host Nation	Value	Label
United Kingdom	1.293645	+0.29
France	8.25E-01	-0.18
Canada	7.70E-01	-0.23", sep = "\t", header = T)

# Read in copy-pasted member nation data
member <- read.table(text = "Member Nation	Value	Label
Italy	3.48E-01	-0.65
Japan	5.89E-01	-0.41
France	7.02E-01	-0.30
United Kingdom	1.362709	+0.36
European Union	1.430905	+0.43", sep = "\t", header = T)

# Read in copy-pasted issue-area data
issue <- read.table(text = "Issue-Area	Value	Label
Gender	5.03E-01	-0.50
Education	5.59E-01	-0.44
Trade	7.63E-01	-0.24
Terrorism	1.288971	+0.29
Environment	1.35096	+0.35
Health	1.360145	+0.36
Nonproliferation	1.406403	+0.41
Energy	1.543528	+0.54
Labour and Employment	2.047327	+1.05
ICT and Digitization	2.302536	+1.30
International Cooperation	2.383784	+1.38
Social Policy	2.664339	+1.66", sep = "\t", header = T)


#### VISUALISE EFFECT OF SUMMIT FEATURES ON COMPLIANCE ####

summit_graph <- summit %>%
  # Sort by value
  arrange(desc(Label)) %>%
  # Edit columns
  mutate(
    # Set names to categories
    Summit.and.Commitment.Characteristics = factor(
    Summit.and.Commitment.Characteristics,
    levels = Summit.and.Commitment.Characteristics),
    # Create text label
    Label.Text = ifelse(Label > 0, paste0("+", round(Label * 100, 2), "%"),
                        paste0("-", round(Label * 100, 2), "%"))) %>%
  # Plot data
  ggplot(aes(x = Summit.and.Commitment.Characteristics, y = Label)) +
  # Plot ratio of housing completions to population change as a line
  geom_bar(stat = "identity", fill = c(blue, blue, gray, gray)) +
  # Add graph title
  labs(title = paste("How do summit and commitment features affect",
                     "the probability of meeting commitments?"),
       subtitle = paste("Effect on member nation compliance with commitments",
                        "made at G7 summits (1975-2021)\n")) +
  # Remove x-axis title
  xlab("") +
  # Remove y-axis title
  ylab("") +
  # Change x-axis formatting
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
  # Change y-axis formatting
  scale_y_continuous(labels = function(x) paste0(ifelse(x >= 0, "+", "-"),
                                                 scales::percent(abs(x)))) + 
  # Add zero line
  geom_hline(yintercept = 0, color = "#59594A", linetype = 'dashed') +
  # Add labels
  geom_text(aes(label = Label.Text), vjust = -0.5,
            family = "CMU Bright SemiBold",  size = 5) +
  # Set graph theme
  theme_minimal() +
  # Customize theme elements
  theme(text = element_text(family = "CMU Bright", size = 18),
        plot.title = element_text(family = "CMU Bright SemiBold", size = 18),
        plot.subtitle = element_text(face = "plain", size = 14),
        plot.margin = unit(c(1, 1, 0.2, 0.2), "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#CCCCCC"),
        plot.background = element_rect(fill = "#E6E6E6", color = NA))

# Export graph
png("summit.png", units = "in",
    width = 12, height = 7, res = 1000)
summit_graph
dev.off()


#### VISUALISE EFFECT OF HOST NATION ON COMPLIANCE ####

host_graph <- host %>%
  # Sort by value
  arrange(desc(Label)) %>%
  # Edit columns
  mutate(
    # Set names to categories
    Host.Nation = factor(
      Host.Nation,
      levels = Host.Nation),
    # Create text label
    Label.Text = ifelse(Label > 0, paste0("+", round(Label * 100, 2), "%"),
                        paste0(round(Label * 100, 2), "%"))) %>%
  # Plot data
  ggplot(aes(x = Host.Nation, y = Label)) +
  # Plot ratio of housing completions to population change as a line
  geom_bar(stat = "identity", fill = c(blue, gray, gray)) +
  # Add graph title
  labs(title = paste("How does the choice of hosting nation affect",
                     "the probability of meeting commitments?"),
       subtitle = paste("Effect on member nation compliance with commitments",
                        "made at G7 summits (1975-2021)\n")) +
  # Remove x-axis title
  xlab("") +
  # Remove y-axis title
  ylab("") +
  # Change x-axis formatting
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
  # Change y-axis formatting
  scale_y_continuous(labels = function(x) paste0(ifelse(x >= 0, "+", "-"),
                                                 scales::percent(abs(x)))) + 
  # Add zero line
  geom_hline(yintercept = 0, color = "#59594A", linetype = 'dashed') +
  # Add labels
  geom_text(aes(label = Label.Text), vjust = ifelse(host$Value > 1, -0.5, 1.5),
            family = "CMU Bright SemiBold",  size = 5) +
  # Set graph theme
  theme_minimal() +
  # Customize theme elements
  theme(text = element_text(family = "CMU Bright", size = 18),
        plot.title = element_text(family = "CMU Bright SemiBold", size = 18),
        plot.subtitle = element_text(face = "plain", size = 14),
        plot.margin = unit(c(1, 1, 0.2, 0.2), "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#CCCCCC"),
        plot.background = element_rect(fill = "#E6E6E6", color = NA))

# Export graph
png("host.png", units = "in",
    width = 12, height = 7, res = 1000)
host_graph
dev.off()


#### VISUALISE EFFECT OF MEMBER NATION ON COMPLIANCE ####

member_graph <- member %>%
  # Sort by value
  arrange(Label) %>%
  # Edit columns
  mutate(
    # Set names to categories
    Member.Nation = factor(
      Member.Nation,
      levels = Member.Nation),
    # Create text label
    Label.Text = ifelse(Label > 0, paste0("+", round(Label * 100, 2), "%"),
                        paste0(round(Label * 100, 2), "%"))) %>%
  # Plot data
  ggplot(aes(x = Member.Nation, y = Label)) +
  # Plot ratio of housing completions to population change as a line
  geom_bar(stat = "identity", fill = c(red, red, red, blue, blue)) +
  # Add graph title
  labs(title = paste("How does nationality affect",
                     "the probability of meeting commitments?"),
       subtitle = paste("Effect on member nation compliance with commitments",
                        "made at G7 summits (1975-2021)\n")) +
  # Remove x-axis title
  xlab("") +
  # Remove y-axis title
  ylab("") +
  # Change x-axis formatting
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
  # Change y-axis formatting
  scale_y_continuous(limits = c(-0.7, 0.5),
                     labels = function(x) paste0(ifelse(x >= 0, "+", "-"),
                                                 scales::percent(abs(x)))) + 
  # Add zero line
  geom_hline(yintercept = 0, color = "#59594A", linetype = 'dashed') +
  # Add labels
  geom_text(aes(label = Label.Text), hjust = ifelse(
    (member %>% arrange(Label))$Value > 1, -0.2, 1.2),
    family = "CMU Bright SemiBold",  size = 5) +
  # Flip graph
  coord_flip() +
  # Set graph theme
  theme_minimal() +
  # Customize theme elements
  theme(text = element_text(family = "CMU Bright", size = 18),
        plot.title = element_text(family = "CMU Bright SemiBold", size = 18),
        plot.subtitle = element_text(face = "plain", size = 14),
        plot.margin = unit(c(1, 1, 0.2, 0.2), "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#CCCCCC"),
        plot.background = element_rect(fill = "#E6E6E6", color = NA))

# Export graph
png("member.png", units = "in",
    width = 12, height = 7, res = 1000)
member_graph
dev.off()


#### VISUALISE EFFECT OF ISSUE-AREA ON COMPLIANCE ####

issue_graph <- issue %>%
  # Sort by value
  arrange(Label) %>%
  # Edit columns
  mutate(
    # Set names to categories
    Issue.Area = factor(
      Issue.Area,
      levels = Issue.Area),
    # Create text label
    Label.Text = ifelse(Label > 0, paste0("+", round(Label * 100, 2), "%"),
                        paste0(round(Label * 100, 2), "%"))) %>%
  # Plot data
  ggplot(aes(x = Issue.Area, y = Label)) +
  # Plot ratio of housing completions to population change as a line
  geom_bar(stat = "identity", fill = c(red, red, gray, rep(blue, 9))) +
  # Add graph title
  labs(title = paste("How does the commitment topic affect",
                     "the probability of meeting commitments?"),
       subtitle = paste("Effect on member nation compliance with commitments",
                        "made at G7 summits (1975-2021)\n")) +
  # Remove x-axis title
  xlab("") +
  # Remove y-axis title
  ylab("") +
  # Change x-axis formatting
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
  # Change y-axis formatting
  scale_y_continuous(limits = c(-0.6, 1.75),
                     labels = function(x) paste0(ifelse(x >= 0, "+", "-"),
                                                 scales::percent(abs(x)))) + 
  # Add zero line
  geom_hline(yintercept = 0, color = "#59594A", linetype = 'dashed') +
  # Add labels
  geom_text(aes(label = Label.Text), hjust = ifelse(
    (issue %>% arrange(Label))$Value > 1, -0.2, 1.2),
    family = "CMU Bright SemiBold",  size = 5) +
  # Flip graph
  coord_flip() +
  # Set graph theme
  theme_minimal() +
  # Customize theme elements
  theme(text = element_text(family = "CMU Bright", size = 18),
        plot.title = element_text(family = "CMU Bright SemiBold", size = 18),
        plot.subtitle = element_text(face = "plain", size = 14),
        plot.margin = unit(c(1, 1, 0.2, 0.2), "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#CCCCCC"),
        plot.background = element_rect(fill = "#E6E6E6", color = NA))

# Export graph
png("issue.png", units = "in",
    width = 12, height = 7, res = 1000)
issue_graph
dev.off()


#### VISUALISE BEST AND WORST ISSUE-AREAS BY NATION ####

# Label countries
countries_dict <- c(
  "argentina" = "◯ Argentina", "australia" = "◯ Australia",
  "brazil" = "◯ Brazil", "canada" = "◯ Canada", "china" = "◯ China",
  "eu" = "◯ European Union", "france" = "◯ France",
  "germany" = "◯ Germany", "india" = "◯ India",
  "indonesia" = "◯ Indonesia", "italy" = "◯ Italy",
  "japan" = "◯ Japan", "korea" = "◯ Republic of Korea",
  "mexico" = "◯ Mexico", "russia" = "◯ Russia",
  "safrica" = "◯ South Africa", "saudi" = "◯ Saudi Arabia",
  "turkey" = "◯ Türkiye", "uk" = "◯ United Kingdom",
  "us" = "◯ United States")

# Define colours for each issue-area
area_colors <- c("Climate Change" = "#cd6001", "Crime and Corruption" = "#971c1d",
                 "Development" = "#2077b5", "Energy" = "#ffd369",
                 "Environment" = "#2ba02a", "Financial Regulation" = "#941e71",
                 "Food and Agriculture" = "#8f6bbb", "Gender" = "#e377c2",
                 "Health" = "#e35734", "ICT and Digitization" = "#673f8e",
                 "IFI Reform" = "#17bfcd", "Infrastructure" = "#7f7f7f",
                 "International Cooperation" = "#89dd87",
                 "International Taxation" = "#bdbd22",
                 "Labour and Employment" = "#56322e",
                 "Macroeconomic Policy" = "#1b641c",
                 "Migration and Refugees" = "#a3b2ed", "Terrorism" = "#262626",
                 "Trade" = "#ff800b")

# Get the five highest scores for each topic and country
df_top <- df %>%
  group_by(country, area) %>%
  summarise(
    score_avg = mean((score + 1) / 2, na.rm = T),
    score_std = sd((score + 1) / 2, na.rm = T),
    score_count = sum(!is.na(score))) %>%
  filter(score_count >= 5) %>%
  top_n(5, score_avg) %>%
  arrange(score_avg) %>%
  mutate(
    rank = row_number(desc(score_avg)),
    country = countries_dict[match(country, names(countries_dict))])

# Get the five lowest scores for each topic and country
df_bottom <- df %>%
  group_by(country, area) %>%
  summarise(score_avg = mean(score, na.rm = T)) %>%
  top_n(-5, score_avg) %>%
  arrange(score_avg) %>%
  mutate(rank = min_rank(score_avg))

# Create a bar plot
top_issues <- ggplot(df_top, aes(x = reorder(rank, score_avg), y = score_avg, fill = factor(area))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = scales::percent(round(score_avg, 2))),
            hjust = -0.1, position = position_dodge(width = 1),
            family = "CMU Bright", size = 3) +
  # Add graph title
  labs(title = paste("Which types of commitments are G20 members most likely to meet?"),
       subtitle = paste("Percent of G20 summit commitments met",
                        "by member country (2008-2021)\n"),
       caption = "Source: G20 Research Group, 2023") +
  labs(x = "", y = "Average Compliance", fill = "Topic") +
  # Remove x-axis title
  xlab("") +
  # Remove y-axis title
  ylab("Probability of Meeting Commitments") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L),
                     limits = c(0,1.2)) +
  scale_fill_manual(values = area_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ country, ncol = 5) +
  coord_flip() +
  # Set graph theme
  theme_minimal() +
  # Customize theme elements
  theme(text = element_text(family = "CMU Bright", size = 18),
        plot.title = element_text(family = "CMU Bright SemiBold", size = 18),
        plot.subtitle = element_text(face = "plain", size = 14),
        plot.caption = element_text(size = 8),
        plot.margin = unit(c(1, 1, 0.2, 0.2), "cm"),
        legend.position = "none",
        strip.text = element_text(hjust = 0),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        panel.spacing = unit(1, "lines"),
        plot.background = element_rect(fill = "#E6E6E6", color = NA))

# Export graph
png("top_issues.png", units = "in",
    width = 12, height = 7, res = 1000)
top_issues
dev.off()
