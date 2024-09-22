# load the necessary libraries
library(ggplot2)
library(data.table)

## Data Preprocessing
PATH = 'C:/Users/milio/Desktop/edemm/programming_tools/R/ergasia/'
FILE_NAME = "Pisa mean perfomance scores 2015 Data.csv"
FILE_PATH = file.path(PATH,FILE_NAME)
df = fread(FILE_PATH, na.strings="..", header=TRUE)

# drop rows with NA values
df <- na.omit(df)

# delete unnecessary columns
col_to_delete = c('Country Name','Series Name')
df[, (col_to_delete) := NULL]

# rename columns
rename_col <- function(df,oldname,newname) {
  colnames(df)[colnames(df) == oldname] <- newname
  return (df)
}
df = rename_col(df,'2015','Score')
df = rename_col(df,'Country Code','Country')
df = rename_col(df,'Series Code','Code')

# create new columns
df[, 'Code' := substr(Code, 9, nchar(Code))]
df[, 'Subject' := substr(Code, 1, 1)]
df[, 'Gender' := substr(Code, 5, 5)]
df[, 'Code' := NULL]

# rearrange columns order, to get 'Score' at last place
df <- df[, c(setdiff(names(df), 'Score'), 'Score'), with = FALSE]

# convert columns to factors
df[, Gender := factor(ifelse(Gender == "", "B", Gender))] # B for both genders
df[, Subject := factor(Subject)]
df[, Country := factor(Country)]


# Data Analysis
both    <- subset(df, Gender == "B")
male    <- subset(df, Gender == "M")
female  <- subset(df, Gender == "F")
mf      <- subset(df, Gender == "M" | Gender == "F")

# Box plot for Gender Comparisons
boxplot = ggplot(mf)+
  aes(x=Gender, y=Score)+
  geom_boxplot(alpha=0.75) +
  aes(fill=Gender) +
  scale_fill_manual(values = c("pink", "lightblue")) +
  scale_x_discrete(labels = c("F"="Female", "M"="Male")) +  
  theme_linedraw() +
  labs(x="Gender",y="Score") +
  facet_wrap(.~Subject, scales = "free_x", nrow=1, ncol=3 ,
             labeller = labeller(Subject = c("M"="Math", "R"="Reading", "S"="Science")))

# Function to calculate boxplot stats for each subject and gender and return as a data frame
calculate_stats <- function(data, gender, subject) {
  scores <- data$Score[data$Gender == gender & data$Subject == subject]
  stats <- fivenum(scores)
  mean_score <- mean(scores, na.rm = TRUE)
  stats_df <- data.table(
    Subject = subject,
    Gender = gender,
    Min = round(stats[1], 2),
    Q1 = round(stats[2], 2),
    Median = round(stats[3], 2),
    Mean = round(mean_score, 2),
    Q3 = round(stats[4], 2),
    Max = round(stats[5], 2)
  )
  return(stats_df)
}

all_stats <- data.table()
for (subject in c("M", "R", "S")){
  for (gender in c("F", "M")) {
    all_stats <- rbind(all_stats, calculate_stats(df, gender, subject))
  }
}
# Convert subject codes to full names
all_stats$Subject <- factor(all_stats$Subject, levels = c("M", "R", "S"),
                            labels = c("Math", "Reading", "Science"))
print(all_stats)

# Barplots for Country comparisons
math    <- subset(df, Subject == "M" & Gender == "B")
reading <- subset(df, Subject == "R" & Gender == "B")
science <- subset(df, Subject == "S" & Gender == "B")

create_barplot = function(df, subj) {
  ggplot(df) +
  aes(x = reorder(Country, Score), y = Score, fill = Score) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_fill_gradient(name = "Score Level") +
  geom_hline(yintercept = mean(df$Score), size = 1, col = "red") +
  theme_grey() +
  labs(x = "Country Name", y = paste(subj, " Score"))
}

create_barplot(math, 'Math')
create_barplot(reading, 'Reading')
create_barplot(science, 'Science')

all_stats <- data.table()
all_stats <- rbind(all_stats, calculate_stats(math, 'B', 'M'))
all_stats <- rbind(all_stats, calculate_stats(reading, 'B', 'R'))
all_stats <- rbind(all_stats, calculate_stats(science, 'B', 'S'))
print(all_stats)

# Aggregated bar plot
aggregate_scores <- aggregate(Score ~ Country, data = df, FUN = sum)
aggregate_scores$Gender <- 'B'
aggregate_scores$Subject <- 'All'
create_barplot(aggregate_scores, '')
all_stats <- calculate_stats(aggregate_scores,'B','All')
print(all_stats)

# Histograms
create_histogram = function(df, subj, binwidth) {
  breaks <- seq(floor(min(df$Score)), ceiling(max(df$Score)), binwidth)
  p <- ggplot(df, aes(x = Score)) +
    geom_histogram(binwidth = binwidth, breaks = breaks, fill = "tomato1", color = "black") +
    scale_x_continuous(breaks = breaks) +  # Set breaks on x-axis
    scale_y_continuous(breaks = seq(0, 12, 1)) +  # Set breaks on y-axis
    labs(x = paste(subj," Score"), y = "Frequency") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  # Rotate x-axis labels
  return(p)
}

h1 <- create_histogram(math, 'Math', 10)
h2 <- create_histogram(reading, 'Reading', 10)
h3<- create_histogram(science, 'Science', 10)
h4<- create_histogram(aggregate_scores, 'Total', 100)

library(gridExtra)
grid.arrange(h1, h2, h3, h4, ncol = 2)


# Create the boxplot
ggplot(both, aes(x = Subject, y = Score, fill = Subject)) +
  geom_boxplot() +  # Boxplot layer
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +  # Jitter layer for points
  scale_fill_brewer(palette="Pastel1") +  # Optional: adds color
  labs(x = "Subject", y = "Score") +
  theme_minimal() 

# Calculate the lower and upper bounds for each Subject
outlier_boundaries <- both[, .(Lower = quantile(Score, 0.25) - 1.5 * IQR(Score), Upper = quantile(Score, 0.75) + 1.5 * IQR(Score)), by = Subject]
# Identify outliers for each Subject
outliers <- both[outlier_boundaries, on = "Subject"][Score < Lower | Score > Upper]
print(outliers)





