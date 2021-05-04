#' Title: Fan Analysis: Sponsor appeal examined 
#' Purpose: Data Cleaning
#' Author: Max Lembke

# Warning

# Some pre-processing steps can be limited by system memory and the single threaded 
# nature of R computations. Memory limits can be adjusted using the following code. 
# Be weary the current memory limit set might have adverse effects. 

memory.size() #Check memory 
memory.limit(size = 500000)#Adjust size limit 

# Set-up

pacman::p_load(tidyverse,lubridate,tm,fst,plyr,dplyr,qdap) 

setwd("~/University/Master (MsBA)/Spring/Text Analysis/R - Files/hult_NLP_student/Private/Case II")

options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

getwd()# Check if set properly

# Data Reading and Prep 

#Grabbing all file names for followers of teams 
data_files_team <- list.files(path = ".\\data\\teamFollowerTimelines\\", pattern = "\\.fst$", full.name = TRUE) 

#Grabbing all file names for followers of player
data_files_players <- list.files(path = ".\\data\\playerFollowerTimelines\\", pattern = "\\.fst$", full.name = TRUE) 

#Importing the files for team followers of teams 
data_team_followers <- ldply(data_files_team , read_fst)

#Importing the files for team followers of players 
data_player_followers <- ldply(data_files_players, read_fst)

#Joining data together to one dataset 
all_data <- full_join(data_team_followers,data_player_followers)

#Removing duplicates based on identicaly tweet texts 
all_data <- all_data[!duplicated(all_data$text), ]

#Renaming the date field 
all_data = all_data %>%
  dplyr::rename(date = created_at, postID = status_id ) #dplyr name:: due to dplyr and plyr issues 

#Parsing dates
all_data$date <- as.Date(all_data$date, format = "ymd", origin = "ymd HMS")

# Filtering out prev. tweets that are irrelevant (data minimization)
#Start date - announcement of the new league (several months before league start)
#End - one wee after finals to capture some post trend 

all_data <- all_data %>% 
  filter(all_data$date > as.Date("2019-10-28") & all_data$date <= as.Date("2020-09-06") )

#Make "NA" in hashtag column " " so that no "NA" are pasted into text in the next field 
all_data$hashtags[is.na(all_data$hashtags)] <- " "

#Adding hashtags onto the text field and then removing the hashtag field 
all_data$text <- paste(all_data$text, all_data$hashtags,sep = " ")

#Removing hashtag column 
all_data = subset(all_data, select = -c(hashtags))

#Order 
col_order <- c("postID", "text", "user_id","screen_name",
               "date", "team","player")

all_data <- all_data[, col_order]

#Removing old elements from global environment 
x<- which(ls()=="all_data")
ls1<- ls()[-x]
rm(list = ls1)

#Choosing sample for further processing and coding, final analysis will be conducted without sample 
set.seed(1) #Always used seed (1) 
all_data <- sample_frac(all_data, 0.1)

# Functions and cleaning 

# To lower function 

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

# Clean corpus function 

cleanCorpus <- function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(replace_contraction)) 
  corpus <- tm_map(corpus, content_transformer(gsub),pattern = "[^\x01-\x7F]", replacement = "")#remove non non-ASCII characters
  corpus <- tm_map(corpus, content_transformer(gsub),pattern = "\bcall+ of+ duty\b", replacement = "cod")#change "call of duty" to cod
  corpus <- tm_map(corpus, content_transformer(gsub),pattern = "codleague", replacement = "cod league")#codleague to cod & league 
  corpus <- tm_map(corpus, content_transformer(gsub),pattern = "\b(\\w*fuck\\w*)\b", replacement = "fuck")#change different fuck variants - not working well due to escape character issues in R 
  corpus <- tm_map(corpus, content_transformer(gsub),pattern = "fucking", replacement = "fuck")#change different fuck variants
  corpus <- tm_map(corpus, content_transformer(gsub),pattern = "\b[s$]h[ai!l|]t\b", replacement = "shit")#change different shit variants
  corpus <- tm_map(corpus, content_transformer(gsub),pattern = "cod:bo|cod|bo|callofduty|callduty|warzone|blackopscoldwar|blackops|coldwar|modernwarfare|warefare|codmw|codbo|callofdutymodernwarfare", replacement = "cod")#change different cod
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Stopword for clean corpus 

stops <- c(stopwords('english'), 'amp','just','get','well','can','one','now','now','new','got','know','see','still','time','dont','back','day','acodut','going','make','first','think','come','really','please','share','christmas','via','will','the')

# Clean and Organize
txt <- VCorpus(VectorSource(all_data$text))

x<- which(ls()=="txt"|ls()=="stops"|ls()=="cleanCorpus"|ls()=="tryTolower"|ls()=="all_data")
ls1<- ls()[-x]
rm(list = ls1)

txt <- cleanCorpus(txt,stops)

# Extract the clean and subbed text to use polarity 
cleanPosts <- data.frame(document = seq_along(all_data$postID), #simple id order
                         postID = all_data$postID, # keep track of posts
                         user_id = all_data$user_id,
                         screen_name = all_data$screen_name,
                         date = all_data$date, 
                         team = all_data$team,
                         player = all_data$player,
                         text = unlist(sapply(txt, `[`, "content")),stringsAsFactors=F)

x<- which(ls()=="cleanPosts"|ls()=="all_data")
ls1<- ls()[-x]
rm(list = ls1)

#Writing File 

write.csv(cleanPosts ,"data_clean_10per.csv", row.names = TRUE)
write.fst(cleanPosts ,"data_clean_10per.fst")

#Inital Exploration of Data 
# Checking for most frequent words etc. to append stops etc. 

all_data <- read_fst("data_clean_10per.fst")

#Order 
col_order <- c("postID", "text", "user_id","screen_name",
               "date", "team","player")

cleanPosts <- all_data[, col_order]

cleanPosts <- subset(cleanPosts, select = c("postID", "text"))

str(cleanPosts)

# Creating corpus 
txtCorpus <- VCorpus(VectorSource(cleanPosts$text))

rm(cleanPosts)

# Creating TDM 
txtTdm <- TermDocumentMatrix(txtCorpus)

#Removing spare terms to reduce the size of TDM 
txtTdm <-removeSparseTerms(txtTdm, 0.999)

#Convert to matrix
txtTdmM <- as.matrix(txtTdm)

#Finding top terms 
topTerms <- rowSums(txtTdmM)

#Creating df 
topTerms <- data.frame(terms = rownames(txtTdmM), freq = topTerms)