#' Title: Fan Analysis: Sponsor appeal examined 
#' Purpose: Analysis 
#' Author: Max Lembke

# Warning

# Some pre-processing steps can be limited by system memory and the single threaded 
# nature of R computations. Memory limits can be adjusted using the following code. 
# Be weary the current memory limit set might have adverse effects. 

memory.size() #Check memory 
memory.limit(size = 500000)#Adjust size limit 

# Set-up

pacman::p_load(tidyverse,lubridate,tm,fst,dplyr,qdap,ggplot2,ggthemes,RColorBrewer,pbapply,quanteda,reshape2,wordcloud,wordcloud2,lexicon,radarchart,tidytext)

setwd("~/University/Master (MsBA)/Spring/Text Analysis/R - Files/hult_NLP_student/Private/Case II")

options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

getwd()# Check if set properly

# Important Information 

# In order to accommodate for RAM issues, the rm() command to clear the global 
# environment is used frequently -> this has no other reason than to account for 
# issues  by clearing the global environment of not needed elements. Be very 
# careful when running parts. 

# Analysis

##Key Terms Overview 

###WordCloud

# Reading pre-prepared file (this can also be conducted through a sample to run code more swiftly - same can be adjusted in the cleaning rmd file)

cleanPosts <- read_csv(".//data//data_clean.csv")

#Order 
col_order <- c("postID", "text", "user_id","screen_name",
               "date", "team","player")

cleanPosts <- cleanPosts[, col_order]

# Creating corpus 
txtCorpus <- VCorpus(VectorSource(cleanPosts$text))

# Removing from memory to have more ram space 
rm(cleanPosts,col_order)

# Bigram token maker
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}

# Make bi-gram TDM according to the tokenize control & convert it to matrix

wordcloudTDM  <- TermDocumentMatrix(txtCorpus, 
                                    control=list(tokenize=bigramTokens))

# Removing spare terms to reduce the size of TDM 
wordcloudTDM <-removeSparseTerms(wordcloudTDM, 0.9999)

# Text corpus not needed anymore, hence removed 
rm(txtCorpus)

# Creating matrix 
wordcloudTDMm <- as.matrix(wordcloudTDM)

# Get Row Sums & organize

wordcloudTDMv <- sort(rowSums(wordcloudTDMm), decreasing = TRUE)
wordcloudDF   <- data.frame(word = names(wordcloudTDMv), freq = wordcloudTDMv)

# Choose a color & drop light ones
pal <- brewer.pal(8, "Purples")
pal <- pal[-(1:2)]

# Creating simple word cloud
set.seed(1)
wordcloud(wordcloudDF $word,
          wordcloudDF $freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))

#Save done manually as not working with wordcloud 

# Clear Environment
rm(list = ls())

###Top Terms

# Reading pre-prepared file (this can also be conducted through a sample to run code more swiftly - same can be adjusted in the cleaning rmd file)

cleanPosts <- read_csv(".//data//data_clean.csv")

# Order & Subset 
col_order <- c("postID", "text", "user_id","screen_name",
               "date", "team","player")

cleanPosts <- cleanPosts[, col_order]

cleanPosts <- subset(cleanPosts, select = c("postID", "text"))

# Creating corpus 
txtCorpus <- VCorpus(VectorSource(cleanPosts$text))

rm(cleanPosts)

# Creating TDM 
txtTdm <- TermDocumentMatrix(txtCorpus)

# Removing spare terms to reduce the size of TDM 
txtTdm <-removeSparseTerms(txtTdm, 0.999)

# Convert to matrix
txtTdmM <- as.matrix(txtTdm)

# Finding top terms 
topTerms <- rowSums(txtTdmM)

# Creating df 
topTerms <- data.frame(terms = rownames(txtTdmM), freq = topTerms)

# Plotting 
toptermsplot <-  topTerms %>%
  arrange(desc(freq)) %>%
  slice(1:25) %>% #Top 25 terms 
  ggplot(., aes(x=reorder(terms, -freq), y=freq, fill = freq))+
  geom_bar(stat = "identity")+ 
  scale_fill_continuous(low="lightblue", high="purple")+
  coord_flip()+
  theme(legend.position  = "none", 
        panel.grid.minor = element_blank(),
        plot.background  = element_rect(fill="transparent", colour=NA),
        axis.line  = element_line(colour = "black"),
        axis.text  = element_text(face="bold", color="#000000", size=10),
        axis.title = element_text(face="italic", color="#000000", size=12),
        plot.title = element_text(face="bold", color="#000000", size=16)) + 
  labs(title = "High Level: Term Frequency",
       x = "Term",
       y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

# Save plot 
ggsave("toptermsplot.png", toptermsplot, bg = "transparent")

# Clear Environment
rm(list = ls())

##Fan engagement with CSL & Sponsors

###Setup

# Data
cleanPosts <- read_csv('.//data//data_clean.csv')

# Order & Subest
col_order <- c("postID", "text", "user_id","screen_name",
               "date", "team","player")

cleanPosts <- cleanPosts[, col_order]

cleanPosts <- subset(cleanPosts, select = c("text","team","date"))

# Term definition 
terms_cdl <- c('cdl|league')
terms_sponsors <- c('astro|scuf|scufgaming|intel|gamefuel|playstation|airforce|intelcod|gfuelenergy|psshare|astro')
cod <- c('cod')

###Enagement with COD

# Calculating engagement with cod
engagement_cod_df <- cleanPosts %>% 
  group_by(team)%>%
  summarise(engagement_cod = (((sum(grepl(cod,text,ignore.case=TRUE))))/n())*100)

# Rounding results 
engagement_cod_df$engagement_cod <- round(engagement_cod_df$engagement_cod,digits = 2)

#Plotting 

engagement_cod_visual <- engagement_cod_df %>% 
  ggplot(.,aes(x = reorder(team, -engagement_cod), y = engagement_cod))+
  geom_bar(stat="identity", position = "dodge", fill = "steelblue")+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  theme(legend.position  = "none", 
        panel.grid.minor = element_blank(),
        plot.background  = element_rect(fill="transparent", colour=NA),
        axis.line  = element_line(colour = "black"),
        axis.text  = element_text(face="bold", color="#000000", size=8),
        axis.title = element_text(face="italic", color="#000000", size=12),
        plot.title = element_text(face="bold", color="#000000", size=16)) + 
  labs(title = "Fan Engagement by team with COD ",
       x = "Team",
       y = "Percentage of Tweets") +
  theme(plot.title = element_text(hjust = 0.5))

# Save plot 
ggsave("engagement_cod_visual.png", engagement_cod_visual, bg = "transparent")

###Enagement with CDL over time

# Calculating team engagement over time & storing
team_engagement_time_df <- cleanPosts %>% 
  group_by(team,date)%>%
  summarise(engagement = (((sum(grepl(terms_cdl,text,ignore.case=TRUE))))/n())*100)

# Rounding results 
team_engagement_time_df$engagement <- round(team_engagement_time_df$engagement,digits = 2)

# Renaming
team_engagement_time_df <- team_engagement_time_df %>% 
  rename(Team = team) 

# Plotting

team_engagement_time <- team_engagement_time_df %>%
  ggplot(.,aes(x=date, y= engagement))+ 
  geom_smooth(aes(color = Team, linetype = Team),se=FALSE)+ 
  theme(legend.position  = ,
        legend.title = element_text(color = "#000000", size = 9),
        legend.text = element_text(color = "#000000", size = 7),
        panel.grid.minor = element_blank(),
        plot.background  = element_rect(fill="transparent", colour=NA),
        axis.line  = element_line(colour = "black"),
        axis.text  = element_text(face="bold", color="#000000", size=8),
        axis.title = element_text(face="italic", color="#000000", size=12),
        plot.title = element_text(face="bold", color="#000000", size=16)) + 
  labs(title = "Team Engagement over the Season",
       x = "Date",
       y = "Percentage of Tweets") +
  theme(plot.title = element_text(hjust = 0.5))

# Save plot 
ggsave("team_engagement_time.png", team_engagement_time, bg = "transparent")

###Enagement with CDL & Sponsors

# Calculating team engagement with sponsors
sponsor_engagement_df <- cleanPosts %>% 
  group_by(team)%>%
  summarise(engagement_sponsors = (((sum(grepl(terms_sponsors,text,ignore.case=TRUE))))/n())*100)

# Rounding results 
sponsor_engagement_df$engagement_sponsors <- round(sponsor_engagement_df$engagement_sponsors,digits = 2)

# Calculating engagement with cdl 
team_engagement_df <- cleanPosts %>% 
  group_by(team)%>%
  summarise(engagement_cdl = (((sum(grepl(terms_cdl,text,ignore.case=TRUE))))/n())*100)

# Rounding results 
team_engagement_df$engagement_cdl <- round(team_engagement_df$engagement_cdl,digits = 2)

# Joining both df together & melting 
engagement_df <- full_join(team_engagement_df, sponsor_engagement_df)
engagement_df<- melt(engagement_df, id="team")

#Plotting 

engagement_cdl_sponsors <- engagement_df %>% 
  ggplot(.,aes(x = reorder(team, value), y = value, fill=variable))+
  geom_bar(stat="identity", position = "dodge")+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_fill_manual(values = c("steelblue", "lightblue"),name = "Tweet", labels = c("CDL", "Sponsors"))+
  theme(legend.position  = ,
        legend.title = element_text(color = "#000000", size = 9),
        legend.text = element_text(color = "#000000", size = 7),
        panel.grid.minor = element_blank(),
        plot.background  = element_rect(fill="transparent", colour=NA),
        axis.line  = element_line(colour = "black"),
        axis.text  = element_text(face="bold", color="#000000", size=8),
        axis.title = element_text(face="italic", color="#000000", size=12),
        plot.title = element_text(face="bold", color="#000000", size=16)) + 
  labs(title = "Fan Engagement: Sponsors vs CDL",
       x = "Team",
       y = "Percentage of Tweets") +
  theme(plot.title = element_text(hjust = 0.5))

# Save plot 
ggsave("engagement_cdl_sponsors.png", engagement_cdl_sponsors, bg = "transparent")

# Clear Environment
rm(list = ls())

##Polarity per team 

###Concerning league by team

# Data
cleanPosts <- read_csv('.//data//data_clean.csv')

# Order 
col_order <- c("postID", "text", "user_id","screen_name",
               "date", "team","player")

cleanPosts <- cleanPosts[, col_order]

cleanPosts <- subset(cleanPosts, select = c("postID", "text","team"))

# Filtering based on contains cod/cdl content

terms <- c('cdl|league')

cleanPosts <- cleanPosts %>%
  filter(.,(grepl(terms,text,ignore.case=TRUE)))%>% 
  group_by(team)

# Get a sentiment polarity 
Polarity <- polarity(cleanPosts$text)

# Join the polarity back
cleanPosts <- cbind(cleanPosts, polarity = Polarity$all$polarity)

# Any of the above data can be a dataframe for visualizing
leaguePolarityByTeam <- aggregate(polarity ~ + team, cleanPosts, mean)

# Plot 
leaguepolarityvisual <- leaguePolarityByTeam %>%
  ggplot(., aes(x=reorder(team, -polarity), y=polarity, fill = polarity))+
  geom_bar(stat = "identity")+ 
  scale_fill_gradient2()+
  coord_flip()+
  theme(legend.position  = "none", 
        panel.grid.minor = element_blank(),
        plot.background  = element_rect(fill="transparent", colour=NA),
        axis.line  = element_line(colour = "black"),
        axis.text  = element_text(face="bold", color="#000000", size=10),
        axis.title = element_text(face="italic", color="#000000", size=12),
        plot.title = element_text(face="bold", color="#000000", size=16)) + 
  labs(title = "Polarity of Fan tweets concerning CDL by Team",
       x = "Team",
       y = "Polarity") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Save plot 
ggsave("leaguepolarityvisual.png", leaguepolarityvisual, bg = "transparent")

# Clear Environment
rm(list = ls())

###Concerning sponsors by team

# Data
cleanPosts <- read_csv('.//data//data_clean.csv')

# Order 
col_order <- c("postID", "text", "user_id","screen_name",
               "date", "team","player")

cleanPosts <- cleanPosts[, col_order]

cleanPosts <- subset(cleanPosts, select = c("postID", "text","team"))

# Filtering based on contains cod/cdl content

terms <- c('astro|scuf|scufgaming|intel|gamefuel|playstation|airforce|intelcod|gfuelenergy|psshare|astro')

cleanPosts <- cleanPosts %>%
  filter(.,(grepl(terms,text,ignore.case=TRUE)))%>% 
  group_by(team)

# Get a sentiment polarity 
Polarity <- polarity(cleanPosts$text)

# Join the polarity back
cleanPosts <- cbind(cleanPosts, polarity = Polarity$all$polarity)

# Any of the above data can be a dataframe for visualizing
leaguePolarityByTeam <- aggregate(polarity ~ + team, cleanPosts, mean)

# Plot 
sponsorpolarityvisual <- leaguePolarityByTeam %>%
  ggplot(., aes(x=reorder(team, -polarity), y=polarity, fill = polarity))+
  geom_bar(stat = "identity")+ 
  scale_fill_gradient2()+
  coord_flip()+
  theme(legend.position  = "none", 
        panel.grid.minor = element_blank(),
        plot.background  = element_rect(fill="transparent", colour=NA),
        axis.line  = element_line(colour = "black"),
        axis.text  = element_text(face="bold", color="#000000", size=10),
        axis.title = element_text(face="italic", color="#000000", size=12),
        plot.title = element_text(face="bold", color="#000000", size=16)) + 
  labs(title = "Polarity of Fan tweets concerning Sponsors by Team",
       x = "Team",
       y = "Polarity") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Save plot 
ggsave("sponsorpolarityvisual.png", sponsorpolarityvisual, bg = "transparent")

# Clear Environment
rm(list = ls())

###Concerning activision by team

# Data
cleanPosts <- read_csv('.//data//data_clean.csv')

# Order 
col_order <- c("postID", "text", "user_id","screen_name",
               "date", "team","player")

cleanPosts <- cleanPosts[, col_order]

cleanPosts <- subset(cleanPosts, select = c("postID", "text","team"))

# Filtering based on contains cod/cdl content

terms <- c('activision')

cleanPosts <- cleanPosts %>%
  filter(.,(grepl(terms,text,ignore.case=TRUE)))%>% 
  group_by(team)

# Get a sentiment polarity 
Polarity <- polarity(cleanPosts$text)

# Join the polarity back
cleanPosts <- cbind(cleanPosts, polarity = Polarity$all$polarity)

# Any of the above data can be a dataframe for visualizing
leaguePolarityByTeam <- aggregate(polarity ~ + team, cleanPosts, mean)

# Plot 
activisionpolarityvisual <- leaguePolarityByTeam %>%
  ggplot(., aes(x=reorder(team, -polarity), y=polarity, fill = polarity))+
  geom_bar(stat = "identity")+ 
  scale_fill_gradient2()+
  coord_flip()+
  theme(legend.position  = "none", 
        panel.grid.minor = element_blank(),
        plot.background  = element_rect(fill="transparent", colour=NA),
        axis.line  = element_line(colour = "black"),
        axis.text  = element_text(face="bold", color="#000000", size=10),
        axis.title = element_text(face="italic", color="#000000", size=12),
        plot.title = element_text(face="bold", color="#000000", size=16)) + 
  labs(title = "Polarity of Fan tweets concerning activision by Team",
       x = "Team",
       y = "Polarity") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Save plot 
ggsave("activisionpolarityvisual.png", activisionpolarityvisual, bg = "transparent")

# Clear Environment
rm(list = ls())

###Concerning non-cod or csl tweets by team

# Data
cleanPosts <- read_csv('.//data//data_clean.csv')

# Order 
col_order <- c("postID", "text", "user_id","screen_name",
               "date", "team","player")

cleanPosts <- cleanPosts[, col_order]

cleanPosts <- subset(cleanPosts, select = c("postID", "text","team"))

# Filtering based on contains cod/cdl content

terms <- c('activision|astro|scuf|scufgaming|intel|gamefuel|playstation|airforce|intelcod|gfuelenergy|psshare|astro|cdl|league')

cleanPosts <- cleanPosts %>%
  filter(.,(!grepl(terms,text,ignore.case=TRUE)))%>% 
  group_by(team)

# Get a sentiment polarity 
Polarity <- polarity(cleanPosts$text)

# Join the polarity back
cleanPosts <- cbind(cleanPosts, polarity = Polarity$all$polarity)

# Any of the above data can be a dataframe for visualizing
leaguePolarityByTeam <- aggregate(polarity ~ + team, cleanPosts, mean)

# Plot 
nonpolarityvisual <- leaguePolarityByTeam %>%
  ggplot(., aes(x=reorder(team, -polarity), y=polarity, fill = polarity))+
  geom_bar(stat = "identity")+ 
  scale_fill_gradient2()+
  coord_flip()+
  theme(legend.position  = "none", 
        panel.grid.minor = element_blank(),
        plot.background  = element_rect(fill="transparent", colour=NA),
        axis.line  = element_line(colour = "black"),
        axis.text  = element_text(face="bold", color="#000000", size=10),
        axis.title = element_text(face="italic", color="#000000", size=12),
        plot.title = element_text(face="bold", color="#000000", size=16)) + 
  labs(title = "Polarity of Fan tweets by Team (non COD/CDL related)",
       x = "Team",
       y = "Polarity") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Save plot 
ggsave("nonpolarityvisual.png", nonpolarityvisual, bg = "transparent")

# Clear Environment
rm(list = ls())