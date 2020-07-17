### Sachsenwahl 2019 ###
########################
#### Topic Modeling ####
########################

setwd("E:\\Nextcloud\\R\\Masterarbeit")
load("SW19_tweets_comm.RDa") # includes community information: top 15 (+"none") classified communities 

library(tidyverse)
library(stm)
library(stminsights)
library(quanteda)
library(lubridate)
library(data.table)
library(xlsx)
library(RColorBrewer)
library(Cairo)
# library(koRpus)
# library(textstem)


#### Make STM Object  --------------

# Only use retweets not OC in the STM. German tweets only

oc <- SW19_tweets_comm[SW19_tweets_comm$is_retweet == F]
rt <- SW19_tweets_comm[SW19_tweets_comm$is_retweet == T] %>% unique(by="retweet_status_id")
ocrt <- rt[!(rt$retweet_status_id %in% oc$status_id)]

stm_tweets <- bind_rows(oc, rt, ocrt) # (rt text is preserved in text)
stm_tweets <- stm_tweets[stm_tweets$lang == "de"]
                                                    # check for duplicates next time. just in case...
                                                    # Alternative approach: check for duplicated text rather than retweets
# change date format to number of days in sample
stm_tweets$day <- as.Date(stm_tweets$created_at)
min_date <- min(stm_tweets$day) 
stm_tweets$day <- difftime(stm_tweets$day, min_date, units = "days") %>% as.integer()+1


# make corpus (w/ quanteda)

corpus <- corpus(stm_tweets, text_field = "text",
                 docid_field = "status_id")
docvars(corpus)$text <- stm_tweets$text # unprocessed text for findThoughts()

# Tokenization

tokens <- quanteda::tokens(corpus, 
                 what = "word",         
                 remove_symbols = TRUE, 
                 include_docvars = TRUE, 
                 remove_url = TRUE, 
                 remove_twitter = TRUE,
                 remove_punct = TRUE,
                 remove_numbers = FALSE)

# Stopword removal

twitter_stopwords <- c("t.co", "https","&amp;", "rt")

tokens <- tokens %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords('german'), padding = F) %>% 
  tokens_remove(twitter_stopwords, padding = F) %>% 
  tokens_remove("innen", padding = F)     
    # instead of compouding gendered words (e.g. "wähler*innen") that got seperated in the process,
    #   the suffix "innen" gets removed. This removes the gendered form, effectively merging these words 


# Lemmatize ? -> no. See Schofield et al (Stopwords removed for computational efficiency)
# #install.koRpus.lang("de")
# library(koRpus.lang.de)   # must install and load german language support as koRpus package
# dictionary_de <- make_lemma_dictionary(tokens, engine = "treetagger", lang="de")


# # Collocations    -> seems nonsensical here
# colls <- textstat_collocations(tokens, min_count = 110)

# Remove user names
#   -> keep to see importance of certain users for topics (if any), e.g. mentions of @spdde



# Document Feature Matrix
dfm <- dfm(tokens, remove_numbers = T)
dim(dfm)            

token_freq <- textstat_frequency(dfm)

dfm <- dfm%>% 
  dfm_keep(min_nchar = 2) %>% # remove chars with only one character
  dfm_trim(min_docfreq = 0.001, max_docfreq = 0.3, #0.1% min, 30% max
           docfreq_type = 'prop') # proportions instead of counts
  # => effectively removes the tokens "ltwsn19" and "Sachsen",
  #       as well as tokens that appear in less than 33 (1% of) documents
dim(dfm)

token_freq2 <- textstat_frequency(dfm)

  # additionally drop other hashtags used for the search from the sample
search_tags <- c("sachsenwahl", "ltwsachsen", "ltwsn", "ltwsn19", "sltw19")

dfm <- dfm %>% 
  dfm_select(pattern = search_tags, selection = "remove")

token_freq2 <- textstat_frequency(dfm)



# Reduce amount of hashtags used
#     since STM does not provide functionality to use lists as metadata, all hashtags of a document must be
#       collapsed into one character string, effectively giving each document a unique combination of hashtags
#     Since this is not an effective contribution to the computation of the STM, we reduce the hashtags to 
#       popular ones (following the same logic as with the reduction of tokens). We also sort the hashtags
#       alphabetically on order prevent the (random) order having an impact
#     This is still not a perfect solution, since it treats combinations of hashtags, rather than the hashtags
#       themselves, as values of the meta variable "hashtag"


hashtags <- stm_tweets %>% select(status_id, hashtags)
hashtags$hashtags <- vapply(hashtags$hashtags, paste, collapse = " ", character(1L))
hashtag_corpus <- corpus(hashtags, text_field = "hashtags",
                          docid_field = "status_id")
hashtag_dfm <- dfm(hashtag_corpus, remove_numbers = T)

hashtag_freq <- textstat_frequency(hashtag_dfm)

#  drop hashtags used for the search from the sample
hashtag_dfm <- hashtag_dfm %>% 
  dfm_select(pattern = search_tags, selection = "remove")

# additionally drop the hashtag "sachsen" since it does not have any topical relevance
hashtag_dfm <- hashtag_dfm %>% 
  dfm_select(pattern = "sachsen", selection = "remove")

# drop "na" (no hashtag)
hashtag_dfm <- hashtag_dfm %>% 
  dfm_select(pattern = "na", selection = "remove")

# Further reduce amount of hashtags
hashtag_dfm <- hashtag_dfm%>% 
  dfm_keep(min_nchar = 2) %>% # remove chars with only one character
  dfm_trim(min_docfreq = 0.005, docfreq_type = 'prop') 
  # only keeps hashtags appearing in a minimum of 0.5% of documents (in at least 203 docs)

hashtag_freq <- textstat_frequency(hashtag_dfm)
  # leaves 50 hashtags



# Convert to STM data type
out <- convert(dfm, to = 'stm')   # drops 169 empty documents (40,343  to 40,174 documents)
names(out)

out$meta$hashtags <- lapply(out$meta$hashtags, tolower)


#  reduce hashtags to terms appearing in hashtag_freq
out$meta$hashtags <- lapply(out$meta$hashtags, function(x) x[which(x %in% hashtag_freq$feature)])

# remove duplicates in the hashtags of each tweet
out$meta$hashtags <- lapply(out$meta$hashtags, unique)

# sort hashtags alphabetically (same hashtags = same variable, nvm the order)
out$meta$hashtags <- lapply(out$meta$hashtags, sort)


# # hashtag character vectors to factors ?
# out$meta$hashtags <- lapply(out$meta$hashtags, function (x) factor(unlist(x), 
#                           levels = unique(unlist(out$meta$hashtags))))
#  # -> doesn't help, model still spits same error


# collapse hashtag vectors
out$meta$hashtags <- vapply(out$meta$hashtags, paste, collapse = " ", character(1L))

# # replace empty cells with NA
# out$meta$hashtags[out$meta$hashtags == ""] <- NA
#   => stm treads NAs as missing rows, resulting in unequal numbers of rows in covariates
#       empty cells have to stay in the model, even though all the ""s are treated as one observation of the
#       prevalence covariate (no known workaround)


out$meta$hashtags %>% table() %>% as.data.frame() %>% View()
# still a rather large amount of unqiue vectors with low occurences in the overal sample (>50% == 1), 
#   and empty vectors (= no/deleted hashtags count as the same obervation in the model)
# However, the hashtag data is as optimized as possible, with an overall okay distribution of unique vectors.
#   It would be possible to reduce low-occurence hashtag combinations even further. This, however, would lead to more
#   empty-value observations
# The only way to fix these problems would be the support of recursive (non-atomic) in the STM models. This, however,
#   is not supported in the package as of now 



# drop unnecessary metadata to reduce overhead
out$meta <- out$meta %>% 
  select(hashtags, day, followers_count, 
         is_quote, is_retweet, retweet_status_id, screen_name, 
         favorite_count, retweet_count,
         quoted_status_id, retweet_status_id,
         text, community)

setwd("Topic Models")

save(out, file = "SW19_out.RDa")


## Topic Models -----------

## (Best to run searchKs, estimateEffects and numerous models on Server. Steps here for completeness)

# hashtags and (splined) days are prevalence variables
# (top) communities are content variables


# try to find appropraite number of topics
Ks <- searchK(documents = out$documents, vocab = out$vocab, K = 5:20, 
              prevalence =~ hashtags + s(day), content = ~community, data = out$meta, 
              gamma.prior = "L1", init.type = "Spectral")

plot.searchK(Ks)

  # 13 or 16 topics look reasonable in their statistical measures:
  #   low residuals, high held-out likelihood 


# Run Models 
  # (preferably on a server)
for (i in 5:20) {
  
  stm <- stm(documents = out$documents, vocab = out$vocab, K = i, 
             prevalence =~ hashtags + s(day), 
             content = ~community,
             data = out$meta, gamma.prior = "L1", init.type = "Spectral" )
  
  model_name <- paste0("SW19_stm", i)  
  
  assign(model_name, stm, envir = .GlobalEnv)
  
  filename = paste0(model_name,".RDa")
  
  save(list=model_name, file = filename)
  
}


## Compare Models

filenames <- list.files(pattern = "SW19_stm*")

for (i in 1:length(filenames)){
  load(filenames[i])
}

models <- mget(ls(pattern="SW19_stm"))


# ## Exclusivity and semantic coherence calcualtions (for models without content variables)

# ggplot() +
#   geom_point(aes(x = semanticCoherence(SW19_stm5, documents = out$documents, M = 20),
#                  y = exclusivity(SW19_stm5, M = 20)), shape = 21, fill = "green") +
#   geom_point(aes(x = semanticCoherence(SW19_stm8, documents = out$documents, M = 20),
#                  y = exclusivity(SW19_stm8, M = 20)), shape = 22, fill = "blue") +
#   geom_point(aes(x = semanticCoherence(SW19_stm10, documents = out$documents, M = 20),
#                  y = exclusivity(SW19_stm10, M = 20)), shape = 23, fill = "red") +
#   geom_point(aes(x = semanticCoherence(SW19_stm12, documents = out$documents, M = 20),
#                  y = exclusivity(SW19_stm12, M = 20)), shape = 1, color = "yellow") +
#   geom_point(aes(x = semanticCoherence(SW19_stm15, documents = out$documents, M = 20),
#                  y = exclusivity(SW19_stm15, M = 20)), shape = 2, color = "orange") +
#   geom_point(aes(x = semanticCoherence(SW19_stm18, documents = out$documents, M = 20),
#                  y = exclusivity(SW19_stm18, M = 20)), shape = 3, color = "grey") +
#   geom_point(aes(x = semanticCoherence(SW19_stm20, documents = out$documents, M = 20),
#                  y = exclusivity(SW19_stm20, M = 20)), shape = 4, color = "purple") +
#   scale_x_continuous("Semantic Coherence") +
#   scale_y_continuous("Exclusivity") 

# # with stminsights
# 
# diag <- get_diag(models =  list(
#   model5 = SW19_stm5,
#   model8 = SW19_stm8,
#   model10 = SW19_stm10,
#   model12 = SW19_stm12,
#   model15 = SW19_stm15,
#   model18 = SW19_stm18,
#   model20 = SW19_stm20),
#   out)
# 
# diag %>%
#   ggplot(aes(x = coherence, y = exclusivity, color = statistic))  +
#   geom_text(aes(label = name), nudge_x = 2) + geom_point() +
#   labs(x = 'Semantic Coherence', y = 'Exclusivity')


# Topics
labelTopics(SW19_stm13)
plot(SW19_stm13, type = "summary", n = 5, xlim = c(0, 0.3))

labelTopics(SW19_stm16)
plot(SW19_stm16, type = "summary",  n = 5, xlim = c(0, 0.3))

# Find Thoughts
findThoughts(SW19_stm13, n = 5, texts = out$meta$text)

findThoughts(SW19_stm16, n = 5, texts = out$meta$text)

  # for only one community
findThoughts(SW19_stm13, texts = out$meta$text, where = community==193, meta = out$meta)



## Analyse topic content ----------
  # 13 Topics seem more precise in their assessment of content

setwd("Topic Models")
load("SW19_out.RDa")
load("SW19_stm13.RDa")

# prevent tweets with the same content (not retweets) to show up repeatedly, easier way to look at tweets
stm_dt <- make.dt(SW19_stm13, meta = out$meta)
stm_dt <- unique(stm_dt, by = "text")
View(stm_dt)

### Topics:

# Topic 1: (Emotionally charged) assessment of the election results, esp. role of the CDU
#           (E.g.: "Was habt ihr denn geschafft? Ihr habt geschafft, dass in eurem Beisammensein Faschisten heranwuchsen. 
#                   Das habt ihr geschafft." https://t.co/ZcdzBCgkWx #ltwsn19) (ZEIT commentary on the AfD's rise)

# Topic 2: Extreme right, Fascism in AfD, esp. Kalbitz and Urban, Flügel

# Topic 3: Election Results, esp. losses of GroKo (SPD/CDU) and other established parties, high amount of pro-AfD/anti-establishment tweets

# Topic 4: Change, esp. education and climate, women in politics ;high correlation of tweets by SPD

# Topic 5: Begin of the 2nd Worldwar (same date as election), paralles between Nazis and AfD Neonazis

# Topic 6: Mixed bag, esp. about campaigning and canvasing

# Topic 7: Campaigning, voter turnout, wahl-o-mat (online tool for finding fitting party)

# Topic 8: Elections in Brandenburg (same day), paralles between the two elections

# Topic 9: (assumed) Election win of AfD

# Topic 10: Mobilisation to go vote (generally, for certain parties, etc.)

# Topic 11: Election Campaign of CDU, esp. (minister president) Kretschmer, and relationship to Maaßen

# Topic 12: Manifestations in Dresden: esp. Unteilbar, also PEGIDA

# Topic 13: topics of the parties, stances on certain positions, app "wahlswiper"

content <- data.frame(topic = paste0("Topic",1:13),
                      topic_nr = 1:13,
                      content = c("Causes for Election Results \n ",
                                  "Right-wing Extremism in the AfD \n ",
                                  "Election Results, \n Losses of established Parties",
                                  "Demanding Political Change \n ",
                                  "Start of the \n Second World War ",
                                  "Campaigning and Canvassing \n ",
                                  "Campaigning, Voter Turnout, \n Wahlomat",
                                  "Brandenburg Elections \n ",
                                  "(Assumed) Election win \n of the AfD",
                                  "Voter Mobilization \n ",
                                  "CDU Campaign, \n Kretschmer and Maaßen",
                                  "Manifestations in Dresden, \n esp. Unteilbar",
                                  "Party Positions on \n specific Topics"))

## Topic Proportions
CairoPNG("topic_proportions.png", width=1600, height=900, pointsize=8, dpi=160)
plot(SW19_stm13, type = "summary", n = 5, xlim = c(0, 0.3))
dev.off()





# Prevalence Effects
gc()

SW19_stm_effects13_prevalence <- estimateEffect(~ hashtags + s(day),
                                                stmobj = SW19_stm13, metadata = select(out$meta, hashtags, day))

SW19_stm_effects13_content <- estimateEffect(~ s(community),
                                             stmobj = SW19_stm13, metadata = select(out$meta, community))
                                  # => Regression on topic prevalence. 
                                  #       For "community", that means: effect on prevalence *if it had been* specified 
                                  #       as prevalence variable (instead of a content variable)


    # => estimateEffects takes forver, even with only 1 topic?
    #       -> reduce meta veriables
    # => cannot allocate vector?
    #       -> remove unecessary objects from the environment, run gc()
    # => Generally: estimateEffects objects can get HUGE, up to 10gb in this case.
    #           This can lead to serious problems loading them into the environment, 
    #           as enough RAM on tap is required to do allocate them

save(SW19_stm_effects13_prevalence, SW19_stm_effects13_content, file = "SW19_stm_effects13.RDa")


summary(SW19_stm_effects13_content)



## Prevalence effects with stminsights
gc()
effects <- get_effects(estimates = SW19_stm_effects13_prevalence,
                       variable = 'hashtags',
                       type = 'pointestimate')
gc()


effects %>% filter(topic == 1) %>%
  ggplot(aes(x = value, y = proportion)) + geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  coord_flip() + labs(x = 'hashtags', y = 'Topic Proportion')


# Export for comparison in stminsights

save(SW19_stm5, SW19_stm8, SW19_stm10, SW19_stm12, SW19_stm15, SW19_stm18, SW19_stm20, effects, out,
     file =  "Topic Models/SW19_stm_comparison.RDa")
run_stminsights()

# Correlation networks with stminsights
stm_corrs <- get_network(model = SW19_stm13,
                         method = 'simple',
                         labels = paste('T', 1:13),
                         cutoff = 0.01,
                         cutiso = TRUE)

plot(stm_corrs)


# Correlation Table

correlations <- topicCorr(SW19_stm13, cutoff = 0.01)
write.xlsx(correlations$poscor, "topic_correlations.xlsx")



### Topic Models and Network Analysis --------------
setwd("Topic Models")
load("SW19_out.RDa")
load("SW19_stm13.RDa")


## topic information for retweets

SW19_tweets_topic <- SW19_tweets_comm

topics <- make.dt(SW19_stm13, meta = out$meta)
topics$status_id <- dimnames(SW19_stm13$mu$mu)[2] # get status_id from dimnames in stm object
topics <- select(topics, starts_with("Topic"), status_id, retweet_status_id)

SW19_tweets_topic <- left_join(SW19_tweets_topic, select(topics, -retweet_status_id),
                               by = "status_id")

  # retweets
SW19_tweets_topic <- left_join(SW19_tweets_topic, select(topics, -status_id), 
                               by = c("status_id"="retweet_status_id"))

# retweets whose original tweet is not in the sample
SW19_tweets_topic <- left_join(SW19_tweets_topic, select(topics, -status_id), 
                               by = "retweet_status_id", na_matches = "never") # ! na_matches="never" to not blow the DF into the million!

SW19_tweets_topic <- unique(as.data.table(SW19_tweets_topic), by="status_id")



# loops don't work for this somehow.... ------
SW19_tweets_topic <- SW19_tweets_topic %>% 
  mutate(Topic1 = coalesce(Topic1.x, Topic1.y, Topic1)) %>% 
  select(-Topic1.x, -Topic1.y) 

SW19_tweets_topic <- SW19_tweets_topic %>% 
  mutate(Topic2 = coalesce(Topic2.x, Topic2.y, Topic2)) %>% 
  select(-Topic2.x, -Topic2.y) 

SW19_tweets_topic <- SW19_tweets_topic %>% 
  mutate(Topic3 = coalesce(Topic3.x, Topic3.y, Topic3)) %>% 
  select(-Topic3.x, -Topic3.y) 

SW19_tweets_topic <- SW19_tweets_topic %>% 
  mutate(Topic4 = coalesce(Topic4.x, Topic4.y, Topic4)) %>% 
  select(-Topic4.x, -Topic4.y) 

SW19_tweets_topic <- SW19_tweets_topic %>% 
  mutate(Topic5 = coalesce(Topic5.x, Topic5.y, Topic5)) %>% 
  select(-Topic5.x, -Topic5.y) 

SW19_tweets_topic <- SW19_tweets_topic %>% 
  mutate(Topic6 = coalesce(Topic6.x, Topic6.y, Topic6)) %>% 
  select(-Topic6.x, -Topic6.y) 

SW19_tweets_topic <- SW19_tweets_topic %>% 
  mutate(Topic7 = coalesce(Topic7.x, Topic7.y, Topic7)) %>% 
  select(-Topic7.x, -Topic7.y) 

SW19_tweets_topic <- SW19_tweets_topic %>% 
  mutate(Topic8 = coalesce(Topic8.x, Topic8.y, Topic8)) %>% 
  select(-Topic8.x, -Topic8.y) 

SW19_tweets_topic <- SW19_tweets_topic %>% 
  mutate(Topic9 = coalesce(Topic9.x, Topic9.y, Topic9)) %>% 
  select(-Topic9.x, -Topic9.y) 

SW19_tweets_topic <- SW19_tweets_topic %>% 
  mutate(Topic10 = coalesce(Topic10.x, Topic10.y, Topic10)) %>% 
  select(-Topic10.x, -Topic10.y) 

SW19_tweets_topic <- SW19_tweets_topic %>% 
  mutate(Topic11 = coalesce(Topic11.x, Topic11.y, Topic11)) %>% 
  select(-Topic11.x, -Topic11.y) 

SW19_tweets_topic <- SW19_tweets_topic %>% 
  mutate(Topic12 = coalesce(Topic12.x, Topic12.y, Topic12)) %>% 
  select(-Topic12.x, -Topic12.y) 

SW19_tweets_topic <- SW19_tweets_topic %>% 
  mutate(Topic13 = coalesce(Topic13.x, Topic13.y, Topic13)) %>% 
  select(-Topic13.x, -Topic13.y) 
#----- 

is.na(SW19_tweets_topic$Topic1) %>% sum()
  # 8904 Tweets without classification. 
  #   Reasons: non-german tweets (sorted out in beginning), or docs dropped as empty after stopword removal


save(SW19_tweets_topic, content, file="SW19_tweets_topic.RDa")



## Topics in Communities

stm_com <- select(SW19_tweets_topic, community_name, starts_with("Topic")) # Topics (theta) per Tweet sum to 1
stm_com <- stm_com[complete.cases(stm_com),] # drop Tweets with NAs

  # number of tweets within a community
n <- stm_com %>% 
  group_by(community_name) %>% 
  summarise(n = n()) %>% select(n)

stm_com <- stm_com %>% 
  group_by(community_name) %>% 
  summarise_all(funs(sum))
  
stm_com$n <- n[[1]]

  # account for nr of tweets in community
stm_com <- stm_com %>% 
  group_by(community_name) %>%  
  mutate_at(vars(contains("Topic")), ~(./n))

write.xlsx(as.data.frame(stm_com), "Topic_prevalence_comm.xlsx")



## Visualize Topics in Communities

  # data to long format
topic_viz <- as.data.table(select(stm_com, -n))
topic_viz <- melt(topic_viz)
names(topic_viz) <- c("community", "topic", "prevalence")
topic_viz <- left_join(topic_viz, content)


CairoPNG("community_topics.png", width=1600, height=900, pointsize=8, dpi=160)
ggplot(topic_viz) +
  geom_col(aes(x = community, y = prevalence, fill = reorder(topic, topic_nr))) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(length(unique(topic_viz$topic)), "Set3"))(length(unique(topic_viz$topic))),
                    labels = unique(topic_viz$content),
                    guide_legend(title = "Topic")) +  # custom palette to accommodate  topics
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, margin = margin(5,0,0,0,"pt"), vjust = 1, hjust = 1), 
        axis.title.x = element_blank(), axis.ticks = element_blank(), legend.margin = margin(25,0,0,0,"pt"),
        legend.key.height  = unit(0.5, "cm"), legend.text = element_text(margin = margin(5,0,0,0,"pt"), vjust = 1, hjust = 0)) 
dev.off()  



### Topics in Interaction

load("E:\\Nextcloud\\R\\Masterarbeit\\SW19_Mentions_NodesEdges.RDa")

stm_inter <- select(edges_df, status_id, crossing, is_retweet)
stm_inter <- left_join(stm_inter, select(SW19_tweets_topic, starts_with("Topic"), status_id))
is.na(stm_inter$Topic1) %>% sum()
  # 8832 Tweets without classification. Reasons: see above. Slightly less than above since tweets without interaction don't show here
stm_inter <- stm_inter[complete.cases(stm_inter),]

# Classify Interaction
stm_inter$interaction[stm_inter$crossing==F & stm_inter$is_retweet==F] <- "Intra-Community Deliberation"
stm_inter$interaction[stm_inter$crossing==T & stm_inter$is_retweet==F] <- "Inter-Community Deliberation" 
stm_inter$interaction[stm_inter$crossing==F & stm_inter$is_retweet==T] <- "Intra-Community Information Diffusion"
stm_inter$interaction[stm_inter$crossing==T & stm_inter$is_retweet==T] <- "Inter-Community Information Diffusion"

stm_inter <- select(stm_inter, starts_with("Topic"), interaction)

# number of tweets within a category
n <- stm_inter %>% 
  group_by(interaction) %>% 
  summarise(n = n()) %>% select(n)

stm_inter <- stm_inter %>% 
  group_by(interaction) %>% 
  summarise_all(funs(sum))

stm_inter$n <- n[[1]]

# account for amount of interaction
stm_inter <- stm_inter %>% 
  group_by(interaction) %>%  
  mutate_at(vars(contains("Topic")), ~(./n))

write.xlsx(as.data.frame(stm_inter), "Topic_prevalence_interaction.xlsx")



## Visualize Topics in Interaction

# data to long format
topic_viz <- as.data.table(select(stm_inter, -n))
topic_viz <- melt(topic_viz)
names(topic_viz) <- c("community", "topic", "prevalence")
topic_viz <- left_join(topic_viz, content)


CairoPNG("interaction_topics.png", width=1600, height=900, pointsize=8, dpi=160)
ggplot(topic_viz) +
  geom_col(aes(x = community, y = prevalence, fill = reorder(topic, topic_nr))) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(length(unique(topic_viz$topic)), "Set3"))(length(unique(topic_viz$topic))),
                    labels = unique(topic_viz$content),
                    guide_legend(title = "Topic")) +  # custom palette to accommodate  topics
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, margin = margin(5,0,0,0,"pt"), vjust = 1, hjust = 1), 
        axis.title.x = element_blank(), axis.ticks = element_blank(), legend.margin = margin(45,0,0,0,"pt"),
        legend.key.height  = unit(0.5, "cm"), legend.text = element_text(margin = margin(5,0,0,0,"pt"), vjust = 1, hjust = 0)) 
dev.off()  



## Word use among Communities

## 

topiclabels <- labelTopics(SW19_stm13)

community_names <- unique(as.data.table(nodes_df),by="community") %>% select(community, community_name) %>% arrange(community)

dimnames(topiclabels$covariate)[[1]] <- community_names$community_name
community_names
      # => closer look at topics 4 and 9 between communities (most popular topics)
      # Torgau: "Markt der Parteien" - campaigning event for all parties

plot(SW19_stm13, type = "labels", topics = 4)

plot(SW19_stm13, type = "perspectives", topics = 4, covarlevels = c(3,11))


