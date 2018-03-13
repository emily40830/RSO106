library(tidyverse)
# https://www.tidytextmining.com/tidytext.html



# Loading data ------------------------------------------------------------

load(url("http://varianceexplained.org/files/trump_tweets_df.rda"))
View(trump_tweets_df)
names(trump_tweets_df)
# [1] "text"          "favorited"     "favoriteCount" "replyToSN"
# [5] "created"       "truncated"     "replyToSID"    "id"
# [9] "replyToUID"    "statusSource"  "screenName"    "retweetCount"
# [13] "isRetweet"     "retweeted"     "longitude"     "latitude"

tweets <- trump_tweets_df




# Inspecting the text -----------------------------------------------------

tweets$text
length(tweets$text)
nchar(tweets$text)
hist(nchar(tweets$text))
?hist
hist(nchar(tweets$text), breaks = 20)



# Seperating sentences to words -------------------------------------------
# https://www.tidytextmining.com/tidytext.html
??unnest_tokens #斷詞斷句

library(tidytext)
unnest.df <- unnest_tokens(tweets, word, text, drop = FALSE)
unnest.df <- unnest_tokens(tweets, word, text, to_lower = TRUE)

count(unnest.df, word,sort= TRUE)



# Removing stop words -----------------------------------------------------

data(stop_words)
View(stop_words)
stop.df <- anti_join(unnest.df, stop_words)




# Counting word frequency -------------------------------------------------

count(stop.df, word, sort = TRUE)



# Re-do it by dplyr -------------------------------------------------------

word_freq <- tweets %>%
    unnest_tokens(word, text, drop = FALSE) %>%
    anti_join(stop_words) %>%
    count(word, sort = TRUE)



# Visualizing words sorted by frequnecy -----------------------------------

unnested <- tweets %>%
    unnest_tokens(word, text, drop = FALSE) %>%
    anti_join(stop_words)


unnested %>%
    count(word, sort = TRUE) %>%
    top_n(50) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    coord_flip()




# Toward Chinese text processing ------------------------------------------

library(stringr)

tweets %>%
    mutate(word = map(text, function(x)unlist(str_split(x, "\\s")))) %>%
    unnest(word)



# Assignment --------------------------------------------------------------

library(jsonlite)
library(httr)
url <- "https://www.dcard.tw/_api/forums/relationship/posts?popular=true"
res <- fromJSON(content(GET(url), "text"))

res <- res %>%
    select(-topics, -tags, -media)

?unnest_tokens

unnested.df <- unnest_tokens(res, word, excerpt, drop = FALSE)


# stopWords <- readRDS(url("https://github.com/R4CSS/RSO106/raw/master/data/stopWords.rds"))
stopWords <- readRDS("data/stopWords.rds")
stopWords <- as.data.frame(stopWords)
names(stopWords) <- "word"

test.stop.df <- anti_join(unnested.df, stopWords)

word_freq <- res %>%
    unnest_tokens(word, excerpt, drop = FALSE) %>%
    anti_join(stopWords) %>%
    count(word, sort = TRUE)
