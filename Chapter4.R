
# 石田基広『実践 Rによるテキストマイニング』森北出版 スクリプト vers.1.0
# 第 4 章
## section 4.2 

library(rtweet)

myApp <- "*****"
consumerKey <- "**********************"
consumerSecret <- "*******************************************"
accessToken <- "*******************************************"
accessTokenSecret <- "****************************************"

token <- create_token(
  app = myApp,
  consumer_key = consumerKey,
  consumer_secret = consumerSecret ,
  access_token =accessToken,
  access_secret = accessTokenSecret)

# twitter_app <- oauth_app(app, key = consumerKey, secret = consumerSecret)

rt <- search_tweets(
  "大阪万博", n = 10000, include_rts = FALSE
)

## Searching for tweets...
## This may take a few seconds...
##  Warning:  Rate limit exceeded - 88
## Finished collecting tweets!
##  Warning Message: 
## Rate limit exceeded

rt %>% dim()

rt %>% colnames()


save(rt, file = "banpaku.Rdata")


rt %>% select(created_at) %>% summary()

rt %>% summarize({last_ = max(created_at) ;
                  first_ = min(created_at) ; 
                  last_ - first_})

rt %>% select(retweet_count) %>% summary()

rt %>% arrange(retweet_count)  %>% select(text) %>% 
         tail(3) %>% pull()

rt %>% count(user_id) %>% arrange(n) %>% tail(10)

rt %>% filter(user_id %in% c("946562830634926080"))  %>% 
         select(text) %>% mutate(text = str_remove_all(text, 
                                 "https?://[\\w/:%#\\$&\\?\\(\\)~\\.=\\+\\-]+")) %>% 
                            unique %>% NROW()

userIDS <-  rt %>% count(user_id) %>% filter(n > 1) %>% 
                     select(user_id) %>% pull()
# library(purrr)
rt2 <-  userIDS %>% map_dfr(., {
                      ~ filter(rt, user_id == .) %>%
                           mutate(text = str_remove_all(text, 
                                       "https?://[\\w/:%#\\$&\\?\\(\\)~\\.=\\+\\-]+"),
                                  text = str_remove_all(text, "\\p{So}|\\p{Cn}") ) %>%
                              select(user_id, status_id, screen_name, text) %>% 
                                distinct(user_id, screen_name, text, .keep_all = TRUE)
  })

rt2 %>% count(user_id) %>% arrange(n) %>% tail(5)

rt2 %>% select(text) %>% pull() %>% 
                           write("banpaku.txt")

library(RMeCab)
gc() ; gc()
txt_df <- docDF("banpaku.txt", type = 1)
# txt_df  <- docDF("/myData/Books/morikita2/Vol2/banpaku.txt", type = 1)

txt_df %>% select(POS1) %>% distinct() %>% pull()

txt_df %>% select(POS2) %>% distinct() %>% pull()

txt_df <- txt_df %>% filter(POS1 %in% c("名詞","形容詞","動詞"), 
                            POS2 %in% c("一般","自立" ,"非自立",
                                        "助詞類接続"))

txt_df %>% NROW()

## section 4.5

library(ggwordcloud)
txt_df %>% filter(banpaku.txt > 10) %>% ggplot() +
                                          aes(label = TERM, size = banpaku.txt) +
                                          geom_text_wordcloud(size = 6) + 
                                          scale_size_area(max_size = 20) + theme_minimal()
dev.off()

stop_words  <- read_tsv(
   "http://svn.sourceforge.jp/svnroot/slothlib/CSharp/Version1/SlothLib/NLP/Filter/StopWord/word/Japanese.txt",
                           col_names = "TERM")
ja_stop_words  <- stop_words %>% 
      add_row(TERM = c("ある", "する", "てる", "いる", "の", "いう", 
                       "しまう", "なる", "万博"))


txt2_df <- txt_df %>% select(TERM, FREQ = banpaku.txt) %>% 
             arrange(FREQ) %>% tail(100) %>% 
               anti_join(ja_stop_words, by = "TERM")

txt2_df %>% filter(FREQ > 10) %>% ggplot() +
                                   aes(label = TERM, color = FREQ, size = FREQ) +
                                   geom_text_wordcloud() +
                                   scale_size_area(max_size = 20) + 
                                   theme_minimal() 
dev.off()


## section 4.6

gc(); gc()
txt3_df <- docDF("banpaku.txt", type = 1, 
                pos = c("名詞", "形容詞", "動詞"), N = 2, nDF = TRUE)



txt3_df %>% head(5)

library(igraph)
library(ggraph)
txt3_df  %>% arrange(banpaku.txt) %>% tail(100) %>% select(N1, N2, banpaku.txt) %>%
  graph_from_data_frame() %>% ggraph(layout = 'graphopt') + 
                                geom_edge_diagonal(alpha = 1, label_colour = "blue") +
                                geom_node_label(aes(label = name), size = 5, repel = TRUE)

## section 4.7

negaposi <- read_tsv(
                "http://www.cl.ecei.tohoku.ac.jp/resources/sent_lex/pn.csv.m3.120408.trim",
                     col_names = c("TERM","VALUE","CRITERIA"))

negaposi  <- negaposi %>%  mutate(VALUE  = case_when(
                                             VALUE == "n" ~ -1,
                                             VALUE == "p" ~ 1,
                                             TRUE ~ 0) )
negaposi %>% select(VALUE) %>% summary()



## for windows users
if( (.Platform$OS.type == "windows") & Encoding(rt2$text[1]) == "UTF-8"){
  rt2 <- rt2 %>% mutate(text = iconv(text, from = "UTF-8", sub = ""))
  negaposi <- negaposi %>% mutate(TERM = iconv(TERM, from = "UTF-8", sub = ""))
} else{
}

rmecabc_ <- function(si, txt){
     txt <- unlist(RMeCabC(txt, 1))
     tibble(status_id  = si, TERM = txt)
}

# library(purrr) 
tweets <- map2_dfr(rt2$status_id, rt2$text,
 ~ rmecabc_(..1, ..2)
)

tweets %>% NROW()
tweets %>% head()

negaposi <- negaposi %>% select(TERM, VALUE)
dat2 <- tweets %>% left_join(negaposi)

dat2 <- dat2 %>% group_by(status_id) %>% 
                   summarise(VALUE = sum(VALUE, na.rm = TRUE))
dat2 %>% select(VALUE) %>% summary()

most <- dat2 %>% filter(VALUE == max(VALUE))
most %>% map_df(~ filter(rt2, status_id %in% .x)) %>% 
           select(text) # %>% pull()

min <- dat2 %>% filter(VALUE == min(VALUE))
min %>% map_df(~ filter(rt, status_id %in% .x)) %>% 
          select(text) #  %>% pull()

## section 4.8
rt2 <- rt %>% select( user_id, status_id, screen_name, text) %>%  left_join(dat2)
rt3 <- rt2 %>% group_by(user_id, screen_name) %>% summarize(VALUE = mean(VALUE, na.rm = TRUE)) %>%
                 ungroup()
rt3 %>% group_by(VALUE > 0) %>% top_n(20, abs(VALUE)) %>%  arrange(VALUE) %>% 
          mutate(user = if_else(VALUE > 0, "PS", "NE"), user = paste0(user, row_number())) %>%   
          ggplot(aes(fct_reorder(user, VALUE), VALUE, fill = VALUE > 0)) +
            geom_col(alpha = 0.8, show.legend = FALSE) + coord_flip()
dev.off()

rt3_desc <- rt3 %>% group_by(VALUE > 0) %>% top_n(20, abs(VALUE)) %>% 
                      left_join(rt %>% distinct(user_id, description))
rt3_desc %>% head()


## section 4.9

## 最初にテキストを保存するフォルダを用意
dir.create("negpos")
rt3_desc %>% filter(VALUE > 0) %>% select(description) %>% pull() %>% 
               str_remove_all("\\p{ASCII}|\\p{So}|\\p{Cn}") %>% write("negpos/plus.txt")
rt3_desc %>% filter(VALUE < 0) %>% select(description) %>% pull() %>%  
               str_remove_all("\\p{ASCII}|\\p{So}|\\p{Cn}") %>% write("negpos/minus.txt")

rt3_desc %>% filter(VALUE > 0) %>% select(description) %>% pull() %>%  str_remove_all("\\p{ASCII}|\\p{So}|\\p{Cn}") %>% write("/myData/Books/morikita2/Vol2/negpos/plus.txt")
rt3_desc %>% filter(VALUE <0 ) %>% select(description) %>% pull() %>%  str_remove_all("\\p{ASCII}|\\p{So}|\\p{Cn}")  %>% write("/myData/Books/morikita2/Vol2/negpos/minus.txt")

negpos <- docDF("negpos", type = 1, pos = c("名詞", "動詞", "形容詞"))
negpos <- negpos %>% anti_join(ja_stop_words, by = "TERM")
negpos %>% head()

pdf(file="/myData/Books/morikita2/Vol2/images/banpaku_compairson.pdf")
library(ggwordcloud)
negpos %>% select(TERM, minus.txt, plus.txt) %>% gather(key = txt, value = freq, -TERM) %>%
              filter(freq > 1) %>% ggplot(aes(label = TERM, size = freq, col = txt, x = txt)) +
               geom_text_wordcloud() + scale_size_area(max_size = 20) + theme_minimal() 
dev.off()




library(tidyverse)


download.file("http://www.db.info.gifu-u.ac.jp/data/tweets_open.csv.bz2", destfile = "tweets_open.csv.bz2")

install.packages("R.utils")
library(R.utils)
bunzip2("tweets_open.csv.bz2", overwrite=TRUE, remove=FALSE)

tweets <- read_csv("tweets_open.csv", 
                   col_names = c("id", "genre", "status_id", 
                                 "PN", "Po", "Ne", "Neu", "Non"),
                   col_types = "ccciiiii")


tweets %>% select(Po, Ne)


tweets

library(rtweet)
iPhone <- tweets %>% filter(genre == "10021") %>% 
                        select(status_id) %>% 
                          pull() %>% lookup_tweets()


save(iPhone, file = "iPhone.Rdata")

# write_as_csv(iPhone, file = "iPhone.Rdata", 
#              fileEncoding = "CP932")

dim(iPhone)



iPhone_data <- iPhone %>% select(status_id,text) %>% 
                            left_join(tweets)
iPhone_data  %>% select(Po, Ne)
iPhone_data  %>% summarise(S = sum(Po))

X <- c(1, 1, 0, 0)
Y <- c(1, 0, 1, 0)
xor(X, Y)

iPhone_data <- iPhone_data %>% filter(xor(Po, Ne))  %>% 
                            select(status_id, text, Po)

dim(iPhone_data)

save(iPhone_data, file = "iPhone_data.Rdata")



library(RMeCab)
gc(); gc()

## for windows users
if( (.Platform$OS.type == "windows") & Encoding(iPhone_data$text[1]) == "UTF-8"){
  iPhone_data <- iPhone_data %>% mutate(text = iconv(text, 
                                                     from = "UTF-8", 
                                                     to = "CP932",
                                                     sub = ""))} else{
                                                     }


rmecabc_po <- function(id, po, txt){
     txt <- unlist(RMeCabC(txt, 1))
     txt  <- txt[names(txt) %in% c("名詞")] 
     tibble(status_id = id, Po = po, TERM = txt)
}

gc() ; gc()

iPhone_tokens <- pmap_dfr(list(iPhone_data$status_id, 
                               iPhone_data$Po, 
                               iPhone_data$text),
                          ~ rmecabc_po(..1, ..2, ..3)
)

iPhone_tokens
## 以下は、筆者の実行した際の結果
## # A tibble: 55,612 x 3 
##    status_id          Po    TERM  
##    <chr>              <lgl> <chr> 
##  1 702448000690319360 TRUE  iPhone
##  2 702448000690319360 TRUE  6     
##  3 702448000690319360 TRUE  Plus  
##  4 702448000690319360 TRUE  標準  
##  5 702448000690319360 TRUE  電卓

iPhone_counts <- iPhone_tokens %>% count(status_id, 
                                         TERM, sort = TRUE)

iPhone_counts  <- iPhone_counts %>% 
                    filter(!str_detect(TERM,
                           "[[:punct:]]|[[:digit:]]"))



iPhone_counts  
# A tibble: 41,453 x 3
 ##   status_id          TERM             n
 ##   <chr>              <chr>        <int>
 ## 1 699870636890402816 卍              12
 ## 2 552035413951381504 ばば             7
 ## 3 650794129010487296 ﾟ                6
 ## 4 691118807138435073 アア             6



