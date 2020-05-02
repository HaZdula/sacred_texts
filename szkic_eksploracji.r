library(stringi)

df <- read.csv('./AllBooks_baseline_DTM_Labelled.csv')
books <- stri_extract(df$X, regex =  "^[a-zA-Z]+")
df$X <- books
colnames(df)
sum(df[-1])
nrow(df)
ncol(df)
head(df, 1)
  

# Pan Tadeusz ma 68 tysięcy dla porównania

wyciagnij_slowa_z_tabelki <- function(df,n){
  #' df word of bag
  #' n number of wiersz
  cols <- colnames(df)
  words <- sapply(2:length(cols), function(i,n, df){if(df[n,i] > 0){rep(cols[i], df[n,i])} else {NULL}}, df=df, n=n, simplify = TRUE)
  unlist(words)
}
  
w <- wyciagnij_slowa_z_tabelki(df, 2)
  
get_words_in_books <- function(df, bookname){
  slice <- df[df[,1] == bookname,]
  all <- sapply(1:nrow(slice), wyciagnij_slowa_z_tabelki, df=df, simplify = TRUE)
  unlist(all)
}


#
library(parallelMap)
parallelStartMulticore(8)
  
# Zobaczmy długości poszczególnych dzieł:
booknames <- unique(books)

starttime <- Sys.time()
inside <- parallelLapply(booknames, get_words_in_books, df=df)
endtime <- Sys.time()
endtime - starttime

names(inside) <- booknames




select_sentiment <- function(word, sent_name){
  #' fukncja zwraca liste posortowanych slow o danym sentymencie
  #' @param word lista slow do analizy
  #' @param sent_name nazwa sentymentu, ktory chcemy otrzymac
  
  require('tidytext')
  require('textdata')
  # wynieramy slowa ze slownika sentymentu
  nrc_sent <- get_sentiments("nrc") %>% 
    filter(sentiment == sent_name)
  
  #zliczamy w danych wystapienia tych slow
  as.data.frame(word) %>%
    inner_join(nrc_sent) %>%
    count(word, sort = TRUE) %>%
    mutate(sent = sent_name)
}

all_sentiment <- function(word, k = 100, all = FALSE, sent = c("trust", 
                                                               "fear", 
                                                               "negative", 
                                                               "sadness",
                                                               "anger", 
                                                               "surprise",
                                                               "positive", 
                                                               "disgust",
                                                               "anticipation", 
                                                               "joy")){
  #' zwraca k najbardziej emocjonalnych slow w zbiorze
  #' @param word lista slow
  #' @param k do head(k)
  #' @param all jesli TRUE zwroci wszystkie slowa niezaleznie od k
  #' @param sent zwroci tylko te sentymenty - domyslnie wszytkie
  
  require('dplyr')
  # wektoryzwujemy funkcje wyznaczajaca sentyment wzgledem sentymentu
  vek_s <- Vectorize(select_sentiment, vectorize.args = "sent_name", SIMPLIFY = FALSE)
  # lista macierzy kazdego sentymentu
  x <- vek_s(word, sent)
  # laczymy w jedna ramke
  y <- do.call(rbind, x)
  # sortujemy 
  y2 <- y %>% arrange(desc(n))
  # obcinamy do k
  if(!all) {
    y2<- head(y2, k)
  }
  return(y2)
}

sentwords <- all_sentiment(inside$Buddhism, all=TRUE)

sentiment_table_mixed <- function(word, name, sent = c("trust", 
                                                       "fear", 
                                                       "negative", 
                                                       "sadness",
                                                       "anger", 
                                                       "surprise",
                                                       "positive", 
                                                       "disgust",
                                                       "anticipation", 
                                                       "joy")) {
  #' zwraca jednokolumnowa tabelke z procentowa zawartoscia kazdego sentymentu
  #' @param word lista slow
  #' @param name nazwa zbioru(przypisana potem kolumnie wyjsciowej)
  
  require("dplyr")
  zliczone <- all_sentiment(word, all = TRUE, sent = sent)
  tab <- zliczone %>%
    group_by(sent) %>%
    summarise(n = sum(n))%>%
    mutate(procent = 100*n / sum(n))%>%
    select(procent)
  
  colnames(tab) <- c(name)
  return(tab)
}


l <- parallelLapply(booknames, function(x, inside){sentiment_table_mixed(unlist(inside[x]), x)}, inside = inside)

wtf <- do.call("cbind",l)

wtf <- cbind(wtf, sent= c("trust", 
                     "fear", 
                     "negative", 
                     "sadness",
                     "anger", 
                     "surprise",
                     "positive", 
                     "disgust",
                     "anticipation", 
                     "joy"))

library(rpivotTable)

melted <- reshape::melt(wtf)

rpivotTable(data = melted, cols = "sent", rows = "variable", rendererName = "Heatmap", aggregatorName = "Sum", vals = "value")


dlugosci_slow <- parallelLapply(inside, function(x){sapply(x, nchar, simplify = TRUE, USE.NAMES = FALSE)})

names(dlugosci_slow) <- booknames

for(i  in 1:8L){
  names(dlugosci_slow[[i]]) <- booknames[i] 
}
x <- parallelLapply(dlugosci_slow, function(x){cbind(len = x, name = names(x)[1])})

lel <- do.call("rbind", x)
rownames(lel) <- NULL

lel <- as.data.frame(lel)
lel$len <- as.numeric(lel$len)

library(ggplot2)
ggplot(lel, aes(x = name, y = as.factor(len), color = name)) + geom_density()

ggplot(lel, aes(x = len, color = name)) + geom_density()

#pomyslimy sobie the hell, ale to są dlugie slowa:
#zobaczmy czy faktycznie tak jest

w <- colnames(df)

long <- parallelLapply(w, function(x){if(nchar(x)<7){x}else NULL})
long <- unlist(long)
long
