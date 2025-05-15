# ładujemy pakiety
library(tm)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(wordcloud)
library(tm)   

# Używamy funkcji: top_terms_by_topic_LDA, by wczytać tekst nastepnie korzystamy z metody LDA

# Wyznaczamy optymalną liczbe k tematów:
top_terms_by_topic_LDA <- function(input_text, plot = TRUE, k = number_of_topics){    
  
  #Tworzymy corpus i macierz DTM
  corpus <- VCorpus(VectorSource(input_text))
  DTM <- DocumentTermMatrix(corpus)
  
  # pobieramy indeks unikalnej wartości
  unique_indexes <- unique(DTM$i)
  
  # do DTM "wrzucamy" tylko te unikalne wartości
  DTM <- DTM[unique_indexes,]

  # wykonujemy LDA
  lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta") # pobierz słowa/tematy w uporządkowanym formacie tidy

  # Pobieramy 12 najczęstszych słów dla każdego tematu
  top_terms <- topics  %>%
    group_by(topic) %>%
    top_n(12, beta) %>%
    ungroup() %>%
    
    #porządkujemy słowa
    arrange(topic, -beta)



  # Rysujemy wykres
  if(plot == T){
      top_terms %>%
      
      #Sortujemy słowa według bety, a następnie rysujemy wykres
      mutate(term = reorder(term, beta)) %>% ggplot(aes(term, beta, fill = factor(topic))) + geom_col(show.legend = FALSE) + facet_wrap(~ topic, scales = "free") + labs(x = "Terminy", y = "β (ważność słowa w temacie)") + coord_flip() + theme_minimal() + scale_fill_brewer(palette = "Set3")
  }else{ 
    #Zwracamy tylko liste słów
    return(top_terms)
  }
}


#tekst jest w pliku CSV o nazwie: "Continentials_reviews.csv", więc go odczytujemy

data <- read.csv("Continentials_reviews.csv", stringsAsFactors = FALSE, encoding = "UTF-8")

#tworzymy korpus 
corpus <- VCorpus(VectorSource(data$Review_Text))


# Sprawdzamy zawartość losowego elementu - potem do zakomentowania :)
 corpus[[1]]
 corpus[[1]][[1]]
 corpus[[20]][1]



#Rozpoczynamy przetwarzanie tekstu :)

# Ustawiamy kodowanie na UTF-8 na cały korpus
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))

#Zamieniamy znaki na spacje
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))

# Usuwamy symbol @
corpus <- tm_map(corpus, toSpace, "@")

# Usuwamy symbol @ ze słowem (zazw. nazwa użytkownika)
corpus <- tm_map(corpus, toSpace, "@\\w+")

# Usuwamy linie pionowa
corpus <- tm_map(corpus, toSpace, "\\|")

# Usuwamy tabulatory
corpus <- tm_map(corpus, toSpace, "[ \t]{2,}")

# Usuwamy CAŁY adres URL:
corpus <- tm_map(corpus, toSpace, "(s?)(f|ht)tp(s?)://\\S+\\b")

# Usuwamy http i https
corpus <- tm_map(corpus, toSpace, "http\\w*")

# Usuwamy tylko ukośnik odwrotny (np. po http)
corpus <- tm_map(corpus, toSpace, "/")

# Usuwamy pozostałość po re-tweecie
corpus <- tm_map(corpus, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)")

# Usuwamy też inne pozostałości
corpus <- tm_map(corpus, toSpace, "www")
corpus <- tm_map(corpus, toSpace, "~")
corpus <- tm_map(corpus, toSpace, "â€“")

# Sprawdzamy zawartość losowego elementu po czyszczeniu - potem do zakomentowania :)
corpus[[1]]
corpus[[1]][[1]]
corpus[[20]][1]


corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)

# Sprawdzamy zawartość losowego elementu po czyszczeniu - potem do zakomentowania :)
corpus[[1]]
corpus[[1]][[1]]
corpus[[20]][1]


#Tworzymy macierz częstości TDM
tdm <- TermDocumentMatrix(corpus)
tdm_m <- as.matrix(tdm)

#Zliczamy częstości słów w macierzach i wypisujemy 12 z największą częstością :)
v <- sort(rowSums(tdm_m), decreasing = TRUE)
tdm_df <- data.frame(word = names(v), freq = v)
head(tdm_df, 12)

#Tworzymy globalną chmure słów 
wordcloud(words = tdm_df$word, freq = tdm_df$freq, min.freq = 7, colors = brewer.pal(8, "Paired"))

# Wyświetlamy top 12
print(head(tdm_df, 12))


#Teraz dobieramy ilość tematów, dla bardziej rozbudowanego testu proponuje sprawdzić 2, 3, 4, 5, 6, 8, 10 tematów w miare potrzeb należy to rozszerzyć 

number_of_topics = 2
top_terms_by_topic_LDA(tdm_df$word)

number_of_topics = 3
top_terms_by_topic_LDA(tdm_df$word)

number_of_topics = 4
top_terms_by_topic_LDA(tdm_df$word)

number_of_topics = 5
top_terms_by_topic_LDA(tdm_df$word)


number_of_topics = 6
top_terms_by_topic_LDA(tdm_df$word)

number_of_topics = 8
top_terms_by_topic_LDA(tdm_df$word)

#Warto dodać że specjalnie dla możliwości sprawdzenia k=10 zmieniliśmy barwę kolorów z Set1 na Set3, gdyż paleta Set1 ma tylko 9 kolorów
number_of_topics = 10
top_terms_by_topic_LDA(tdm_df$word)


#Aby rozszerzyć nasz projekt poza Modelowaniem tematów postanowiliśmy zrobić tteż Asocjacje

#Przypominamy częstości słów (dla TOP12 słów) i wyświetlamy to w tabeli
head(tdm_df, 12)

# Następnie zaczynamy asocjacje dla 10 przez nas wybranych słów z powyższych TOP12 najczęstszych słów
# z założenia szukamy progu powyżej 0,5 który miałbym wypisywać tylko silną i bardzo silną korelacje
# w celu przypomnienia poniżej legenda co oznaczają dane wartości:
#  0  - 0,3 to słaba korelacja
# 0,3 - 0,5 to umiarkowana korelacja
# 0,5 - 0,7 to silna korelacja
# 0,7 -  1  to bardzo silna korelacja

findAssocs(tdm,"hotel",0.5)
findAssocs(tdm,"good",0.5)
findAssocs(tdm,"staff",0.5)
findAssocs(tdm,"great",0.5)
findAssocs(tdm,"breakfast",0.5)
findAssocs(tdm,"floor",0.5)
# Ze względu na brak co najmniej silnej korelacji zmniejszamy próg do 0,3 sprawdzając czy występuje jakaś umiarkowana
findAssocs(tdm,"location",0.3)
findAssocs(tdm,"nice",0.5)
findAssocs(tdm,"warsaw",0.5)
findAssocs(tdm,"service",0.5)

# Następnie postanowiliśmy zwizualnizować niektóre z powyższych w tzw. wykresie lizakowym z natężeniem
# za próg wzieliśmy r>= 0,5 i założyliśmy że musi mieć badane słowo musi mieć co najmniej 8 silnie lub bardzo silnie skorelowanych słów:
# jednak żeby wykres ten był czytelny ograniczyliśmy go do max 14 słów z najlepszą korelacją


# Obliczamy asocjacje dla słowa: "warsaw"
associations <- findAssocs(tdm, "warsaw" , corlimit = 0.5)
assoc_vector <- associations[["warsaw"]]
assoc_sorted <- sort(assoc_vector, decreasing = TRUE)

# Bierzemy tylko 14 najlepszych śłów
assoc_sorted <- head(assoc_sorted, 14)

# Tworzymy ramka danych
assoc_df <- data.frame(word = factor(names(assoc_sorted), levels = names(assoc_sorted)[order(assoc_sorted)]),score = assoc_sorted)



ggplot(assoc_df, aes(x = score, y = reorder(word, score), color = score)) +
  geom_segment(aes(x = 0, xend = score, y = word, yend = word), size = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = round(score, 2)), hjust = -0.2, size = 3, color = "black") +
  scale_color_gradientn(
    colors = c("#1b9e77", "#80e0a7", "#ffeb3b", "#ff9a3b", "#ff4f58"),  # Odwrócony gradient (zielony, żółty, pomarańczowy, czerwony)
    limits = c(0, max(assoc_df$score) + 0.1)
  ) +
  scale_x_continuous(
    limits = c(0, max(assoc_df$score) + 0.1),
    expand = expansion(mult = c(0, 0.2))
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = paste0("Asocjacje z terminem: 'warsaw'"),
    subtitle = paste0("Próg r ≥ 0.5   ---   TOP 14 słów"),
    x = "Współczynnik korelacji",
    y = "Słowo",
    color = "Natężenie\nskojarzenia"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "right"
  )



# Obliczamy asocjacje dla słowa: "service"
associations <- findAssocs(tdm, "service" , corlimit = 0.5)
assoc_vector <- associations[["service"]]
assoc_sorted <- sort(assoc_vector, decreasing = TRUE)

# Bierzemy tylko 14 najlepszych słów
assoc_sorted <- head(assoc_sorted, 14)

# Tworzymy ramke danych
assoc_df <- data.frame(word = factor(names(assoc_sorted), levels = names(assoc_sorted)[order(assoc_sorted)]), score = assoc_sorted)

# Wykres
ggplot(assoc_df, aes(x = score, y = reorder(word, score), color = score)) +
  geom_segment(aes(x = 0, xend = score, y = word, yend = word), size = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = round(score, 2)), hjust = -0.2, size = 3, color = "black") +
  scale_color_gradientn(
    colors = c("#1b9e77", "#80e0a7", "#ffeb3b", "#ff9a3b", "#ff4f58"),  # Odwrócony gradient (zielony, żółty, pomarańczowy, czerwony)
    limits = c(0, max(assoc_df$score) + 0.1)
  ) +
  scale_x_continuous(
    limits = c(0, max(assoc_df$score) + 0.1),
    expand = expansion(mult = c(0, 0.2))
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = paste0("Asocjacje z terminem: 'service'"),
    subtitle = paste0("Próg r ≥ 0.5   ---   TOP 14 słów"),
    x = "Współczynnik korelacji",
    y = "Słowo",
    color = "Natężenie\nskojarzenia"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "right"
  )






