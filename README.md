# Recommendation-system
## 1. Текстовый и сетевой анализ

### Текстовый анализ

Загружаем данные и библиотеки

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  error = FALSE,
  message = FALSE,
  warning = FALSE
)
```

Необходимые библиотеки:

```{r}
library(dplyr)
library(ggplot2)
library(ggraph)
library(stringr)
library(tidyr)
library(RColorBrewer)
library(lubridate)
library(devtools)
library(rsample)
library(coin)
library(party)
library(rpart.plot)
library(rpart)
library(ggdendro)
library(tidyverse)
library(tidygraph)
library(readr)
library(igraph)
library(igraphdata)
library(openxlsx)
library(stats)
library(visNetwork)
library(lsa)
library(tidyr)
library(wordcloud2)
library(stopwords)
library(tidytext)
library(topicmodels)
library(data.table)
library(text2vec)
library(recommenderlab)
library(textstem)
library(tidyverse)
library(gridExtra)
library(tidytext)
library(tnet)

load("~/shared/minor2_2020/data/good_read/books_g_6.RData")
load("~/shared/minor2_2020/data/good_read/reviews_g_6.RData")

```

Проведем текстовый анализ описания комиксов и названия.

Выделяем столбик с описание **description** и названиями **title**, выделяем каждое слово, сделаем широкий формат. Создадим два отдельных датафрейма для описаний и для названий.

```{r}

#Оставляем нужные столбцы, столбец с издателями, который в дальнейшем понадобится
words_des = goodread_comics %>% dplyr::select(book_id, description, publisher) %>% unnest_tokens(words, description)

words_tit = goodread_comics %>% select(book_id, title) %>% unnest_tokens(words, title)

#length(unique(words_des$words)) #9449


#rates = pivot_wider(ratings, names_from = movieId, values_from = rating)
```

Проведем лемматизацию, уберем числа и знаки пунктуации. Также следует убрать слова, состоящие из 1 буквы (они могли появится после того как мы убрали знаки пунктуации).

```{r}
words_des$words = lemmatize_words(words_des$words)
words_des$words = str_replace_all(words_des$words, "[0-9]", "")
words_des$words = str_replace_all(words_des$words, "[[:punct:]]", "")

words_tit$words = lemmatize_words(words_tit$words)
words_tit$words = str_replace_all(words_tit$words, "[0-9]", "")
words_tit$words = str_replace_all(words_tit$words, "[[:punct:]]", "")

words_des = words_des %>% filter(!words %in% stopwords("en"))
words_tit = words_tit %>% filter(!words %in% stopwords("en"))

words_des = words_des %>% filter(str_length(words) > 1)
words_tit = words_tit %>% filter(str_length(words) > 1)

```

Посмотрим на количество уникальных слов в описаниях и в названиях

В описании количество уникальных слов равно:

```{r}

length(unique(words_des$words))


```

В названиях комиксов уникальных слов:

```{r}
length(unique(words_tit$words))
```


Посмотрим на распределение слов в обоих датасетах

В описании

```{r}
words_des %>%
  dplyr::count(words, sort = TRUE) %>%
  filter(n > 10) %>%
  ggplot(aes(reorder(words, n), n)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_minimal() +
  labs(x = "frequency")

```

В названиях комиксов

```{r}
words_tit %>%
  dplyr::count(words, sort = TRUE) %>%
  filter(n > 5) %>%
  ggplot(aes(reorder(words, n), n)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_minimal() +
  labs(x = "frequency")

```


Распределения получились с длинным хвостом, а значит закон Ципфа выполнен, хотя в случае слов из названий комиксов не сильно заметно, так как количество слов намного меньше. 

Теперь посмотрим на самые частые 15 слов.

```{r}
a = words_des %>%
    dplyr::count(words) %>%
    top_n(15, n) %>%
    ggplot(aes(x = reorder(words, n), y = n)) +
    geom_col(fill = "red") +
    ggtitle("Description Words Frquency") +
    xlab("Word") +
    ylab("Number of Occurences") + 
    coord_flip() +
    theme_minimal()

b = words_tit %>%
    dplyr::count(words) %>%
    top_n(15, n) %>%
    ggplot(aes(x = reorder(words, n), y = n)) +
    geom_col(fill = "blue") +
    ggtitle("Title Words Frequency") +
    xlab("Word") +
    ylab("Number of Occurences") + 
    coord_flip() +
    theme_minimal()

grid.arrange(a, b, ncol = 2)
```


В датасете с названиями самыми популярными словами с большим отрывом оказались слова vol и volume, так как они обозначают принадлежность данного комикса к какой-то серии(vol как ссокращение от volume). По частоте слов вописаниях с отрывом побеждает слово new, а в среди названий комиксов vol и volume. В целом можно видеть некоторые совпадения, хоть их не так много.

Посмотрим то же самое в виде облаков слов.

Описания

```{r}
cloudw = words_des %>%
    dplyr::count(words, sort=TRUE) %>% 
    top_n(50, n)

wordcloud2(data = cloudw, color = "blue", rotateRatio = 0)
```

Названия

```{r}
cloudw = words_tit %>%
    dplyr::count(words, sort=TRUE) %>% 
    top_n(50, n)

wordcloud2(data = cloudw, color = "blue", rotateRatio = 0)
```

Видим существенную разницу. В описаниях слова new, collect самые популярные. А в названиях the, vol. volume. 

Посмотрим какое совпадение слов в описаниях между комиксами разных издателей. В датасете много издателей, к которым относятся мало комиксов, так что возьмем издателей, у которых в датасете есть больше 10 комиксов, таких комиксов 7.

```{r}

library(lsa)

h = goodread_comics %>% filter(!is.na(publisher)) %>% group_by(publisher) %>% dplyr::summarise(b = n()) %>% filter(b > 10) %>% filter(publisher != "")

sim = words_des %>%
  filter(publisher %in% c(h$publisher)) %>% 
  group_by(publisher) %>%
    dplyr::count(words, sort=TRUE) %>%
   cast_sparse(publisher, words, n) %>% 
  as.matrix()

#Построим матрицу схожести
m_sim=lsa::cosine(t(sim))

m_sim %>% round(3)

```

Видим больше всего похожи комиксы Marvel и DC Comics, Marvel и Vertigo, Vertigo и Image Comics. 

Давайте теперь посмотрим насколько похожи комиксы друг на друга по словам в описаниях, если они от одного издателя.

```{r}
k <- c()

for (i in c(1:7)) {
  sim= words_des %>%
    filter(publisher == h$publisher[i]) %>% 
    group_by(book_id) %>%
    dplyr::count(words, sort=TRUE) %>%
    cast_sparse(book_id, words, n) %>% 
    as.matrix()
  m_sim=lsa::cosine(t(sim))
  c1 = m_sim %>% mean() %>% round(3)
  k <- c(k, h$publisher[i])
  k <- c(k, c1)
}

k

```


Видим, что принадлежность к одному издателю предполагает некоторую схожесть описаний.

Выводы:

*При помощи текстого анализа можно выделить некоторые важные компоненты для content-based рекомендательной системы: издатель.
*Ничего больше содержательного для наших систем найти не получилось.

### Сетевой анализ 

Здесь продемонстрирвана логика сетевого анализа, который используется в данном проекте. С помощью него мы определили факторы, которые в дальнейшем были использованы в content-based рекомендательной системе. Начнем с загрузки данных.

```{r}
book_genres <- goodread_comics %>%
  select(book_id, popular_shelves.0.name:popular_shelves.3.name) %>%
  gather(n, genre, popular_shelves.0.name:popular_shelves.3.name) %>%
  mutate(n = 1)

book_genres <- book_genres %>%
  spread(key = genre, value = n, fill = 0)
rownames(book_genres) <- book_genres$book_id

book_genres <- book_genres %>%
  select(-book_id) %>%
  as.matrix()

genre.net <- graph_from_incidence_matrix(book_genres)
#cоздадим двудольный граф
proj.net <- bipartite.projection(genre.net)
genre.proj.net <- proj.net[[1]]
```

Мы хотим создать сеть, в которой нодами служат книги (комиксы), а связями - общий жанр. Для объединения информации из 4 полок в одну была использована функция gather. Мы создали двудольный граф, состоящий из одной проекции. Визуализируем его и посмотрим на результаты:

```{r}
V(genre.proj.net)$label <- NA
plot(genre.proj.net, vertex.size = 2, layout = layout.fruchterman.reingold)

```



Сеть выглядит кучно: в центре образовалось целое скопление комиксов, а вокруг него - разбросанные и менее сгруппированные узлы. Поэтому создадим новую взвешенную проекцию методом Ньюмана, при которой связи между непопулярными жанрами становятся сильнее, чем связи между популярными. Сохраняем id комиксов, так как алгоритмы убирают названия строк.


```{r}
books_id <- rownames(book_genres)
id_df <- data.frame(book = books_id, i = 1:nrow(book_genres), stringsAsFactors = FALSE)
```

Создаем взвешенную проекцию методом Ньюмана и проверяем, как она выглядит.

```{r}

books.genres.weight = projecting_tm(book_genres, method = "Newman")
head(books.genres.weight)

```

Колонки i и j обозначают фильмы, расположенные в порядке исходной матрицы, а w - вес данной связи. Для удобства заменим эти порядковые номера на реальные id комиксов и удалим лишние переменные.

```{r}
books.genres.weight = left_join(books.genres.weight, id_df, by = "i")
books.genres.weight = books.genres.weight %>% dplyr::rename(book_1 = book)
books.genres.weight = left_join(books.genres.weight, id_df, by = c("j" = "i")) %>% dplyr::rename(book_2 = book)
books.genres.weight = dplyr::select(books.genres.weight, book_1, j = book_2, w)
```

Теперь посмотрим на распределение взвешенных коэффициентов.

```{r}
ggplot(books.genres.weight) + 
  geom_histogram(aes(x = w), fill = "#F8766D") +
  geom_vline(aes(xintercept = mean(w)), color = "#00BFC4", linetype = "dashed", size=1) +
  scale_x_log10() +
  xlab("Newman's Coefficient") +
  ylab("Number of Occurences") +
  theme_bw()
```

В данных много наблюдений с небольшим значением коэффициента, но все же посмотрим на новую взвешенную сеть.

```{r}

books.genres.weight1 = filter(books.genres.weight, w > 0.01) %>% select(-w)

set.seed(123)
 
books.net.weigth <- simplify(graph_from_edgelist(as.matrix(books.genres.weight1), directed = FALSE))
books.net.weigth = igraph::simplify(books.net.weigth, remove.multiple = TRUE)
V(books.net.weigth)$color <- "steel blue"
V(books.net.weigth)$label <- NA
 
plot(books.net.weigth, vertex.label.color = "black", vertex.size = 3, layout = layout.kamada.kawai)
```



Выглядит намного нагляднее и сгруппированнее, так что мы можем выделить сообщества данной сети. Для этого определим, какой метод разделения на сообщества наиболее удачен в данном случае. Используем методы Walktrap и Fast Greedy алгоритмы. Показатель модулярности больше у Walktrap - используем его(0.4863737 против 0.4613927).



```{r}
com_w <- walktrap.community(books.net.weigth)
com_f <- fastgreedy.community(books.net.weigth)


# modularity(com_w) #0.4863737
# modularity(com_f) #0.4613927

#победил walktrap

plot(com_w, books.net.weigth, layout = layout.kamada.kawai, 
     vertex.label.color = "black", vertex.size = 3)
```

Можно заметить, что не все комиксы, находящиеся в одном сообществе, связаны между собой, а значит, они не имеют общих жанров. Соответственно, нахождение в одном сообществе может означать жанровую схожесть книг даже если они не имеют одинаковых, т.е. можно сказать, что между жанрами может быть что-то общее.

*Выводы:* В ходе сетевого анализа нам не удалось сделать никаких существенных выводов, которые можно было бы использовать в дальнейшем.

## 2. Коллаборативная фильтрация и рекомендательная система на основе нее.

**Первичная оценка данных**


В первую очередь, следует сделать первичную оценку данных. Для этого посчитаем максимальное,минимальное и среднее число комиксов, которые оценил один пользователь

```{r}
counts = goodread_reviews %>% count(user_id) 

minn = counts %>% top_n(-1, n) %>% arrange(n)
maxx = counts %>% top_n(1, n) %>% arrange(-n)


paste("max" ,maxx$n[1])

paste("min" ,minn$n[1]) 

paste("mean", round(mean(counts$n), 2))

ggplot(counts) + geom_histogram(aes(x = n))


```

Самый активный пользователь - 37b3e60b4e4152c580fd798d405150ff - оценил 116 книг, в то время как довольное большое количество пользователей оценило лишь 1 книгу.Это же можно видеть на гистограмме. В среднем же пользователи оценивали 2 фильма.

Теперь проделаем такую же процедуру для комиксов и найдем максимальное, минимальное и среднее число оценок у одного комикса.

```{r results  = FALSE}
counts1 = goodread_reviews %>%
  count(book_id)

minn = counts1 %>% top_n(-1, n) %>% arrange(n)
maxx = counts1 %>% top_n(1, n) %>% arrange(-n)


paste("max" ,maxx$n[1])

paste("min" ,minn$n[1])

paste("mean" ,mean(counts1$n))
```


Максимальное количество оценок для комикса составило 50, тогда как минимальное - 10. В среднем же, комиксы имели по 24 оценки.

Все эти данные помогут нам при построении функции: для выбора минимального количества комиксов, оцененного у 1го пользователя и минимального количества оценок у 1го комикса, необходимых для использования в рекомендации.


**Рекомендация для нового пользователя**
Для новых пользователей было решено выдавать топ-10 комиксов-новинок. Самая поздняя дата выхода комикса в датасете - 2017-й год, поэтому берем за "новые" книги, те, которые вышли после в 2016-м году и позднее. Рейтинг должен быть больше или равен 4-м.

```{r results = FALSE}
top10 = filter(goodread_comics, 
               publication_year>=2016, average_rating>=4) %>% top_n(10, average_rating)

top10=select(top10, title,average_rating,publication_year)
```


### UBCF vs IBCF

Прежде чем составлять функцию, необходимо выбрать между user- и item-based фильтрациями. Мы принимали решение на основе средних модуля и квадрата отклонения, а также корня из среднего квадрата отклонения.


```{r}
reviews= goodread_reviews %>% select(user_id, book_id, rating) 
rates = pivot_wider(reviews, names_from = book_id, values_from = rating)
userNames = rates$user_id
rates = select(rates, -user_id)
rates = as.matrix(rates)
rownames(rates) = userNames
r = as(rates, "realRatingMatrix")
books <- r[rowCounts(r) > 15, colCounts(r) > 12] 

set.seed(100)
eval_sets_10 <- evaluationScheme(data = books, 
                              method = "split",
                              train = 0.8, # доля обучающей выборки
                              given = 12, # сколько оценок используется для  предсказания
                              goodRating = 4) # если предсказанная оценка < 4, то фильм не рекомендуем

#IBCF
eval_sets_IBCF_10 = eval_sets_10

recc_model_IBCF_10 <-
  Recommender(data = getData(eval_sets_IBCF_10, "train"), method = "IBCF")
recc_predicted_IBCF_10 <-  predict(object = recc_model_IBCF_10,
                           newdata = getData(eval_sets_IBCF_10, "known"),
                           n = 11, type = "ratings")

eval_accuracy_IBCF_10 <- calcPredictionAccuracy(x = recc_predicted_IBCF_10,
                            data = getData(eval_sets_IBCF_10, "unknown"),
                                        byUser = T) # averaging for each user

eval_accuracy_IBCF_10 <- calcPredictionAccuracy(x = recc_predicted_IBCF_10,
                                         # predicted values
                                         data = getData(eval_sets_IBCF_10, "unknown"),
                                         byUser = F) # not averaging for each user

eval_accuracy_IBCF_10

#UBCF
eval_sets_UBCF_10 = eval_sets_10

recc_model_UBCF_10 <-
  Recommender(data = getData(eval_sets_UBCF_10, "train"), method = "UBCF", parameter = list(nn = 20)) #снизили количество схожих пользователей (nearest neighbors — nn), необходимых для рекомендации (по умолчанию — 25). 
recc_predicted_UBCF_10 <-  predict(object = recc_model_UBCF_10,
                           newdata = getData(eval_sets_UBCF_10, "known"),
                           n = 11, type = "ratings")
eval_accuracy_UBCF_10 <- calcPredictionAccuracy(x = recc_predicted_UBCF_10,
                                 data = getData(eval_sets_UBCF_10, "unknown"),
                                        byUser = T) # averaging for each user
eval_accuracy_UBCF_10 <- calcPredictionAccuracy(x = recc_predicted_UBCF_10,
                                         # predicted values
                               data = getData(eval_sets_UBCF_10, "unknown"),
                                         byUser = F) # not averaging for each user
eval_accuracy_UBCF_10
```

По всем трем показателям ошибки, у UBCF были показатели ниже, поэтому далее мы будем работать именно с этим видом фильтрации.

```{r}
#Подготовка к созданию функции
reviews= goodread_reviews %>% select(user_id, book_id, rating)
rates = pivot_wider(reviews, names_from = book_id, values_from = rating)
userNames = rates$user_id
rates = select(rates, -user_id)
rates = as.matrix(rates)
rownames(rates) = userNames
r = as(rates, "realRatingMatrix")
ratings_comics <- r[rowCounts(r) > 15, colCounts(r) > 12]
#Для использования в рекомендации, пользователь должен оценить более 15 фильмов, а фильм должен иметь более 12 оценок
    
set.seed(100)
test_ind <- sample(1:nrow(ratings_comics), size= nrow(ratings_comics)*0.2)
recc_data_train <- ratings_comics[-test_ind, ]
recc_data_test <- ratings_comics[test_ind, ]
recc_model_UBCF <- Recommender(data = recc_data_train, method =
                                   "UBCF",parameter = list(nn = 14))
noname=goodread_reviews%>% select(user_id) %>% group_by(user_id) %>% count(count=n()) %>% filter (count<2)
```


**Функция**

Создаём функцию по методу UBCF рекомендации. В функции учтено, что если пользователя нет в системе- рекомендуем ему список из ТОП-10 комиксов-новинок.

```{r}
kollab_func = function(user_id_vvod){ 
  osobenniye= reviews %>% group_by(user_id) %>% count() %>% filter(n>=15)
  user_osob= osobenniye %>% filter(user_id == user_id_vvod)

  if (user_id_vvod %in% noname$user_id | nrow(user_osob)==0 ) {
    recommend = top10$title
  } else {
    recc_predicted_UBCF <- predict(object = recc_model_UBCF, newdata =
                                   ratings_comics[user_id_vvod,], n = 10)
    recc_user <- recc_predicted_UBCF@items[[user_id_vvod]]
    comics_user <- recc_predicted_UBCF@itemLabels[recc_user]
    recommend <- goodread_comics$title[match(comics_user,
                                                     goodread_comics$book_id)]
  } 
 return(recommend)
}
```


### Оценивание рекомендации:

Сначала проведена *первичная оценка* работы функции. Проверяем функцию для трех пользователей: пользователя нет в системе, пользователи с максимальным и минимальным числом оценок.

### Примеры

Пользователя нет в системе

```{r}
kollab_func("ggg111") #пользователя нет в системе
```

Пользователь с максимальным числом оценок

```{r}
kollab_func("37b3e60b4e4152c580fd798d405150ff") #максимальное число оценок
```

Пользователь с минимальным числом оценок.

```{r}
kollab_func("008ffafc7ea81f88131f5a254a8cef89") #минимальное число оценок
```


Для пользователя с неизвестным id, т.е. новому пользователю, система выдала топ-10 книг-новинок, так же как и пользователю с минимальным количеством оценок. В то же время, пользователю с максимальных количеством оценок был составлен его собственный топ.

### Внутренняя пользовательская оценка

Далее была проведена *внутренняя пользовательская оценка*
Добавляем трех новых пользователей в систему. Для каждого пользователя создаем отдельную базу с оценками. Т.е. к первоначальной базе добавляем нового человека и записываем в новый датасет и так 3 раза.

```{r}
#1e5028f077c905873f90706a600499fc
user_id0= c("000probniy000")
book_id0= c("21325", "2280874", "15984353", "10088114", "18345615","28963358", "25591187", "28665325", "29507104", "29507109","31222856","33233923", "17671913", "24426209", "27279102","29066588")
rating0= c("5", "3", "1", "3","4", "5", "4", "1", "2","5", "3", "1", "5", "5","1", "3")
data0 <- data.frame(user_id=user_id0, book_id=book_id0, rating=rating0)
data0$book_id= as.double(data0$book_id)
data0$rating= as.double(data0$rating)
reviews1= bind_rows(reviews, data0)

#37b3e60b4e4152c580fd798d405150ff
user_id1= c("111probniy111")
book_id1= c("21325", "59962", "43718", "72112", "79368", "103546", "105874", "209962", "217330", "838915", "1214485", "2140338", "22339", "2280874", "11036353", "17137601", "17791463	", "31387202")
rating1= c("5", "5", "4", "5", "4", "4", "5", "4", "5", "3", "1", "2", "4", "2", "5", "5", "2", "1")
data1 <- data.frame(user_id=user_id1, book_id=book_id1, rating=rating1)
data1$book_id= as.double(data1$book_id)
data1$rating= as.double(data1$rating)
reviews2= bind_rows(reviews, data1)

#70da424cfb5d9e0e9839651405092f56
user_id2= c("222probniy222")
book_id2=c("106980", "1172306", "1214485", "3409425", "6672187", "6761617", "6832685", "8414892", "13021244", "9593394", "9919139", "17251104", "17860796", "22105703", "23017976", "25066824", "25319042", "26052237", "29507104", "21325", "22373", "72112", "79368", "217330")
rating2= c("5", "4", "5", "4", "4", "5", "4", "3", "1", "2", "2", "3", "5", "1", "3", "5", "3", "3", "4", "1", "3", "3", "1", "5")
data2 <- data.frame(user_id=user_id2, book_id=book_id2, rating=rating2)
data2$book_id= as.double(data2$book_id)
data2$rating= as.double(data2$rating)
reviews3= bind_rows(reviews, data2)

```

Создаём новую функцию, которая будет выдавать рекомендацию в зависимости от одной из трех новых баз с оценками.

```{r}
do_kollab <- function(reviews0){
  rates0 = pivot_wider(reviews0, names_from = book_id, values_from = rating)
  userNames0 = rates0$user_id
  rates0 = select(rates0, -user_id)
  rates0 = as.matrix(rates0)
  rownames(rates0) = userNames0
  r0 = as(rates0, "realRatingMatrix")
  ratings_comics0 <- r0[rowCounts(r0) > 15, colCounts(r0) > 12]
return(ratings_comics0) 
}

do_kollab2<- function(reviews0){
  set.seed(100)
  tt= do_kollab(reviews0)
  test_ind0 <- sample(1:nrow(tt), size= nrow(tt)*0.2)
  recc_data_train0 <- tt[-test_ind0, ]
  recc_data_test0 <- tt[test_ind0, ]
  recc_model_UBCF0 <- Recommender(data = recc_data_train0, method =
                                   "UBCF",parameter = list(nn = 14)) 
return(recc_model_UBCF0)
}
    
noname0=goodread_reviews%>% select(user_id) %>% group_by(user_id) %>% count(count=n()) %>% filter (count<2)

kollab_func_test = function(user_id_vvod, reviews0){ 
  osobenniye= reviews0 %>% group_by(user_id) %>% count() %>% filter(n>=15)
  user_osob= osobenniye %>% filter(user_id == user_id_vvod)

  if (user_id_vvod %in% noname0$user_id | nrow(user_osob)==0 ) {
    recommend = top10$title
  } else { 
    kk=do_kollab(reviews0)
    dd=do_kollab2(reviews0)
    recc_predicted_UBCF <- predict(object = dd, newdata =
                                   kk[user_id_vvod,], n = 10)

    recc_user <- recc_predicted_UBCF@items[[user_id_vvod]]
    comics_user <- recc_predicted_UBCF@itemLabels[recc_user]
    recommend <- c(goodread_comics$title[match(comics_user,goodread_comics$book_id)], comics_user)
  } 
 return(recommend)
}
```


Ожидается, что рекомендации для искуственно созданного пользователя будут похожи на рекомендации для того пользователя, на основе которого был создан новый. 

Проверим первого пользователя. Мы думаем, что его рекомендации будут похожи на рекомендации для пользователя 1e5028f077c905873f90706a600499fc или же те комиксы, которые  оценены у данного пользователя. 

Как видим, в рекомендации из 10 комиксов 2 совпали.

Предположение о том, что рекомендоваться новому пользователю могут комиксы, который оценил старый пользователь не верно.

```{r}
c1= data.frame(rec= kollab_func_test("1e5028f077c905873f90706a600499fc", reviews1))
c2= data.frame(rec= kollab_func_test("000probniy000", reviews1))
data_new= data.frame(old= c1, new=c2)
recomm= inner_join(c1, c2)
fil= reviews1 %>% filter(user_id == "1e5028f077c905873f90706a600499fc")
vybor= c2$rec[11:20]
filvyb= filter(fil, book_id %in% vybor)
recomm$rec[1:2] #совпадение фильмов в рекомендации
#filvyb #совпадение фильмов, рекомендованных новому пользователю с фильмами, которые оценил старый пользователь
```


Проверим схожесть рекомендаций нового пользователя 111probniy111 и 37b3e60b4e4152c580fd798d405150ff.
Как видим, в рекомендации из 10 книг 4 совпали.<br> 
Новому пользователю рекомендовалось 4 книги, которые оценил старый пользователь.<br> 

```{r}
c1= data.frame(rec= kollab_func_test("37b3e60b4e4152c580fd798d405150ff", reviews2))
c2= data.frame(rec= kollab_func_test("111probniy111", reviews2))
data_new= data.frame(old= c1, new=c2)
recomm= inner_join(c1, c2)
fil= reviews2 %>% filter(user_id == "37b3e60b4e4152c580fd798d405150ff")
vybor= c2$rec[11:20]
filvyb= filter(fil, book_id %in% vybor)
recomm$rec[1:4] #совпадение фильмов в рекомендации
#filvyb #совпадение фильмов, рекомендованных новому пользователю с фильмами, которые оценил старый пользователь
```

Проверим схожесть рекомендаций нового пользователя 222probniy222 и 70da424cfb5d9e0e9839651405092f56.

Как видим, в рекомендации из 10 комиксов 6 совпали.

Новому пользователю рекомендовался 1 комикс, который оценил старый пользователь.

```{r}
c1= data.frame(rec= kollab_func_test("70da424cfb5d9e0e9839651405092f56", reviews2))
c2= data.frame(rec= kollab_func_test("222probniy222", reviews3))
data_new= data.frame(old= c1, new=c2)
recomm= inner_join(c1, c2)
fil= reviews3 %>% filter(user_id == "70da424cfb5d9e0e9839651405092f56")
vybor= c2$rec[11:20]
filvyb= filter(fil, book_id %in% vybor)
recomm$rec[1:6] #совпадение фильмов в рекомендации
#filvyb #совпадение фильмов, рекомендованных новому пользователю с фильмами, которые оценил старый пользователь
```

## 3. Content-based рекомендательная система.

В данном чанке описывается преобразование данных из начального формата в подходящий для создания матрицы схожести. Также, в самом начале, мы выбираем переменные, которые будут использоваться для определения сходства. 

Сначала изменим формат языковой переменной с факторного на логический "есть или нет на английском языке". Это было сделано вместо приведения к широкому формату, так как подавляющее большинство комиксов есть на английском языке, однако если объединить все другие языке, их тоже будет достаточно.

```{r}
data1 = goodread_comics %>% dplyr::select(book_id, average_rating, language_code,  popular_shelves.1.name, popular_shelves.2.name, popular_shelves.3.name, ratings_count)

data1 = data1 %>% mutate(language_code = str_replace(language_code, "en-US","eng")) %>% mutate(language_code = str_replace(language_code, "en-GB","eng")) %>% mutate(language_code = str_replace(language_code, "en-CA","eng")) %>% mutate(is_eng = case_when((language_code == "eng") ~ TRUE,(language_code == "")~TRUE, T ~ FALSE)) %>% select(-language_code) %>% mutate(shelves_blin = 1, shelves_blin2 = 1, shelves_blin3 = 1)
```


Теперь переводем в широкий формат переменных полка1, полка2, полка3 (полка0 исключена из характеристик сходства, так как она обозначала саму принадлежность к жанру комикс, а разные названия - комикс, графический рисунок, обозначали там одно и то же)(имеются в виду переменные popular_shelves.0.name,popular_shelves.1.name, popular_shelves.2.name, popular_shelves.3.name).

Приходилось во избежание дублирования разделять на несколько датасетов и в конце склеивать обратно, но в итоге мы получили почти готовый датасет. Оставалось убрать лишние переменные, получившиеся при переходе к широкму формату жанровых переменных - их было слишком много, но половины из них практически всех строках были нули, что обозначало несоответсвтие практически никакому комиксу, поэтому мы отфильтровали и эти колонки. 

```{r}
data2 = data1 %>% pivot_wider(names_from = popular_shelves.1.name, values_from = shelves_blin, values_fill = 0) %>% select(-popular_shelves.2.name, -popular_shelves.3.name, -shelves_blin2, -shelves_blin3,-is_eng, -average_rating, -ratings_count)

data3 = data1 %>% pivot_wider(names_from = popular_shelves.2.name, values_from = shelves_blin2, values_fill = 0) %>% select(-popular_shelves.1.name, -popular_shelves.3.name, -shelves_blin, -shelves_blin3, -is_eng, -average_rating, -ratings_count)

data4 = data1 %>% pivot_wider(names_from = popular_shelves.3.name, values_from = shelves_blin3, values_fill = 0) %>% select(-popular_shelves.1.name, -popular_shelves.2.name, -shelves_blin, -shelves_blin2, -is_eng, -average_rating, -ratings_count)

data1 = data1 %>% select(book_id, average_rating, ratings_count, is_eng)
data1_2 = inner_join(data1, data2, by = "book_id")
data123 = inner_join(data1_2,data3, by = "book_id")
all_data = inner_join(data123,data4, by = "book_id")

checkie = all_data %>% select(-book_id, -average_rating, -ratings_count, -is_eng)
help_me = as.data.frame(colSums(checkie[-1], na.rm = TRUE))
help_me = help_me %>% rename(column1 = "colSums(checkie[-1], na.rm = TRUE)") %>% filter( column1 >5)

#Мы решили оставить жанры, у которых есть хотя бы 5 комкисов.
#Мы не смогли придумать более короткий код, для выделения нужных столбцов, поэтому получился такой длинный код

finalochka = all_data %>% select(book_id,average_rating, ratings_count, is_eng, "comics.x" ,           "currently-reading.x", "graphic-novels.x"   , "to-read.x" ,         
 "manga.x"          ,   "graphic-novel.x" ,    "comics.y"    ,        "graphic-novels.y"   ,
 "currently-reading.y", "horror.y"     ,       "romance.x"     ,      "marvel.x"   ,        
 "fantasy.y"     ,      "mangá.y"      ,       "graphic-novel.y"   ,  "graphic-novels"  ,   
 "dc"        ,          "graphic-novel"   ,    "dc-comics.y"   ,      "fantasy"     ,       
 "comics"     ,         "marvel.y"     ,       "horror"    ,          "currently-reading"  ,
 "favorites"     ,      "cómics"        ,      "batman.y"    ,        "romance.y"   ,       
 "science-fiction.y" ,  "non-fiction.y"      )
```


До создания самой матрицы схожести мы решили создать новую переменную popularity как частное между количесвом отзывов и максимальным значением в этом столбце. Также мы добавили новую переменную как средний сентимент слов в описаниях комиксов. Он позволяет лучше понят эмоциональный настрой самого комикса(столбец value). Еще уберем book_id, чтобы он не влиял на рекомендации, переведем его в названия строк.

```{r}
#Создаем столбце value
sentim = get_sentiments("afinn")
sentim$value = as.numeric(sentim$value)

words_sent = words_des %>%
inner_join(sentim, by = c("words" = "word"))
words_sent = words_sent %>% group_by(book_id) %>% summarise(value)
meansent = aggregate(words_sent, list(words_sent$book_id), mean)
meansent = meansent %>% select(-Group.1)

goodread_comics = left_join(goodread_comics, meansent)
goodread_comics$value = goodread_comics$value %>% replace_na(0)

goodread_comics$value=round(as.numeric(goodread_comics$value),3)

finalochka$value = goodread_comics$value

#Создаем столбец popularity
finalochka = finalochka %>% mutate(popularity = as.numeric(ratings_count)/max(as.numeric(ratings_count))) %>% select(-ratings_count)

rownames = finalochka$book_id
finalochka = finalochka %>% dplyr::select(-book_id)
rownames(finalochka) = rownames

finalochka$average_rating = as.numeric(finalochka$average_rating)
```

Теперь мы можем наконец построить саму матрицу схожести.

```{r}
sim1 = lsa::cosine(t(as.matrix(finalochka)))
diag(sim1) = 0
colnames(sim1)= rownames
rownames(sim1)=rownames
```


Создаем функцию.

В случае когда пользователя нет в нашей базе или у него нет ни одного отзыва с оценкой 5, то спрашиваем его какие издатели комиксов ему нравятся(2 или 3) и просим его оценить его настроение. Фильтруем нашу базу по выданным издательствам, берем комиксы средний рейтинг которых больше 4 и берем комиксы с ближайшим сентиментом.

```{r}
getComics_CB = function(x) {
 user1 = goodread_reviews %>% filter(user_id == as.character(x))

  if (max(user1$rating) < 4) {
    publ <- readline(prompt = "Enter 2 or 3 your favorite publisher separated by commas with spaces: ")
    sent = readline(prompt = "Can you evaluate your mood from -5 to 5? ")
    sent = str_remove_all(sent, " ")
    sent = str_replace_all(sent, ",", ".")
    g = as.list(strsplit(publ, ", "))
  
    if (length(g) == 3) {
      p1 = g[[1]][1]
      p2 = g[[1]][2]
      p3 = g[[1]][3] 
      if (p1 %in% unique(goodread_comics$publisher) | p2 %in% unique(goodread_comics$publisher) | p3 %in% unique(goodread_comics$publisher)) { 
        comics =  goodread_comics %>% filter(publisher == p1 | publisher == p2 | publisher == p3) %>% filter(average_rating >= 4) 
        comics$value1 = abs(comics$value - as.double(sent))
        comics = comics %>% arrange(value1)
        print("Due to chosen publishers we would recommend you next comics:")
        recommend = comics$title[1:10]
      } 
    else {
      comics =  goodread_comics %>% filter(average_rating >= 4)
      comics$value1 = abs(comics$value - as.double(sent))
      comics = comics %>% arrange(value1)
      print("There are no such publishers in our database, so we would recommend you next comics due to your mood:")
      recommend = comics$title[1:10]
      }
    } else {
      p1 = g[[1]][1]
      p2 = g[[1]][2]
      if (p1 %in% unique(goodread_comics$publisher) | p2 %in% unique(goodread_comics$publisher)) { 
        comics =  goodread_comics %>% filter(publisher == p1 | publisher == p2) %>% filter(average_rating >= 4)
        comics$value1 = abs(comics$value - as.double(sent))
        comics = comics %>% arrange(value1)
        print("Due to chosen publishers we would recommend you next comics:")
        recommend = comics$title[1:10]
      } 
    else {
      comics =  goodread_comics %>% filter(average_rating >= 4)
      comics$value1 = abs(comics$value - as.double(sent))
      comics = comics %>% arrange(value1)
      print("There are no such publishers in our database, so we would recommend you next comics due to your mood:")
      recommend = comics$title[1:10]
    }
    }
  }

  else {                              
 user1 = user1 %>% filter(rating == max(user1$rating))
    mostSimilar1 = head(sort(sim1[,as.character(user1$book_id)], decreasing = T), n = 10)
   a1 = which(sim1[,as.character(user1$book_id)] %in% mostSimilar1, arr.ind = TRUE, useNames = T)
    index1 = arrayInd(a1, .dim = dim(sim1))
    result1 = rownames(sim1)[index1[,1]]
    print("We have analyzed all your reviews and we would recommend you next comics:")
    recommend = filter(goodread_comics, book_id %in% result1) %>% dplyr::select(title)
  }
 return(recommend)
}

```

### Оценка рекомендательной системы content-based*

### Примеры

Пример с пользователем, у которого есть оценки больше 4

```{r}
getComics_CB("e57005c4f076dffb626e9073e2f943e1")
```

Пример с пользователем, у которого нет оценок больше 4

```{r}
getComics_CB("dffb99ec44f82f3e2026cdfec79536c2")
```


### Внешняя пользовательская оценка

 В целях выяснить, насколько эффективны созданные нами системы, мы решили **испытать их на практике и привлекли более десяти человек из круга наших друзей и знакомых**. Мы предложили им оценить, **насколько хорошо подобранные программой фильмы отвечают их предпочтениям**, и попросили также **написать развернутые комментарии**, давшие нам более глубокое понимание как о недостатках наших систем, так и о том, где они показывают себя успешно.

В целом, пользователи отмечают **превосходство системы, основанной на методе коллаборативной фильтрации, над системой, построенной на контенте**, а самым частым упреком в сторону последней является **неясная логика, по которой она рекомендует последние n комиксов** (n в каждом из случаев различно).
  
"Трудно понять, как именно работает система, когда получает входные данные; я хочу сказать, мне тяжело увидеть некий порядок и последовательность в ее работе", - утверждает респондент.
  Другой, подтверждая, говорит: "В целом content-based система удовлетворяет запросу в 2-3 случаях из десяти: чаще всего это связано с тем, что комиксы не соответствуют настроению изначальной картины". 

Под "настроением", следует отметить, подразумевается атмосфера того или иного комикса, и нередко случалось, что **система**, согласно словам пользователей, **выводила чрезмерно мрачные комиксы в противовес легким и веселым, и наоборот**. Несмотря на низкие оценки, пользователи уверены, что дело не в несовершенстве самой системы ("Ведь несколько первых комиксов она предлагает довольно точно"), но в *ограниченности первоначального датасета, считая, что системе просто недостает простора.*

**Другая проблема content-based системы**, как утверждает доля респондентов, состоит в неочевидности критериев, по которым подбираются ленты: часто пользователи были убеждены, что **система**, скорее, **пытается угадать необходимый output**, нежели имеет четкое основание для рекомендации картины. Иными словами, респонденты затруднялись уловить паттерн, которому следует система: к примеру, закономерно, когда программа предлагает работы того же режиссера или фильмы того же периода или жанра. Один из оценивающих говорит: "Использовала систему порядка трех раз и на выходе получала разные результаты; затрудняюсь дать четкую оценку и понять, на что опирается система, когда выдает очередную подборку фильмов".

## Выводы

Была проведена большая работа, в ходе которой мы провели текстовый и сетевой анализы, результаты которых не были включены в рекомендательные системы в дальнейшем. Также были созданы рекомендательные системы двумя различными способами:
   * методом коллаборативной фильтрации;
   * с учетом признаков (content-based). 
Обе рекомендательные системы были оформлены в виде функций и оценены (для оценки коллаборативной р. с. была использована внутренняя пользовательская оценка, а для content-based - внешняя). 

При работе с коллаборативной фильтрацией мы использовали UBCF фильтрацию, основываясь на показателях среднего модуля и квадрата отклонения, а также корне из среднего квадрата отклонения. Для новых же пользователей было решено выдавать топ-10 комиксов-новинок. 

При работе с content-based рекомендательной системой пользователь, если он есть в системе и у него есть оценки выше 4, то выводятся 10 наиболее близких по выбранным критериям комиксов к тем, которые он высоко оценил. В случае, если пользователь новый, то система просит его ввести 2-3 его любимых издательства и оценит свое настроение от -5 до 5, потом оно комиксы, чей средний рейтинг выше 4 фильтрует по введенным названиям издательств и рекомендует самые близкие по *sentiment* комиксы.

При оценке данных рекомендательных систем можем сделать вывод, что в нашем случае коллаборативная фильтрация дала более точные результаты, так как логика рекомендации последних комиксов в content-based "не ясна".


## Примеры из отзывов


### Для collaborative filtering

1) В пуле вопросов мы нашли лишь один, который можно протестировать на нашей модели: **Что будет рекомендовано пользователю, который любит комиксы с очень низким рейтингом?**

Чтобы ответить на него, мы добавили пользователя, который оценил 22 комикса, у которых процент хороших оценок (4 и 5) меньше чем плохих (0-3), на 4 и 5 и попробовали дать ему рекоменлацию. Система выдала ему топ-10 новинок, что довольно предсказуемо, так как людей положительно оценивших эти фильмы досттаточно мало.


```{r}
#Добавляем нового пользователя
user_id3= c("proba3")
book_id3= c("154576", "266599", "476911", "538791", "548418","598827", "707649", "710187", "724312", "754100","838915","992597", "1442300", "1803934", "2280874","2387801","2546282","2592108","2935566","2660314","3003042","3202876")
rating3= c("5", "4", "4", "4","4", "5", "4", "5", "4","5", "4", "4", "5", "5","4", "3","5","5","5","5","5","5")
data3 <- data.frame(user_id=user_id3, book_id=book_id3, rating=rating3)
data3$book_id= as.double(data3$book_id)
data3$rating= as.double(data3$rating)
reviews4 = bind_rows(reviews, data3)

kollab_func_test("proba3", reviews4)[1:10]
```


2) **Что порекомендует система пользователю, у которого нет оценок?**

Как и ожидалось, для пользователя, не имеющего оценок в системе выдается топ-10 новинок

```{r}
kollab_func("random")
```

### Для content-based

1) Рассмотрим пример пользователя, который любит больше всего английские комиксы и предпочитает жанр графические новеллы. 

Возьмем в качестве примера пользователя 71952337fe8e1594f431a674dcb73e7e (пусть будет Максим). Посмотрим на id книг которые, он оценил.

```{r}
g = goodread_reviews %>% filter(user_id =="71952337fe8e1594f431a674dcb73e7e")
g$book_id
```

Теперь посмотрим на их характеристики.

```{r}
g = goodread_comics %>% filter(book_id == 9526 | book_id == 52841 | book_id == 744615 | book_id == 3409425 | book_id == 6662883 | book_id == 13602241 | book_id == 29436571)
 
g$title

```
 
Как видим 6 из 7 комиксов на английском, также 6 из 7 комиксов принадлежат к жанку графических новел(popular_shelves.1.name), по другой полке 4 из них просто комиксы, 2 графические новэллы и одна относится к юмору. Издатели абсолютно разные, только две комиксы выпустили одни и те же издатели. По количеству оценок они разнятся на 2 группы, больше 20 тысяч и меньше 13 тысяч. Средний рейтинг комиксов около 4.

Посмотрим что порекомендует нашему пользователю модель

```{r}
getComics_CB("71952337fe8e1594f431a674dcb73e7e")
```

Проанализируем их характеристики

```{r}
g = goodread_comics %>% filter(title == "Two Brothers" | title == "Local" | title == "East of West, Vol. 2: We Are All One"| title == "The Underwater Welder" | title == "Saga #15"| title == "Jem and the Holograms, Volume 3: Dark Jem"| title == "Moon Girl and Devil Dinosaur, Vol. 1: BFF" | title == "The Walking Dead, Issue #3" | title == "Buffy the Vampire Slayer: Welcome to the Team (Season 9, #4)") 

g$title
```
 
Средний рейтинг комиксов также около 4, 8 из 9 комиксов на английском языке, 7 из 9 комиксов принадлежат к жанру графических новелл по 1 полке, по второй 8 из 9 являются просто комиксами, издатели разбросаны, Количество оценок меньше 7000.

*Ответ* В целом можно сказать, что наша система учла предпочтения этого пользователя и предложила комиксы, схожие с его вкусами. Такому пользователю наша система предложила комиксы, которые преимущественно на английском и преимущественно относятся к жанру графических новелл.

2) **Вопрос:** Что будет рекомендовано пользователю, который любит комиксы с очень низким рейтингом?

Найдем такого пользователя. Возьмем пользователя 405906491ef3c7326db7b444a7fcaa4d(пусть это будет Ваня)

```{r}
goodread_reviews %>% filter(user_id == "405906491ef3c7326db7b444a7fcaa4d")
```

Что ему порекомендует наша модель ? **Напишите в консоле название какого-то издательства, иначе модель ничего не даст**

```{r}
getComics_CB("405906491ef3c7326db7b444a7fcaa4d")

```

*Ответ:* Модель воспринимает такого пользователя как нового (если нет ни одной оценки больше 3) и предложить ему выбрать любимое издательство комиксов и выдаст 10 самых лучших по рейтингу комиксов этого издательства.


## Ответы на вопросы peer review

Здесь представлены ответы на вопросы, которые были заданы студентами курса.

### Сетевой и текстовый анализ:


**Вопрос:**
Как интерпретировать кластеры в сетевом анализе? 

Непонятно, как пользоваться сетью. Из видео мы узнали, что если комикс есть в выделенной группе, то его можно рекомендовать как схожий, но пока сама сеть выглядит очень громоздко, на глаз из неё ничего не понятно.


*Ответ:* Кластеры в данном случае показывают разбиение книг по похожести жанров. На глаз с сетью действительно сложно разобраться. Поэтому для того, чтобы возможно было использовать данную сеть, мы создали новую колонку, в которой каждой книге приписано название кластера, в котором она находилась. Это существенно облегчит работу с сетью.

### Коллаборативная фильтрация:


**Вопрос:** 
Много ли пользователей с 15+ оценками? И как было получено это число как ограничение?


*Ответ:* 54 пользователя с 15+ оценками  (counts %>% count(n>15))
Выбранный given = 12, а значение нашего ограничения немного должно быть больше given

**Вопрос:**
Для пользователей, оценивших менее 15 фильмов тоже должны учитываться те комиксы, которые они оценили ранее, потому что если человек дал например 10 оценок, и ему нравятся старые комиксы, то нелогично рекомендовать ему новые.


*Ответ:* Исходя из информации в лекциях, 10 все еще считается маленьким количеством оценок, так как данные будут смещены. Более того, так как у нас given = 12, а число оценок у пользователей должно быть чуть больше этого значения, нам кажется, что 15 оценок-это оптимальное значение.


**Вопрос:** 
На данном этапе у ребят не готова content-based система, поэтому вопрос по UBCf: почему вы решили рекомендовать новым пользователям только новинки, что если человек хочет начать свое знакомство с миром комиксов с культовых произведений, может ли он это как-то указать? 


*Ответ:* “Культовые” довольно абстрактное понятие.Во-первых, можно было бы добавить как фильтр - popular_shelves.0.name == “to read” (если считать, что “to read” эквивалентно “бестселлеру”, однако, почти комиксы в этой колонке помечены таким образом.  В качестве альтернативы, можно было бы убрать ограничение по году выпуска и оставить только комиксы с высшими оценками (>=4). Тем не менее, нам кажется, что рекомендовать новинки лучше, потому что таким образом новые работы получат оценки, и могут быть использованы позднее в нашей системе рекомендаций, тогда как культовые произведения уже имеют аудиторию и достаточное количество оценок.


### Content-based:


**Вопрос:** 
Насколько я помню, бОльшая часть комиксов (90%) написаны на английском языке,
есть ли смысл брать эту переменную для выдачи рекомендации?


*Ответ:* В данном случае имеет смысл брать данную переменную для выдачи рекомендаций, так как японский язык все же где то проскакивает. И если пользователь лайкнул его, то рекомендация может выдать комикс на том же языке, что будет более релевантно для этого человека. Поэтому данная переменная нужна.


**Вопрос:** 
Насколько релевантно делать рекомендацию пользователю, у которого нет отличных оценок, а есть 4 и ниже или 3 и ниже (если этим пользователям не идеально понравились комиксы, то как тогда понять, что им рекомендовать, т.е. что они бы оценили на отлично)?


*Ответ:* Да, действительно, в случае оценок 3 и ниже действительно не надо их брать, но среди всех пользователей очень мало таких, которые поставили хотя бы одну оценку “5”. И не хочется исключать практически всех, что будет не очень релевантно.


**Вопрос:** 
Какие способы ребята будут использовать для оценки качества модели content-based?


*Ответ:* В данном случае в качестве метода оценивания будет использована внешняя пользовательская оценка и оценка на адекватность.

### Вопрос из отзыва на "черновик"

**Вопрос:**
Что будет, если в content-based системе ввести название несуществующего издательства?

*Ответ:* Мы доработали функцию, теперь вместо одного издателя надо ввести минимум 2 любимых издательства. Также надо оценить собственное настроение от -5 до 5. Если одного из них не существует, то функция возьмем комиксы другого издательства. Если оба издательства не существуют, то функция среди комиксов, чьи средние рейтинги больше 4 отберет 10 комиксов, которые наиболее близки по *sentiment* к настроению пользователя.


