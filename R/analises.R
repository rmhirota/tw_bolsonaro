library(dplyr)
library(lubridate)
library(lexiconPT)
library(tidytext)
library(ggplot2)
library(stringr)
library(scales)



# Leitura dos dados
tweets <- data.table::fread("data/bolsonaro300k_search.csv", header = T)
tweets <- janitor::clean_names(tweets) %>% select(-c(v1, latitude, longitude, new))
glimpse(tweets)

# Formata data
tweets <- tweets %>% mutate(created = as_datetime(created))


# Filtro de horário: a partir das 21h (GMT)
# tweets <- tweets %>% filter(created >= "2019-10-29 21:00:00")
# Filtro: apenas tweets com "bolsonaro" no texto
tweets <- tweets %>% mutate(text = str_to_lower(text)) %>% filter(str_detect(text, "bolsonaro"))
tweets$id <- as.character(tweets$id)

# Análise de sentimento
data("oplexicon_v3.0")
data("sentiLex_lem_PT02")
op30 <- oplexicon_v3.0 %>% group_by(term) %>% summarise(polarity = mean(polarity))
sent <- sentiLex_lem_PT02 %>% group_by(term) %>% summarise(polarity = mean(polarity))

# O léxico não contempla acentos, portanto, vamos tirar caracteres especiais
tweets <- tweets %>%
  mutate(text = iconv(text, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         text = str_to_lower(str_replace_all(text, "[//^~']+", "")))

tweets_unnested <- tweets %>% unnest_tokens(term, text)

tweets_unnested <- tweets_unnested %>%
  inner_join(op30, by = "term") %>%
  inner_join(sent %>% select(term, lex_polarity = polarity), by = "term") %>%
  group_by(id) %>%
  summarise(
    tw_sentiment_op = sum(polarity),
    tw_sentiment_lex = sum(lex_polarity),
    n_words = n()
  ) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    most_neg = min(tw_sentiment_lex, tw_sentiment_op),
    most_pos = max(tw_sentiment_lex, tw_sentiment_op)
  )

nrow(tweets_unnested) / nrow(tweets)  # 43,5% dos tweets são qualificáveis

tweets_unnested %>%
  ggplot(aes(x = tw_sentiment_op, y = tw_sentiment_lex)) +
  geom_point(aes(color = n_words)) +
  scale_color_continuous(low = "green", high = "red") +
  labs(x = "Polaridade no OpLexicon", y = "Polaridade no SentiLex") +
  #geom_smooth(method = "lm") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed")


# Tweet mais positivo
most_pos <- which.max(tweets_unnested$most_pos)
cat(tweets$text[tweets$id == tweets_unnested$id[most_pos]])
tweets %>% filter(id == tweets_unnested$id[most_pos]) %>% View()

# tirando esse usuário que tem "bolsominion" no nome:
# filtro_ids <- tweets %>% filter(screen_name == "RafaelRabello14") %>% pull(id)
# tweets_unnested2 <- tweets_unnested %>% filter(!id %in% filtro_ids)
# most_pos <- which.max(tweets_unnested2$most_pos)
# cat(tweets$text[tweets$id == tweets_unnested$id[most_pos]])
# tweets %>% filter(id == tweets_unnested$id[most_pos])

# Tweet mais negativo
most_neg <- which.min(tweets_unnested$most_neg)
cat(tweets$text[tweets$id == tweets_unnested$id[most_neg]])
tweets %>% filter(id == tweets_unnested$id[most_neg]) %>% View()

# Juntando informações de sentimento aos dados dos tweets
tweets_fim <- tweets %>% inner_join(
  tweets_unnested %>% select(id, sentiment = tw_sentiment_op),
  by = "id")

glimpse(tweets_fim)


# Média por minuto
tweets_fim %>%
  group_by(created) %>%
  summarise(media = mean(sentiment)) %>%
  ggplot(aes(x = created, y = media)) +
  geom_line() +
  geom_smooth(stat = 'smooth', method = "loess", span = 0.2) +
  scale_x_datetime(date_breaks = "1 hour", labels = date_format("%H %M"))

# Quantidade de tweets positivos por minuto
tweets_fim %>% filter(sentiment > 0) %>%
  group_by(created) %>% summarise(n = n()) %>%
  ggplot(aes(x = created, y = n)) +
  geom_line() +
  geom_smooth(stat = 'smooth', method = "loess", span = 0.2) +
  scale_x_datetime(date_breaks = "1 hour", labels = date_format("%H %M"))

# Quantidade de tweets negativos por minuto
tweets_fim %>% filter(sentiment < 0) %>%
  group_by(created) %>% summarise(n = n()) %>%
  ggplot(aes(x = created, y = n)) +
  geom_line() +
  geom_smooth(stat = 'smooth', method = "loess", span = 0.2) +
  scale_x_datetime(date_breaks = "1 hour", labels = date_format("%H %M"))

# Quantidade de tweets positivos e quantidade de tweets negativos
tweets_fim %>%
  mutate(cat = ifelse(sentiment > 0, "positivo", ifelse(sentiment < 0, "negativo", NA))) %>%
  group_by(created, cat) %>%
  summarise(n = n()) %>%
  filter(!is.na(cat)) %>%
  ggplot(aes(x = created, y = n, color = cat)) +
  geom_line()
