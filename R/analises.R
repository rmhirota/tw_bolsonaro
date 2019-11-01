library(dplyr)
library(lubridate)
library(lexiconPT)
library(tidytext)
library(ggplot2)
library(stringr)
library(scales)


# Leitura dos dados
tweets_orig <- data.table::fread("data/total_bolsonaro.csv", header = T)
tweets_orig <- janitor::clean_names(tweets_orig) %>% select(-c(v1, x, new))
glimpse(tweets_orig)

# Formata data e id
tweets_orig <- tweets_orig %>% mutate(created = as_datetime(created),
                            id = as.character(id))

# Cria id única - chave dupla (screen_name, id)
tweets_orig <- tweets_orig %>% mutate(id_unica = paste0(screen_name, "_", id))

# Deduplica tweets/retweets
tweets <- tweets_orig %>%
  select(id_unica, screen_name, text, created) %>%
  distinct() %>%
  group_by(id_unica, screen_name, created) %>%
  summarise(text = first(text))

# Total: 850029 tweets
# nrow(tweets)

# Filtro: apenas tweets com "bolsonaro" no texto
tweets_bolso <- tweets %>% mutate(text = str_to_lower(text)) %>% filter(str_detect(text, "bolsonaro"))


# Análise de sentimento
data("oplexicon_v3.0")
data("sentiLex_lem_PT02")
op30 <- oplexicon_v3.0 %>% group_by(term) %>% summarise(polarity = mean(polarity))
sent <- sentiLex_lem_PT02 %>% group_by(term) %>% summarise(polarity = mean(polarity))

# O léxico não contempla acentos, portanto, vamos tirar caracteres especiais
tweets_bolso <- tweets_bolso %>%
  mutate(text = iconv(text, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         text = str_to_lower(str_replace_all(text, "[//^~']+", "")))

tweets_unnested <- tweets_bolso %>% unnest_tokens(term, text)

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

nrow(tweets_unnested) / nrow(tweets_bolso)  # x% dos tweets categorizados

tweets_unnested %>%
  ggplot(aes(x = tw_sentiment_op, y = tw_sentiment_lex)) +
  geom_point(aes(color = n_words)) +
  scale_color_continuous(low = "green", high = "red") +
  labs(x = "Polaridade no OpLexicon", y = "Polaridade no SentiLex") +
  #geom_smooth(method = "lm") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed")


# Tirando outliers
tweets_unnested %>% filter(abs(tw_sentiment_op - tw_sentiment_lex) > 5) %>% nrow()
tweets_unnested <- tweets_unnested %>% filter(abs(tw_sentiment_op - tw_sentiment_lex) <= 5)



# Tweet mais positivo
most_pos <- which.max(tweets_unnested$most_pos)
cat(tweets$text[tweets$id == tweets_unnested$id[most_pos]])
tweets_bolso %>% filter(id == tweets_unnested$id[most_pos]) %>% View()

# tirando esse usuário que tem "bolsominion" no nome:
# filtro_ids <- tweets %>% filter(screen_name == "RafaelRabello14") %>% pull(id)
# tweets_unnested2 <- tweets_unnested %>% filter(!id %in% filtro_ids)
# most_pos <- which.max(tweets_unnested2$most_pos)
# cat(tweets$text[tweets$id == tweets_unnested$id[most_pos]])
# tweets %>% filter(id == tweets_unnested$id[most_pos])

# Tweet mais negativo
most_neg <- which.min(tweets_unnested$most_neg)
cat(tweets_bolso$text[tweets$id == tweets_unnested$id[most_neg]])
tweets_bolso %>% filter(id == tweets_unnested$id[most_neg]) %>% View()

# Juntando informações de sentimento aos dados dos tweets
tweets_fim <- tweets_bolso %>% inner_join(
  tweets_unnested %>% select(id, sentiment = tw_sentiment_op),
  by = "id")

glimpse(tweets_fim)


# Média por minuto
tweets_fim %>%
  mutate(datahora = floor_date(created, "minutes")) %>%
  group_by(datahora) %>%
  summarise(media = mean(sentiment)) %>%
  ggplot(aes(x = datahora, y = media)) +
  geom_line() +
  geom_smooth(stat = 'smooth', method = "loess", span = 0.2) +
  scale_x_datetime(date_breaks = "1 hour", labels = date_format("%H %M"))

# Quantidade de tweets positivos por minuto
tweets_fim %>% filter(sentiment > 0) %>%
  mutate(datahora = floor_date(created, "minutes")) %>%
  group_by(datahora) %>% summarise(n = n()) %>%
  ggplot(aes(x = datahora, y = n)) +
  geom_line() +
  geom_smooth(stat = 'smooth', method = "loess", span = 0.2) +
  scale_x_datetime(date_breaks = "1 hour", labels = date_format("%H %M"))

# Quantidade de tweets negativos por minuto
tweets_fim %>% filter(sentiment < 0) %>%
  mutate(datahora = floor_date(created, "minutes")) %>%
  group_by(datahora) %>% summarise(n = n()) %>%
  ggplot(aes(x = datahora, y = n)) +
  geom_line() +
  geom_smooth(stat = 'smooth', method = "loess", span = 0.2) +
  scale_x_datetime(date_breaks = "1 hour", labels = date_format("%H %M"))

# Quantidade de tweets positivos e quantidade de tweets negativos
tweets_fim %>%
  mutate(datahora = floor_date(created, "minutes")) %>%
  mutate(cat = ifelse(sentiment > 0, "positivo", ifelse(sentiment < 0, "negativo", NA))) %>%
  group_by(datahora, cat) %>%
  summarise(n = n()) %>%
  filter(!is.na(cat)) %>%
  ggplot(aes(x = datahora, y = n, color = cat)) +
  geom_line()
