# Clean Data

library(quanteda)
library(dplyr)

download.file(
    "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
    destfile = "./data/data.zip"
)
unzip(
    "./data/data.zip",
    exdir = "./data/"
)
path_to_data <- file.path(".", "data", "final", "en_US")

twitter <- file.path(path_to_data, "en_US.twitter.txt")
news <- file.path(path_to_data, "en_US.news.txt")
blogs <- file.path(path_to_data, "en_US.blogs.txt")

get_data <- function(path_to_file) {
    file_con <- file(path_to_file)
    txt <- readLines(file_con)
    close(file_con)
    txt
} 

twitter_txt <- get_data(twitter)
news_txt <- get_data(news)
blogs_txt <- get_data(blogs)

corp <- corpus(c(
    twitter = twitter_txt,
    news = news_txt,
    blogs = blogs_txt
))

corp <- corpus_trim(corp, what = "sentences", min_ntoken = 3)

set.seed(123)
corp$doc_id <- 1:ndoc(corp)
training_ids <- sample(1:ndoc(corp), ndoc(corp) * 0.70, replace = FALSE)
training_corp <- corpus_subset(corp, doc_id %in% training_ids)
testing_corp <- corpus_subset(corp, !doc_id %in% training_ids)

clean_and_create_toks <- function(x) {
    toks <- tokens(
        x,
        remove_punct = TRUE,
        remove_symbols = TRUE,
        remove_numbers = TRUE,
        remove_url = TRUE,
        remove_separators = TRUE,
        split_hyphens = TRUE,
    ) %>% 
        tokens_tolower(.) %>%
        tokens_select(
            .,
            "[A-Za-z]",
            selection = "keep",
            valuetype = "regex"
        ) %>%
        tokens_select(
            .,
            "#",  #  | _ | . | :
            selection = "remove",
            valuetype = "regex"
        ) %>%
        tokens_select(
            .,
            "[1-9]",
            selection = "remove",
            valuetype = "regex"
        ) %>%
        tokens_select(
            .,
            "(\\.com)",
            selection = "remove",
            valuetype = "regex"
        ) %>%
        tokens_select(
            .,
            "(.)\\1{3,}", # remove words that containing repeating characters,
            selection = "remove",
            valuetype = "regex",
            window = 100000
        )
    toks
}

training <- clean_and_create_toks(training_corp)
testing <- clean_and_create_toks(testing_corp)

freqs <- featfreq(dfm(training))
single_freqs <- names(freqs[freqs %in% c(1,2)])
training <- tokens_select(
    training, single_freqs, selection = "remove", window = 10000
)
training <- tokens_sample(training, size = 1000000)

saveRDS(training, file = "training_tokens.RDS")
saveRDS(testing, file = "testing_tokens.RDS")
