
library(quanteda)
library(data.table)

mdl <- readRDS("mdl.RDS")
mdl <- setDT(mdl)

top_10_mdl <- mdl[1:10]
top_10_mdl <- unique(top_10_mdl, by = "y")
top_10_mdl <- setorder(top_10_mdl, -GTy)

tokenize_phrase <- function(phrase) {
    toks <- tokens(
        phrase, 
        remove_punct = T,
        remove_symbols = T,
        remove_numbers = T,
        remove_url = T,
        remove_separators = T,
    )
    toks <- tokens_tolower(toks)
    toks <- as.list(toks)
    toks <- toks[[1]]
    
    
    for (tok in toks) {
        toks[toks == tok] <- ifelse(
            tok %in% mdl$y, tok, "<UNK>"
        )
    }
    
    toks
}

nextWord <- function(phrase, n = 1) {
    
    toks <- tokenize_phrase(phrase)
    n_toks <- length(toks)
    
    x4_start = n_toks - 2
    x3_start = n_toks - 1
    
    x4_phrase <- ifelse(
        x4_start > 0, 
        paste(toks[x4_start:n_toks], collapse = " "),
        ""
    )
    
    x3_phrase <- ifelse(
        x3_start > 0,
        paste(toks[x3_start:n_toks], collapse = " "),
        ""
    )
    
    x2_phrase <- toks[n_toks]
    
    mdl <- mdl[x2 == x2_phrase]
    
    candidates.4 <- mdl[x4 == x4_phrase]
    
    if (nrow(candidates.4) > 0) {
        candidates.4 <- candidates.4[GT4 == max(GT4)]
    }
    candidates.3 <- mdl[x3 == x3_phrase & !y %in% candidates.4$y]
    
    if (nrow(candidates.3) > 0) {
        candidates.3 <- candidates.3[GT3 == max(GT3)]
        candidates.3 <- unique(candidates.3, by = "y")
        candidates.3$GT4 <- NA
        candidates.3$x4 <- NA
    }
    
    candidates.2 <- mdl[
        x2 == x2_phrase & !y %in% candidates.3$y & !y %in% candidates.4$y
    ]
    
    if(nrow(candidates.2) > 0) {
        candidates.2 <- candidates.2[GT2 == max(GT2)]
        candidates.2 <- unique(candidates.2, by = "y")
        candidates.2$GT3 <- NA
        candidates.2$GT4 <- NA
        candidates.2$x4 <- NA
        candidates.2$x3 <- NA
    }
    
    all_candidates <- rbind(candidates.4, candidates.3, candidates.2)
    
    if (nrow(all_candidates) > 0) {
        all_candidates$GT <- mapply(
            function(GTy, GT2, GT3, GT4){
                unigram_gt2 <- (GTy * 0.20) + (GT2 * 0.80)
                unigram_gt2_gt3 <- unigram_gt2 * 0.25
                
                if (is.na(GT4) & is.na(GT3)) {
                    unigram_gt2
                } else if (is.na(GT4)) {
                    unigram_gt2_gt3 + (GT3 * 0.75)
                } else {
                    ((unigram_gt2_gt3 + (GT3 * 0.75)) * 0.25) + (GT4 * 0.75)
                }
            }, 
            all_candidates$GTy, 
            all_candidates$GT2, 
            all_candidates$GT3, 
            all_candidates$GT4
        )
        all_candidates <- setorder(all_candidates, -GT)
        all_candidates[1:n]
    } else {
        ## If the model can not find a prediction, output a random guess from
        ## the most popular words
        rand_idx <- sample(1:10, 1)
        top_10_mdl[rand_idx]$x2 <- NA
        top_10_mdl[rand_idx]$x3 <- NA
        top_10_mdl[rand_idx]$x4 <- NA
        top_10_mdl[rand_idx]$GT2 <- NA
        top_10_mdl[rand_idx]$GT3 <- NA
        top_10_mdl[rand_idx]$GT4 <- NA
        top_10_mdl[rand_idx]
    }
    
}