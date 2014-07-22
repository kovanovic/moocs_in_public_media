#!/usr/bin/Rscript

source("util.R")

suppressMessages(library(tm))
suppressMessages(library(lda))
suppressMessages(library(gdata))
suppressMessages(library(slam))
suppressMessages(library(topicmodels))
suppressMessages(library(stats4))
suppressMessages(library(foreach))
suppressMessages(library(doMC))

all.word.counts.save.name    <- "../../data/all.word.counts.RData"
all.file.contents.save.name  <- "../../data/all.file.contents.RData"
all.file.headlines.save.name <-  "../../data/all.file.headlines.RData"

eval.dtm.save.name           <- "../../data/eval.dtm.RData"
full.dtm.save.name           <- "../../data/full.dtm.RData"

tf.threshold <- 0.95
tf.idf.threshold <- 0.9
train.perc <- 0.2

process.all.files <- function(fn, load.dir="../../data_private/lemmatized/") {
    results <- list()
    file.names <- list()

    for(input.file in list.files(path=load.dir)) {
        results <- append(results, fn(paste0(load.dir, input.file)))
        file.names <- append(file.names, input.file)
    }

    list(results=unlist(results),
         file.names=unlist(file.names))
}

process.single.file.contents <- function(f) {
    d.lines <- readLines(f)
    d.lines <- d.lines[!grepl("^http", d.lines)]
    d.lines <- d.lines[!grepl("^www", d.lines)]

    d <- tolower(paste(d.lines, collapse=" "))
    d <- gsub("-lrb-", "", d)
    d <- gsub("-rrb-", "", d)

    gsub("[^[:alpha:] ]", " ", d) # replace non letter characters with blank
}

process.single.file.word.counts <- function(f) {
    count.words(process.single.file.contents(f))
}

process.single.file.headline <- function(f) {
    paste(f, ":", readLines(f)[3])
}

load.all.file.headlines <- function() {
    if(file.exists(all.file.headlines.save.name)) {
        log.message("Loading already processed file headlines")
        load(all.file.headlines.save.name)
    } else {
        log.message("Generating file headlines")
        loading.res <- process.all.files(process.single.file.headline, load.dir="../../data_private/raw/")
        all.file.headlines <- loading.res$results
        all.file.names <- loading.res$file.names

        log.message("Saving generated file headlines")
        save(all.file.headlines, all.file.names, file = all.file.headlines.save.name)
    }
    list(all.file.headlines=all.file.headlines, all.file.names=all.file.names)
}

load.all.file.contents <- function() {
    if(file.exists(all.file.contents.save.name)) {
        log.message("Loading already processed file contents")
        load(all.file.contents.save.name)
    } else {
        log.message("Generating file contents")
        loading.res <- process.all.files(process.single.file.contents)
        all.file.contents <- loading.res$results
        all.file.names <- loading.res$file.names

        log.message("Saving generated file contents")
        save(all.file.contents, all.file.names, file = all.file.contents.save.name)
    }

    list(all.file.contents=all.file.contents, all.file.names=all.file.names)
}

load.all.word.counts <- function() {
    if(file.exists(all.word.counts.save.name)) {
        log.message("Loading already processed word counts")
        load(all.word.counts.save.name)
    } else {
        log.message("Generating word counts")
        loading.res <- process.all.files(process.single.file.word.counts)
        all.word.counts <- loading.res$results
        all.file.names <- loading.res$file.names

        log.message("Saving generated word counts")
        save(all.word.counts, all.file.names, file = all.word.counts.save.name)
    }

    list(all.word.counts=all.word.counts, all.file.names=all.file.names)
}

get.eval.data <- function(idxs) {
    set.seed(2)
    sample(idxs, floor(train.perc*length(idxs)))
}

build.dtm.for.data <- function(data) {

    dtm <- DocumentTermMatrix(Corpus(VectorSource(data)),
                              control = list(
                                  stemming = F,
                                  stopwords = T,
                                  minWordLength = 3,
                                  removeNumbers = T,
                                  removePunctuation = T))
    log.message(paste("Original number of terms:", dim(dtm)[2]))
    print(dtm)

    dtm <- removeSparseTerms(dtm, tf.threshold)

    log.message(paste("After TF filtering:", dim(dtm)[2]))
    print(dtm)

    term.tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))

    median.tf.idf <- median(term.tfidf)

    threshold <- tf.idf.threshold*median.tf.idf

    dtm <- dtm[,term.tfidf >= threshold]
    dtm <- dtm[row_sums(dtm) > 0,]

    log.message(paste("After TF-IDF filtering:", dim(dtm)[2]))
    print(dtm)

    gc()
    return(dtm)
}

load.eval.dtm <- function() {
    if(file.exists(eval.dtm.save.name)) {
        log.message("Loading saved eval DTM")
        load(eval.dtm.save.name)

    } else {
        log.message("Generating eval DTM")
        all.file.contents <- load.all.file.contents()$all.file.contents
        eval.dtm <- build.dtm.for.data(all.file.contents[get.eval.data(seq(all.file.contents))])

        log.message("Saving generated eval DTM")
        save(eval.dtm, file=eval.dtm.save.name)
    }
    return(eval.dtm)
}

load.full.dtm <- function() {
    if(file.exists(full.dtm.save.name)) {
        log.message("Loading saved eval DTM")
        load(full.dtm.save.name)

    } else {
        log.message("Generating eval DTM")
        all.file.contents <- load.all.file.contents()$all.file.contents
        dtm <- build.dtm.for.data(load.all.file.contents()$all.file.contents)

        log.message("Saving generated eval DTM")
        save(dtm, file=full.dtm.save.name)
    }
    return(dtm)
}
