#!/usr/bin/Rscript

source("prepare_data.R")

suppressMessages(library(tm))
suppressMessages(library(lda))
suppressMessages(library(gdata))
suppressMessages(library(slam))
suppressMessages(library(topicmodels))
suppressMessages(library(stats4))
suppressMessages(library(foreach))
suppressMessages(library(doMC))

all.ks <- 2:200
models.dir.name <- paste0("../../data/models/", tf.threshold, "/")

run.lda <- function(k, dtm) {
    burnin <- 0
    iter <- 2000
    keep <- 50
    seed <- 1

    LDA(dtm, k = k, method = "Gibbs", control = llist(burnin, iter, keep, seed))
}

build.eval.topic.models <- function(dtm) {
    registerDoMC(4)
    foreach(k=all.ks) %dopar% {
        model.file.name <- paste0(models.dir.name, k, ".RData")
        if (file.exists(model.file.name))
            log.message(paste("K=", k, "already finished, skipping"))
        else {
            log.message(paste("K=", k, "started"))
            res <- run.lda(k, dtm)
            save(res, file = model.file.name)
            log.message(paste("K=", k, "finished"))

            res <- NULL
            gc()
            print(lsos())
        }
    }

    NULL
}

load.topic.models <- function() {
    log.message("Loading topic models")

    models <- list()
    for(model.file in list.files(path=models.dir.name)) {
        load(paste0(models.dir.name, model.file))
        models[substr(model.file, 1, nchar(model.file)-6)] <- res
    }

    res <- NULL
    return(models[as.character(sort(as.integer(names(models))))])
}
