#!/usr/bin/Rscript

source("build_topic_models.R")

suppressMessages(library(tm))
suppressMessages(library(reshape2))
suppressMessages(library(stringr))
suppressMessages(library(lda))
suppressMessages(library(gdata))
suppressMessages(library(slam))
suppressMessages(library(topicmodels))
suppressMessages(library(ggplot2))
suppressMessages(library(xtable))
suppressMessages(library(stats4))
suppressMessages(library(foreach))
suppressMessages(library(doMC))
suppressMessages(library(Rmpfr))
suppressMessages(library(RColorBrewer))

n.terms <- 50
selected.to.interpret <- 30
target.percentage <- 0.51
max.y <- 40

periods <- c("08.1","08.2","08.3","08.4",
             "09.1","09.2","09.3","09.4",
             "10.1","10.2","10.3","10.4",
             "11.1","11.2","11.3","11.4",
             "12.1","12.2","12.3","12.4",
             "13.1","13.2","13.3","13.4",
             "14.1","14.2")

pal <- rep(rainbow(12, s=1, v=0.8)[c(1,11,5,7,8)], 10)
lsty <- rep(c(1,3,5), 100)
lwds <- rep(c(0.5,1.3,0.7), 100)

par.wc.hist           <- list(oma=c(0, 0, 0, 0), mai=c(0.50, 0.50, 0.10, 0.05), cex=0.6)
par.year.hist         <- list(oma=c(0, 0, 0, 0), mai=c(0.50, 0.00, 0.10, 0.00), cex=0.7, mgp=c(2.0, 1.0, 0.0))
par.pubs              <- list(oma=c(0, 0, 0, 0), mai=c(0.50, 0.50, 0.10, 0.10), cex=0.6)
par.ks                <- list(oma=c(0, 0, 0, 0), mai=c(0.50, 0.30, 0.10, 0.05), cex=0.6)
par.topic.frequencies <- list(oma=c(0, 0, 0, 0), mai=c(0.35, 0.35, 0.10, 0.05), cex=0.6, mgp=c(1.5, 0.5, 0.0))
par.trends            <- list(oma=c(0, 0, 0, 0), mai=c(0.35, 0.35, 0.10, 0.05), cex=0.6, mgp=c(1.5, 0.5, 0.0))

figures.dir <- "../../out/figures/"
tables.dir <- "../../out/tables/"

full.model.name <- paste0("../../data/full_model/full_model_", tf.threshold, ".RData")
topic.names <- c("EdX",
                 "Coursera",
                 "FutureLearn",
                 "Udacity",
                 "MOOCs in Australia",
                 "MOOC accreditation",
                 "Business and management MOOCs",
                 "Assesment in MOOCs",
                 "MOOCs as comunity college alternative",
                 "MOOCs and cuts in education fundings",

                 "MOOCs in India",
                 "MOOCs in California",
                 "MOOCs and growing number of students",
                 "MOOCs and problem of student debts",
                 "MOOCs in the news",
                 "MOOCs in China",
                 "MOOC startups",
                 "Flipped classroom",
                 "MOOC conferences",
                 "openHPI",

                 "MOOCs and virtual classrooms",
                 "Course Builder",
                 "MOOCs and government",
                 "Critical review of MOOCs",
                 "Mobile computing and MOOCs",
                 "MOOCs and distance education benefits",
                 "Georgia Tech and Udacity MSc program",
                 "MOOC certificates",
                 "Data analytics in MOOCs",
                 "MOOCs and empoyment",
                 paste("Topic", 31:92))

harmonicMean <- function(logLikelihoods) {
    precision <- 2000L
    llMed <- median(logLikelihoods)
    as.double(llMed - log(mean(exp(-mpfr(logLikelihoods, prec = precision) + llMed))))
}

evaluate.topic.models <- function(topic.models) {
    log.message("Evaluating all topic models")

    model.harmonic.means <- sapply(unlist(lapply(topic.models,logLik)), harmonicMean)

    return(list(k=all.ks[which.max(model.harmonic.means)], scores=model.harmonic.means))
}

get.topic.assignments <- function(best, selected.k) {
    gammaDF <- as.data.frame(best@gamma)
    names(gammaDF) <- 1:length(names(gammaDF))

    as.data.frame(cbind(document = row.names(gammaDF), topic = apply(gammaDF,1,function(x) names(gammaDF)[which(x==max(x))])))
}

print.topics.and.term <- function(tyc, full.topic.assignments, best) {
    topic.freqs <- sort(rowSums(tyc), decreasing=T)
    total.count <- sum(topic.freqs)
    selected.k <- length(topic.freqs)

    cat("\nTopic Count, Cummulative Percentage of articles.\n")
    for (i in seq(1, length(topic.freqs)))
        cat(i, "\t\t", sum(topic.freqs[1:i])/total.count, "\n")

    terms <- terms(best, n.terms)

    topic.names.old <- names(topic.freqs)[1:selected.k]

    imp.terms <- data.frame(terms[,as.numeric(topic.names.old)])
    imp.terms <- data.frame(do.call(paste, c(data.frame(t(imp.terms)), sep=", ")))
    imp.terms[,2] <- topic.names.old
    imp.terms[,3] <- as.character(topic.freqs[1:selected.k])
    imp.terms[,4] <- 1:nrow(imp.terms)

    imp.terms <- imp.terms[,c(4, 2, 3, 1)]
    colnames(imp.terms) <- c("\\#", "Topic Label", "\\texttt{N}", "Distinctive Terms")

    cat("\n", n.terms, "most important terms for each topic.\n")
    print(imp.terms)

    addtorow          <- list()
    addtorow$pos      <- list()
    addtorow$pos[[1]] <- c(0)
    addtorow$command  <- c(paste("\\midrule \n",
                                 "\\endhead \n",
                                 "\\midrule \n",
                                 "\\multicolumn{3}{c}{Continued on next page} \n",
                                 "\\endfoot \n",
                                 "\\bottomrule \n",
                                 "\\endlastfoot \n",sep=""))


    t2.1 <- imp.terms[1:15, 1:2]
    t2.2 <- imp.terms[16:30, 1:2]
    t2 <- data.frame(cbind(t2.1, t2.2))
    colnames(t2) <- rep(c("Topic Label", "\\texttt{N}"), 2)
    rownames(t2) <- NULL

    lt <- xtable(t2,
                 label = "tab:topics",
                 caption = paste("Top thirty discovered topics."))

    count.column.alignment <- ">{\\raggedleft\\arraybackslash}p{0.8cm}"

    align(lt) <- c("l", "X", count.column.alignment, "X", count.column.alignment)
    print(lt,
          file=paste0(tables.dir, "topics.tex"),
          booktabs=T,
          size="footnotesize",
          floating=T,
          table.placement="!b",
          tabular.environment="tabularx",
          width="\\textwidth",
          caption.placement="top",
          include.rownames=F,
          sanitize.text.function=function(x){x})

    lt <- xtable(imp.terms,
                 label = "tab:topic_terms",
                 caption = paste("Fifteen most important terms of the top thirty discovered topics"))

    align(lt) <- c("r", "l", "l", "r", "X")
    print(lt,
          file=paste0(tables.dir, "topic_terms.tex"),
          booktabs=T,
          floating=T,
          caption.placement="top",
          include.rownames=F,
          tabular.environment="tabularx",
          width="\\textwidth",
          size="footnotesize",
          sanitize.text.function=function(x){x})
}

print.topic.headlines <- function(tyc, full.topic.assignments) {

    all.file.headlines.res <- load.all.file.headlines()
    all.file.headlines     <- all.file.headlines.res$all.file.headlines
    all.file.names         <- all.file.headlines.res$all.file.names

    topic.freqs <- sort(rowSums(tyc), decreasing=T)
    topic.names.old <- names(topic.freqs)[1:nrow(tyc)]

    cat("\nTopic headlines:\n")
    for(topic.id in 1:nrow(tyc)) {
        cat("\nTopic", topic.names.old[topic.id], "new:", topic.id, "\n")
        inspect.topic(full.topic.assignments, topic.names.old[topic.id], all.file.names, all.file.headlines)
    }
}

get.topic.periods <- function(article.topics) {
    years.data <- read.csv("../../data/years.txt")

    topic.years <- list()

    for(current.article.idx in seq(nrow(article.topics))) {
        year.short <- years.data$year[current.article.idx]
        current.article.period <- paste0(year.short, ".", years.data$quartile[current.article.idx])
        current.article.topics <- article.topics[[current.article.idx, 2]]

        for(current.article.topic.idx in seq(current.article.topics)) {
            current.article.topic <- current.article.topics[[current.article.topic.idx]]
            topic.years[[as.character(current.article.topic)]] <- append(topic.years[[as.character(current.article.topic)]], current.article.period)
        }
    }

    return(topic.years)
}

calculate.topic.year.counts <- function(topic.years) {

    topic.year.counts <- NULL
    topic.names <- NULL
    for(t in names(topic.years)) {
        current.topic.period.list <- topic.years[[t]]
        topic.year.counts <- rbind(topic.year.counts, table(factor(current.topic.period.list, levels=periods)))
        topic.names <- c(topic.names, t)
    }

    topic.year.counts <- data.frame(topic.year.counts)

    colnames(topic.year.counts) <- periods
    rownames(topic.year.counts) <- topic.names

    topic.year.counts <- topic.year.counts[,-c(1:16)]

    topic.year.counts[order(rowSums(topic.year.counts), decreasing=T), ]
}

init.dirs <- function() {
    unlink(figures.dir, recursive=T)
    unlink(tables.dir, recursive=T)

    dir.create(figures.dir)
    dir.create(tables.dir)
}

plot.topic.frequencies <- function(tyc, best.k) {
    best.k <- nrow(tyc)
    max.count <- closest.10s(max(rowSums(tyc), decreasing=T))

    topic.freqs <- sort(rowSums(tyc), decreasing=T)

    pdf(paste0(figures.dir, "topic_frequencies.pdf"), width=3, height=2.5)
    par(par.topic.frequencies)

    plot(topic.freqs, ylab="Number of articles", xlab = "", axes=F, type="h", ylim=range(0,max.count), xlim=range(1,best.k))
    ticks <- sort(unique(c(1, selected.to.interpret, best.k, seq(10, best.k, by=10))))

    axis(1,                  at=ticks,
         col.axis="black",   tcl=-0.3,
         tick=F,             las=1)

    title(xlab="Topics")

    axis(2,                    at=seq(0, max.count, by=10),
         col.axis="black",     tcl=-0.3,
         tick=F,               las=1)

    text(x=selected.to.interpret,
         y=0.8*max.count,
         labels=paste(target.percentage*100, "%"))

    abline(v=selected.to.interpret, col="red", lty=3)
    invisible(dev.off())
}

addline_format <- function(x,...){
    gsub('\\.','\n',x)
}

plot.trends <- function(full.topic.year.count) {

    full.topic.year.count.sorted           <- full.topic.year.count
    rownames(full.topic.year.count.sorted) <- sprintf("Topic %02d", 1:nrow(full.topic.year.count.sorted))

    cols.p <- c(rep("#CC6666",4), rep("#9999CC",4), rep("#66CC99",2))
    cols.l <- c(rep("#CC6666",3), rep("#9999CC",4), rep("#66CC99",3))

    df <- full.topic.year.count[1:selected.to.interpret,]
    wtn <- lapply(strwrap(as.character(topic.names[1:selected.to.interpret]), width=22, simplify=F), paste, collapse="\n")
    df2 <- cbind(factor(wtn, levels=wtn), df)
    colnames(df2) <- c("Topic", colnames(df))
    df2.2 <- melt(df2, id="Topic")
    colnames(df2.2) <- c("Topic", "Quartile", "Count")

    pdf(paste0(figures.dir, "topic_trends.pdf"), width=7, height=6)
    par(par.trends)
    print(ggplot(df2.2, aes(x = Quartile, y = Count, group = Topic)) +
          coord_cartesian(ylim = c(0, max.y)) +
          scale_x_discrete(labels=addline_format(periods[-c(1:16)])) +
          geom_line(colour=cols.l, size=0.8) +
          geom_point(color=cols.p, size=1, shape=20) +
          theme_classic() +
          labs(x="Annual quartile", y="Article count") +
          theme(axis.text.x = element_text(size=6),
                axis.text.y = element_text(size=6),
                axis.ticks = element_blank(),
                strip.text.x = element_text(size = 8),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_rect(fill=NA, color="black", size=0.5, linetype="solid"),
                strip.background = element_blank())+
          facet_wrap( ~ Topic, ncol = 5, scales = 'fixed', as.table=T))

    invisible(dev.off())
}

plot.pubs <- function() {
    pubs <- read.csv("../../data/publications.txt")
    pub.freqs <- sort(table(pubs$publication), decreasing=T)
    cat("\nPubs: Mean=", round(mean(pub.freqs), 0), ", SD=", round(sd(pub.freqs), 0), "\n", sep="")

    pdf(paste0(figures.dir, "publications.pdf"), width=5, height=2.5)
    par(par.pubs)
    barplot(sort(table(pub.freqs), decreasing=T), ylim=c(0,250), xlab="Article count", ylab="Sources Count", space=0, las=1)
    invisible(dev.off())
}

make.pubs.table <- function() {
    pubs <- read.csv("../../data/publications.txt")
    pub.freqs <- sort(table(pubs$publication), decreasing=T)

    top.pubs <- data.frame(sort(pub.freqs, decreasing=T)[1:20])
    top.pubs$id <- 1:nrow(top.pubs)
    colnames(top.pubs) <- c("count", "id")

    top.pubs.2 <- cbind(top.pubs[1:10,"id"], rownames(top.pubs[1:10,]), top.pubs[1:10, "count"], top.pubs[11:20,"id"], rownames(top.pubs[11:20,]), top.pubs[11:20,"count"])
    top.pubs.2 <- data.frame(top.pubs.2)
    colnames(top.pubs.2) <- rep(c("\\#", "Source", "Article Count"), 2)
    rownames(top.pubs.2) <- NULL

    lt <- xtable(top.pubs.2,
                 label = "tab:top_pubs",
                 caption = paste("Twenty most important news sources."))

    count.column.alignment <- ">{\\raggedleft\\arraybackslash}p{0.8cm}"

    align(lt) <- c("l", "l", "X", count.column.alignment, "l", "X", count.column.alignment)
    print(lt,
          file=paste0(tables.dir, "top_pubs.tex"),
          booktabs=T,
          size="footnotesize",
          floating=T,
          table.placement="t!",
          tabular.environment="tabularx",
          width="\\textwidth",
          caption.placement="top",
          include.rownames=F,
          sanitize.text.function=function(x){x})
}

plot.wc.hist <- function() {
    wcs <- load.all.word.counts()$all.word.counts

    pdf(paste0(figures.dir, "word_counts.pdf"), width=5, height=2.5)
    par(par.wc.hist)

    cat("\nWord Counts: Mean=", round(mean(wcs), 0), ", SD=", round(sd(wcs), 0), "\n", sep="")

    breaks <- seq(0, max(wcs)+100, by=200)

    hist(wcs, breaks=breaks, col="gray", main="", sub="", axes=F, xlab="Word Count", ylab="Article Count")

    axis(1, at=seq(0, 6500, by=500), las=1)
    axis(2, at=seq(0, 1100, by=100), las=1)
    invisible(dev.off())
}

plot.years.hist <- function() {
    years.data <- read.csv("../../data/years.txt")
    pdf(paste0(figures.dir, "years_detailed.pdf"), width=4, height=2.5)
    par(par.year.hist)
    par(cex.axis=0.7)

    year.counts <- table(factor(paste0(sprintf("%02d", years.data$year),".", years.data$quartile), levels=periods))
    names(year.counts) <- addline_format(names(year.counts))
    year.counts <- year.counts[-c(1:4)]

    bp <- barplot(year.counts, ylim=range(0, 800), space=0.2,las=1, axes=F, xlab="Annual quartile")
    title(ylab="Article Count", mgp=c(-1,0,0))

    text(x=bp,y=year.counts, label=year.counts, po=3)

    invisible(dev.off())
}

plot.google.trends <- function() {
    gt <- read.csv("../../data/google_trends.csv")
    gt$x <- 1
    gt$period <- paste0(sprintf("%02d", gt$year), ".", gt$quartile)
    gt$period <- factor(gt$period, levels=unique(gt$period))

    gt.data <- unlist(lapply(split(gt, gt$period), function(period.data) {mean(period.data$mooc)}))
    names(gt.data) <- addline_format(names(gt.data))
    gt.data <- gt.data[-c(1:4)]

    pdf(paste0(figures.dir, "google_trends.pdf"), width=4, height=2.5)
    par(par.year.hist)
    par(cex.axis=0.7)

    bp <- barplot(gt.data, ylim=range(0, 80), space=0.2,las=1, xlab="Annual quartile", axes=F)
    title(ylab="Google Trend score", mgp=c(-1,0,0))
    text(x=bp,y=gt.data, label=round(gt.data,0), po=3)

    invisible(dev.off())
}

plot.ks <- function(res) {
    scores <- res$scores
    best.k <- res$k

    pdf(paste0(figures.dir, "topic_count.pdf"), width=3, height=2.5)
    par(par.ks)

    plot(all.ks, scores, type = "l", xlab = "Number of topics", axes=F, ylab="")
    axis(1, at=c(2,25,50,75,100,125,150,175,200), col.axis="black", tick=T, las=1)
    axis(2, at=range(scores), labels=F,           col.axis="black", las=1)
    title(ylab = "Log-likelihood", mgp=c(1,0,0))

    if(is.null(best.k)==F) {
        abline(v=best.k, col="red", lty=3)

        y.text.pos <- range(scores)[1] + (0.8*(range(scores)[2]-range(scores)[1]))

        text(x=best.k,
             y=y.text.pos,
             labels=as.character(best.k))
    }

    invisible(dev.off())
}

closest.10s <- function(num) {
    rem <- num %% 10
    return(num - rem + 10)
}

inspect.topic <- function(topic.assignments, topic.id, all.file.names, headlines) {

    all.file.names <-unlist(all.file.names)

    target.docs <- unlist(lapply(full.topic.assignments$topic,
                                 function(topics) {
                                     return(as.character(topic.id) %in% topics)
                                 }))

    target.doc.names <- all.file.names[target.docs]

    cat("\nthere are", length(target.doc.names), "files about topic", topic.id, "\n\n")
    cat(headlines[str_detect(headlines, paste0("^", paste(target.doc.names, collapse="|")))], sep="\n")
}

load.full.model <- function () {
    if(file.exists(full.model.name)) {
        log.message("Loading full LDA model")
        load(full.model.name)
    } else {
        log.message("Generating full LDA model")
        full.model <- run.lda(res$k, load.full.dtm())
        save(full.model, file=full.model.name)
    }
    return(full.model)
}

init.dirs()

if(exists("res")==F)
    res <- evaluate.topic.models(load.topic.models())

if(exists("full.model")==F)
    full.model <- load.full.model()

plot.wc.hist()
plot.years.hist()
plot.google.trends()

full.topic.assignments <- get.topic.assignments(full.model)
full.topic.year.count <- calculate.topic.year.counts(get.topic.periods(full.topic.assignments))

make.pubs.table()
plot.pubs()
plot.ks(res)
plot.topic.frequencies(full.topic.year.count)
plot.trends(full.topic.year.count)
print.topics.and.term(full.topic.year.count, full.topic.assignments, full.model)
#print.topic.headlines(full.topic.year.count, full.topic.assignments)
