Sys.setlocale("LC_COLLATE", "C")

log.message <- function(str) {
    cat(date(), "\t", str, "\n")
}

count.words <- function(s) {
    sapply(gregexpr("\\W+", s), length) + 1
}
