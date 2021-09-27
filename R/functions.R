write_daily_files <- function(filename, outdir, verbose = TRUE) {

    old.scipen <- options(scipen = 99)
    on.exit(options(scipen = old.scipen))

    f <- file(filename, "r")
    on.exit(close(f))

    if (!dir.exists(outdir)) {
        if (verbose)
            message("create ", normalizePath(outdir))
        dir.create(outdir, recursive = TRUE)
    }

    i <- 0
    current_permno <- "0"
    repeat {
        line <- readLines(f, n = 1)
        if (length(line) == 0L)
            break

        i <- i + 1
        spl_line <- strsplit(line, "|", fixed = TRUE)[[1]]
        permno <- spl_line[1L]
        if (permno != current_permno) {
            fn <- file.path(outdir, permno)
            cat("", file = fn)
            current_permno <- permno
        }
        cat(spl_line[-1], "\n",
            file = fn, append = TRUE, sep = ",")

        if (verbose && i %% 1000000 == 0)
            message("one million lines processed")
    }
    invisible(i)
}

shares_history <- function(file, ...) {
    ans <- read.table(file, sep = "|", as.is = TRUE)
    colnames(ans) <- c("KYPERMNO", "SHRSDT", "SHRSENDDT", "SHROUT", "SHRFLG")
    date.fields <- c("SHRSDT", "SHRSENDDT")
    for (d in date.fields)
        ans[, d] <- as.Date(as.character(ans[, d]),
                            format = "%Y%m%d")
    ans
}

distributions <- function(file, ...) {
    ans <- read.table(file, sep = "|", as.is = TRUE)

    colnames(ans) <- c("KYPERMNO", "DISTCD", "DIVAMT",
                       "FACPR", "FACSHR", "DCLRDT", "EXDT",
                       "RCRDDT", "PAYDT", "ACPERM", "ACCOMP")

    date.fields <- c("DCLRDT", "EXDT", "RCRDDT", "PAYDT")
    for (d in date.fields)
        ans[, d] <- as.Date(as.character(ans[, d]),
                            format = "%Y%m%d")
    ans
}

nam_fun <- function(file, empty.NA = TRUE, ...) {

    old.scipen <- options(scipen = 99)
    on.exit(options(scipen = old.scipen))
    NAME_HISTORY <- read.table(file,
                               sep = "|",
                               stringsAsFactors = FALSE)

    colnames(NAME_HISTORY) <- c("KYPERMNO", "NAMEDT", "NAMEENDDT",
                                "NCUSIP", "NCUSIP9", "TICKER",
                                "COMNAM", "SHRCLS", "SHRCD",
                                "EXCHCD", "SICCD", "TSYMBOL",
                                "SNAICS", "PRIMEXCH", "TRDSTAT",
                                "SECSTAT")

    NAME_HISTORY[["NAMEDT"]] <- as.Date(
        as.character(NAME_HISTORY[["NAMEDT"]]), format = "%Y%m%d")
    NAME_HISTORY[["NAMEENDDT"]] <- as.Date(
        as.character(NAME_HISTORY[["NAMEENDDT"]]), format = "%Y%m%d")

    if (empty.NA)
        for (i in which(unlist(lapply(NAME_HISTORY, mode)) == "character"))
            NAME_HISTORY[[i]][ NAME_HISTORY[[i]] == "" ] <- NA

    function(search,
             include.ticker = FALSE,
             ticker.only = FALSE,
             ignore.case = TRUE,
             ...) {
        if (missing(search))
            NAME_HISTORY
        else {
            if (ticker.only)
                ii <- grepl(search, NAME_HISTORY$TICKER, ignore.case = ignore.case)
            else
                ii <- grepl(search, NAME_HISTORY$COMNAM, ignore.case = ignore.case)

            if (include.ticker && !ticker.only)
                ii <- ii | grepl(search, NAME_HISTORY$TICKER, ignore.case = ignore.case)
            if (!sum(ii))
                return(invisible(NULL))
            NAME_HISTORY[ii, , drop = FALSE]
        }
    }
}
