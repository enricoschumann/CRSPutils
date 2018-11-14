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
