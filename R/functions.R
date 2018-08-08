write_daily_files <- function(filename, outdir, verbose = TRUE) {
    
    options(scipen = 50)
    f <- file(filename, "r")
    
    i <- 0
    if (!dir.exists(outdir))
        dir.create(outdir, recursive = TRUE)

    setwd(outdir)
    repeat {
        i <- i + 1
        line <- readLines(f, n = 1)
        if (length(line) == 0L)
            break
        
        spl_line <- strsplit(line, "|", fixed = TRUE)[[1]]
        permno <- spl_line[1L]
        cat(spl_line[-1], "\n",
            file = permno, append = TRUE, sep = ",")
        
        if (verbose && i %% 1000000 == 0)
            message("a million lines done")
        
    }
    invisible(i - 1)
}
