library(methods)
library(knitr)

knitr::opts_chunk$set(
        background = "#ededed",#"#FCFCFC",# code chunk color in latex
        comment = "#>", # 
        collapse = TRUE,
        cache = TRUE,
        warning = FALSE,
        fig.pos = "H",
        fig.path = "figuras/figLibro/",
        fig.align = 'center',
        fig.width = 6,
        fig.asp = 0.618,# 1 / phi
        fig.show = "hold",
        width = 85,
        linewidth = 48, #Agregado por mi
        out.width = "100%" 
        )


library(tmap)
set.seed(2020)
options(digits = 3,
        OutDec = ",",
        # width = 85, #Deberia ser menos?
        sf_max_print = 3,
        dplyr.print_min = 4,
        dplyr.print_max = 4)

if(!knitr::is_html_output()) {
        
        # options(width = 85)
        
        knitr::opts_chunk$set(
        #         linewidth = 48,
                out.width = "\\linewidth")
# Set an output hook to split st_read output so it doesn't extend beyond line width
knitr::knit_hooks$set(
        # modify the output
        output = function(x, options){
                
                # Split in lines
                lines <- unlist(strsplit(x, split = "\n"))
                
                #--------------------------------------------------------------------------*
                # modify the lines we expect to bleed out of the margins ----
                #--------------------------------------------------------------------------*
                
                # For the dsn specification
                lines <- gsub(
                        pattern = "' ",
                        replacement = "'\n \t", #"'\n#>\t"
                        x = lines
                )
                lines <- unlist(sapply(lines, strsplit, split = "\n"))
                
                
                # For the dsn file path
                lines <- ifelse(
                        test = grepl("from data source ", lines),
                        # Adjust so segments are split in /
                        yes = gsub(
                                pattern = "/([^/]*)\n",
                                replacement = "/\n \t \\1",
                                # Split in fixed width segments
                                x = gsub(
                                        pattern = "(.{,65})",
                                        replacement = "\\1\n",
                                        x = lines
                                )
                        ),
                        no = lines
                )
                
                
                # For the proj4 string specifications
                lines <- ifelse(
                        test = grepl("proj4string:", lines),
                        yes = gsub(
                                pattern = "[+]",
                                replacement = "\n \t+ ", #"\n#> \t+ "
                                x = lines
                        ),
                        no = lines
                )
                
                # bind the lines
                x <- paste(
                        "```",
                        paste0(lines, collapse = "\n"), #\n#>
                        "```",
                        sep = "\n"
                )
                
                return(x)
        }
)


hook_output = knit_hooks$get('output')
knit_hooks$set(output = function(x, options) {
        # this hook is used only when the linewidth option is not NULL
        if (!is.null(n <- options$linewidth)) {
                x = knitr:::split_lines(x)
                # any lines wider than n should be wrapped
                if (any(nchar(x) > n)) x = strwrap(x, width = n)
                x = paste(x, collapse = '\n')
        }
        hook_output(x, options)
})

}
# def.chunk.hook  <- knitr::knit_hooks$get("chunk")
# knitr::knit_hooks$set(chunk = function(x, options) {
#         x <- def.chunk.hook(x, options)
#         ifelse(options$size != "normalsize", paste0("\n \\", options$size,"\n\n", x, "\n\n \\normalsize"), x)
# })

