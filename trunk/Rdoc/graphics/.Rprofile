#Pierre BADY <pierre.bady@free.fr>
# 2009-11-02 15:21:56
cat("\n-------------------------------------------------------------------------\n")
cat("Bonjour !\n")
cat("comment vas-tu ?\n")
cat("Bonne utilisation de R !")
cat("\n-------------------------------------------------------------------------\n")
cat(paste("Working directory =",getwd(),"\n"))
cat("Rprofile file =",Sys.getenv("R_PROFILE"),"\n")
dirperm <- dirname(Sys.getenv("R_PROFILE"))
cat("-------------------------------------------------------------------------\n")
cat("Library paths =\n")
source("/export/scratch/Rplus/Rconfig.R")
.libPaths()
cat("-------------------------------------------------------------------------\n")
library(methods)
library(RColorBrewer)
library(stats)
library(ade4)
library(utils)
library(pixmap)
options(width=80)
options(papersize="a4")
options (prompt="R> ",digits=7)
options(chmhelp=TRUE)
cat("Packages =\n")
search()
#.Last <- function() {  savehistory();}
cat("-------------------------------------------------------------------------\n")
source("/export/scratch/Rplus/Rutility.R")
# END #
