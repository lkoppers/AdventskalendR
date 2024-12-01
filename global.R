##############
## Roxygen2 ##
##############
library(roxygen2)
roxygenize(package.dir = ".")


###################
## build install ##
###################
setwd("..")

system("R CMD build AdventskalendR --resave-data")
system("R CMD INSTALL adventskalendR_0.5.tar.gz")

#########################
## Install from github ##
#########################

devtools::install_github("lkoppers/adventskalendR")
