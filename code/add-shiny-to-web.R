# add shiny to web

# Goal is to host the CritBot shiny app on the github page for CritBot

# packages
install.packages("shiny")
install.packages("shinylive")

# convert shiny to shinylive
library(shinylive)
shinylive::export(appdir = "critbot-shiny", destdir = "docs")

