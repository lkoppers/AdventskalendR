#' Öffnen von Adventskalender-Türchen leicht gemacht
#'
#' Mit diese Funktion wird der Adventskalender geöffnet. Natürlich dürfen auch Türen aus der Vergangenheit geöffnet werden. Aber hüte dich davor Türchen zu früh zu öffnen.
#'
#' @param datum Standardmäßig das aktuelle Datum. Es können aber auch zwei Datumsangaben als Character angegeben werden, die als Zeitraum interpretiert werden. ACHTUNG: Datumsangaben aus der Zukunft werden gemeldet!
#' @examples
#' adventskalendR()
#' adventskalendR(datum = c("2021-12-01", "2021-12-03"))
#' adventskalendR(datum = c("2021-12-01", as.character(today())))
#' adventskalendR(datum = "alle")
#' @export adventskalendR
#' @import dplyr
#' @import knitr
#' @import jpeg
#' @import lubridate
#' @import readr

adventskalendR <- function(datum = today()){
  existierende_daten <- read_csv("https://github.com/lkoppers/AdventskalendR_Data/raw/refs/heads/main/existierende_daten.csv", show_col_types = FALSE)$Datum
  if(as.character(datum[1]) == "alle"){
    datum <- existierende_daten[existierende_daten <= today()]
    }
  datum <- as.Date(datum)
  if(length(datum) == 2){datum <- seq.Date(datum[1], datum[2], "1 day")}
  
  if(any(datum > today())){
    document <- c("### Sicherheitsalarm! \n", "Dieser Vorfall wird gemeldet! \n",
    "<img src=\"https://imgs.xkcd.com/comics/incident.png\" alt=\"Alarm\">", "[XKCD](https://xkcd.com/838/)")
    writeLines(document, con = file.path(tempdir(), "adventskalendR.Rmd"))
    knit2html(file.path(tempdir(), "adventskalendR.Rmd"), output = file.path(tempdir(), "adventskalendR.html"))
    if (interactive()) browseURL(file.path(tempdir(), "adventskalendR.html"))
    stop()
  }
  datum <- datum[datum %in% existierende_daten]

  adventskalenData_selected <- NULL
  
  for(i in as.character(datum)){
    tmp <- NULL
    try(tmp <- read_rds(paste0("https://github.com/lkoppers/AdventskalendR_Data/raw/refs/heads/main/Tuerchen/", i, ".rds")))
    adventskalenData_selected <- bind_rows(adventskalenData_selected, tmp)
  }
  
  adventskalenData_selected <- 
    adventskalenData_selected |> 
    mutate(Text = unname(sapply(adventskalenData_selected$Text, intToUtf8)))
  
  adventskalenData_selected <- 
    adventskalenData_selected |> 
    group_by(Datum) |> 
    filter(No == min(No)) |> 
    mutate(No = No - 0.1, 
           Text = paste("###", Datum), 
           Typ = "text") |> 
    bind_rows(adventskalenData_selected) |> 
    arrange(No)
  
  Bildexport <- adventskalenData_selected |> filter(Typ == "bild")
  
  for(i in 1:nrow(Bildexport)){
    writeJPEG(image = Bildexport$Bild[i][[1]], target = file.path(tempdir(), paste0(Bildexport$No[i], ".jpg")))
  }
  
  document <- NULL
  for (i in 1:nrow(adventskalenData_selected)) {
    if(adventskalenData_selected$Typ[i] == "text"){
      document <- c(document, paste(adventskalenData_selected$Text[i], "\n"))
      }
    if(adventskalenData_selected$Typ[i] == "bild"){
      document <- c(document, 
                    paste0("<img src=\"", tempdir(), "/", adventskalenData_selected$No[i], ".jpg\" alt=\"Bild\"> \n"))
    }
  }

  writeLines(document, con = file.path(tempdir(), "adventskalendR.Rmd"))
  knit2html(file.path(tempdir(), "adventskalendR.Rmd"), output = file.path(tempdir(), "adventskalendR.html"))
  if (interactive()) browseURL(file.path(tempdir(), "adventskalendR.html"))
}

# adventskalendR(datum = c(today()-4, today()))
# adventskalendR(datum = c("2021-12-01", "2021-12-24"))
# 
# library(tidyverse)
# library(knitr)
# library(jpeg)
# library(lubridate)
# 
# 
# adventskalenData <-
#   read_csv("texte.csv") |>
#   mutate(No = row_number())
# adventskalenData <-
#   adventskalenData |>
#   mutate(Text = unname(sapply(adventskalenData$Text, utf8ToInt)),
#          Bild = unname(sapply(adventskalenData$Bild, function(x){readJPEG(file.path("bilder", paste0(x, ".jpg")))})))
# 
# Adventstage <- as.character(unique(adventskalenData$Datum))
# 
# for(i in Adventstage){
#   adventskalenData |>
#     filter(Datum == as.Date(i)) |>
#     write_rds(
#       file = file.path("clouddata", paste0(i, ".rds")),
#       compress = "gz"
#       )
# }

# read_csv("texte.csv") |>
#   select(Datum) |> 
#   distinct() |> 
#   write_csv("existierende_daten.csv")
