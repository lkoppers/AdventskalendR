# AdventskalendR
Ein R-Paket-Adventskalender

ACHTUNG: 2023 startet der Adventskalender erst am 2023-12-11, dafür geht er auch nach Weihnachten noch weiter.

### Installation

devtools::install_github("lkoppers/adventskalendR")

### Benutzung
Mit der Funktion adventskalendR() kann jeden Tag das aktuelle Türchen geöffnet werden. 

library(adventskalendR)<br>
adventskalendR()

*Zeitraum*
adventskalendR(datum = c("2021-12-01", "2021-12-03"))

*Alle ab bestimmtem Datum bis heute*
adventskalendR(datum = c("2021-12-01", as.character(lubridate::today())))

*alle verfügbaren Einträge*
adventskalendR(datum = "alle")
