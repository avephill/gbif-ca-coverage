library(rsconnect)

rsconnect::deployApp("/home/ahill/Projects/random-requests/random-requests/gbif-ca-coverage/shiny",
  appFiles = c(
    "app.R", "shiny.db", "www/style.css"
  ),
  appName = "california-gbif-coverage"
)
