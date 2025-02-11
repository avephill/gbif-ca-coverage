library(tidyverse)
library(duckdb)

dcon |>
  tbl("gbif") |>
  select(decimallatitude, decimallongitude)
head(4) |>
  collect() |>
  View()
dcon |> dbDisconnect(shutdown = T)
# --- Move to shiny -----------------------------------------------------------
# Connect to a new shiny database
scon <- dbConnect(duckdb(dbdir = "random-requests/gbif-ca-coverage/shiny/shiny.db"))
scon |> dbExecute("INSTALL spatial; LOAD spatial;")

# Attach the working database instead of copying it
scon |> dbExecute("ATTACH '~/Data/Occurrences/GBIF/gbif.duckdb' AS gbifmain;")


# Copy GBIF points over
scon |> dbExecute(
  "CREATE OR REPLACE TABLE shiny.gbif AS
(SELECT DISTINCT year,kingdom,phylum,class,\"order\",family,species,decimallatitude,decimallongitude FROM gbifmain.gbif
WHERE stateprovince='California'
AND decimallatitude IS NOT NULL
AND decimallongitude IS NOT NULL)
"
)
# Hmm.. removing 'geom' column reduced size from 1.5GB to 175M

# Verify the new tables in shiny.db
scon |> dbExecute("DETACH gbifmain")
scon |> dbListTables()

# Disconnect from the database
scon |> dbDisconnect(shutdown = TRUE)

scon |> tbl("gbif")
scon |> dbGetQuery("describe gbif")
