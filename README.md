# Soortenmeetnetten-queries

This repository contains R scripts for data management of the Flemish [Species Monitoring Programme](https://www.meetnetten.be/).

Following scripts can be found in the [src](https://github.com/inbo/soortenmeetnetten-queries/tree/master/src) folder:

-   [get_data_meetnetten.Rmd](https://github.com/inbo/soortenmeetnetten-queries/blob/master/src/get_data_meetnetten.Rmd): export data from the meetnetten-database and store locally as [git2rdata](https://ropensci.github.io/git2rdata/) files (only possible when connected to the INBO network)

-   [update_plantendata.Rmd](https://github.com/inbo/soortenmeetnetten-queries/blob/master/src/update_plantendata.Rmd): process external data from the plant monitoring schemes in order to migrate the data to the meetnetten-database

-   [migratie_rugstreeppad.Rmd](https://github.com/inbo/soortenmeetnetten-queries/blob/master/src/migratie_rugstreeppad.Rmd): process external data from the natterjack toad monitoring scheme in order to migrate the data to the meetnetten-database

-   [overige_data_extern.Rmd](https://github.com/inbo/soortenmeetnetten-queries/blob/master/src/overige_data_extern.Rmd): process external data for monitoring schemes that are not included in the meetnetten-database (badger and hamster) and store locally as a [git2rdata](https://ropensci.github.io/git2rdata/) file.

-   [update_metadata.Rmd](https://github.com/inbo/soortenmeetnetten-queries/blob/master/src/update_metadata.Rmd): update metadata of the monitoring schemes and store as a [git2rdata](https://ropensci.github.io/git2rdata/) file

-   [update_jaardoelen.Rmd](https://github.com/inbo/soortenmeetnetten-queries/blob/master/src/update_jaardoelen.Rmd): update yearly monitoring targets of the monitoring schemes and store as a [git2rdata](https://ropensci.github.io/git2rdata/) file
