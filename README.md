# pfr-data

Repo for storing raw html from PFR from 1994 to 1998 with credit to **[Anthony Reinhard](https://twitter.com/reinhurdler)** for the scraper.

## Usage

```
game_html <- xml2::read_html("https://raw.githubusercontent.com/guga31bb/pfr-data/master/raw/1994_01_ARI_RAM.html")

game_html

sched <- readRDS(url("https://github.com/guga31bb/pfr-data/blob/master/data/pfr_schedule.rds?raw=true"))

```

