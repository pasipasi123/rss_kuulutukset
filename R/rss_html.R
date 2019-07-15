library(pdftools)
library(tidyRSS)
library(tidyverse)
library(rvest)

# html-table & RSS ----

kuulutus_nodes <- read_html("http://asiakirjat.ouka.fi/ktwebbin/dbisa.dll/ktwebscr/kuullist_tweb.htm") %>%
  html_nodes(xpath = "//*[@id=\"content\"]/div[2]/table")

liite_urls <- kuulutus_nodes %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  str_subset("\\+") %>%
  head(20)

kuulutus_table <- kuulutus_nodes %>%
  html_table(fill = TRUE) %>%
  unlist(recursive = FALSE) %>%
  as_tibble() %>%
  select(-ncol(.))

new_cols <- unlist(kuulutus_table[2, ])

kuulutus_table_20 <- kuulutus_table %>%
  set_names(new_cols) %>%
  tail(-2) %>%
  head(20) %>%
  bind_cols(liite_url = paste0("http://asiakirjat.ouka.fi", liite_urls)) %>%
  set_names("naht", "otsikko", "laji", "liitteet", "liite_url")

# rss

rss <- "http://asiakirjat.ouka.fi/ktwebbin/dbisa.dll/ktwebscr/kuul_rssfeed.htm"

feed <- tidyfeed(rss)

kuulutukset <- kuulutus_table_20 %>%
  bind_cols(item_link = feed$item_link,
            item_date = feed$item_date_published)

today_yesterday <- c(lubridate::today(), lubridate::today() - lubridate::days(1))

# kaavoitus ----

kaavat <- kuulutukset %>%
  filter(str_detect(laji, "Kaav")) #%>%
  # filter(lubridate::as_date(item_date) %in% today_yesterday)

stopifnot(nrow(kaavat) > 0)

kaavat %>%
  mutate(filename = paste0("kaavakuulutus", row_number(), ".pdf")) %>%
  pwalk(~ download.file(..6, ..8, method = "curl"))

kuul_pdfs <- list.files() %>%
  str_subset("kaavakuu") %>%
  map(pdf_text) %>%
  map(str_split, "\n", simplify = TRUE) %>%
  map(str_replace, "\r", "") %>%
  map(str_trim)

proj_nrot <- kuul_pdfs %>%
  map(str_c, collapse = " ") %>%
  # map(str_extract, pattern = "(?<=kohtaan )[[:digit:][-]]++")
  map(str_extract, pattern = "[:digit:]{3}-[:digit:]{4}")

haku_url <- "https://www.oukapalvelut.fi/tekninen/Suunnitelmat/index_iframe_nocss.asp?selLaji=0&selKunnanosa=0&selVaihe=0&Submit=Hae&Ko_Haku=Yes"

haku_htmls <- proj_nrot %>%
  map(~ paste0(haku_url, "&tfSana=", .)) %>%
  map(read_html)

naht <- haku_htmls %>%
  map(html_nodes, xpath = "/html/body/div/table") %>%
  map(html_table, fill = TRUE, header = TRUE) %>%
  map(as.data.frame) %>%
  # map(as_tibble) %>%
  map(head, 1) %>%
  map(select, naht = `Nähtävillä`) %>%
  unlist()

kortti_urls <- haku_htmls %>%
  map(html_nodes, "a") %>%
  map(html_attr, "href") %>%
  map(~ paste0("https://www.oukapalvelut.fi/tekninen/Suunnitelmat/", .))

koonti <- kaavat %>%
  bind_cols(proj_nrot = unlist(proj_nrot)) %>%
  bind_cols(kortti_url = unlist(kortti_urls)) %>%
  bind_cols(naht_proj = naht) %>%
  filter(lubridate::as_date(item_date) %in% today_yesterday)

stopifnot(nrow(koonti) > 0)

tviitit <- koonti %>%
  mutate(naht_proj = str_replace(naht_proj, "-", "–")) %>%
  mutate(tviitti = paste0(otsikko, ". ", proj_nrot,
                          if_else(naht_proj == "",
                                  "",
                                  paste0(". Nähtävillä ", naht_proj)),
                          ". Projektikortti: ", kortti_url, ". Kuulutus: ", item_link)) %>%
  mutate(liitetviitti = paste0("Kuulutuksen liitteet: ", liite_url)) %>%
  transmute(liitteet = liitteet != 0,
            tviitti, liitetviitti)

yesterday <- readRDS("yesterday.RDS")

stopifnot((tviitit %>% anti_join(yesterday) %>% nrow()) > 0)

source("R/twitter.R")

saveRDS(tviitit, "yesterday.RDS")

list.files() %>%
  str_subset("kaavakuu") %>%
  walk(file.remove)

