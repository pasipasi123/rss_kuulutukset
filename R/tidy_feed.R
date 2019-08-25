tidyfeed_fix <- function(feed) {

  formats <- c("a d b Y H:M:S z", "a, d b Y H:M z",
               "Y-m-d H:M:S z", "d b Y H:M:S",
               "d b Y H:M:S z", "a b d H:M:S z Y",
               "a b dH:M:S Y")

  doc <- httr::GET(feed)
  doc <- doc %>% xml2::read_xml()

  channel <- xml2::xml_find_all(doc, "channel")
  site <- xml2::xml_find_all(channel, "item")

  res <- suppressWarnings({tibble::tibble(
    feed_title = xml2::xml_text(xml2::xml_find_first(channel, "id")),
    feed_link = xml2::xml_text(xml2::xml_find_first(channel, "link")),
    feed_description = xml2::xml_text(xml2::xml_find_first(channel, "description")),
    feed_last_updated = xml2::xml_text(xml2::xml_find_first(channel, "lastBuildDate")) %>%
      lubridate::parse_date_time(orders = formats),
    feed_language = xml2::xml_text(xml2::xml_find_first(channel, "language")),
    feed_update_period = xml2::xml_text(xml2::xml_find_first(channel, "updatePeriod")),
    item_title = xml2::xml_text(xml2::xml_find_first(site, "title")),
    #item_creator = xml2::xml_text(xml2::xml_find_first(site, "dc:creator")),
    item_date_published = xml2::xml_text(xml2::xml_find_first(site, "pubDate")) %>%
      lubridate::parse_date_time(orders = formats),
    item_description = xml2::xml_text(xml2::xml_find_first(site, "description")),
    item_link = xml2::xml_text(xml2::xml_find_first(site, "link"))
  )})

  if(length(xml2::xml_find_all(site, "category")) > 0){
    res <- res %>%
      dplyr::mutate(item_categories = purrr::map(site, xml2::xml_find_all, "category") %>%
                      map(xml2::xml_text))
  }

  # suppressWarnings(
  #   res$feed_update_period[is.na(res$feed_update_period)] <- xml2::xml_text(
  #     xml2::xml_find_first(channel, "sy:updatePeriod"))
  # )
  suppressWarnings(
    res$feed_title[is.na(res$feed_title)] <- xml2::xml_text(
      xml2::xml_find_first(channel, "title"))
  )

  res <- Filter(function(x) !all(is.na(x)), res)

  return(res)

}
