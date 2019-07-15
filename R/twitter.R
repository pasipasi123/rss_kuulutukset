# twitter

library(twitteR)

use_oauth_token(readRDS("token.rds"))

twiitti_fun <- function(tviitti, liitetviitti, liitteet) {
  if (liitteet) {
    s1 <- updateStatus(tviitti, bypassCharLimit = TRUE)
    Sys.sleep(5)
    updateStatus(liitetviitti, bypassCharLimit = TRUE, inReplyTo = s1$id)
    Sys.sleep(10)
  } else {
    updateStatus(tviitti, bypassCharLimit = TRUE)
    Sys.sleep(10)
  }
}

tviitit %>%
  pwalk(~ twiitti_fun(..2, ..3, ..1))
