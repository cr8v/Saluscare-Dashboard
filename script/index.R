list_PID <- readRDS("/home/rstudio/Data/object/list_PID.rds")

set.seed(926)
print(paste("Recommended sample size:", f_sample_size(nrow(list_PID$PID))))

RESULT <- list()
RESULT[["VALUEBOX"]] <- c(nrow(list_PID$PID), nrow(list_PID$ALL))

RESULT[["TABLE"]][["COUNT"]][["YEAR"]] <- list_PID$PID %>% 
  count(COUNT_YEAR, name = "N") %>% 
  arrange(desc(COUNT_YEAR)) %>%
  mutate(
    "%" = N/nrow(list_PID$PID),
    "Cumulative N" = cumsum(N),
    "Cumulative %" = cumsum(`%`)
  )

RESULT[["TABLE"]][["COUNT"]][["CENTER"]] <- list_PID$PID %>% 
  count(COUNT_CENTER, name = "N") %>%
  mutate(
    "%" = N/nrow(list_PID$PID),
    "Cumulative %" = cumsum(`%`)
  )

RESULT[["TABLE"]][["COUNT"]][["RGSTNO"]] <- list_PID$PID %>% 
  mutate(MEAN_RGSTNO = round(COUNT_RGSTNO/COUNT_YEAR,0)) %>% 
  count(MEAN_RGSTNO, name = "N") %>%
  mutate(
    "%" = N/nrow(list_PID$PID),
    "Cumulative %" = cumsum(`%`)
  )

RESULT[["TABLE"]][["EXAMPLE"]][["YEAR"]] <- list_PID$PID %>% 
  slice_sample(n = 100) %>% 
  arrange(desc(COUNT_YEAR))

RESULT[["TABLE"]][["EXAMPLE"]][["CENTER"]] <- 
  list_PID$PID %>% 
  arrange(desc(COUNT_CENTER)) %>% 
  head(1000) %>%
  select(PID_ANONY, contains("CENTER"))

RESULT[["TABLE"]][["EXAMPLE"]][["RGSTNO"]] <- 
  list_PID$PID %>% 
  mutate(MEAN_RGSTNO = round(COUNT_RGSTNO/COUNT_YEAR,1)) %>% 
  arrange(desc(COUNT_RGSTNO)) %>% 
  head(1000) %>%
  select(PID_ANONY, starts_with("COUNT_"), ends_with("_RGSTNO"), contains("RGSTNO"))


saveRDS(RESULT, "/home/rstudio/Website/save/index.rds")
