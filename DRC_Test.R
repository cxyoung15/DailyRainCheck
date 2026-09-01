library(dplyr, quietly = TRUE)
library(furrr, quietly = TRUE)
library(future, quietly = TRUE)
library(future.apply, quietly = TRUE)
library(openxlsx, quietly = TRUE)
library(polite, quietly = TRUE)
library(rvest, quietly = TRUE)
library(stringr, quietly = TRUE)

sga_sta <- data.frame("50-Hempstead Harbor", "KNYGLENC2")
colnames(sga_sta) <- c("Growing Area", "Station_ID")
rainfall_stations <- data.frame(sga_sta$Station_ID)
colnames(rainfall_stations) <- "Station_ID"

date_to_run <- format((Sys.Date()-1), "%Y-%m-%d")
date_for_table <- format((Sys.Date()-1), "%m-%d-%Y")
date_for_file <- format((Sys.Date()-1), "%Y%m%d")

wunder_link <- "https://www.wunderground.com/dashboard/pws/"
link_list <- data.frame()
precip_amounts <- data.frame()
rainfall_stations <- rainfall_stations$Station_ID
for (station in rainfall_stations){
  link1 <- paste0(wunder_link, station, "/", "table", "/", date_to_run, "/", date_to_run, "/", "daily")
  link_list <- rbind(link_list, link1)
}
colnames(link_list) <- "Links"
all_links <- link_list$Links

html_documents <- suppressWarnings(future_map(.x = all_links, .f = ~{
  scrape_results <- tryCatch({
    wu_session <- polite::bow(.x)
    page_html <- polite::scrape(wu_session)
    if (is.null(page_html)){
      return(NA)
    } else {
      html_tab <- page_html %>% html_elements("table") %>% html_table()
      html_tab <- html_tab[[1]]
      html_txt <- page_html %>% html_elements("div.heading, div.sub-heading") %>% html_text()
      html_tab_txt <- append(html_tab, html_txt)
      station_name <- data.frame(str_split(.x, "/"))[6,]
      return(html_tab_txt)
    }
  }, error = function(e) NA)
  scrape_results
}, .options = furrr_options(seed = TRUE)))

for (i in html_documents){
  tryCatch({
    wu_name <- as.data.frame(i[[6]])
    wu_name <- str_split(wu_name, " - ")
    wu_name <- data.frame(wu_name)
    wu_name <- tail(wu_name, n=1)[1,]
    wu_name <- str_sub(wu_name, end = -5)
    coordinates <- i[[5]]
    coordinates <- str_split(coordinates, ", ")
    coordinates <- data.frame(coordinates)
    coordinates <- coordinates[2:3,]
    coordinates <- str_split(coordinates, " °")
    wu_lat <- coordinates[[1]]
    wu_lon <- coordinates[[2]]
    wu_lat <- as.numeric(wu_lat[1])
    wu_lon <- as.numeric(wu_lon[1]) * -1
    precip <- data.frame(i[[2]])
    precip <- tail(precip, n = 1)
    precip <- stringr::str_extract(precip, "^.{4}")
    station_data <- cbind(wu_name, wu_lat, wu_lon, precip)
    precip_amounts <- rbind(precip_amounts, station_data)
  }, error = function(e) NA)
}

rainfall_table <- data.frame(precip_amounts)
colnames(rainfall_table) <- c("Station_ID", "Latitude", "Longitude", date_for_table)
rainfall_totals <- merge(sga_sta, rainfall_table, by = "Station_ID", all = FALSE)
rainfall_totals <- rainfall_totals %>% relocate(`Growing Area`)
colnames(rainfall_totals) <- c("Growing Area", "Station", "Latitude", "Longitude", date_for_table)
rainfall_totals_sorted <- rainfall_totals %>% arrange(across(ncol(.), desc))

current_time <- format(Sys.time(), "%H%M")
file_loc <- paste0("Totals/Rainfall_Data_", date_for_file, "_@_", current_time, ".xlsx")
create_wb <- createWorkbook()
addWorksheet(create_wb, sheetName = "Daily Rainfall")
writeData(create_wb, sheet = "Daily Rainfall", x = rainfall_totals_sorted, startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, keepNA = FALSE)
saveWorkbook(create_wb, file = file_loc, overwrite = TRUE)