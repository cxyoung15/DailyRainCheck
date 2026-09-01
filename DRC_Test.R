library(dplyr, quietly = TRUE)
library(furrr, quietly = TRUE)
library(future, quietly = TRUE)
library(future.apply, quietly = TRUE)
library(openxlsx, quietly = TRUE)
library(polite, quietly = TRUE)
library(rvest, quietly = TRUE)
library(stringr, quietly = TRUE)

sga_sta <- read.xlsx("RainfallStations.xlsx", sheet = 1)
rainfall_stations <- data.frame(sga_sta$Station_ID)
rainfall_stations <- data.frame(rainfall_stations[1:10,])
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
rainfall_totals <- rainfall_totals %>% relocate(`Growing.Area`)
colnames(rainfall_totals) <- c("Growing Area", "Station", "Latitude", "Longitude", date_for_table)
rainfall_totals <- rainfall_totals %>% mutate(across(names(rainfall_totals[3]):names(rainfall_totals[5]), as.numeric))
rainfall_totals_sorted <- rainfall_totals %>% arrange(across(ncol(.), desc))

level1 <- which(rainfall_totals_sorted[5] >= 2.5 & rainfall_totals_sorted[5] < 2.6)
level2 <- which(rainfall_totals_sorted[5] >= 2.6 & rainfall_totals_sorted[5] < 2.7)
level3 <- which(rainfall_totals_sorted[5] >= 2.7 & rainfall_totals_sorted[5] < 2.8)
level4 <- which(rainfall_totals_sorted[5] >= 2.8 & rainfall_totals_sorted[5] < 2.9)
level5 <- which(rainfall_totals_sorted[5] >= 2.9 & rainfall_totals_sorted[5] < 3.0)
level6 <- which(rainfall_totals_sorted[5] >= 3)

current_time <- format(Sys.time(), "%H%M")
file_loc <- paste0("Totals/Rainfall_Data_", date_for_file, "_@_", current_time, ".xlsx")

create_wb <- createWorkbook()
addWorksheet(create_wb, sheetName = "Daily Rainfall")
writeData(create_wb, sheet = "Daily Rainfall", x = rainfall_totals_sorted, startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, keepNA = FALSE)

num_style <- createStyle(numFmt = "#,##0.00")
addStyle(create_wb, sheet = "Daily Rainfall", style = num_style, rows = 2:(nrow(rainfall_totals_sorted)+1), cols = 3:5, gridExpand = TRUE, stack = TRUE)

highlight_style_level1 <- createStyle(fgFill = "#FFD900")
highlight_style_level2 <- createStyle(fgFill = "#FFBA0D")
highlight_style_level3 <- createStyle(fgFill = "#FF9C1A")
highlight_style_level4 <- createStyle(fgFill = "#FF7D26")
highlight_style_level5 <- createStyle(fgFill = "#FF5F33")
highlight_style_level6 <- createStyle(fgFill = "#FF4040")

# Conditionally changes the background color of cells depending on how much rainfall was received
if (length(level1) > 0){
  addStyle(create_wb, sheet = "Daily Rainfall", style = highlight_style_level1, rows = level1+1, cols = 5, gridExpand = TRUE, stack = TRUE)
}
if (length(level2) > 0){
  addStyle(create_wb, sheet = "Daily Rainfall", style = highlight_style_level2, rows = level2+1, cols = 5, gridExpand = TRUE, stack = TRUE)
}
if (length(level3) > 0){
  addStyle(create_wb, sheet = "Daily Rainfall", style = highlight_style_level3, rows = level3+1, cols = 5, gridExpand = TRUE, stack = TRUE)
}
if (length(level4) > 0){
  addStyle(create_wb, sheet = "Daily Rainfall", style = highlight_style_level4, rows = level4+1, cols = 5, gridExpand = TRUE, stack = TRUE)
}
if (length(level5) > 0){
  addStyle(create_wb, sheet = "Daily Rainfall", style = highlight_style_level5, rows = level5+1, cols = 5, gridExpand = TRUE, stack = TRUE)
}
if (length(level6) > 0){
  addStyle(create_wb, sheet = "Daily Rainfall", style = highlight_style_level6, rows = level6+1, cols = 5, gridExpand = TRUE, stack = TRUE)
}

nums_center_style <- createStyle(halign = "center", valign = "center")
addStyle(create_wb, sheet = "Daily Rainfall", style = nums_center_style, 
         rows = 1:(nrow(rainfall_totals_sorted)+1), cols = 1:5, gridExpand = TRUE, stack = TRUE)

setColWidths(create_wb, sheet = "Daily Rainfall", cols = 1:5, widths = c(30, 15, 12, 12, 12))

saveWorkbook(create_wb, file = file_loc, overwrite = TRUE)

rainfall_for_kml <- data.frame(precip_amounts)
colnames(rainfall_for_kml) <- c("Station_ID", "Latitude", "Longitude", "Rain")
rainfall_totals_for_kml <- merge(sga_sta, rainfall_for_kml, by = "Station_ID", all = FALSE)
rainfall_totals_for_kml <- rainfall_totals_for_kml %>% relocate(`Growing.Area`)
rainfall_over3 <- rainfall_totals_for_kml %>% filter(Rain <= 3)

if (nrow(rainfall_over3 > 0)){
  SGA <- rainfall_over3$`Growing.Area`
  Station <- rainfall_over3$Station
  Latitude <- rainfall_over3$Latitude
  Longitude <- rainfall_over3$Longitude
  Rainfall <- rainfall_over3$Rain
  
  current_time <- format(Sys.time(), "%H%M")
  file_loc_kml <- paste0("Google_Earth/Emergency_Stations_", date_for_file, "_", current_time, ".kml")
  file_loc_onedrive <- paste0("Alert_Texts/Rainfall_Alert_", date_for_file, "_@_", current_time, ".txt")
  
  rainfall_df <- data.frame(SGA, Station, Latitude, Longitude, Rainfall)
  rainfall_df$Description_Label <- paste0("SGA: ", rainfall_df$SGA, "\n", "\n", "WU Station: ", rainfall_df$Station, "\n", "\n", "Date: ", date_for_table)
  rainfall_df$Rain_Amounts <- paste0(format(rainfall_df$Rainfall, digits = 3), '"')
  rain_points <- rainfall_df %>% st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
    select(Description = Description_Label, Name = Rain_Amounts)
  
  sf::st_write(rain_points, file_loc_kml, append = FALSE)
  
  rainfall_over3$Alert <- paste0(rainfall_over3$Station, " - ", rainfall_over3$Rain, " inches (", rainfall_over3$`Growing.Area`,
                                 " ; ", rainfall_over3$Latitude, ", ", rainfall_over3$Longitude, ")")
  
  alert_message <- c("Extraordinary rainfall levels detected at: ", rainfall_over3$Alert)
  
  writeLines(alert_message, file_loc_onedrive)
}
