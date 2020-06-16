library(tidyverse)
library(ggplot2)
library(googlesheets4)
library(googledrive)
library(WriteXLS)
library(scales)
library(lubridate)
library(gganimate)
library(ggrepel)

# DATA IMPORT --------------------------------------------------------------
file_list <- read_csv("backup_csv/file_list.csv")

my_col_names <- c("Sl.No.", "Date", "Taxi_number", "DL_number", "QR_displayed",
                  "Log_book", "Inspection_location", "Updated_by", "Loc_name")


file_1 <- read_sheet(ss = file_list$id[1])
colnames(file_1) <- my_col_names

file_1 <- file_1 %>%
    mutate(loc_name = file_list$Loc_name[1]) %>% 
    select(-Sl.No.) %>% 
    filter(!is.na(Taxi_number)) %>% 
    mutate(Date = as.Date(Date, "%d/%m/%Y")) %>% 
    mutate(Taxi_number = as.character(Taxi_number)) %>% 
    mutate(DL_number = as.character(DL_number)) %>% 
    mutate(QR_displayed = as.character(QR_displayed)) %>% 
    mutate(Log_book = as.character(Log_book)) %>% 
    mutate(Inspection_location = as.character(Inspection_location)) %>% 
    mutate(Updated_by = as.character(Updated_by)) %>% 
    mutate(loc_name = as.character(loc_name))
#.....................................................................
file_2 <- read_sheet(ss = file_list$id[2])
colnames(file_2) <- my_col_names

file_2 <- file_2 %>%
    mutate(loc_name = file_list$Loc_name[2]) %>% 
    select(-Sl.No.) %>% 
    filter(!is.na(Taxi_number)) %>% 
    mutate(Date = as.Date(Date, "%d/%m/%Y")) %>% 
    mutate(Taxi_number = as.character(Taxi_number)) %>% 
    mutate(DL_number = as.character(DL_number)) %>% 
    mutate(QR_displayed = as.character(QR_displayed)) %>% 
    mutate(Log_book = as.character(Log_book)) %>% 
    mutate(Inspection_location = as.character(Inspection_location)) %>% 
    mutate(Updated_by = as.character(Updated_by)) %>% 
    mutate(loc_name = as.character(loc_name))
#.....................................................................
file_3 <- read_sheet(ss = file_list$id[3])
colnames(file_3) <- my_col_names

file_3 <- file_3 %>%
    mutate(loc_name = file_list$Loc_name[3]) %>% 
    select(-Sl.No.) %>% 
    filter(!is.na(Taxi_number)) %>% 
    mutate(DL_number = as.character(DL_number)) %>% 
    mutate(Date = as.Date(Date, "%d/%m/%Y")) %>% 
    mutate(Taxi_number = as.character(Taxi_number)) %>% 
    mutate(DL_number = as.character(DL_number)) %>% 
    mutate(QR_displayed = as.character(QR_displayed)) %>% 
    mutate(Log_book = as.character(Log_book)) %>% 
    mutate(Inspection_location = as.character(Inspection_location)) %>% 
    mutate(Updated_by = as.character(Updated_by)) %>% 
    mutate(loc_name = as.character(loc_name))
# .....................................................................
file_4 <- read_sheet(ss = file_list$id[4])
colnames(file_4) <- my_col_names

file_4 <- file_4 %>%
    mutate(loc_name = file_list$Loc_name[4]) %>% 
    select(-Sl.No.) %>% 
    filter(!is.na(Taxi_number)) %>% 
    mutate(Date = as.Date(Date, "%d-%m-%y")) %>% 
    mutate(Taxi_number = as.character(Taxi_number)) %>% 
    mutate(DL_number = as.character(DL_number)) %>% 
    mutate(QR_displayed = as.character(QR_displayed)) %>% 
    mutate(Log_book = as.character(Log_book)) %>% 
    mutate(Inspection_location = as.character(Inspection_location)) %>% 
    mutate(Updated_by = as.character(Updated_by)) %>% 
    mutate(loc_name = as.character(loc_name))

# .....................................................................

file_5 <- read_sheet(ss = file_list$id[5])
colnames(file_5) <- my_col_names

file_5 <- file_5 %>%
    mutate(loc_name = file_list$Loc_name[5]) %>% 
    select(-Sl.No.) %>% 
    filter(!is.na(Taxi_number)) %>% 
    mutate(Date = as.Date(Date, "%d/%m/%Y")) %>% 
    mutate(Taxi_number = as.character(Taxi_number)) %>% 
    mutate(DL_number = as.character(DL_number)) %>% 
    mutate(QR_displayed = as.character(QR_displayed)) %>% 
    mutate(Log_book = as.character(Log_book)) %>% 
    mutate(Inspection_location = as.character(Inspection_location)) %>% 
    mutate(Updated_by = as.character(Updated_by)) %>% 
    mutate(loc_name = as.character(loc_name))

# .....................................................................

file_6 <- read_sheet(ss = file_list$id[6])
colnames(file_6) <- my_col_names

file_6 <- file_6 %>%
    mutate(loc_name = file_list$Loc_name[6]) %>% 
    select(-Sl.No.) %>% 
    filter(!is.na(Taxi_number)) %>% 
    mutate(Date = as.Date(Date, "%d/%m/%Y")) %>% 
    mutate(Taxi_number = as.character(Taxi_number)) %>% 
    mutate(DL_number = as.character(DL_number)) %>% 
    mutate(QR_displayed = as.character(QR_displayed)) %>% 
    mutate(Log_book = as.character(Log_book)) %>% 
    mutate(Inspection_location = as.character(Inspection_location)) %>% 
    mutate(Updated_by = as.character(Updated_by)) %>% 
    mutate(loc_name = as.character(loc_name))

# .....................................................................
file_7 <- read_sheet(ss = file_list$id[7])
colnames(file_7) <- my_col_names

file_7 <- file_7 %>%
    mutate(loc_name = file_list$Loc_name[7]) %>% 
    select(-Sl.No.) %>%
    filter(!is.na(Taxi_number)) %>% 
    mutate(Date = as.Date(Date, "%d-%B-%Y")) %>% 
    mutate(Taxi_number = as.character(Taxi_number)) %>% 
    mutate(DL_number = as.character(DL_number)) %>% 
    mutate(QR_displayed = as.character(QR_displayed)) %>% 
    mutate(Log_book = as.character(Log_book)) %>% 
    mutate(Inspection_location = as.character(Inspection_location)) %>% 
    mutate(Updated_by = as.character(Updated_by)) %>% 
    mutate(loc_name = as.character(loc_name))

#.....................................................................
file_8 <- read_sheet(ss = file_list$id[8])
colnames(file_8) <- my_col_names

file_8 <- file_8 %>%
    mutate(loc_name = file_list$Loc_name[8]) %>% 
    select(-Sl.No.) %>% 
    filter(!is.na(Taxi_number)) %>% 
    mutate(Date = as.Date(Date, "%d/%m/%Y")) %>% 
    mutate(Taxi_number = as.character(Taxi_number)) %>% 
    mutate(DL_number = as.character(DL_number)) %>% 
    mutate(QR_displayed = as.character(QR_displayed)) %>% 
    mutate(Log_book = as.character(Log_book)) %>% 
    mutate(Inspection_location = as.character(Inspection_location)) %>% 
    mutate(Updated_by = as.character(Updated_by)) %>% 
    mutate(loc_name = as.character(loc_name))
# .....................................................................
file_9 <- read_sheet(ss = file_list$id[9])
colnames(file_9) <- my_col_names

file_9 <- file_9 %>%
    mutate(loc_name = file_list$Loc_name[9]) %>% 
    select(-Sl.No.) %>% 
    filter(!is.na(Taxi_number)) %>% 
    mutate(Date = as.Date(Date, "%d/%m/%Y")) %>% 
    mutate(Taxi_number = as.character(Taxi_number)) %>% 
    mutate(DL_number = as.character(DL_number)) %>% 
    mutate(QR_displayed = as.character(QR_displayed)) %>% 
    mutate(Log_book = as.character(Log_book)) %>% 
    mutate(Inspection_location = as.character(Inspection_location)) %>% 
    mutate(Updated_by = as.character(Updated_by)) %>% 
    mutate(loc_name = as.character(loc_name))
# .....................................................................
file_10 <- read_sheet(ss = file_list$id[10])
colnames(file_10) <- my_col_names

file_10 <- file_10 %>%
    mutate(loc_name = file_list$Loc_name[10]) %>% 
    select(-Sl.No.) %>% 
    filter(!is.na(Taxi_number)) %>% 
    mutate(Date = as.Date(Date, "%d-%m-%Y")) %>% 
    mutate(Taxi_number = as.character(Taxi_number)) %>% 
    mutate(DL_number = as.character(DL_number)) %>% 
    mutate(QR_displayed = as.character(QR_displayed)) %>% 
    mutate(Log_book = as.character(Log_book)) %>% 
    mutate(Inspection_location = as.character(Inspection_location)) %>% 
    mutate(Updated_by = as.character(Updated_by)) %>% 
    mutate(loc_name = as.character(loc_name))
# .....................................................................
file_11 <- read_sheet(ss = file_list$id[11])
colnames(file_11) <- my_col_names

file_11 <- file_11 %>%
    mutate(loc_name = file_list$Loc_name[11]) %>% 
    select(-Sl.No.) %>% 
    filter(!is.na(Taxi_number)) %>% 
    mutate(Date = as.Date(Date, "%d/%m/%Y")) %>% 
    mutate(Taxi_number = as.character(Taxi_number)) %>% 
    mutate(DL_number = as.character(DL_number)) %>% 
    mutate(QR_displayed = as.character(QR_displayed)) %>% 
    mutate(Log_book = as.character(Log_book)) %>% 
    mutate(Inspection_location = as.character(Inspection_location)) %>% 
    mutate(Updated_by = as.character(Updated_by)) %>% 
    mutate(loc_name = as.character(loc_name))
# .....................................................................
file_12 <- read_sheet(ss = file_list$id[12])
colnames(file_12) <- my_col_names

file_12 <- file_12 %>%
    mutate(loc_name = file_list$Loc_name[12]) %>% 
    select(-Sl.No.) %>% 
    filter(!is.na(Taxi_number)) %>% 
    mutate(Date = as.Date(Date, "%d/%m/%Y")) %>% 
    mutate(Taxi_number = as.character(Taxi_number)) %>% 
    mutate(DL_number = as.character(DL_number)) %>% 
    mutate(QR_displayed = as.character(QR_displayed)) %>% 
    mutate(Log_book = as.character(Log_book)) %>% 
    mutate(Inspection_location = as.character(Inspection_location)) %>% 
    mutate(Updated_by = as.character(Updated_by)) %>% 
    mutate(loc_name = as.character(loc_name))
# .....................................................................
file_13 <- read_sheet(ss = file_list$id[13])
colnames(file_13) <- my_col_names

file_13 <- file_13 %>%
    mutate(loc_name = file_list$Loc_name[13]) %>% 
    select(-Sl.No.) %>% 
    filter(!is.na(Taxi_number)) %>% 
    mutate(Date = as.Date(Date, "%d/%m/%Y")) %>% 
    mutate(Taxi_number = as.character(Taxi_number)) %>% 
    mutate(DL_number = as.character(DL_number)) %>% 
    mutate(QR_displayed = as.character(QR_displayed)) %>% 
    mutate(Log_book = as.character(Log_book)) %>% 
    mutate(Inspection_location = as.character(Inspection_location)) %>% 
    mutate(Updated_by = as.character(Updated_by)) %>% 
    mutate(loc_name = as.character(loc_name))
# .....................................................................
file_14 <- read_sheet(ss = file_list$id[14])
colnames(file_14) <- my_col_names

file_14 <- file_14 %>%
    mutate(loc_name = file_list$Loc_name[14]) %>% 
    select(-Sl.No.) %>% 
    filter(!is.na(Taxi_number)) %>% 
    mutate(Date = as.Date(Date, "%d/%m/%Y")) %>% 
    mutate(Taxi_number = as.character(Taxi_number)) %>% 
    mutate(DL_number = as.character(DL_number)) %>% 
    mutate(QR_displayed = as.character(QR_displayed)) %>% 
    mutate(Log_book = as.character(Log_book)) %>% 
    mutate(Inspection_location = as.character(Inspection_location)) %>% 
    mutate(Updated_by = as.character(Updated_by))
#.....................................................................
file_15 <- read_sheet(ss = file_list$id[15])
colnames(file_15) <- my_col_names

file_15 <- file_15 %>% 
    filter(!is.na(Taxi_number)) %>% 
    mutate(loc_name = file_list$Loc_name[15]) %>% 
    select(-Sl.No.) %>% 
    filter(!is.na(Taxi_number)) %>% 
    mutate(Date = as.Date(Date, "%d/%m/%Y")) %>% 
    mutate(Taxi_number = as.character(Taxi_number)) %>% 
    mutate(DL_number = as.character(DL_number)) %>% 
    mutate(QR_displayed = as.character(QR_displayed)) %>% 
    mutate(Log_book = as.character(Log_book)) %>% 
    mutate(Inspection_location = as.character(Inspection_location)) %>% 
    mutate(Updated_by = as.character(Updated_by)) %>% 
    mutate(loc_name = as.character(loc_name))
#.....................................................................
file_16 <- read_sheet(ss = file_list$id[16])
colnames(file_16) <- my_col_names

file_16 <- file_16 %>%
    mutate(loc_name = file_list$Loc_name[16]) %>% 
    select(-Sl.No.) %>% 
    filter(!is.na(Taxi_number)) %>% 
    mutate(Date = as.Date(Date, "%d/%m/%Y")) %>% 
    mutate(Taxi_number = as.character(Taxi_number)) %>% 
    mutate(DL_number = as.character(DL_number)) %>% 
    mutate(QR_displayed = as.character(QR_displayed)) %>% 
    mutate(Log_book = as.character(Log_book)) %>% 
    mutate(Inspection_location = as.character(Inspection_location)) %>% 
    mutate(Updated_by = as.character(Updated_by)) %>% 
    mutate(loc_name = as.character(loc_name))
#.....................................................................
file_17 <- read_sheet(ss = file_list$id[17])
colnames(file_17) <- my_col_names

file_17 <- file_17 %>%
    mutate(loc_name = file_list$Loc_name[17]) %>% 
    select(-Sl.No.) %>% 
    filter(!is.na(Taxi_number)) %>% 
    mutate(Date = as.Date(Date, "%d/%m/%Y")) %>% 
    mutate(Taxi_number = as.character(Taxi_number)) %>% 
    mutate(DL_number = as.character(DL_number)) %>% 
    mutate(QR_displayed = as.character(QR_displayed)) %>% 
    mutate(Log_book = as.character(Log_book)) %>% 
    mutate(Inspection_location = as.character(Inspection_location)) %>% 
    mutate(Updated_by = as.character(Updated_by)) %>% 
    mutate(loc_name = as.character(loc_name))
# .....................................................................
file_18 <- read_sheet(ss = file_list$id[18])
colnames(file_18) <- my_col_names

file_18 <- file_18 %>%
    mutate(loc_name = file_list$Loc_name[18]) %>% 
    select(-Sl.No.) %>% 
    filter(!is.na(Taxi_number)) %>% 
    mutate(Date = as.Date(Date, "%d/%m/%y")) %>% 
    mutate(Taxi_number = as.character(Taxi_number)) %>% 
    mutate(DL_number = as.character(DL_number)) %>% 
    mutate(QR_displayed = as.character(QR_displayed)) %>% 
    mutate(Log_book = as.character(Log_book)) %>% 
    mutate(Inspection_location = as.character(Inspection_location)) %>% 
    mutate(Updated_by = as.character(Updated_by)) %>% 
    mutate(loc_name = as.character(loc_name))
#.....................................................................
file_19 <- read_sheet(ss = file_list$id[19])
colnames(file_19) <- my_col_names

file_19 <- file_19 %>%
    mutate(loc_name = file_list$Loc_name[19]) %>% 
    select(-Sl.No.) %>% 
    filter(!is.na(Taxi_number)) %>% 
    mutate(Date = as.Date(Date, "%d/%m/%Y")) %>% 
    mutate(Taxi_number = as.character(Taxi_number)) %>% 
    mutate(DL_number = as.character(DL_number)) %>% 
    mutate(QR_displayed = as.character(QR_displayed)) %>% 
    mutate(Log_book = as.character(Log_book)) %>% 
    mutate(Inspection_location = as.character(Inspection_location)) %>% 
    mutate(Updated_by = as.character(Updated_by)) %>% 
    mutate(loc_name = as.character(loc_name))
#.....................................................................
file_20 <- read_sheet(ss = file_list$id[20])
colnames(file_20) <- my_col_names

file_20 <- file_20 %>%
    mutate(loc_name = file_list$Loc_name[20]) %>% 
    select(-Sl.No.) %>% 
    filter(!is.na(Taxi_number)) %>% 
    mutate(Date = as.Date(Date, "%d/%m/%Y")) %>% 
    mutate(Taxi_number = as.character(Taxi_number)) %>% 
    mutate(DL_number = as.character(DL_number)) %>% 
    mutate(QR_displayed = as.character(QR_displayed)) %>% 
    mutate(Log_book = as.character(Log_book)) %>% 
    mutate(Inspection_location = as.character(Inspection_location)) %>% 
    mutate(Updated_by = as.character(Updated_by)) %>% 
    mutate(loc_name = as.character(loc_name))
#.....................................................................
file_21 <- read_sheet(ss = file_list$id[21])
colnames(file_21) <- my_col_names

file_21 <- file_21 %>%
    mutate(loc_name = file_list$Loc_name[21]) %>% 
    select(-Sl.No.) %>% 
    filter(!is.na(Taxi_number)) %>% 
    mutate(Date = as.Date(Date, "%d/%m/%Y")) %>%
    mutate(Taxi_number = as.character(Taxi_number)) %>% 
    mutate(DL_number = as.character(DL_number)) %>% 
    mutate(QR_displayed = as.character(QR_displayed)) %>% 
    mutate(Log_book = as.character(Log_book)) %>% 
    mutate(Inspection_location = as.character(Inspection_location)) %>% 
    mutate(Updated_by = as.character(Updated_by)) %>% 
    mutate(loc_name = as.character(loc_name))
#.....................................................................
file_22 <- read_sheet(ss = file_list$id[22])
colnames(file_22) <- my_col_names

file_22 <- file_22 %>%
    mutate(loc_name = file_list$Loc_name[22]) %>% 
    select(-Sl.No.) %>% 
    filter(!is.na(Taxi_number)) %>% 
    mutate(Date = as.Date(Date, "%d/%m/%Y")) %>% 
    mutate(Taxi_number = as.character(Taxi_number)) %>% 
    mutate(DL_number = as.character(DL_number)) %>% 
    mutate(QR_displayed = as.character(QR_displayed)) %>% 
    mutate(Log_book = as.character(Log_book)) %>% 
    mutate(Inspection_location = as.character(Inspection_location)) %>% 
    mutate(Updated_by = as.character(Updated_by)) %>% 
    mutate(loc_name = as.character(loc_name))
#.....................................................................

druk_trace_master <- bind_rows(file_1, file_2, file_3, file_3, file_4, 
                               file_5, file_6, file_7, file_8, file_9, 
                               file_10, file_11, file_12, file_13, file_14, 
                               file_15, file_16, file_17, file_18, file_19, 
                               file_20, file_21, file_22)

# bind_data_1 <- bind_rows(file_1, file_2, file_3, file_4, file_5)
# bind_data_2 <- bind_rows(file_6, file_7, file_8, file_9, file_10)
# bind_data_3 <- bind_rows(file_11, file_12, file_13, file_14, file_15)
# bind_data_4 <- bind_rows(file_16, file_17, file_18, file_19, file_20)
# bind_data_5 <- bind_rows(file_21, file_22)

druk_trace_master <- druk_trace_master %>% 
    mutate(QR_displayed = ifelse(QR_displayed == "yes", "YES", QR_displayed)) %>% 
    mutate(QR_displayed = ifelse(QR_displayed == "Yes", "YES", QR_displayed)) %>% 
    mutate(QR_displayed = ifelse(QR_displayed == "no", "NO", QR_displayed)) %>% 
    mutate(QR_displayed = ifelse(QR_displayed == "No", "NO", QR_displayed))

WriteXLS(druk_trace_master, paste("excel_output/", paste(Sys.Date(), "daily_backup.xlsx", sep = "_"), sep = ""))

druk_trace_master %>% 
    filter(is.na(QR_displayed))

druk_trace_grouped <- druk_trace_master %>% 
    group_by(loc_name, QR_displayed) %>% 
    summarise(number = n())

druk_trace_grouped %>% 
    ggplot(aes(x = reorder(loc_name, -number), y = number, 
               fill = QR_displayed)) +
    geom_bar(stat = "identity", alpha = 0.5) +
    coord_flip() +
    labs(y = "Number of taxis inspected",
         x = "Dzongkhag",
         fill = "QR code displayed?",
         title = "Inspection for Druk Trace app implementation in taxis",
         subtitle =  paste("Total number of taxi inspected = ", 
                           sum(druk_trace_grouped$number), sep = " "),
         caption = paste("Generated on ", Sys.Date(), sep = " ")) +
    theme_minimal() +
    theme(axis.text = element_text(size = 11, family = "Times"),
          axis.title = element_text(size = 12, family = "Times"),
          plot.title = element_text(size = 15, family = "Times", face = "bold", hjust = 0.5), 
          plot.subtitle = element_text(size = 13, family = "Times", hjust = 0.5),
          plot.caption = element_text(size = 10, family = "Times", hjust = 0.95))

ggsave(paste("image_output/", 
             paste(Sys.Date(),
                   "Druk trace inspection detail.jpg", 
                   sep = "_"),
             sep = ""), width = 25, height = 15, units = "cm")

write_csv(druk_trace_grouped, paste("excel_output/", 
                                    paste(Sys.Date(), "master_file.csv", 
                                          sep = "_"),
                                    sep = "")
          )

# Daily progress ----------------------------------------------------------

list_1 <- file_list$Loc_name
list_2 <- druk_trace_grouped$loc_name
(insp_not_conducted <- list_1[!(list_1 %in% list_2)])

daily_summary <- read_csv("backup_csv/daily_summary.csv")
glimpse(daily_summary)

today_summary <- tibble(date = Sys.Date(),
                        total_taxis_insp = sum(druk_trace_grouped$number))

daily_summary <- rbind(daily_summary, today_summary)

daily_summary <- daily_summary %>% 
    mutate(date = as.POSIXct(date))

write_csv(daily_summary, "backup_csv/daily_summary.csv")

WriteXLS::WriteXLS(daily_summary, "excel_output/daily_summary.xlsx")

glimpse(daily_summary)

daily_summary %>% 
    ggplot(aes(x = date, y = total_taxis_insp)) +
    geom_line(color = "#A9CCE3", size = 1.5) +
    geom_point(color = "#2471A3", size = 3) +
    theme_minimal() +
    ylim(0, 2500) +
    labs(x = "Date",
         y = "No. of taxis inspection",
         title = "Daily progress of Druk Trace app") +
    scale_x_datetime(date_breaks = "1 day", date_labels = "%d-%b") +
    theme(axis.title = element_text(family = "Times", size = 12),
          axis.text = element_text(family = "Times", size = 10),
          plot.title = element_text(family = "Times", size = 14, 
                                    face = "bold", hjust = 0.5), 
          axis.text.x = element_text(angle = 15))

ggsave("daily_progress_QR.jpg", width = 25, height = 15, units = "cm")

# Dzongkhag-wise overview --------------------------------------------------------

daily_DT <- druk_trace_master %>% 
    group_by(Date, loc_name) %>% 
    summarise(number = n())

druk_trace_master %>% 
    filter(is.na(Date)) %>% 
    View()

daily_DT %>% 
    ggplot(aes(x = Date, y = number)) +
    geom_line(color = "#A9CCE3") +
    geom_point(color = "#2471A3") +
    labs(x = "Date",
         y = "Number of taxis",
         title = "Dzongkhag-wise progress of Druk Trace app implementation in taxis") +
    facet_wrap(loc_name~., ncol = 3) +
    theme(axis.text = element_text(size = 10, family = "Times"),
          axis.title = element_text(size = 12, family = "Times"),
          plot.title = element_text(size = 14, family = "Times", face = "bold", hjust = 0.5)) 
ggsave(paste("image_output/", 
             paste(Sys.Date(), "Druk_Trace_Overview.jpg", sep = "_"),
             sep = ""),
             width = 25, height = 15, units = "cm")

# Animation ---------------------------------------------------------------
daily_summary <- read_csv("backup_csv/daily_summary.csv")

animate_plot <- daily_summary %>% 
    ggplot(aes(x = date, y = total_taxis_insp)) +
    geom_line(color = "#D4E6F1", size = 1.5) +
    geom_point(color = "#2980B9", size = 3) +
    labs(x = "Date",
         y = "Number of taxis inspected",
         title = "Daily progress of Druk Trace app in taxis") +
    ylim(0, 2500) +
    theme_light() +
    scale_x_datetime(date_breaks = "1 day", date_labels = "%d-%b") +
    theme(axis.title = element_text(family = "Times", size = 10),
          axis.text = element_text(family = "Times", size = 10),
          plot.title = element_text(family = "Times", size = 12, 
                                    face = "bold", hjust = 0.5), 
          axis.text.x = element_text(angle = 15))+
    geom_label_repel(aes(label = round(daily_summary$total_taxis_insp, digits = 0)), label.size = 0)+
    transition_time(date) +
    ease_aes('linear') +
    geom_point(aes(group = seq_along(date))) +
    transition_reveal(date)

animate(animate_plot, nframes = 45, width = 900, height = 600, fps = 10)
anim_save("my_gif.gif")
