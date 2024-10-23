setwd("C:/Users/TOURE/Mes documents/REPOSITORIES/IM_raw_data/IM_level/")

library(tidyverse)

library(flextable)

library(lubridate)



#read_all_files
list_of_files <- list.files(paste0("C:/Users/TOURE/Mes documents/REPOSITORIES/IM_raw_data/IM_level/"), recursive = T, full.names = T)
#lapply()

A <- lapply(list_of_files, function(x){read_csv(x)}) |> 
   bind_rows()
 IM_rep<-A |> 
    select(country = Country, region = Region, district = District, response = Response, vaccine.type=Vaccine.type, roundNumber, start_date, endate_date, year, Number_of_HH_visited, u5_present, u5_FM, missed_child, cv, r_non_FM_Absent, r_non_FM_NC, r_non_FM_hh_notvisited, r_non_FM_hh_notrevisited, r_non_FM_sleep, r_non_FM_vaccinatedRoutine, r_non_FM_hh_Noparent, r_non_FM_hh_distance, r_non_FM_travel, Other_reasons, care_Giver_Informed_SIA, percent_care_Giver_Informed_SIA) |>
     filter(year > 2019,
            response != "NA") |> 
    arrange(start_date)
 write_csv(IM_rep,"C:/Users/TOURE/Mes documents/REPOSITORIES/IM_raw_data/AFRO_IM.csv")
 
 A <- read_csv("C:/Users/TOURE/Mes documents/REPOSITORIES/IM_raw_data/AFRO_IM.csv")
 B<-A |> 
     mutate(
         start_date = case_when(
             response == "Mop_Up" & start_date ==2024-06-05~ as_date("2024-04-28"),
             response == "Revaccination" & start_date ==2024-05-05~ as_date("2024-04-28"),
             response == "Revaccination" & start_date ==2024-05-04~ as_date("2024-04-28"),
             response == "SNID" & start_date ==2024-05-04~ as_date("2024-04-28"),
             response == "LBR-2024-05-01_nOPV" & start_date ==2024-11-06~ as_date("2024-06-11"),
             response == "DRC-NID-03-2024-B_nOPV" & start_date ==2024-11-04~ as_date("2024-04-11"),
             response == "DRC-NID-03-2024-B_nOPV" & start_date ==2024-11-06~ as_date("2024-06-11"),
             response == "ALG-2023-09-01_nOPV" & start_date ==2024-11-02~ as_date("2024-02-11"),
             response == "LBR-2024-05-01_nOPV" & start_date ==2024-10-06~ as_date("2024-06-10"),
             response == "DRC-NID-03-2024-B_nOPV" & start_date ==2024-10-04~ as_date("2024-04-10"),
             response == "SSD-2024-02-01_nOPV2" & start_date ==2024-10-03~ as_date("2024-03-10"),
             response == "ALG-2023-09-01_nOPV" & start_date ==2024-10-02~ as_date("2024-02-10"),
             response == "DRC-NID-01-2024-bOPV" & start_date ==2024-10-02~ as_date("2024-02-10"),
             response == "DRC-NID-03-2024-B_nOPV" & start_date ==2024-09-04~ as_date("2024-04-09"),
             response == "SSD-2024-02-01_nOPV2" & start_date ==2024-09-03~ as_date("2024-03-09"),
             response == "ALG-2023-09-01_nOPV" & start_date ==2024-09-02~ as_date("2024-02-09"),
             response == "DRC-NID-03-2024-B_nOPV" & start_date ==2024-08-04~ as_date("2024-04-08"),
             response == "SSD-2024-02-01_nOPV2" & start_date ==2024-08-03~ as_date("2024-03-08"),
             response == "ALG-2023-09-01_nOPV" & start_date ==2024-08-02~ as_date("2024-02-08"),
             response == "BEN-2023-09-01_nOPV" & start_date ==2024-08-02~ as_date("2024-02-08"),
             response == "DRC-NID-03-2024-B_nOPV" & start_date ==2024-07-04~ as_date("2024-02-07"),
             response == "SSD-2024-02-01_nOPV2" & start_date ==2024-07-03~ as_date("2024-02-07"),
             response == "ALG-2023-09-01_nOPV" & start_date ==2024-07-02~ as_date("2024-02-07"),
             response == "BEN-2023-09-01_nOPV" & start_date ==2024-07-02~ as_date("2024-02-07"),
             response == "OPVb May2021" & country == "MAL" ~ as_date("2021-05-10"),
             TRUE ~ start_date),
         endate_date = case_when(
             response == "LBR-2024-05-01_nOPV" & start_date ==2024-11-06~ as_date("2024-06-11"),
             response == "DRC-NID-03-2024-B_nOPV" & start_date ==2024-11-04~ as_date("2024-04-11"),
             response == "DRC-NID-03-2024-B_nOPV" & start_date ==2024-11-06~ as_date("2024-06-11"),
             response == "ALG-2023-09-01_nOPV" & start_date ==2024-11-02~ as_date("2024-02-11"),
             response == "LBR-2024-05-01_nOPV" & start_date ==2024-10-06~ as_date("2024-06-10"),
             response == "DRC-NID-03-2024-B_nOPV" & start_date ==2024-10-04~ as_date("2024-04-10"),
             response == "SSD-2024-02-01_nOPV2" & start_date ==2024-10-03~ as_date("2024-03-10"),
             response == "ALG-2023-09-01_nOPV" & start_date ==2024-10-02~ as_date("2024-02-10"),
             response == "DRC-NID-01-2024-bOPV" & start_date ==2024-10-02~ as_date("2024-02-10"),
             response == "DRC-NID-03-2024-B_nOPV" & start_date ==2024-09-04~ as_date("2024-04-09"),
             response == "SSD-2024-02-01_nOPV2" & start_date ==2024-09-03~ as_date("2024-03-09"),
             response == "ALG-2023-09-01_nOPV" & start_date ==2024-09-02~ as_date("2024-02-09"),
             response == "DRC-NID-03-2024-B_nOPV" & start_date ==2024-08-04~ as_date("2024-04-08"),
             response == "SSD-2024-02-01_nOPV2" & start_date ==2024-08-03~ as_date("2024-03-08"),
             response == "ALG-2023-09-01_nOPV" & start_date ==2024-08-02~ as_date("2024-02-08"),
             response == "BEN-2023-09-01_nOPV" & start_date ==2024-08-02~ as_date("2024-02-08"),
             response == "DRC-NID-03-2024-B_nOPV" & start_date ==2024-07-04~ as_date("2024-02-07"),
             response == "SSD-2024-02-01_nOPV2" & start_date ==2024-07-03~ as_date("2024-02-07"),
             response == "ALG-2023-09-01_nOPV" & start_date ==2024-07-02~ as_date("2024-02-07"),
             response == "BEN-2023-09-01_nOPV" & start_date ==2024-07-02~ as_date("2024-02-07"),
             TRUE ~ endate_date)) |>  
     filter(district != "NA")
 
 C<-B |> 
     mutate(country =  case_when(
         country =="Ethiopia" ~ "ETH",
         country == "BURKINA_FASO" ~ "BFA",
         country == "BENIN" ~ "BEN",
         country == "CAMEROON" ~ "CAE",
         country == "CHAD" ~ "CHD",
         TRUE ~ country)) 
 D<-C |> 
     mutate(roundNumber =  case_when(
         roundNumber =="RND2" ~ "Rnd2",
         TRUE ~ roundNumber))
 
 
 
 write_csv(D,"C:/Users/TOURE/Mes documents/REPOSITORIES/IM_raw_data/AFRO_IM_data.csv")
 
 



