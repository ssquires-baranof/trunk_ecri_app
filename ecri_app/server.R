library(shiny)
library(terra)
library(reactablefmtr)
library(sf)
library(tibble)
library(stringr)
library(highcharter)
library(lubridate)
library(dplyr)
library(tidyr)
library(htmlwidgets)

library(readxl)
library(dplyr)
library(lubridate)

sf_bin <- data.frame(
  lower = c(0,39,60,80,120,240,400),
  upper = c(39, 60, 80, 120, 240, 400, 10000),
  sf_bin_name = c("0-39", "39.01-60", "60.01-80", "80.01-120", "120.01-240", "240-400", "400-10000")
)

unit_attribute_codes <- data.frame(
  unit_attribute_code = c(
    "NOP", "CDN", "CDNM", "NON", "CEN", "CENM", "CDE", "CDNU", "CDP", "CEE", "CEP",
    "CIN", "CIP", "CON", "CONM", "COP", "NDN", "NEE", "NEN", "NENM", "CEPH", "CEPM",
    "CEPU", "CEPW", "CEPR", "CEPB", "CENH", "CEY", "CDY", "CDXV", "CDNV"
  ),
  
  unit_description = c(
    "Drive Up", "Climate Downstairs", "Climate Downstairs", "Drive Up", "Climate Upstairs", 
    "Climate Upstairs", "Climate Downstairs", "Climate Downstairs", "Climate Downstairs", 
    "Climate Upstairs", "Climate Upstairs", "Climate Indoor Drive Up", "Climate Indoor Drive Up", 
    "Climate Drive Up", "Climate Drive Up", "Climate Drive Up", "Non Climate Downstairs", 
    "Non Climate Upstairs", "Non Climate Upstairs", "Non Climate Upstairs", 
    "Climate Upstairs", "Climate Upstairs", "Climate Upstairs", "Climate Upstairs", 
    "Climate Upstairs", "Climate Upstairs", "Climate Upstairs", "Locker", "Locker", 
    "Wine Locker", "Wine Storage"
  )
  
)

premium_buckets <- data.frame(
  low = c(-100000, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
  high = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1, 10000000),
  premium_bucket = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
  
)

occupancy_buckets <- data.frame(
  low = c(0, .5, .75, .9),
  high = c(.5, .75, .9, 1),
  occupancy_bucket = c(1, 2, 3, 4)
)

unit_type_pct_buckets <- data.frame(
  low = c(0, .05, .1, .15, .2),
  high = c(.05, .1, .15, .2, 1),
  unit_type_bucket = c(1, 2, 3, 4, 5)
)

sf_by <- join_by(between(sf, lower, upper))

unit_type_rr_calc <- function(df, prob) {
  
  df<- df |>
    group_by(unit_description, sf_bin_name) |>
    summarise(top_quartile_rate = quantile(rent_psf, probs = prob, na.rm = TRUE))
  
  return(df)
}

move_in_rate_calc <- function(df, months) {
  
  df <- df |>
    filter(los <=months) |>
    group_by(sf_bin_name) |>
    summarise(
      mean_move_in_rent_psf = mean(rent_psf, na.rm=T)) 
  
  return(df)
}

premium_bucket_by <- join_by(between(in_place_premium, low, high)) 
rr_percentiles_by <- join_by(between(ActualRent, rate_low, rate_high))

ecri_cap <- 80

rr_joiner <- function(rr_df, unit_type_rr_df, move_in_rate_df, premium_buckets_df, occ_df, rr_percentiles_df, ecri_cap_input) {
  
  rr_df <- rr_df |> left_join(unit_type_rr_df, by = c("unit_description", "sf_bin_name")) |>
    left_join(move_in_rate_df, by = "sf_bin_name") |>
    mutate(in_place_premium = (rent_psf - mean_move_in_rent_psf) / mean_move_in_rent_psf ) |>
    left_join(premium_buckets_df, premium_bucket_by) |>
    left_join(occ_df |> select(`Unit Type`, unit_type_occ_sf_rate, occupancy_bucket, unit_type_pct, unit_type_bucket), by = c("UnitType" = "Unit Type")) |>
    left_join(rr_percentiles_df, rr_percentiles_by) |>
    mutate(
      ecri_cap = ecri_cap_input,
      potential_ecri = (top_quartile_rate - rent_psf) * ecri_multiplier * sf,
      ecri = case_when(
        rent_psf > top_quartile_rate ~ 0,
        TRUE ~ pmin(ecri_cap, potential_ecri)
      ),
      ecri_pct = ecri / ActualRent,
      los_bin = case_when(
        los < 6 ~ "0 to < 6 Months",	
        los >= 6 & los < 12 ~ "6 to < 12 Months",	
        los >= 12 & los < 18 ~ "12 to < 18 Months",	
        los >= 18 & los < 24 ~ "18 to < 24 Months",
        los >= 24 ~ "24+ Months",
        TRUE ~ "Other"
      )
    )
  
  return(rr_df)
}

ecri_premium_bucket_calc <- function(rr_joined_filter_df, premium_buckets_df) {
  df <- rr_joined_filter_df |>
    group_by(premium_bucket) |>
    summarise(mean_ecri_pct = mean(ecri_pct, na.rm=T),
              ecri_count = n()) |>
    right_join(premium_buckets_df, by = "premium_bucket") |>
    arrange(premium_bucket)
  
  return(df)
}

los_bins <- data.frame(los_bin = c("0 to < 6 Months",	"6 to < 12 Months",	
                                   "12 to < 18 Months",	"18 to < 24 Months","24+ Months"))


los_ecri_calc <- function(rr_joined_filter_df, los_bins_df) {
  
  los_ecri_df <- rr_joined_filter_df |>
    group_by(los_bin) |>
    summarise(mean_ecri_pct = mean(ecri_pct, na.rm=T),
              ecri_count = n()) |>
    right_join(los_bins, by = "los_bin")
  
  return(los_ecri_df)
}





function(input, output, session) {
  
  output$premium_buckets <- renderReactable({
    sticky_style <- list(backgroundColor = "#f7f7f7")
    
    reactable(premium_buckets) 
  })
  
  output$sf_buckets <- renderReactable({
    sticky_style <- list(backgroundColor = "#f7f7f7")
    
    reactable(sf_bin) 
  })
  
  rv <- reactiveValues(
    
    rr = NULL,
    occ = NULL,
    unit_type_occ = NULL,
    unit_type_rr = NULL,
    move_in_rate = NULL,
    rr_percentiles = NULL,
    ecri_premium_buckets = NULL,
    los_ecri = NULL,
    current_in_place_rate_psf = NULL,
    projected_in_place_rate_psf = NULL
    
  )
  
  observeEvent(input$rr, {

    req(input$rr)
    
    rr_path <- input$rr$datapath
    
    read.csv(rr_path)
    
    rv$rr <- read.csv(rr_path) |>
      mutate(
        sf = 
          as.numeric(sub("X.*", "", toupper(UnitType))) * 
          as.numeric(gsub("[^0-9.]", "", sub(".*X", "", toupper(UnitType)))),
        unit_attribute_code = sub(".*-", "", toupper(UnitType)),
        rent_psf = ActualRent / sf,
        MoveInDate = as_date(mdy_hms(MoveInDate)),
        los = as.numeric(Sys.Date() - MoveInDate) / 30.44,
      ) |> left_join(sf_bin, sf_by) |>
      left_join(unit_attribute_codes, by = "unit_attribute_code")
    
    rv$rr_percentiles <- data.frame(
      percentile_low = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9),
      percentile_high = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
      ecri_multiplier = c(1.22, 1.17, 1.12, 1.07, 1.02, 0.97, 0.92, 0.87, 0.82, 0.77)
    ) |> 
      mutate(
        rate_low = quantile(rv$rr$ActualRent, percentile_low),
        rate_high = quantile(rv$rr$ActualRent, percentile_high)-.01
      )
    
    rv$unit_type_rr <- unit_type_rr_calc(rv$rr, .7)
    
    rv$move_in_rate <- move_in_rate_calc(rv$rr, 3)
    
    
    output$rr <- renderReactable({
      sticky_style <- list(backgroundColor = "#f7f7f7")
      
      reactable(rv$rr)
      
    })
    
    output$unit_type_rr <- renderReactable({
      sticky_style <- list(backgroundColor = "#f7f7f7")
      
      reactable(rv$unit_type_rr)
      
    })
    
    output$move_in_rate <- renderReactable({
      sticky_style <- list(backgroundColor = "#f7f7f7")
      
      reactable(rv$move_in_rate)
      
    })
    
    output$rr_percentiles <- renderReactable({
      sticky_style <- list(backgroundColor = "#f7f7f7")
      
      reactable(rv$rr_percentiles)
      
    })
    
    rv$current_in_place_rate_psf = ( sum(rv$rr$ActualRent, na.rm = T) ) /
      sum(rv$rr$sf, na.rm=T)
    
    output$current_in_place_rate_psf <- renderUI({
      paste0("$", scales::label_comma(accuracy = 0.01)(rv$current_in_place_rate_psf))
    })
    
    }
  )
  
  observeEvent(input$occ, {
    
    req(input$occ)
    
    occ_path <- input$occ$datapath
    
    rv$occ <- read_excel(occ_path, skip = 3)
    #rv$occ$facility <- rv$occ[1,1]$`Unit Type`
    rv$occ <- rv$occ[-1,]
    rv$occ <- rv$occ |> filter(! `Unit Type` %in% c("Totals", "Grand Totals") ) |>
      mutate(
        sf = 
          as.numeric(sub("X.*", "", toupper(`Unit Type`))) * 
          as.numeric(gsub("[^0-9.]", "", sub(".*X", "", toupper(`Unit Type`)))),
        unit_attribute_code = sub(".*-", "", toupper(`Unit Type`)),
        Total = as.numeric(as.numeric(Total)),
        total_sf = sf * Total,
        occ_sf = sf * Occ,
        occ_sf_rate = occ_sf / total_sf
      ) |> left_join(sf_bin, sf_by) |>
      left_join(unit_attribute_codes, by = "unit_attribute_code")
    
    rv$unit_type_occ <- rv$occ |>
      group_by(unit_description, sf_bin_name) |>
      summarise(unit_type_total_sf = sum(total_sf, na.rm=T),
                unit_type_occ_sf = sum(occ_sf, na.rm=T),
                total_units_unit_type = sum(Total, na.rm=T)) |>
      ungroup() |>
      mutate(
        total_units_building = sum(total_units_unit_type, na.rm=T),
        unit_type_occ_sf_rate = unit_type_occ_sf / unit_type_total_sf,
        unit_type_pct = total_units_unit_type / total_units_building)
    
    occupancy_bucket_by <- join_by(between(unit_type_occ_sf_rate, low, high))
    build_mix_bucket_by <- join_by(between(unit_type_pct, low, high))
    
    rv$occ <- rv$occ |>
      left_join(rv$unit_type_occ, by = c("unit_description", "sf_bin_name"))|>
      left_join(occupancy_buckets, occupancy_bucket_by) |>
      left_join(unit_type_pct_buckets, build_mix_bucket_by) 
    
    output$occ <- renderReactable({
      sticky_style <- list(backgroundColor = "#f7f7f7")
      
      reactable(rv$occ)
      
    })
    
    }
  )
  
  
  observeEvent(list(rv$rr, rv$occ),{
    
    req(rv$rr)
    req(rv$occ)
  
  rv$rr_joined <- rr_joiner(rv$rr, rv$unit_type_rr, rv$move_in_rate, premium_buckets, rv$occ, rv$rr_percentiles, ecri_cap)
  }
  )
  
  
  observeEvent(input$vt, {
    
    req(input$vt)
    
    vt_path <- input$vt$datapath
    
    rv$vt <- read.csv(vt_path) |>
      mutate(UnitNo = sub('"', "", sub(".* ", "", toupper(Unit.No))))
    }
  )
  
  observeEvent(list(rv$rr, rv$occ, rv$vt),{
    
    req(rv$rr)
    req(rv$occ)
    req(rv$vt)
  
  rv$rr_joined_filter <- rv$rr_joined |> filter(UnitNo %in% rv$vt$UnitNo)
  
  output$rr_joined_filter <- renderReactable({
    sticky_style <- list(backgroundColor = "#f7f7f7")
    
    reactable(rv$rr_joined_filter)
    
  })
  
  
  rv$ecri_premium_buckets <- ecri_premium_bucket_calc(rv$rr_joined_filter, premium_buckets)
  
  output$ecri_premium_buckets <- renderReactable({
    sticky_style <- list(backgroundColor = "#f7f7f7")
    
    reactable(rv$ecri_premium_buckets)
    
  })
  
  rv$los_ecri <-  los_ecri_calc(rv$rr_joined_filter, los_bins)
  
  output$los_ecri <- renderReactable({
    sticky_style <- list(backgroundColor = "#f7f7f7")
    
    reactable(rv$los_ecri)
    
  })
  
  
  rv$projected_in_place_rate_psf = ( sum(rv$rr_joined_filter$ecri, na.rm = T) + 
                                       sum(rv$rr$ActualRent, na.rm = T) ) /
    sum(rv$rr$sf, na.rm=T)
  
  output$projected_in_place_rate_psf <- renderUI({
    paste0("$", scales::label_comma(accuracy = 0.01)(rv$projected_in_place_rate_psf))
  })
  
  })
  
      
    
  

}
