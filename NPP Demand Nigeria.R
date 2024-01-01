
# Sub-setting and preparing data for the Algorithm ------------------------

country = "Nigeria"

the_population <- population_ %>% filter(Country == country)

CNCCRR <- Coniferous_Non_coniferous_and_Charcoal_real_real %>% 
  filter(Area == country)
FF <- Food_and_Feed_Lake_Chad %>% 
  filter(Country == country)
LSURR <- Livestock_Unit_real_real_to_check %>% 
  filter(Area == country)
RRRR <- Residual_Residual_real_real %>% 
  filter(Area == country) %>% .[-c(1:5),]
TADMBRR <- Total_Amount_of_dry_matter_burned_realreal %>% 
  filter(Area == country)
EOD <- every_other_data %>% 
  filter(Area == country)

produced <- EOD %>% filter(Element == "Production") %>% 
  .[, c(4, 6, 8, 10, 12)]
prod_crops <- c("Beans, dry", "Cassava, fresh", 
                "Maize (corn)", "Millet", "Potatoes", 
                "Rice", "Sorghum", "Soya beans", 
                "Tomatoes")
dry_matt_carb_cont <- c("Beans, dry", "Cassava", "Maize",
                        "Millet", "Potatoes", "Rice",
                        "Sorghum", "Soybeans", "Tomatoes")
DMCCC_to_use <- 
  Dry_matter_and_carbon_content_of_the_27_types_of_crops %>% 
  filter(Crop %in% dry_matt_carb_cont) %>% 
  replace(is.na(.), 0)
DMCCC_to_use <- cbind(DMCCC_to_use, 
                      the_product = 
                        DMCCC_to_use$`Dry Matter Content (%)` *
                        DMCCC_to_use$`Carbon Content`)
prod_data <- produced %>% filter(Item %in% prod_crops) %>% 
  select(Year, Item, Value)

crop_variation <- EOD %>% filter(Element == "Area harvested") %>% 
  .[, c(4, 6, 8, 10, 12)]

wet_carcass <- EOD %>% filter(Element == "Yield/Carcass Weight") %>% 
  .[, c(4, 6, 8, 10, 12)]

residual_factor <- harvest_factor %>% filter(Crop %in% dry_matt_carb_cont)


crops <- ((FF[-2])[1:37])[, c(1, 8:11, 14:21, 24:27, 34,35)]
# str(FF)
variation_data <- crop_variation %>% 
  filter(Item %in% prod_crops) %>% .[, -c(1,2)]

animals <- c("Horse meat, fresh or chilled", 
             "Meat of cattle with the bone, fresh or chilled",
             "Meat of chickens, fresh or chilled", 
             "Meat of goat, fresh or chilled",
             "Meat of pig with the bone, fresh or chilled",
             "Meat of sheep, fresh or chilled")

carcass_weight <- wet_carcass %>% filter(Item %in% animals) %>%
  .[, -c(1,2)]
# View(TADMBRR)

#--- For individual year
Year_ <- c(2000:2020)
NPP = c("Year", "Food", "Feed", "Residue", "Fuel", "Burned", "Demand")

for(i in c(1:length(Year_))){
  year_ <- Year_[i]
  popul_ <- the_population[i + 1]
  prod_to_use <- prod_data %>% filter(Year == year_)
  crops_to_use <- crops %>% filter(Year == year_)
  variation_to_use <- variation_data %>% filter(Year == year_)
  wet_carcass_weight <- carcass_weight %>% filter(Year == year_)
  resid_fact_to_use <- residual_factor
  
  cassava_ <- (prod_to_use$Value[1] + crops_to_use[1, 2] - 
                 crops_to_use[1, 3] + variation_to_use$Value[1]) * 
    DMCCC_to_use$the_product[2]
  
  # beans_ <- (prod_to_use$Value[1] + crops_to_use[1, 4] - 
  #              crops_to_use[1, 5] + variation_to_use$Value[1]) * 
  #   DMCCC_to_use$the_product[1]
  
  maize_ <- (prod_to_use$Value[2] + crops_to_use[1, 6] - 
               crops_to_use[1, 7] + variation_to_use$Value[2]) * 
    DMCCC_to_use$the_product[3]
  
  millet_ <- (prod_to_use$Value[3] + crops_to_use[1, 8] - 
                crops_to_use[1, 9] + variation_to_use$Value[3]) * 
    DMCCC_to_use$the_product[4]
  
  potatoes_ <- (prod_to_use$Value[4] + crops_to_use[1, 10] - 
                  crops_to_use[1, 11] + variation_to_use$Value[4]) * 
    DMCCC_to_use$the_product[5]
  
  rice_ <- (prod_to_use$Value[5] + crops_to_use[1, 12] - 
              crops_to_use[1, 13] + variation_to_use$Value[5]) * 
    DMCCC_to_use$the_product[6]
  
  sorghum_ <- (prod_to_use$Value[6] + crops_to_use[1, 14] - 
                 crops_to_use[1, 15] + variation_to_use$Value[6]) * 
    DMCCC_to_use$the_product[7]
  
  soybean_ <- (prod_to_use$Value[7] + crops_to_use[1, 16] -
                 crops_to_use[1, 17] + variation_to_use$Value[7]) *
    DMCCC_to_use$the_product[8]

  tomatoes_ <- (prod_to_use$Value[8] + crops_to_use[1, 18] -
                  crops_to_use[1, 19] + variation_to_use$Value[8]) *
    DMCCC_to_use$the_product[9]
  
  
  part_1 <- cassava_ + 
    #beans_ + 
    maize_ + millet_ + potatoes_ + rice_ +
    sorghum_ + soybean_ + tomatoes_ 
  
  part_2 <- sum(wet_carcass_weight$Value * dry_matter_intake$Value) * .45
  
  #------------ NPP FOOD --------
  NPP_food <- part_1 + part_2
  
  
  #------------ NPP FRESIDUE-----
  NPP_residues <- sum(resid_fact_to_use$`Harvest Factor`) * .45
  
  
  
  NCCNCH_to_use <- CNCCRR %>% filter(Year == year_) %>% 
    .[, c(6, 8, 10, 12)]
  
  the_NC <- NCCNCH_to_use %>% filter(Item == "Wood fuel, non-coniferous")
  sum_Wnc <- sum(the_NC$Value)
  the_CN <- NCCNCH_to_use %>% filter(Item == "Wood fuel, coniferous")
  sum_Wcn <- sum(the_CN$Value)
  the_CH <- NCCNCH_to_use %>% filter(Item == "Wood charcoal")
  sum_Wch <- sum(the_CH$Value)
  
  #------------ NPP FUEL --------
  NPP_fuel <- (sum_Wnc * .58 * .45) + (sum_Wcn * .43 * .45) +
    (sum_Wch * .75)
  
  
  TADMBRR_to_use <- TADMBRR %>% filter(Year == year_) %>%
    .[, c(6, 8, 10, 14)]
  # View(TADMBRR_to_use)
  
  #------------ NPP BURNED-------
  NPP_burned <- sum(TADMBRR_to_use$Value) * .45
  NPP_demand <- (NPP_food + NPP_feed + NPP_residues + NPP_fuel + NPP_burned) / 
    popul_
  NPP_return <- c(year_, NPP_food, NPP_feed, NPP_residues,
                  NPP_fuel, NPP_burned, NPP_demand)
  NPP <- cbind(NPP, NPP_return)
}

Nigeria_NPPs <- t(NPP)[, -1]
Nigeria_NPPs <- row_to_names(Nigeria_NPPs, 1, remove_rows_above = T)
rownames(Nigeria_NPPs) <- Year_

View(Nigeria_NPPs)
