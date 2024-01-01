library(readxl)
library(tidyverse)
library(janitor)

# Loading data into the environment ---------------------------------------

Coniferous_Non_coniferous_and_Charcoal_real_real <- 
  as.data.frame(read_excel(
    "Coniferous Non coniferous and Charcoal real real.xlsx"))

Coniferous_Non_coniferous_and_Charcoal_real_real[is.na(
  Coniferous_Non_coniferous_and_Charcoal_real_real)] <- 0


Dry_matter_and_carbon_content_of_the_27_types_of_crops <- 
  as.data.frame(read_excel(
    "Dry matter and carbon content of the 27 types of crops.xlsx"))
Dry_matter_and_carbon_content_of_the_27_types_of_crops[is.na(
  Dry_matter_and_carbon_content_of_the_27_types_of_crops)] <- 0

Dry_matter_and_carbon_content_of_the_27_types_of_crops$`Dry Matter Content (%)` <- 
  (Dry_matter_and_carbon_content_of_the_27_types_of_crops$`Dry Matter Content (%)`)/100

Dry_matter_and_carbon_content_of_the_27_types_of_crops$`Carbon Content` <- 
  (Dry_matter_and_carbon_content_of_the_27_types_of_crops$`Carbon Content`)/100
View(Dry_matter_and_carbon_content_of_the_27_types_of_crops)

Food_and_Feed_Aral_Sea <- 
  as.data.frame(read_excel("Food and Feed Aral Sea.xlsx"))
Food_and_Feed_Aral_Sea[is.na(
  Food_and_Feed_Aral_Sea)] <- 0

for(i in 1:length(Food_and_Feed_Aral_Sea$Country)){
  if(Food_and_Feed_Aral_Sea$Country[i] == 'Tajikstan'){
    Food_and_Feed_Aral_Sea$Country[i] <- 'Tajikistan'
  }else if(Food_and_Feed_Aral_Sea$Country[i] == 'Turkemanistan'){
    Food_and_Feed_Aral_Sea$Country[i] <- "Turkmenistan"
  }
}


Food_and_Feed_Lake_Chad <- 
  as.data.frame(read_excel("Food and Feed Lake Chad.xlsx"))
Food_and_Feed_Lake_Chad[is.na(
  Food_and_Feed_Lake_Chad)] <- 0


Livestock_Unit_real_real_to_check <- 
  as.data.frame(read_excel("Livestock Unit real real to check.xlsx"))
Livestock_Unit_real_real_to_check[is.na(
  Livestock_Unit_real_real_to_check)] <- 0


Tropical_Livestock_Unit_and_Annual_Dry_Matter <- 
  as.data.frame(read_excel(
    "Tropical livestock unit and the annual amount of dry matter.xlsx"))
Tropical_Livestock_Unit_and_Annual_Dry_Matter[is.na(
  Tropical_Livestock_Unit_and_Annual_Dry_Matter)] <- 0


Residual_Residual_real_real <- 
  as.data.frame(read_excel("Residual Residual real real.xlsx"))
Residual_Residual_real_real[is.na(
  Residual_Residual_real_real)] <- 0


Total_Amount_of_dry_matter_burned_realreal <- 
  as.data.frame(read_excel("Total Amount of dry matter burned realreal.xlsx"))
Total_Amount_of_dry_matter_burned_realreal[is.na(
  Total_Amount_of_dry_matter_burned_realreal)] <- 0


every_other_data <- 
  as.data.frame(read_excel("NPP FAO/NPP FAO/FAOSTAT_data_en_10-3-2023.xls"))
for(i in c(3, 5, 7, 9, 10, 12)){
  every_other_data[, i] <- 
    as.numeric(every_other_data[, i])
}

every_other_data[is.na(
  every_other_data)] <- 0

population_ <- as.data.frame(read_excel("Population data.xlsx"))

rows <- colnames(population_)
columns <- population_$Country

population <- population_ %>% .[, -1] %>% as.matrix() %>% t()
colnames(population) <- columns

harvest_factor <- as.data.frame(read_excel("Harvest Factor.xlsx"))

head(Coniferous_Non_coniferous_and_Charcoal_real_real)
head(Dry_matter_and_carbon_content_of_the_27_types_of_crops)
head(Food_and_Feed_Aral_Sea)
head(Food_and_Feed_Lake_Chad)
head(Livestock_Unit_real_real_to_check)
head(Tropical_Livestock_Unit_and_Annual_Dry_Matter)
head(Residual_Residual_real_real)
head(Total_Amount_of_dry_matter_burned_realreal)
head(every_other_data)
head(harvest_factor)
head(population)

# View(Tropical_Livestock_Unit_and_Annual_Dry_Matter)

dry_matter_intake <- data.frame(
  animal = c("Camels", "Cattle", "Chickens", "Goats", "Pigs", "Sheeps"),
  Value = c(12, 6.5, 5.5, 6.3, 8.5, 4.5)
)


#------------ NPP FEED --------
NPP_feed <- sum(
  Tropical_Livestock_Unit_and_Annual_Dry_Matter$`Tropical Livestock Unit Equivalent` *
    Tropical_Livestock_Unit_and_Annual_Dry_Matter$`Annual Dry Mattter Requirement (Kg)`
) * .45

