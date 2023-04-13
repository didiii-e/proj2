## code to prepare `DATASET` dataset goes here

mobility <- haven::read_dta("/Users/dinan_elsyad/Desktop/Stat108/proj2/data-raw/mobility.dta")

usethis::use_data(mobility, overwrite = TRUE)


