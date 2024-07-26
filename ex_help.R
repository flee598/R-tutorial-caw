


# First hour will be going over the basics of R and RStudio, the second hour will be 
# a breif introduction to the tidyverse and then doing some excerses 



# laod some data
fish_df <- read.csv("https://raw.githubusercontent.com/flee598/R-tutorial-caw/main/data/messy_data.csv")


# how many rows and columns doe the dataset have?


# what 

# install and load the package "janitor"
library(janitor)


# find out what the function "clean_names()" does and use it on the fish df.
?clean_names()

fish_df <- clean_names(fish_df)


# replace "empty" cells with NA
fish_df[fish_df==""] <- NA

# drop columns that have all NA values (use base R , or janitor or dplyr)
fish_df <- janitor::remove_empty(fish_df, which = "cols")


# drop the columns:
# x, institution_record_number, sampling_protocol:observation_area, data_version
fish_df <- fish_df |> 
  select(!c(x, institution_record_number, sampling_protocol:observation_area, data_version))


# filter out the taxon_name records with the value "Nil"
fish_df <- fish_df |> 
  filter(taxon_name != "Nil")


# create a dataframe whith two columns, i) taxon_name and ii) n_records,
# where n_records is the number of records associated with each species.
# arrange the dataframe with the species with the most records at the top.
fish_df |> 
  group_by(taxon_name) |> 
  summarise(tot_records = n()) |> 
  arrange(-tot_records)


# on average how far are sites from the ocean?



# what is the elevation of the highest site?



# how many sites have a downstream barrier and how many do not?
fish_df |> 
  group_by(downstream_barrier) |> 
  summarise(n_bar = n())


# create a new column "year" which give the year the sampling occurred.
# hint as.Date() and format() could be helpful

fish_df |> 
  mutate(date = as.Date(event_date, tryFormats = "%d/%m/%Y")) |> 
  mutate(year = format(date, "%Y"))


# bringing it all together
# keep all the records that use some form of Electric-fishing - hint grepl()
# add a new column dist_2_ocean_m
# for each of the fishing methods calculate the mean distance to the ocean (in m)
# arrange from nearest to furtherest


fish_df |> 
  filter(grepl("Electric", sampling_method)) |> 
  mutate(dist_2_ocean_m = distance_ocean * 1000) |> 
  group_by(sampling_method) |> 
  summarise(dist_mean = mean(dist_2_ocean_m)) |> 
  arrange(dist_mean)
