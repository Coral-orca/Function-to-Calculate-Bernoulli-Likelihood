# Load required libraries
library(dplyr)
library(ggplot2)

# `countryname` - a vector of country names; only country names work; ISO codes will not work; default = c("Australia", "Canada", "Ecuador", "Egypt", "Mexico",  "India", "Russia",  "Senegal", "UK")
# `countryvalue` - a vector of values that are to be plot on the map; default = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
# `leglab` - a string to label the legend containing the country fill gradient; ideally the variable name of `countryvalue`; default = "Gradient"
# `colgrad` - a vector of two colours to act as the two ends of the country fill gradient; first value is lower tail, second value is upper tail; default = c("lightgreen", "darkgreen")
# `bdcol` - colour of the country borders; default = "darkgrey"
# `nacol` - colour of the countries with NA values; default = "lightgrey"

# Define the function along with the default values of the arguments
worldplot <- function(countryname = c("Australia", "Canada", "Ecuador", "Egypt", "Mexico",  "India", "Russia",  "Senegal", "UK"),
                      countryvalue = c(1, 2, 3, 4, 5, 6, 7, 8, 9), 
                      leglab = "Gradient", 
                      colgrad = c("lightgreen", "darkgreen"), 
                      bdcol = "darkgrey", 
                      nacol = "lightgrey"){
  
  data <- data.frame(        # Create a data frame
    country = countryname,   # Variable `country` containing the user input country names
    value = countryvalue     # Variable `value` containing the user input country values
  )
  
  # Merge `data` with `world_map_data` using country names
  world_map_data <- map_data("world") %>%
    left_join(data, by = c("region" = "country"))
  
  # Plot the world map
  ggplot() +
    geom_polygon(data = world_map_data, aes(x = long, y = lat, group = group, fill = value), color = bdcol) +
    scale_fill_gradient(name = leglab, low = colgrad[1], high = colgrad[2], na.value = nacol) +
    theme_void()
}
