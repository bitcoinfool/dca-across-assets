library(tidyverse)
library(quantmod)
library(scales)
library(gganimate)

# Define a function to extract and process data from Yahoo Finance
fetch_and_save_asset_data <- function(symbol, asset_name, start_date = "2000-01-01", end_date = Sys.Date()) {
  getSymbols(symbol, src = 'yahoo', from = start_date, to = end_date, auto.assign = TRUE)   # Fetch data
  data <- get(symbol)
  data_filled <- na.approx(data)   # Fill missing data
  data_df <- data.frame(date = index(data_filled), coredata(data_filled))   # Create data frame
  column_name <- paste0(asset_name,"_price")
  data_df_filtered <- data_df %>%
    select(1, column_name = ncol(.))
  # Select the first column and rename the last column to 'asset_name_price'
  last_col_name <- names(data_df)[ncol(data_df)] # Get the name of the last column
  column_name <- paste0("price_", asset_name)    # Define the new column name
  data_df_filtered <- data_df %>%
    select(1, !!column_name := all_of(last_col_name)) %>%
    filter(date >= (Sys.Date() - years(10)))
  return(data_df_filtered)
}

# Apply this function on the various assets
gold <- fetch_and_save_asset_data("GC=F", "gold")
oil <- fetch_and_save_asset_data("CL=F", "oil")
snp <- fetch_and_save_asset_data("SPY", "snp")
btc <- fetch_and_save_asset_data("BTC-USD", "btc") # The BTC data frame only starts from 2014-09-17

# Bring in a separate csv file for dates before 2014-09-17 to get its price over a decade
btc_add <- read_csv("BTC-USD_add.csv") %>%
  select(date = Date, price_btc = Close) %>%
  mutate(date = as.Date(date, format = "%m/%d/%y"))

# Merge the rows of both datasets
btc <- bind_rows(btc_add, btc) %>%
  filter(date >= (Sys.Date() - years(10))) %>%
  arrange(date)

# Create a function to calculate DCA value for each portfolio
calculate_dca_value <- function(data_frame, price_column, allocation) {
  # Create a separate frame for monthly average prices and total monthly investments
  monthly_investment <- data_frame %>%
    mutate(year = lubridate::year(date), month = lubridate::month(date)) %>%
    group_by(year, month) %>%
    summarise(avg_price = mean(.data[[price_column]], na.rm = TRUE), .groups = "drop") %>%
    mutate(investment = allocation / avg_price,
           cumulative_investment = cumsum(investment))
  
  # Join this monthly data back to the original data frame
  data_frame <- data_frame %>%
    mutate(year = lubridate::year(date), month = lubridate::month(date)) %>%
    left_join(monthly_investment, by = c("year", "month")) %>%
    arrange(date)
  
  # Fill down the cumulative investment for days without new investments
  data_frame <- data_frame %>%
    fill(cumulative_investment, .direction = "down") %>%
    mutate(total_value = cumulative_investment * .data[[price_column]])
  
  # Cleanup and return the final dataframe
  data_frame <- data_frame %>%
    select(date, all_of(price_column), total_value)
  
  return(data_frame)
}

# Apply the function to the four asset classes to calculate the respective portfolio values
btc_dca <- calculate_dca_value(btc, "price_btc", 1000)
gold_dca <- calculate_dca_value(gold, "price_gold", 1000)
oil_dca <- calculate_dca_value(oil, "price_oil", 1000)
snp_dca <- calculate_dca_value(snp, "price_snp", 1000)

# Combine all data into one data frame
all_assets <- btc_dca %>%
  select(date, btc_value = total_value) %>%
  left_join(gold_dca %>% select(date, gold_value = total_value), by = "date") %>%
  left_join(oil_dca %>% select(date, oil_value = total_value), by = "date") %>%
  left_join(snp_dca %>% select(date, snp_value = total_value), by = "date")

# Fill in the NA values over the weekends
all_assets_filled <- all_assets %>%
  fill(everything(), .direction = "down") 

# Combine the columns
all_assets_long <- all_assets_filled %>%
  pivot_longer(
    cols = c(btc_value, snp_value, gold_value, oil_value),
    names_to = "asset",
    values_to = "portfolio_value"
  ) %>%
  mutate(asset = case_when(
    asset == "btc_value" ~ "Bitcoin",
    asset == "snp_value" ~ "S&P 500",
    asset == "gold_value" ~ "Gold",
    asset == "oil_value" ~ "Oil"
  ))

# Define the specific colour set and match it to assets
colour_set <- c("Bitcoin" = "#FF7F00", "S&P 500" = "#00DD00", "Gold" = "#ffdb58", "Oil" = "#E41A55")

# Extract the last value for each asset
last_points <- all_assets_filled %>%
  summarise(
    date = max(date),
    btc_value = last(btc_value),
    snp_value = last(snp_value),
    gold_value = last(gold_value),
    oil_value = last(oil_value)
  ) %>%
  pivot_longer(cols = -date, names_to = "asset", values_to = "portfolio_value") %>%
  mutate(asset = case_when(
    asset == "btc_value" ~ "Bitcoin",
    asset == "snp_value" ~ "S&P 500",
    asset == "gold_value" ~ "Gold",
    asset == "oil_value" ~ "Oil"
  ))

# Plot the investment value of a DCA portfolio for the four asset classes
p1 <- ggplot(all_assets_long, aes(x = date, y = portfolio_value, color = asset)) +
  geom_line(size = 1) +
  geom_point(data = last_points, aes(y = portfolio_value), size = 4) +
  geom_text(data = last_points, aes(label = paste0("$", comma(portfolio_value))),
            nudge_x = 80, hjust = 0, size = 5, show.legend = FALSE) +
  scale_y_log10(labels = dollar_format()) +
  scale_x_date(limits = as.Date(c("2014-04-01", "2025-03-01")), date_labels = "%Y") +
  scale_color_manual(values = colour_set) +
  labs(title = "A Comparative Analysis of Dollar-Cost Averaging Across Four Assets",
       subtitle = "Assuming a monthly allocation of $1,000 over a decade\n",
       x = "",
       y = "Portfolio Value",
       color = "",
       caption = "@bitcoinfool") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "black", color = "black"),
    plot.background = element_rect(fill = "black", color = "black"),
    panel.grid.major.x = element_line(colour = "#2A2A2A80"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#2A2A2A80"),
    panel.grid.minor.y = element_blank(),
    text = element_text(color = "white", size = 14),
    plot.title = element_text(color = "white", size = 22, face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_text(color = "white"),
    plot.caption = element_text(color = "white", hjust = 1),
    plot.caption.position = "plot",
    plot.margin = margin(t = 40, r = 20, b = 20, l = 40, unit = "pt"),
    axis.text.y = element_text(colour = "darkgrey"),
    axis.text.x = element_text(colour = "darkgrey"),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )
p1
ggsave("p1_image.jpg", plot = p1, width = 16, height = 9, dpi = 300)


# Make an animation
p1_animation <- ggplot(all_assets_long, aes(x = date, y = portfolio_value, color = asset)) +
  geom_line(size = 0.7) +
  geom_point(size = 2) +
  scale_y_log10(labels = dollar_format()) +
  scale_x_date(limits = as.Date(c("2014-04-01", "2025-03-01")), date_labels = "%Y") +
  scale_color_manual(values = colour_set) +
  labs(title = "A Comparative Analysis of Dollar-Cost Averaging Across Four Assets",
       subtitle = "Assuming a monthly allocation of $1,000 over a decade      Date: {frame_along}  \n",
       x = "",
       y = "Portfolio Value",
       color = "",
       caption = "@bitcoinfool") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "black", color = "black"),
    plot.background = element_rect(fill = "black", color = "black"),
    panel.grid.major.x = element_line(colour = "#2A2A2A"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#2A2A2A"),
    panel.grid.minor.y = element_blank(),
    text = element_text(color = "white"),
    plot.title = element_text(color = "white", size = 11, face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_text(color = "white", size = 8),
    plot.caption = element_text(color = "white", size = 8, hjust = 1),
    plot.caption.position = "plot",
    plot.margin = margin(t = 10, r = 10, b = 5, l = 10, unit = "pt"),
    axis.text.y = element_text(size = 8, colour = "darkgrey"),
    axis.text.x = element_text(size = 8, colour = "darkgrey"),
    axis.title.y = element_text(size = 8, colour = "darkgrey"),
    legend.position = "right",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.5, "cm"),
  ) +
  transition_reveal(date) +
  view_follow(fixed_y = TRUE) +
  ease_aes('linear') 
p1_animation

p1_animation_video <- animate(p1_animation, height = 1200, width = 1920, fps = 40, duration = 24, end_pause = 150, res = 300, renderer = av_renderer())
anim_save("p1_animation.mp4", p1_animation_video)

