# HOTEL BOOKING DEMAND — SAMPLE DATA CREATED IN R
library(dplyr)
library(ggplot2)
# 1. CREATE SAMPLE HOTEL BOOKING DATA
hotel <- data.frame(
  hotel = c("City Hotel","Resort Hotel","City Hotel","Resort Hotel","City Hotel",
            "Resort Hotel","City Hotel","Resort Hotel","City Hotel","Resort Hotel"),
  is_canceled = c(0,1,0,0,1,1,0,1,0,0),
  lead_time = c(50,120,30,200,15,300,60,90,25,40),
  adr = c(100,150,110,200,90,180,105,160,115,130),
  market_segment = c("Online","Direct","Online","Corporate","Direct",
                     "Online","Corporate","Online","Direct","Online"),
  children = c(0,1,0,2,0,1,0,0,1,0)
)
print(hotel)
# 2. DATA CLEANING
# Remove duplicates
hotel <- distinct(hotel)
# Replace NA children with 0 (none in sample)
hotel$children[is.na(hotel$children)] <- 0
# Remove rows with missing values
hotel <- na.omit(hotel)
# Summary after cleaning
summary(hotel)
# 3. BASIC EDA
# Bookings per hotel type
print(table(hotel$hotel))
# Cancellation count
print(table(hotel$is_canceled))
# Market segment distribution
print(table(hotel$market_segment))
# 4. EXPLORE RELATIONSHIPS
# Relationship: Hotel type vs cancellation
relation <- hotel %>%
  group_by(hotel, is_canceled) %>%
  summarise(count = n())
print(relation)
# Correlation between lead time and ADR
print(cor(hotel$lead_time, hotel$adr))
# 5. BAR GRAPHS (MAIN OUTPUT)
# Bar 1 — Bookings per hotel type
ggplot(hotel, aes(x = hotel)) +
  geom_bar(fill = "skyblue") +
  ggtitle("Bookings by Hotel Type") +
  ylab("Count") +
  xlab("Hotel Type")
# Bar 2 — Cancellations by hotel
ggplot(hotel, aes(x = hotel, fill = factor(is_canceled))) +
  geom_bar(position = "dodge") +
  ggtitle("Cancellation Status by Hotel Type") +
  xlab("Hotel Type") +
  ylab("Count") +
  scale_fill_discrete(name = "Canceled (1 = Yes)")
# Bar 3 — Market Segment
ggplot(hotel, aes(x = market_segment)) +
  geom_bar(fill = "orange") +
  ggtitle("Market Segment Distribution") +
  xlab("Market Segment") +
  ylab("Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
