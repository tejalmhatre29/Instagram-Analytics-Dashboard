# ==============================
# Data Loading & Preparation
# ==============================

data <- read.csv(
  "data/cleaned_instagram_data.csv",
  header = FALSE,
  stringsAsFactors = FALSE
)

# Column names
colnames(data) <- c(
  "post_id",
  "account_type",
  "follower_count",
  "media_type",
  "content_category",
  "traffic_source",
  "post_hour",
  "day_of_week",
  "likes",
  "comments",
  "shares",
  "saves",
  "reach",
  "impressions",
  "engagement_rate",
  "followers_gained",
  "hashtags_count"
)

# Total engagement
data$total_engagement <-
  data$likes +
  data$comments +
  data$shares +
  data$saves