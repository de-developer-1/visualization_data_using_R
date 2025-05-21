# ライブラリ読み込み
library(missRanger)
library(dplyr)
library(ggplot2)

set.seed(123)

# 1. サンプルデータ作成（iris + 欠損付与）
data <- iris
data$Species <- as.character(data$Species)  # factor を character にして扱いやすく

# 20% の確率で欠損を付与
make_missing <- function(x, prop = 0.2) {
  x[sample(seq_along(x), size = floor(length(x) * prop))] <- NA
  return(x)
}

data_miss <- data %>%
  mutate(across(.cols = everything(), .fns = ~make_missing(.x, prop = 0.2)))

cat("欠損があるか確認:\n")
sapply(data_miss, function(x) sum(is.na(x)))

# 2. missRanger による欠損補完
data_imputed <- missRanger(data_miss, pmm.k = 3, num.trees = 100)

cat("\n補完後の欠損確認:\n")
sapply(data_imputed, function(x) sum(is.na(x)))

# 3. 補完前後の比較: 例として Sepal.Length の density plot
df_plot <- data.frame(
  Original = data$Sepal.Length,
  Missing = data_miss$Sepal.Length,
  Imputed = data_imputed$Sepal.Length
)

# ggplot で補完の様子を視覚化
ggplot(df_plot, aes(x = Original)) +
  geom_density(color = "black", linetype = "dashed") +
  geom_density(aes(x = Missing), color = "red", na.rm = TRUE) +
  geom_density(aes(x = Imputed), color = "blue") +
  labs(title = "Sepal.Length: Original vs Missing vs Imputed",
       x = "Sepal.Length", y = "Density",
       subtitle = "Black: Original | Red: Missing | Blue: Imputed")

# 4. 補完精度評価（元データとのRMSE）
rmse <- function(true, pred) {
  sqrt(mean((true - pred)^2, na.rm = TRUE))
}

num_cols <- names(data)[sapply(data, is.numeric)]
rmse_results <- sapply(num_cols, function(col) {
  miss_idx <- is.na(data_miss[[col]])
  rmse(data[[col]][miss_idx], data_imputed[[col]][miss_idx])
})

cat("\nRMSE (元の値と補完値の差):\n")
print(rmse_results)