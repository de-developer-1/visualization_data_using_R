# rayshaderを使った可視化サンプルスクリプト
# 必要なパッケージのロード

library(rayshader)
library(ggplot2)
library(tidyverse)
library(rgl)

# -------- サンプルデータの準備 --------

# 1. 山岳地形データの作成（マトリックスとして）
# ガウス分布を使った標高データの生成
mountain_matrix <- function(dim = 200) {
  x <- seq(-4, 4, length.out = dim)
  y <- seq(-4, 4, length.out = dim)
  
  # xとyのグリッドを作成
  grid <- expand.grid(x = x, y = y)
  
  # 複数のガウス分布を重ね合わせてランダムな山を作る
  peaks <- data.frame(
    x = runif(5, -3, 3),
    y = runif(5, -3, 3),
    height = runif(5, 5, 15),
    sigma_x = runif(5, 0.5, 1.5),
    sigma_y = runif(5, 0.5, 1.5)
  )
  
  # 各ピーク（山頂）からの距離に基づいて高さを計算
  z <- matrix(0, nrow = dim, ncol = dim)
  for (i in 1:nrow(peaks)) {
    peak <- peaks[i, ]
    z_peak <- peak$height * exp(
      -((grid$x - peak$x)^2 / (2 * peak$sigma_x^2) + 
          (grid$y - peak$y)^2 / (2 * peak$sigma_y^2))
    )
    z <- z + matrix(z_peak, nrow = dim, ncol = dim)
  }
  
  # 標高の最小値を0に設定
  z <- z - min(z)
  return(z)
}

# サンプル山岳地形データを生成（200x200マトリックス）
set.seed(123) # 再現性のため
mountain_mat <- mountain_matrix(200)

# 2. データフレーム形式のサンプルデータの作成（ヒートマップなど用）
# 火山データを使用
volcano_df <- expand.grid(x = 1:nrow(volcano), y = 1:ncol(volcano)) %>%
  mutate(height = as.vector(volcano))

# 3. 別のサンプルデータ（ガウス分布の混合）
n <- 100
x <- seq(-5, 5, length.out = n)
y <- seq(-5, 5, length.out = n)
gaussian_mixture <- expand.grid(x = x, y = y) %>%
  mutate(
    z = 3 * exp(-0.5 * ((x - 1.5)^2 + (y - 1.5)^2)) + 
      2 * exp(-0.5 * ((x + 2)^2 + (y + 2)^2)/1.5)
  )

# -------- rayshaderによる可視化 --------

# 1. 基本的な3D山岳地形の可視化
par(mfrow = c(1, 1))
mountain_mat %>%
  sphere_shade(texture = "desert") %>%
  plot_3d(mountain_mat, zscale = 3, fov = 0, theta = 45, zoom = 0.75, phi = 45)
# スクリーンショットの撮影
render_snapshot()
rgl::rgl.close()

# 2. 等高線付きの山岳地形
par(mfrow = c(1, 1))
mountain_mat %>%
  sphere_shade(texture = "imhof1") %>%
  add_shadow(ray_shade(mountain_mat, zscale = 3, lambert = TRUE), 0.5) %>%
  add_shadow(ambient_shade(mountain_mat), 0) %>%
  add_water(detect_water(mountain_mat), color = "lightblue") %>%
  plot_3d(mountain_mat, zscale = 3, fov = 0, theta = 45, zoom = 0.75, phi = 45)
# スクリーンショットの撮影
render_snapshot(filename = "mountain_with_water.png")
rgl::rgl.close()

# 3. ヒートマップの3D可視化
# volcanoデータで3Dヒートマップ
volcano %>%
  height_shade() %>%
  add_shadow(ray_shade(volcano, zscale = 3), 0.5) %>%
  add_shadow(ambient_shade(volcano), 0) %>%
  plot_3d(volcano, zscale = 3, fov = 0, theta = 135, zoom = 0.75, phi = 45)
render_snapshot(filename = "volcano_3d.png")
rgl::rgl.close()

# 4. ggplotと連携した可視化
# ガウシアン混合データを使用
ggplot_gaussian <- ggplot(gaussian_mixture, aes(x = x, y = y)) +
  geom_tile(aes(fill = z)) +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(title = "Gaussian Mixture", x = "X", y = "Y")

# ggplotを3D化
plot_gg(ggplot_gaussian, width = 5, height = 5, scale = 300, multicore = TRUE, 
        zoom = 0.7, theta = 45, phi = 30)
render_snapshot(filename = "ggplot_3d.png")
rgl::rgl.close()

# 5. 地形に光と影を追加
par(mfrow = c(1, 1))
mountain_mat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(mountain_mat), color = "lightblue") %>%
  add_shadow(ray_shade(mountain_mat, zscale = 3, lambert = TRUE), 0.5) %>%
  add_shadow(ambient_shade(mountain_mat), 0) %>%
  plot_3d(mountain_mat, zscale = 3, fov = 0, theta = 45, zoom = 0.75, phi = 45)
render_snapshot(filename = "mountain_with_shadows.png")
rgl::rgl.close()

# 6. 異なるテクスチャを使った可視化
texture_options <- c("imhof1", "imhof2", "imhof3", "imhof4", "desert", "bw", "unicorn")

# 複数のテクスチャを適用した例の可視化
par(mfrow = c(2, 3))
for (i in 1:6) {
  mountain_mat %>%
    sphere_shade(texture = texture_options[i]) %>%
    plot_map()
  title(texture_options[i])
}

# 7. ray_shade を使用した影付き2D可視化
par(mfrow = c(1, 1))
mountain_mat %>%
  sphere_shade(texture = "desert") %>%
  add_shadow(ray_shade(mountain_mat, zscale = 3, lambert = TRUE), 0.5) %>%
  plot_map()
title("Ray-shaded Map")

# 8. lambertシェーディングとambientシェーディングの比較
par(mfrow = c(1, 2))
# Lambertシェーディング
mountain_mat %>%
  sphere_shade(texture = "desert") %>%
  add_shadow(ray_shade(mountain_mat, zscale = 3, lambert = TRUE), 0.5) %>%
  plot_map()
title("Lambert Shading")

# Ambientシェーディング
mountain_mat %>%
  sphere_shade(texture = "desert") %>%
  add_shadow(ambient_shade(mountain_mat), 0.5) %>%
  plot_map()
title("Ambient Shading")

# 9. 3D可視化のアニメーション（一連のフレームを生成）
# 注意: 実行すると複数の画像が生成されるため、必要に応じてコメントアウト
mountain_mat %>%
  sphere_shade(texture = "desert") %>%
  add_shadow(ray_shade(mountain_mat, zscale = 3, lambert = TRUE), 0.5) %>%
  add_shadow(ambient_shade(mountain_mat), 0) %>%
  plot_3d(mountain_mat, zscale = 3, fov = 0, theta = 45, zoom = 0.75, phi = 45)

# 回転するアニメーションの作成 (theta: 0°から360°まで)
# render_movie(filename = "mountain_rotation.mp4", type = "orbit", frames = 360, fps = 30)
# rgl::rgl.close()

# 10. 複数のプロットを組み合わせた最終的な可視化
par(mfrow = c(2, 2))

# 2D山岳マップ
mountain_mat %>%
  sphere_shade(texture = "desert") %>%
  plot_map()
title("2D Mountain Map")

# 水と影付き山岳マップ
mountain_mat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(mountain_mat), color = "lightblue") %>%
  add_shadow(ray_shade(mountain_mat, zscale = 3), 0.5) %>%
  plot_map()
title("Mountain Map with Water and Shadows")

# Volcanoデータの等高線マップ
volcano %>%
  height_shade() %>%
  plot_map()
title("Volcano Height Map")

# Gaussianデータのヒートマップ
gaussian_matrix <- matrix(gaussian_mixture$z, nrow = n, ncol = n)
gaussian_matrix %>%
  height_shade(texture = "viridis") %>%
  plot_map()
title("Gaussian Mixture Heatmap")

# -------- 結果の表示と説明 --------
cat("\nrayshaderパッケージを使った可視化が完了しました。\n")
cat("このスクリプトでは以下の種類の可視化を実行しました：\n")
cat("1. 基本的な3D山岳地形の可視化\n")
cat("2. 等高線と水を追加した山岳地形\n")
cat("3. Volcanoデータを使用した3Dヒートマップ\n")
cat("4. ggplotとrayshaderを組み合わせた可視化\n")
cat("5. 光と影を追加した地形可視化\n")
cat("6. 様々なテクスチャを使った地形の表現\n")
cat("7. ray_shadeを使用した影付き2D可視化\n")
cat("8. シェーディング手法の比較\n")
cat("9. 3D可視化のアニメーション生成（コメントアウト中）\n")
cat("10. 複数のプロットを組み合わせた可視化\n")