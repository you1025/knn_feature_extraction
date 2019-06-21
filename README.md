# k-NN Feature Extraction

k-NN Feature Extraction のお試し実装

## お試し

```
library(tidyverse)

# データ定義
N <- 500
df.data <- tibble(
  x = runif(N, min = -1, max = 1),
  y = runif(N, min = -1, max = 1),
  class = dplyr::if_else(x * y > 0, "a", "b") %>% factor()
)

# 最近傍のデータまでの距離を算出
df.data.knn_d <- df.data %>%
  add_knn_d_columns(col_class = class, k = 1)

# 散布図で確認
# 線形分離できるっぽい分布に変換されている
df.data.knn_d %>%
  ggplot(aes(class_a_1, class_b_1)) +
    geom_point(aes(colour = class))
```

## 実験

[k-NN Feature Extraction を試す](https://stat-you1025.blogspot.com/2019/05/k-nn-feature-extraction.html)  
[Iris で k-NN Feature Extraction](https://stat-you1025.blogspot.com/2019/06/iris-k-nn-feature-extraction.html)
