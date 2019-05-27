# target ベクトルの 1〜k 番目までの近傍との距離を算出する
# 2 番目以降の距離は target からの距離を d_k(k=1…) として d_1, d_1 + d2, d_1 + d_2 + d_3... と算出される
# target: 距離を測定する時の中心となるベクトル
# df.data: 距離を測定する際の target 以外のべクトルを含む data.frame
# col_class: df.data の項目でクラスを指定している項目の名称
# k: 何番目までの距離まで算出するかを指定する
knn_d <- function(target, df.data, col_class, k=1) {
  library(tidyverse)

  # NSE
  col_class <- dplyr::enquo(col_class)

  # ユークリッド距離
  d <- function(v1, v2) {
    sqrt(sum((v1 - v2)^2))
  }

  df.data %>%

    # target ベクトルと各行の距離を追加
    {
      df.tmp <- (.)

      # target との距離が格納されたベクトル
      # nrow(df.data) 個の要素から成る
      v.d <- df.tmp %>%
        dplyr::select(-!!col_class) %>%
        purrr::transpose() %>%
        purrr::map_dbl(function(row) {
          v <- as.numeric(row)
          d(target, v)
        })

      df.tmp %>%
        dplyr::mutate(d = v.d)
    } %>%

    # 距離が 0 の場合は除外
    # 同一データ(行)を想定しているが偶然の一致も除外してしまう…
    dplyr::filter(d > 0) %>%

    # 各クラスごとに項目を生成
    # - 距離を用いて降順に並び替えた k 件を取得し番号を振る(1~k)
    # - 距離の小さい順に累積和を取る
    dplyr::group_by(class) %>%
    dplyr::top_n(k, -d) %>%
    dplyr::arrange(class, d) %>%
    dplyr::mutate(
      order = dplyr::row_number(d),  # 同着でも並び順に応じて強制的に異なる番号を割り振る
      d = cumsum(d)
    ) %>%
    dplyr::filter(order <= k) %>%    # 同着で k 件以上となる場合を排除
    dplyr::ungroup() %>%

    # wide-form に変換
    # ex. class_a_1, class_a_2, ..., class_a_k, class_b_1, class_b_2, ..., class_b_k, ...
    dplyr::select(class, order, d) %>%
    dplyr::mutate(class = stringr::str_c("class", class, sep = "_")) %>%
    tidyr::unite(col = "class_order", class, order) %>%
    tidyr::spread(key = class_order, value = d)
}

# 各行ごとに 1~k 番目までの距離を算出する
# df.data: 距離を測定する対象となる data.frame
# col_class: df.data の項目でクラスを指定している項目の名称
# k: 何番目まで距離を算出するかを指定する
add_knn_d_columns <- function(df.data, col_class, k=1) {
  library(tidyverse)

  # NSE
  col_class <- dplyr::enquo(col_class)

  # df.data の各行ごとに 1~k 番目までの距離を算出
  df.distances <- df.data %>%
    dplyr::select(-!!col_class) %>%
    apply(1, function(target) { knn_d(target, df.data, !!col_class, k) }) %>%
    purrr::reduce(dplyr::bind_rows)
  
  df.data %>%
    dplyr::bind_cols(df.distances)
}
