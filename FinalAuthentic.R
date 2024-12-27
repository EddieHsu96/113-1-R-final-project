# 載入套件
library(tidyverse)

# 讀取年齡別檔案
df <- readr::read_csv("Age.csv")

 ## 顯示Columns
 names(df)

 ## 簡化欄位名稱
 colnames(df) <- stringr::str_replace(colnames(df), "_人數$", "")

 ## 刪除那些除了第一列外皆為 0 的列
 df <- df |>
 dplyr::select(where(~ any(. != 0)))  # 保留至少有一個非零數值的列

 ## 分類縮排
 library(dplyr)
 library(tidyr)
 library(forcats)
 
 ## 將 "年齡別" 轉換為因子並按照指定的順序進行排序
 df <- df |>
   mutate(
     `年齡別` = fct_relevel(`年齡別`, 
                         "0-5歲", "6-11歲", "12-17歲", "18-23歲", 
                         "24-29歲", "30-39歲", "40-49歲", "50-59歲", 
                         "60-64歲", "65-69歲", "70歲以上", "不詳")
   )
 
 ## 查看結果
 df
 
  ### 提取 "年齡別" 和 "性別" 列，並從 df 中移除這些列
  df_age_gender <- df %>%
   select(`年齡別`, `性別`)
 
  df_rest <- df %>%
   select(-`年齡別`, -`性別`)
 
  ### 定義每組合併的列
  column_groups <- list(
   `傷害與強制類` = c("妨害自由", "重傷害", "一般傷害", "一般恐嚇取財", "擄人勒贖"),
   `強盜與偷竊類` = c("普通竊盜", "汽車竊盜", "機車竊盜", "強盜", "搶奪", "竊佔", "侵占"),
   `毒品類` = c("第一級毒品", "第二級毒品", "第三級毒品"),
   `殺人類` = c("故意殺人", "過失殺人"),
   `性犯罪類` = c("強制性交", "共同強制性交", "對幼性交", "性交猥褻"),
   `詐騙與背信類` = c("詐欺", "背信", "偽造文書印文", "偽造有價證券", "偽造貨幣", "遺棄", "偽證", "誣告", "湮滅證據", "藏匿頂替", "重利"),
   `社會秩序類` = c("賭博", "妨害婚姻及家庭", "妨害風化", "妨害公務", "妨害秩序", "妨害名譽", "妨害秘密", "違反就業服務法", "違反著作權法", "違反商標法", "違反藥事法"),
   `國家與公共安全類` = c("公共危險", "違反森林法", "違反貪污治罪條例", "違反選罷法", "懲治走私條例", "違反槍砲彈藥刀械管制條例")
 )
 
  ### 將資料框轉換為長格式，根據規則進行合併並加總
  AgeA <- df |>
  ### 將需要合併的列轉換為長格式
   pivot_longer(cols = -c(`年齡別`, `性別`), names_to = "crime_type", values_to = "count") |>
  ### 根據 crime_type 合併至新的組別
   mutate(
     crime_group = case_when(
       crime_type %in% column_groups$`傷害與強制類` ~ "傷害與強制類",
       crime_type %in% column_groups$`強盜與偷竊類` ~ "強盜與偷竊類",
       crime_type %in% column_groups$`毒品類` ~ "毒品類",
       crime_type %in% column_groups$`殺人類` ~ "殺人類",
       crime_type %in% column_groups$`性犯罪類` ~ "性犯罪類",
       crime_type %in% column_groups$`詐騙與背信類` ~ "詐騙與背信類",
       crime_type %in% column_groups$`社會秩序類` ~ "社會秩序類",
       crime_type %in% column_groups$`國家與公共安全類` ~ "國家與公共安全類",
       TRUE ~ NA_character_
     )
   ) |>
   ### 去除沒有對應分類的行
   filter(!is.na(crime_group)) |>
   ### 將相同 crime_group 合併並加總
   group_by(`年齡別`, `性別`, crime_group) |>
   summarise(count = sum(count, na.rm = TRUE), .groups = "drop") |>
   ### 轉回寬格式，將 crime_group 當作列名稱
   pivot_wider(names_from = crime_group, values_from = count)
 
   ### 查看結果
   AgeA
 
 
   ### 輸出結果為 AgeA.csv
   write_csv( AgeA, "AgeA.csv")

 
# 讀取教育別檔案  
 df1 <- readr::read_csv("Edu.csv")
 
 ## 簡化欄位名稱
 colnames(df1) <- stringr::str_replace(colnames(df1), "_人數$", "")
 
 ## 刪除那些除了第一列外皆為 0 的列
 df1 <- df1 |>
   dplyr::select(where(~ any(. != 0)))  # 保留至少有一個非零數值的列
 
 ## 分類縮排
 
    ### Parse”教育程度別“為可排序
    library(forcats)

    df1 <- df1 |>
    ### 將 "教育程度別" 轉換為因子並按照指定的順序進行排序
   mutate(
     `教育程度別` = fct_relevel(`教育程度別`, 
                           "不識字", "國小", "國中", "高中職", "大專", 
                           "研究所", "自修", "其他含不詳", "總計")
   )
 
    ### 提取 "教育程度別" 和 "性別" 列，並從 df 中移除這些列
    df1_edu_gender <- df1 %>%
    select(`教育程度別`, `性別`)
 
    df1_rest <- df1 %>%
    select(-`教育程度別`, -`性別`)
    
    ### 載入 dplyr 和 tidyr
    library(dplyr)
    library(tidyr)
 
    ### 定義每組合併的列
    column_groups <- list(
   `傷害與強制類` = c("妨害自由", "重傷害", "一般傷害", "一般恐嚇取財", "擄人勒贖"),
   `強盜與偷竊類` = c("普通竊盜", "汽車竊盜", "機車竊盜", "強盜", "搶奪", "竊佔", "侵占"),
   `毒品類` = c("第一級毒品", "第二級毒品", "第三級毒品"),
   `殺人類` = c("故意殺人", "過失殺人"),
   `性犯罪類` = c("強制性交", "共同強制性交", "對幼性交", "性交猥褻"),
   `詐騙與背信類` = c("詐欺", "背信", "偽造文書印文", "偽造有價證券", "偽造貨幣", "遺棄", "偽證", "誣告", "湮滅證據", "藏匿頂替", "重利"),
   `社會秩序類` = c("賭博", "妨害婚姻及家庭", "妨害風化", "妨害公務", "妨害秩序", "妨害名譽", "妨害秘密", "違反就業服務法", "違反著作權法", "違反商標法", "違反藥事法"),
   `國家與公共安全類` = c("公共危險", "違反森林法", "違反貪污治罪條例", "違反選罷法", "懲治走私條例", "違反槍砲彈藥刀械管制條例")
 )
 
    ### 假設資料框 df 已經包含了 "教育程度別" 和 "性別" 以及 56 列犯罪類型
    EduA <- df1 |>
    ### 轉換為長格式，以便合併
    pivot_longer(cols = -c(`教育程度別`, `性別`), names_to = "crime_type", values_to = "count") |>
    ### 根據 crime_type 合併到新的組別
    mutate(
      crime_group = case_when(
       crime_type %in% column_groups$`傷害與強制類` ~ "傷害與強制類",
       crime_type %in% column_groups$`強盜與偷竊類` ~ "強盜與偷竊類",
       crime_type %in% column_groups$`毒品類` ~ "毒品類",
       crime_type %in% column_groups$`殺人類` ~ "殺人類",
       crime_type %in% column_groups$`性犯罪類` ~ "性犯罪類",
       crime_type %in% column_groups$`詐騙與背信類` ~ "詐騙與背信類",
       crime_type %in% column_groups$`社會秩序類` ~ "社會秩序類",
       crime_type %in% column_groups$`國家與公共安全類` ~ "國家與公共安全類",
       TRUE ~ NA_character_
     )
   ) |>
    ### 去除沒有對應分類的行
    filter(!is.na(crime_group)) |>
    ### 按 "教育程度別" 和 "性別" 以及 crime_group 分組，並加總 count
    group_by(`教育程度別`, `性別`, crime_group) |>
    summarise(count = sum(count, na.rm = TRUE), .groups = "drop") |>
    ### 轉換回寬格式，以每個 crime_group 作為列名稱
    pivot_wider(names_from = crime_group, values_from = count)
 
   ## 查看結果
   EduA
 
   ## 輸出結果為   EduA.csv
   write_csv( EduA, "EduA.csv")
 
 
 
 
 #讀取職業別檔案
 df2 <- readr::read_csv("Job.csv")
 
   ## 簡化欄位名稱
   colnames(df2) <- stringr::str_replace(colnames(df2), "_人數$", "")
 
   ## 刪除那些除了第一列外皆為 0 的列
   df2 <- df2 |>
   dplyr::select(where(~ any(. != 0)))  # 保留至少有一個非零數值的列
 
   ## 分類縮排
 
    #### 載入 dplyr 和 tidyr
    library(dplyr)
    library(tidyr)
 
    ### 提取 "職業別" 和 "性別" 列，並從 df 中移除這些列
    df2_job_gender <- df2 %>%
    select(`職業別`, `性別`)
 
    df2_rest <- df2 %>%
    select(-`職業別`, -`性別`)

 
     ### 定義每組合併的列
     column_groups <- list(
    `傷害與強制類` = c("妨害自由", "重傷害", "一般傷害", "一般恐嚇取財", "擄人勒贖"),
    `強盜與偷竊類` = c("普通竊盜", "汽車竊盜", "機車竊盜", "強盜", "搶奪", "竊佔", "侵占"),
    `毒品類` = c("第一級毒品", "第二級毒品", "第三級毒品"),
    `殺人類` = c("故意殺人", "過失殺人"),
    `性犯罪類` = c("強制性交", "共同強制性交", "對幼性交", "性交猥褻"),
    `詐騙與背信類` = c("詐欺", "背信", "偽造文書印文", "偽造有價證券", "偽造貨幣", "遺棄", "偽證", "誣告", "湮滅證據", "藏匿頂替", "重利"),
    `社會秩序類` = c("賭博", "妨害婚姻及家庭", "妨害風化", "妨害公務", "妨害秩序", "妨害名譽", "妨害秘密", "違反就業服務法", "違反著作權法", "違反商標法", "違反藥事法"),
    `國家與公共安全類` = c("公共危險", "違反森林法", "違反貪污治罪條例", "違反選罷法", "懲治走私條例", "違反槍砲彈藥刀械管制條例")
)
 
    ### 假設資料框 df 已經包含了 "職業別" 和 "性別" 以及 56 列犯罪類型
    JobA<- df2 |>
    ### 轉換為長格式，以便合併
    pivot_longer(cols = -c(`職業別`, `性別`), names_to = "crime_type", values_to = "count") |>
    ### 根據 crime_type 合併到新的組別
    mutate(
     crime_group = case_when(
       crime_type %in% column_groups$`傷害與強制類` ~ "傷害與強制類",
       crime_type %in% column_groups$`強盜與偷竊類` ~ "強盜與偷竊類",
       crime_type %in% column_groups$`毒品類` ~ "毒品類",
       crime_type %in% column_groups$`殺人類` ~ "殺人類",
       crime_type %in% column_groups$`性犯罪類` ~ "性犯罪類",
       crime_type %in% column_groups$`詐騙與背信類` ~ "詐騙與背信類",
       crime_type %in% column_groups$`社會秩序類` ~ "社會秩序類",
       crime_type %in% column_groups$`國家與公共安全類` ~ "國家與公共安全類",
       TRUE ~ NA_character_
     )
   ) |>
   ### 去除沒有對應分類的行
   filter(!is.na(crime_group)) |>
   ### 按 "職業別" 和 "性別" 以及 crime_group 分組，並加總 count
   group_by(`職業別`, `性別`, crime_group) |>
   summarise(count = sum(count, na.rm = TRUE), .groups = "drop") |>
   ### 轉換回寬格式，以每個 crime_group 作為列名稱
   pivot_wider(names_from = crime_group, values_from = count)
 
  ## 查看結果
  JobA
 
  ## 輸出結果為   JobA.csv
  write_csv( JobA, "JobA.csv")

  
  
# 視覺化
  library(tidyverse)
  
  ## 年齡別
  df_long <- AgeA |> 
    tidyr::pivot_longer(
      cols = starts_with("傷害與強制類"):starts_with("詐騙與背信類"),
      names_to = "類別",
      values_to = "數值"
    )
  
  # 繪製長條圖，X 軸為年齡別，Y 軸為數值，顏色依照犯罪類別
  ggplot(df_long, aes(x = `年齡別`, y = `數值`, fill = `類別`)) +
    geom_bar(stat = "identity", position = "dodge") +  # 使用 stat = "identity" 來直接使用數值
    theme_minimal() +
    labs(
      title = "不同年齡別的犯罪類別數值",
      x = "年齡別",
      y = "數值",
      fill = "犯罪類別"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
   ## 教育別
   df_filtered <- EduA |> 
    dplyr::filter(`教育程度別` != "總計")  # 刪除 "總計" 行
  
   ## 將資料轉換為長格式
   df_long <- df_filtered |> 
    tidyr::pivot_longer(
      cols = starts_with("傷害與強制類"):starts_with("詐騙與背信類"),
      names_to = "類別",
      values_to = "數值"
    )
  
  # 繪製長條圖，X 軸為教育程度別，Y 軸為數值，顏色依照犯罪類別
  ggplot(df_long, aes(x = `教育程度別`, y = `數值`, fill = `類別`)) +
    geom_bar(stat = "identity", position = "dodge") +  # 使用 stat = "identity" 來直接使用數值
    theme_minimal() +
    labs(
      title = "不同教育程度別的犯罪類別數值",
      x = "教育程度別",
      y = "數值",
      fill = "犯罪類別"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  
  ## 職業別
  df_long <- JobA |> 
    tidyr::pivot_longer(
      cols = starts_with("傷害與強制類"):starts_with("詐騙與背信類"),
      names_to = "類別",
      values_to = "數值"
    )
  
  # 繪製長條圖，交換 X 軸和 Y 軸
  ggplot(df_long, aes(y = `職業別`, x = `數值`, fill = `類別`)) +
    geom_bar(stat = "identity", position = "dodge") +  # 使用 stat = "identity" 來直接使用數值
    theme_minimal() +
    labs(
      title = "不同職業別的犯罪類別數值",
      x = "數值",
      y = "職業別",
      fill = "犯罪類別"
    ) +
    theme(axis.text.y = element_text(angle = 0, hjust = 1))

  
  
  #總結

  # 資料轉換為長格式
  JobA_long <- JobA |> 
    tidyr::pivot_longer(cols = starts_with("傷害") | starts_with("國家") | starts_with("強盜") |
                          starts_with("性犯罪") | starts_with("殺人") | starts_with("毒品") |
                          starts_with("社會") | starts_with("詐騙"),
                        names_to = "類別", values_to = "數量")
  
  EduA_long <- EduA |> 
    tidyr::pivot_longer(cols = starts_with("傷害") | starts_with("國家") | starts_with("強盜") |
                          starts_with("性犯罪") | starts_with("殺人") | starts_with("毒品") |
                          starts_with("社會") | starts_with("詐騙"),
                        names_to = "類別", values_to = "數量")
  
  AgeA_long <- AgeA |> 
    tidyr::pivot_longer(cols = starts_with("傷害") | starts_with("國家") | starts_with("強盜") |
                          starts_with("性犯罪") | starts_with("殺人") | starts_with("毒品") |
                          starts_with("社會") | starts_with("詐騙"),
                        names_to = "類別", values_to = "數量")
  # 繪製 JobA 的熱圖
  ggplot(JobA_long, aes(x = `職業別`, y = `類別`, fill = `數量`)) +
    geom_tile() + 
    scale_fill_gradient(low = "white", high = "blue") +  # 設定顏色漸變
    theme_minimal() + 
    labs(title = "職業別類別的熱圖",
         x = "職業別",
         y = "類別") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  # 讓 X 軸的文字垂直顯示
  
  # 繪製 EduA 的熱圖
  ## 刪除 "總計" 行
  EduA_clean <- EduA |> 
    dplyr::filter(`教育程度別` != "總計")
  ## 將資料轉換為長格式
  EduA_long <- EduA_clean |> 
    tidyr::pivot_longer(cols = starts_with("傷害") | starts_with("國家") | starts_with("強盜") |
                          starts_with("性犯罪") | starts_with("殺人") | starts_with("毒品") |
                          starts_with("社會") | starts_with("詐騙"),
                        names_to = "類別", values_to = "數量")
  ## 繪製 EduA 的熱圖
  ggplot(EduA_long, aes(x = `教育程度別`, y = `類別`, fill = `數量`)) +
    geom_tile() + 
    scale_fill_gradient(low = "white", high = "red") +  # 顏色設定
    theme_minimal() + 
    labs(title = "教育程度別類別的熱圖",
         x = "教育程度別",
         y = "類別") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  # 垂直顯示 X 軸文字
  
  
  
  # 繪製 AgeA 的熱圖
  ggplot(AgeA_long, aes(x = `年齡別`, y = `類別`, fill = `數量`)) +
    geom_tile() + 
    scale_fill_gradient(low = "white", high = "green") +  # 顏色設定
    theme_minimal() + 
    labs(title = "年齡別類別的熱圖",
         x = "年齡別",
         y = "類別") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  # 垂直顯示 X 軸文字
  
  