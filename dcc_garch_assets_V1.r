# ---------------------------
# 01. 环境设置与依赖加载
# ---------------------------
# 设置工作目录
setwd('/Users/lzc19950714/工作文件夹/招银理财--资产配置赛道')

# 加载分析所需的R包
library(zoo)           # 提供时间序列基础操作功能
library(xts)           # 处理金融时间序列数据结构
library(ggplot2)       # 数据可视化
library(dplyr)         # 数据清洗、转换与操作
library(PerformanceAnalytics)  # 金融绩效分析
library(rugarch)       # 单变量GARCH模型拟合
library(rmgarch)       # 多变量GARCH模型估计
library(showtext)      # 支持中文字体显示
library(reshape2)      # 数据重塑操作
library(ggcorrplot)    # 相关系数矩阵热力图绘制
library(fBasics)       # 基础统计函数
library(forecast)      # 时间序列预测
library(fUnitRoots)    # 单位根检验
library(scales)        # 坐标轴刻度格式化

# 初始化图形设备（MacOS系统适用）
quartz(width = 10, height = 4)

# 启用中文字体自动检测与显示
showtext_auto()

# ---------------------------
# 02. 数据导入与预处理
# ---------------------------
# 读取经Excel和Python初步处理的CSV数据
data <- read.csv("Data/指数行情_processed.csv") 

# 查看数据维度（行数、列数）
cat("数据维度：", dim(data), "\n")
# 查看数据结构（各列名称、类型等）
str(data)

# 转换日期列格式为标准日期格式
data$日期 <- as.Date(data$日期, format = "%Y-%m-%d")

# 查看数据前5行
head(data, 5)

# 构建价格时间序列（xts格式）
dates <- data$日期                          # 提取日期向量
data_columns <- data[, -which(names(data) == "日期")]  # 提取除日期外的所有数据列
ts_prices <- xts(data_columns, order.by = dates)       # 转换为xts时间序列对象

# 计算对数收益率（去除首行因差分产生的NA）
ts_returns <- diff(log(ts_prices))[-1, ]

# 查看收益率数据前5行
head(ts_returns, 5)

# 计算滚动年化波动率（窗口大小设为20日）
window_size <- 20
rolling_vol <- rollapply(
  ts_returns, 
  width = window_size, 
  FUN = function(x) apply(x, 2, StdDev.annualized),  # 对每列数据计算年化标准差
  by.column = FALSE, 
  align = "right"
)

# ---------------------------
# 03. 单资产可视化分析
# ---------------------------
# 将时间序列转换为数据框（便于ggplot处理）
returns <- data.frame(
  date = index(ts_returns),
  coredata(ts_returns)
)

volatility <- data.frame(
  date = index(rolling_vol),
  coredata(rolling_vol)
)

assets <- names(returns)[-1]  # 提取资产名称（排除日期列）
dir.create("Plots", showWarnings = FALSE)  # 创建结果存储目录

# 循环绘制各资产的分析图（收益率、收益率分布、波动率）
for (asset in assets) {
  # 1. 绘制收益率时间序列图
  p1 <- ggplot(returns, aes(x = date, y = .data[[asset]])) +
    geom_line(colour = "steelblue", linewidth = 0.8) +
    labs(
      title = paste0(asset, " 收益率时间序列"),
      x = "日期",
      y = "收益率"
    ) +
    theme_minimal()
  
  ggsave(
    filename = file.path("Plots", paste0(gsub("[^[:alnum:]]", "_", asset), "收益率.png")),
    plot = p1,
    width = 10, height = 6, dpi = 300
  )
  
  # 2. 绘制收益率分布直方图
  asset_mean <- mean(returns[[asset]], na.rm = TRUE)
  asset_sd <- sd(returns[[asset]], na.rm = TRUE)
  
  p2 <- ggplot(returns, aes(x = .data[[asset]])) +
    geom_histogram(
      aes(y = after_stat(density)),
      binwidth = 0.001,
      color = "steelblue", fill = "lightgray",
      linewidth = 0.5
    ) +
    stat_function(
      fun = dnorm, 
      args = list(mean = asset_mean, sd = asset_sd),
      color = "red", linewidth = 1
    ) +
    labs(
      title = paste0(asset, " 收益率分布"),
      x = "收益率",
      y = "密度"
    ) +
    theme_minimal() +
    annotate(
      "text", 
      x = -Inf, y = Inf,
      hjust = -0.1, vjust = 1.5,
      label = paste0("均值: ", round(asset_mean, 4), "\n标准差: ", round(asset_sd, 4)),
      size = 3
    )
  
  ggsave(
    filename = file.path("Plots", paste0(gsub("[^[:alnum:]]", "_", asset), "收益率分布.png")),
    plot = p2,
    width = 10, height = 6, dpi = 300
  )
  
  # 3. 绘制波动率时间序列图
  p3 <- ggplot(volatility, aes(x = date, y = .data[[asset]])) +
    geom_line(color = "steelblue", linewidth = 0.8) +
    labs(
      title = paste0(asset, " 波动率时间序列"),
      x = "日期",
      y = "波动率"
    ) +
    theme_minimal()
  
  ggsave(
    filename = file.path("Plots", paste0(gsub("[^[:alnum:]]", "_", asset), "波动率.png")),
    plot = p3,
    width = 10, height = 6, dpi = 300
}
}

# ---------------------------
# 04. 静态相关性分析
# ---------------------------
# 计算全样本的静态相关系数矩阵（处理缺失值方式：成对完整观测）
static_cor_mat <- cor(ts_returns, use = "pairwise.complete.obs")

# 按年份分组进行相关性分析
ts_years <- format(index(ts_returns), "%Y")
unique_years <- unique(ts_years)

for (year in unique_years) {
  yearly_returns <- ts_returns[ts_years == year]
  
  if (nrow(yearly_returns) < 2) continue  # 若数据不足2行则跳过该年份
  
  cor_mat <- cor(yearly_returns, use = "pairwise.complete.obs")
  
  p <- ggcorrplot(
    cor_mat,
    hc.order = FALSE,       # 保持原始变量顺序
    type = "upper",         # 显示上三角部分
    outline.color = "white",
    ggtheme = theme_gray(),
    colors = c("#6D9EC1", "white", "#E46726"),
    lab = TRUE, lab_size = 3.5
  ) +
    theme(
      axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
      axis.text.y = element_text(size = 10)
    )
  
  ggsave(
    filename = file.path("Plots", paste0(year, "年资产间静态相关性热力图.png")),
    plot = p,
    width = 8, height = 7.5, dpi = 300
  )
}

# ---------------------------
# 05. 时间序列统计检验
# ---------------------------
# ADF单位根检验（检查序列平稳性，无截距和趋势项）
adf_results <- list()
for (asset in assets) {
  adf_result <- adfTest(ts_returns[, asset], type = "nc")  # 执行ADF检验
  adf_results[[asset]] <- adf_result@test$p.value
}
cat("ADF检验P值（<0.05表示平稳）:\n"); print(adf_results)

# 绘制ACF和PACF图（分析序列自相关结构）
for (asset in assets) {
  acf_file <- file.path("Plots", paste0(asset, "_acf.png"))
  pacf_file <- file.path("Plots", paste0(asset, "_pacf.png"))
  
  png(acf_file, width = 10, height = 6, res = 300)
  acf(ts_returns[, asset], main = paste("自相关函数 (ACF):", asset))
  dev.off()
  
  png(pacf_file, width = 10, height = 6, res = 300)
  pacf(ts_returns[, asset], main = paste("偏自相关函数 (PACF):", asset))
  dev.off()
}

# Ljung-Box检验（残差白噪声检验）与ARCH效应检验
LB_results <- list()
LB_ARCH_results <- list()
mean_arma_orders <- list()

for (asset in assets) {
  mean_arma <- auto.arima(ts_returns[, asset])  # 自动选择ARMA模型
  ar_order <- mean_arma$arma[1]
  ma_order <- mean_arma$arma[2]
  mean_arma_orders[[asset]] <- c(ar_order, ma_order)
  
  res <- mean_arma$residuals
  LB_results[[asset]] <- Box.test(res, type = "Ljung")$p.value  # 均值方程残差的白噪声检验
  LB_ARCH_results[[asset]] <- Box.test(res^2, type = "Ljung")$p.value  # ARCH效应检验（残差平方的白噪声检验）
}

cat("Ljung-Box检验结果（均值方程残差）:\n"); print(LB_results)
cat("ARCH效应检验结果（残差平方）:\n"); print(LB_ARCH_results)

# ---------------------------
# 06. 单变量GARCH模型参数搜索
# ---------------------------
# 定义模型参数搜索空间
distribution_models <- c("norm", "snorm", "std", "sstd")  # 分布类型
variance_models <- c("sGARCH", "gjrGARCH")                # 方差模型类型
mean_models <- list(c(0,0), c(1,0), c(0,1), c(1,1))      # 均值模型的ARMA阶数组合

results <- data.frame(
  column_name = character(),
  distribution_model = character(),
  variance_model = character(),
  mean_model = character(),
  AIC = numeric(),
  BIC = numeric(),
  logLik = numeric(),
  stringsAsFactors = FALSE
)

best_specs <- list()  # 存储各资产的最佳模型规格

for (col_idx in 1:ncol(ts_returns)) {
  column_data <- ts_returns[, col_idx]
  column_name <- colnames(ts_returns)[col_idx]
  cat("\n=== 处理资产: ", column_name, " (", col_idx, "/", ncol(ts_returns), ") ===\n")
  
  column_results <- data.frame()
  
  for (dist_model in distribution_models) {
    for (var_model in variance_models) {
      for (mean_order in mean_models) {
        # 构建单变量GARCH模型规格
        spec <- ugarchspec(
          variance.model = list(model = var_model, garchOrder = c(1,1)),
          mean.model = list(armaOrder = mean_order, include.mean = TRUE),
          distribution.model = dist_model
        )
        
        # 错误处理：捕获模型拟合过程中的错误
        current_fit <- tryCatch(
          ugarchfit(spec, data = column_data, solver = "solnp"),
          error = function(e) {
            cat("✗ 模型拟合失败: ", dist_model, ",", var_model, ",", mean_order, "\n")
            return(NULL)
          }
        )
        
        if (!is.null(current_fit)) {
          info_crit <- infocriteria(current_fit)
          model_result <- data.frame(
            column_name = column_name,
            distribution_model = dist_model,
            variance_model = var_model,
            mean_model = paste(mean_order, collapse = ","),
            AIC = info_crit[1],
            BIC = info_crit[2],
            logLik = likelihood(current_fit),
            stringsAsFactors = FALSE
          )
          results <- rbind(results, model_result)
          column_results <- rbind(column_results, model_result)
          cat("✓ 完成模型: ", dist_model, ",", var_model, ",", mean_order, "\n")
        }
      }
    }
  }
  
  if (nrow(column_results) > 0) {
    best_model <- column_results[which.min(column_results$AIC), ]  # 基于AIC选择最佳模型
    best_spec <- ugarchspec(
      variance.model = list(model = best_model$variance_model, garchOrder = c(1,1)),
      mean.model = list(armaOrder = as.numeric(strsplit(best_model$mean_model, ",")[[1]]), include.mean = TRUE),
      distribution.model = best_model$distribution_model
    )
    best_specs[[column_name]] <- best_spec
    cat("✅ 最佳模型: ", best_model$distribution_model, ",", best_model$variance_model, ",", best_model$mean_model, "\n")
  } else {
    cat("⚠️ 无有效模型: ", column_name, "\n")
  }
}

# 提取各资产的最佳模型（基于AIC准则）
best_models <- results %>%
  group_by(column_name) %>%
  filter(AIC == min(AIC)) %>%
  ungroup()

print("各资产最佳GARCH模型（基于AIC）:"); print(best_models)

# ---------------------------
# 07. 多变量DCC-GARCH模型分析
# ---------------------------
# 构建多变量模型规格（使用各资产最佳单变量规格）
mspec_1 <- multispec(best_specs)
spec_1 <- dccspec(
  uspec = mspec_1,          # 输入单变量GARCH规格
  dccOrder = c(1, 1),       # DCC过程的阶数
  distribution = "mvnorm"   # 假设多元正态分布
)

# 拟合DCC-GARCH模型
res_1 <- dccfit(spec_1, data = ts_returns)

# 构建统一规格的多变量模型（假设使用gjrGARCH+std分布）
uspec <- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
  distribution.model = "std"
)
mspec_2 <- multispec(replicate(ncol(ts_returns), uspec))  # 复制单变量规格以构建多变量规格
spec_2 <- dccspec(uspec = mspec_2, dccOrder = c(1,1), distribution = "mvnorm")

# 重新估计DCC-GARCH模型
res_2 <- dccfit(spec_2, data = ts_returns)

# 提取协方差矩阵序列并计算特定资产对的相关系数
H <- res_2@mfit$H
rho_18 <- H[1, 8, ] / sqrt(H[1, 1, ] * H[8, 8, ])  # 计算资产1与资产8的动态相关系数

# 计算滚动相关系数（窗口大小260日）
ts_returns_18 <- ts_returns[, c(1, 8)]
rolling_cor <- rollapply(
  ts_returns_18,
  width = 260,
  FUN = function(x) cor(x[, 1], x[, 2]),
  by.column = FALSE,
  align = "right"
)

# ---------------------------
# 08. 动态相关性可视化（ggplot实现）
# ---------------------------
date <- index(ts_returns)
df <- data.frame(date = date, correlation = rho_18, type = "DCC")

p <- ggplot(df, aes(x = date, y = correlation)) +
  geom_line(color = "forestgreen", size = 1.2) +
  labs(
    title = "沪深300与中债国债指数动态相关性",
    subtitle = "基于DCC-GARCH模型的滚动相关系数",
    x = "日期",
    y = "相关系数",
    caption = "数据来源：内部处理"
  ) +
  scale_y_continuous(
    limits = c(-1, 1),
    breaks = seq(-1, 1, 0.2),
    labels = scales::percent_format()
  ) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%Y-%m"
  ) +
  theme_minimal()

ggsave(
  filename = "Plots/dcc_correlations_ggplot.png",
  plot = p,
  width = 10, height = 7, dpi = 300
)

# ---------------------------
# 09. 滚动预测与性能评估
# ---------------------------
# 定义预测时间点序列（步长为5）
sequence <- seq(from = 1501, to = 1812, by = 5)
forecast_list <- list()
actual_list <- list()
actual_pre_list <- list()
n <- length(sequence)
last_valid_forecast <- NULL

for (i in sequence) {
  start_idx <- i - 1500  # 设定1500日为训练窗口
  end_idx <- i - 1
  
  current_forecast <- tryCatch({
    res <- dccfit(spec_2, data = ts_returns[start_idx:end_idx])
    forecast <- dccforecast(res, n.ahead = 20)
    cov_pred <- forecast@mforecast$H[[1]]
    cov_avg <- apply(cov_pred, c(1, 2), mean)  # 计算预测协方差矩阵的均值
    
    # 将协方差矩阵转换为相关系数矩阵
    std_devs <- sqrt(diag(cov_avg))
    corr_matrix <- cov_avg / outer(std_devs, std_devs)
    corr_matrix
  }, error = function(e) {
    cat("错误在i=", i, ": ", conditionMessage(e), "\n")
    if (is.null(last_valid_forecast)) return(NA)
    else return(last_valid_forecast)  # 若当前预测失败，使用上一期有效预测
  })
  
  if (!all(is.na(current_forecast))) last_valid_forecast <- current_forecast
  
  # 记录历史相关矩阵与实际相关矩阵
  actual_pre_list[[length(actual_pre_list)+1]] <- cor(ts_returns[(i-260):(i-1), ])
  actual_list[[length(actual_list)+1]] <- cor(ts_returns[i:(i+19), ])
  forecast_list[[length(forecast_list)+1]] <- current_forecast
  
  cat("✓ 完成预测: ", length(forecast_list), "/", n, "\n")
}

# 预测性能评估（基于Frobenius范数计算误差）
total_f <- total_h <- 0
for (i in seq_along(forecast_list)) {
  diff_f <- forecast_list[[i]] - actual_list[[i]]
  diff_h <- actual_pre_list[[i]] - actual_list[[i]]
  total_f <- total_f + sqrt(sum(diff_f^2))
  total_h <- total_h + sqrt(sum(diff_h^2))
}

cat("预测总误差 (Frobenius范数):\n")
cat("DCC模型: ", total_f, "\n")
cat("历史平均: ", total_h, "\n")