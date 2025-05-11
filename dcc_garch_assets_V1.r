# 设置工作目录
setwd('/Users/lzc19950714/工作文件夹/招银理财--资产配置赛道')

# 导入必要的包
# zoo包提供时间序列相关功能
library(zoo)
# xts包用于处理时间序列数据
library(xts)
# ggplot2包用于数据可视化
library(ggplot2)
# dplyr包用于数据操作和处理
library(dplyr)
# PerformanceAnalytics包提供金融性能分析功能
library(PerformanceAnalytics)
# rugarch包用于拟合GARCH等波动率模型
library(rugarch)
# rmgarch包用于估计多元GARCH模型
library(rmgarch)
# showtext包用于自动检测和加载系统中文字体
library(showtext)
# reshape2包用于数据重塑
library(reshape2)
# ggcorrplot包用于绘制相关系数矩阵热力图
library(ggcorrplot)
# fBasics包提供基本的统计函数
library(fBasics)
# forecast包用于时间序列预测
library(forecast)
# fUnitRoots包用于单位根检验
library(fUnitRoots)
library(scales)

# quartz函数设置绘图设备的宽度和高度
quartz(width = 10, height = 4)

# 自动检测并加载系统中文字体
showtext_auto()

# 读取 CSV 文件
# 数据通过Excel和Python进行初步处理
# read.csv函数读取CSV文件数据
data <- read.csv("Data/指数行情_processed.csv") 
# dim函数查看数据的维度
dim(data)

# 查看数据的基本信息
# str函数查看数据的结构
str(data)

# 转换日期格式
# as.Date函数将日期列转换为日期格式，格式为%Y-%m-%d
data$日期 <- as.Date(data$日期, format = "%Y-%m-%d")

# 查看数据的前几行
# head函数查看数据的前几行
head(data)

# 转换为时间序列
# 提取日期列
dates <- data$日期
# 去除日期列，保留其他数据列
data_columns <- data[, -which(names(data) == "日期")]
# 将数据转换为xts格式的时间序列，设置order.by为日期
ts_prices <- xts(data_columns, order.by = dates)

# 计算对数收益率
# diff函数计算对数收益率，log函数计算对数，[-1,]去除第一个元素（因为对数收益率计算会少一个元素）
ts_returns <- diff(log(ts_prices))[-1,]

# 查看收益率数据的前几行
head(ts_returns)

# 滚动计算波动率
# 设置滚动窗口大小为20
window_size <- 20
# rollapply函数滚动应用函数，这里计算年化标准差
rolling_vol <- rollapply(
  ts_returns, 
  width = window_size, 
  FUN = function(x) apply(x, 2, StdDev.annualized), 
  by.column = FALSE, 
  align = "right"
)

# 可视化分析
# 将时间序列数据转换为数据框，包含日期和收益率数据
returns <- data.frame(
  date = index(ts_returns),
  coredata(ts_returns)
)
# 将滚动波动率数据转换为数据框，包含日期和波动率数据
volatility <- data.frame(
  date = index(rolling_vol), 
  coredata(rolling_vol)
)
# 获取收益率数据框的列名（除日期列）
assets <- names(returns)[-1]
# 创建Plots文件夹，showWarnings = FALSE表示不显示警告信息
dir.create("Plots", showWarnings = FALSE)

for (asset in assets){
  # 1. 收益率变化
  # ggplot函数创建绘图，aes设置x和y变量，geom_line绘制折线图，labs设置标题、x轴和y轴标签，theme_minimal设置主题
  p1 <- ggplot(returns, aes(x = date, y = .data[[asset]])) + 
    geom_line(colour = "steelblue") + 
    labs(
      title = paste0(asset, " 收益率时间序列"), 
      y = "收益率", 
      x = "日期"
    ) +
    theme_minimal()
  
  # 保存时间序列图
  ggsave(
    filename = paste0("Plots/", gsub("[^[:alnum:]]", "_", asset), "收益率.png"),
    plot = p1,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  # 2. 直方图与正态分布拟合
  # 计算资产收益率的均值，na.rm = TRUE表示处理缺失值
  asset_mean <- mean(returns[[asset]], na.rm = TRUE)
  # 计算资产收益率的标准差，na.rm = TRUE表示处理缺失值
  asset_sd <- sd(returns[[asset]], na.rm = TRUE)
  
  p2 <- ggplot(returns) + 
    geom_histogram(
      aes(x = .data[[asset]], y = after_stat(density)), 
      binwidth = 0.001, 
      color = "steelblue", 
      fill = "lightgray", 
      linewidth = 0.5
    ) +
    stat_function(
      fun = dnorm, 
      args = list(mean = asset_mean, sd = asset_sd), 
      color = "red",
      linewidth = 1
    ) +
    labs(
      title = paste0(asset, " 收益率分布"),
      x = "收益率",
      y = "密度"
    ) +
    theme_minimal() +
    annotate(
      "text", 
      x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5,
      label = paste0("均值: ", round(asset_mean, 4), 
                     "\n标准差: ", round(asset_sd, 4)),
      size = 3
    )
  
  # 保存直方图
  ggsave(
    filename = paste0("Plots/", gsub("[^[:alnum:]]", "_", asset), "收益率分布.png"),
    plot = p2,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  # 3. 波动率变化
  # ggplot函数创建绘图，aes设置x和y变量，geom_line绘制折线图，labs设置标题、x轴和y轴标签，theme_minimal设置主题
  p3 <- ggplot(volatility, aes(x=date, y = .data[[asset]])) + 
    geom_line( color="steelblue") + 
    labs(
      title = paste0(asset, " 波动率率时间序列"), 
      y = "波动率", 
      x = "日期"
    ) +
    theme_minimal()
  
  # 保存时间序列图
  ggsave(
    filename = paste0("Plots/", gsub("[^[:alnum:]]", "_", asset), "波动率.png"),
    plot = p3,
    width = 10,
    height = 6,
    dpi = 300
  )
}

# 静态相关性计算
# cor函数计算时间序列的相关性，use = "pairwise.complete.obs"处理缺失值
static_cor_mat <- cor(ts_returns, use = "pairwise.complete.obs")

# 提取年份信息
# format函数提取时间序列索引中的年份信息
ts_years <- format(index(ts_returns), "%Y")
# unique函数获取唯一的年份
unique_years <- unique(ts_years)

# 循环每一年，计算相关矩阵 & 绘图
# 遍历每一个唯一的年份
for (year in unique_years) {
  # 提取该年度数据
  # 根据年份提取对应的时间序列数据
  yearly_returns <- ts_returns[ts_years == year]
  
  # 检查是否数据过少
  # nrow函数获取数据行数，当行数小于2时跳过
  if (nrow(yearly_returns) < 2) next
  
  # 计算静态相关性矩阵
  # cor函数计算年度数据的相关性，use = "pairwise.complete.obs"处理缺失值
  cor_mat <- cor(yearly_returns, use = "pairwise.complete.obs")
  
  # 转换相关矩阵为ggplot可用的长格式
  # reshape2::melt函数将相关矩阵转换为长格式
  cor_melted <- reshape2::melt(cor_mat)
  
  p <- ggcorrplot(
    cor_mat,
    hc.order = FALSE,  # 保持原始变量顺序
    type = "upper",
    outline.color = "white",
    ggtheme = ggplot2::theme_gray,
    colors = c("#6D9EC1", "white", "#E46726"),
    lab = TRUE,
    lab_size = 3.5  # 控制标签数字的字体大小
  ) + 
    theme(
      axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
      axis.text.y = element_text(size = 10),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA)
    )
  
  # 保存图像
  # ggsave函数保存绘图，设置文件名、绘图对象、宽度、高度和分辨率
  ggsave(filename = paste0("Plots/", year, "年资产间静态相关性热力图.png"),
         plot = p,
         width = 8,
         height = 7.5,
         dpi = 300)
}

# 初始化结果列表
adf_results <- list()

# 对每个资产进行 ADF 检验
# 遍历每个资产
for (asset in assets) {
  # adfTest函数进行ADF检验，type = "nc"表示无常数项和趋势项
  adf_result <- adfTest(ts_returns[, asset], type = "nc")
  # 将ADF检验的P值存储在列表中
  adf_results[[asset]] <- adf_result@test$p.value
}

# 打印ADF检验的P值，P值均为0.01，说明序列平稳
adf_results 

# 循环每个资产，绘制并保存 ACF 和 PACF 图
# 遍历每个资产
for (asset in assets) {
  # 设置文件名
  acf_file <- paste0("Plots/", asset, "_acf.png")
  pacf_file <- paste0("Plots/", asset, "_pacf.png")
  
  # 绘制并保存 ACF 图
  # png函数打开png设备，设置文件名
  png(acf_file)  
  # acf函数绘制自相关函数图，设置标题
  acf(ts_returns[, asset], main = paste("ACF of", asset))  
  # dev.off函数关闭图形设备
  dev.off()  
  
  # 绘制并保存 PACF 图
  # png函数打开png设备，设置文件名
  png(pacf_file)  
  # pacf函数绘制偏自相关函数图，设置标题
  pacf(ts_returns[, asset], main = paste("PACF of", asset))  
  # dev.off函数关闭图形设备
  dev.off()  
}

# 初始化结果列表
LB_results <- list()
LB_ARCH_results <- list()
mean_arma_orders <- list()

# 遍历每个资产
for (asset in assets) {
  # auto.arima函数自动选择ARMA模型
  mean_arma <- auto.arima(ts_returns[, asset])
  
  # 提取AR和MA的阶数
  ar_order <- mean_arma$arma[1]
  ma_order <- mean_arma$arma[2]
  # 将AR和MA的阶数存储在列表中
  mean_arma_orders[[asset]] <- c(ar_order, ma_order)
  
  # 提取均值模型的残差
  res <- mean_arma$residuals
  
  # 检验均值模型充分性
  # Box.test函数进行Ljung检验，检验均值模型的残差是否为白噪声
  LB_results[[asset]] <- Box.test(res, type = "Ljung")$p.value  # 建模均充分
  
  # 检验ARCH效应
  # Box.test函数对残差平方进行Ljung检验，检验是否存在ARCH效应
  LB_ARCH_results[[asset]] <- Box.test(res^2, type = "Ljung")$p.value  # 均有ARCH效应
}

print(LB_results)
print(LB_ARCH_results)

# 定义需要测试的参数组合
# 定义分布模型列表
distribution_models <- c("norm", "snorm", "std", "sstd")
# 定义方差模型列表
variance_models <- c("sGARCH", "gjrGARCH")
# 定义均值模型列表
mean_models <- list(c(0,0), c(1,0), c(0,1), c(1,1))

# 创建空的数据框来存储结果
# 创建数据框，定义列名和数据类型
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

# 开始循环遍历每一列和每一种参数组合
# 初始化最佳模型规格列表
best_specs <- list()
# 遍历时间序列的每一列
for (col_idx in 1:ncol(ts_returns)) {
  # 提取当前列的数据
  column_data <- ts_returns[, col_idx]
  # 提取当前列的列名
  column_name <- colnames(ts_returns)[col_idx]
  
  cat(paste0("\n=== 处理列: ", column_name, " (", col_idx, "/", ncol(ts_returns), ") ===\n"))
  
  # 临时存储当前列的所有模型结果
  column_results <- data.frame()
  
  # 遍历每种分布模型
  for (dist_model in distribution_models) {
    # 遍历每种方差模型
    for (var_model in variance_models) {
      # 遍历每种均值模型
      for (mean_order in mean_models) {
        # 构建模型规格
        spec <- ugarchspec(
          variance.model = list(model = var_model, garchOrder = c(1,1)),
          mean.model = list(armaOrder = mean_order, include.mean = TRUE),
          distribution.model = dist_model
        )
        
        # 使用 tryCatch 捕获可能的错误
        tryCatch({
          # ugarchfit函数拟合模型
          fit <- ugarchfit(spec, data = column_data, solver = "solnp")
          
          # 提取信息准则
          info_crit <- infocriteria(fit)
          aic <- info_crit[1]
          bic <- info_crit[2]
          loglik <- likelihood(fit)
          
          # 记录结果
          model_result <- data.frame(
            column_name = column_name,
            distribution_model = dist_model,
            variance_model = var_model,
            mean_model = paste(mean_order, collapse = ","),
            AIC = aic,
            BIC = bic,
            logLik = loglik,
            stringsAsFactors = FALSE
          )
          
          results <- rbind(results, model_result)
          column_results <- rbind(column_results, model_result)
          
          cat(paste0("✓ 完成: ", dist_model, ", ", var_model, ", ARMA(", 
                     paste(mean_order, collapse=","), ")\n"))
          
        }, error = function(e) {
          cat(paste0("✗ 失败: ", dist_model, ", ", var_model, ", ARMA(", 
                     paste(mean_order, collapse=","), ") - 错误: ", e$message, "\n"))
        })
      }
    }
  }
  
  # 检查是否有成功拟合的模型
  if (nrow(column_results) > 0) {
    # 根据AIC选择最佳模型
    best_model <- column_results[which.min(column_results$AIC), ]
    
    # 重新构建最佳模型的规格
    best_spec <- ugarchspec(
      variance.model = list(model = best_model$variance_model, garchOrder = c(1,1)),
      mean.model = list(armaOrder = as.numeric(strsplit(best_model$mean_model, ",")[[1]]), include.mean = TRUE),
      distribution.model = best_model$distribution_model
    )
    
    best_specs[[column_name]] <- best_spec
    cat(paste0("✅ 列 ", column_name, " 的最佳模型: ", 
               best_model$distribution_model, ", ", 
               best_model$variance_model, ", ARMA(", 
               best_model$mean_model, ")\n"))
  } else {
    cat(paste0("⚠️ 列 ", column_name, " 没有成功拟合的模型\n"))
    # 可以选择为该列设置默认规格或跳过
    best_specs[[column_name]] <- NULL  # 或设置默认规格
  }
}

# 根据AIC选择每列的最佳模型
best_models <- results %>%
  group_by(column_name) %>%
  filter(AIC == min(AIC)) %>%
  ungroup()

# 打印最佳模型结果
print(best_models)

# 创建多变量GARCH模型规格
# multispec函数将多个单变量GARCH模型规格组合为多变量规格
mspec_1 <- multispec(best_specs)

# 创建DCC模型规格
spec_1 <- dccspec(
  # 单变量规格 - 需要是multispec对象
  uspec = mspec_1,
  # DCC规格，假设为ARMA(1,1)类过程
  dccOrder = c(1,1),
  # 分布，这里使用多元正态分布
  distribution = "mvnorm"
)

# 估计DCC模型
# dccfit函数拟合DCC-GARCH模型
res_1 <- dccfit(spec_1, data = ts_returns)

# 创建单变量 GARCH 模型规格
# 参考best_models使用统一的uspec
uspec <- ugarchspec(
  variance.model = list(
    model = "gjrGARCH",
    garchOrder = c(1,1)),
  mean.model = list(
    armaOrder = c(0,0), 
    include.mean = FALSE),
  distribution.model = "std"
)

# 创建多变量 GARCH 模型规格
# replicate函数复制单变量规格，创建多变量规格
mspec_2 <- multispec(replicate(14, uspec))

# 创建DCC模型规格
spec_2 <- dccspec(
  # 单变量规格 - 需要是multispec对象
  uspec = mspec_2,
  # DCC规格，假设为ARMA(1,1)类过程
  dccOrder = c(1,1),
  # 分布，这里使用多元正态分布
  distribution = "mvnorm"
)

# 估计DCC模型
res_2 <- dccfit(spec_2, data = ts_returns)

# 提取协方差矩阵序列
H = res_2@mfit$H

# 计算两个资产间的相关系数序列
rho_18 <- H[1,8,] / sqrt(H[1,1,]*H[8,8,])

# 计算实际的滚动相关系数
# 选择两个资产的数据
ts_returns_18 = ts_returns[, c(1, 8)]
# rollapply函数计算滚动相关系数，窗口大小为260
rolling_cor <- rollapply(
  ts_returns_18, 
  width = 260, 
  FUN = function(x) cor(x[,1], x[,2]),  # 显式定义函数
  by.column = FALSE,  # 确保按窗口整体计算
  align = "right"
)

# 绘制相关性趋势图
date <- index(ts_returns)
matplot(date, cbind(rho_18, rolling_cor),
        type='l',
        bty='l',
        lty=1,
        col=c("green"),
        main="DCC correlations for 沪深300 and 中债国债指数",
        ylab="Correlations",
        las=1
)
legend("bottomright",
       legend=c("DCC"),
       lty=1,
       col=c("green"),
       bty='n'
)
# 绘制滚动相关性趋势图并保存
# 假设date, rho_18, rolling_cor是已有的数据向量
# 转换为数据框
df <- data.frame(
  date = date,
  correlation = rho_18,  # 假设rho_18是要绘制的相关系数
  type = "DCC"
)

# 创建ggplot对象
p <- ggplot(df, aes(x = date, y = correlation)) +
  # 添加线条层
  geom_line(color = "forestgreen", size = 1) +
  
  # 添加标题和标签
  labs(
    title = "相关性分析：沪深300与中债国债指数",
    subtitle = "滚动相关系数走势",
    y = "相关系数",
    x = "日期",
    caption = "数据来源：XX数据库"
  ) +
  
  # 设置Y轴范围
  scale_y_continuous(
    limits = c(-1, 1),
    breaks = seq(-1, 1, 0.2),
    labels = percent_format()  # 以百分比形式显示
  ) +
  
  # 设置X轴日期格式（根据date类型调整）
  scale_x_date(
    date_breaks = "3 months",  # 每3个月一个刻度
    date_labels = "%Y-%m"     # 日期格式
  ) +

# 保存图形
ggsave("dcc_correlations_ggplot.png", 
       plot = p, 
       width = 10, 
       height = 7, 
       dpi = 300,
       units = "in")

# 生成未来20期的预测
forecast <- dccforecast(res_2, n.ahead = 20)
# 提取预测的协方差矩阵
cov_pred = forecast@mforecast$H[[1]]
# 查看预测协方差矩阵的维度
dim(cov_pred)
# 查看第一个预测协方差矩阵
cov_pred[, , 1]
# 计算预测协方差矩阵的总和
cov_pred_sum = apply(cov_pred, c(1, 2), sum)
# 查看总和矩阵的维度
dim(cov_pred_sum)
# 打印总和矩阵
print(cov_pred_sum)

# 重新进行滚动预测，这次计算相关系数矩阵
sequence <- seq(from = 1501, to = 1812, by = 5)
forecast_list <- list()  
actual_list <- list()
actual_pre_list <- list()
n <- length(sequence)
last_valid_forecast <- NULL  # 用于存储上一期的有效预测值

for (i in sequence) {
  # 确定训练数据的起始和结束索引
  # 使用1500日数据拟合DCC
  start_idx <- i - 1500  
  end_idx <- i - 1      
  
  # 使用tryCatch捕获可能的错误
  current_forecast <- tryCatch({
    # 拟合DCC模型
    res <- dccfit(spec_2, data = ts_returns[start_idx:end_idx])
    
    # 生成未来20期的预测
    forecast <- dccforecast(res, n.ahead = 20)
    
    # 提取预测的协方差矩阵
    cov_pred <- forecast@mforecast$H[[1]]
    # 计算预测协方差矩阵的平均值
    cov_pred_avg <- apply(cov_pred, c(1, 2), mean)
    
    # 计算标准差向量
    std_devs <- sqrt(diag(cov_pred_avg))
    # 创建标准差的外积矩阵
    std_outer <- outer(std_devs, std_devs)
    # 计算相关系数矩阵
    corr_matrix <- cov_pred_avg / std_outer
    
    corr_matrix
  }, error = function(e) {
    # 打印错误信息
    cat(paste0("错误: 在i=", i, "时出现问题: ", conditionMessage(e), "\n"))
    
    # 如果是第一次出现错误且没有上一期预测，则返回NA
    if (is.null(last_valid_forecast)) {
      cat("警告: 这是第一次预测且出现错误，没有上一期预测可使用\n")
      return(NA)
    } else {
      # 使用上一期的预测值
      cat("使用上一期的预测值作为替代\n")
      return(last_valid_forecast)
    }
  })
  
  # 如果当前预测有效，则更新last_valid_forecast
  if (!all(is.na(current_forecast))) {
    last_valid_forecast <- current_forecast
  }
  
  # 将相关系数矩阵添加到结果列表中
  forecast_list[[length(forecast_list) + 1]] <- current_forecast
  
  # 计算历史相关系数矩阵
  start_idx_0 <- i - 260
  end_idx_0 <- i - 1
  corr_actual_pre <- cor(ts_returns[start_idx_0:end_idx_0])
  actual_pre_list[[length(actual_pre_list) + 1]] <- corr_actual_pre
  
  # 计算实际未来相关系数矩阵
  start_idx_1 <- i 
  end_idx_1 <- i + 19
  corr_actual <- cor(ts_returns[start_idx_1:end_idx_1])
  actual_list[[length(actual_list) + 1]] <- corr_actual
  
  cat(paste0("✓ 完成", length(forecast_list), "/", n, "\n"))
}

# 查看第一个预测结果、实际结果和历史结果
forecast_list[[1]]
actual_list[[1]]
actual_pre_list[[1]]
 
# 评估预测性能
seq <- 1:length(forecast_list)
total_f <- 0
total_h <- 0
for (i in seq) {
  # 计算预测误差
  diff_forecast = forecast_list[[i]] - actual_list[[i]]
  # 计算历史方法误差
  diff_historical = actual_pre_list[[i]] - actual_list[[i]]
  # 计算Frobenius范数
  frobenius_norm_f <- sqrt(sum(diff_forecast^2))
  frobenius_norm_h <- sqrt(sum(diff_historical^2))
  # 累加误差
  total_f <- total_f + frobenius_norm_f
  total_h <- total_h + frobenius_norm_h
}

# 打印总误差
total_f
total_h
