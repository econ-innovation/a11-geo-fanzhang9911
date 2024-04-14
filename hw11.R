# 加载包
library(sf)
library(sp)
library(dplyr)
library(readr)
library(ggplot2)
library(geojsonio)
setwd("D:/Rprogram/data_geo/")

hefei <- read.table("hefei.txt", header=TRUE)
hefei_sf <- st_as_sf(hefei, coords = c("lng", "lat"), crs = 4326)
# 读取开发区坐标
dv_zone1 <- read_sf(dsn = "G341022合肥经济技术开发区.txt")
dv_zone2 <- read_sf(dsn = "G342020合肥高新技术产业开发区区块一.txt")
dv_zone3 <- read_sf(dsn = "G342020合肥高新技术产业开发区区块二.txt")
# 开发区内部的企业
inner_dvz1 <- st_intersects(hefei_sf, dv_zone1)
num_corp_dvz1 <- sum(!is.na(inner_dvz1))

inner_dvz2 <- st_intersects(hefei_sf, dv_zone2)
num_corp_dvz2 <- sum(!is.na(inner_dvz2))

inner_dvz3 <- st_intersects(hefei_sf, dv_zone3)
num_corp_dvz3 <- sum(!is.na(inner_dvz3))

# T1a 根据开发区的地理边界与企业地址坐标，计算出开发区内部的企业数量
cat("Number of companies within Development Zone 1:", num_corp_dvz1, "\n")
cat("Number of companies within Development Zone 2:", num_corp_dvz2, "\n")
cat("Number of companies within Development Zone 3:", num_corp_dvz3, "\n")

# 开发区1km，3km，5km范围内的企业数量
radius <- c(1000, 3000, 5000) # in meters

# 
for (r in radius) {
  dv_zone1_buffer <- st_buffer(dv_zone1, dist = r)
  dv_zone2_buffer <- st_buffer(dv_zone2, dist = r)
  dv_zone3_buffer <- st_buffer(dv_zone3, dist = r)
  
  inner1_buffer <- st_intersects(hefei_sf, dv_zone1_buffer)
  inner2_buffer <- st_intersects(hefei_sf, dv_zone2_buffer)
  inner3_buffer <- st_intersects(hefei_sf, dv_zone3_buffer)
  
  num_corp_dvz1_buffer <- sum(sapply(inner1_buffer, length))
  num_corp_dvz2_buffer <- sum(sapply(inner2_buffer, length))
  num_corp_dvz3_buffer <- sum(sapply(inner3_buffer, length))
  
  # T1b 计算出开发区1km，3km，5km范围内的企业数量
  cat("Number of companies within", r/1000, "km from Development Zone 1:", num_corp_dvz1_buffer, "\n")
  cat("Number of companies within", r/1000, "km from Development Zone 2:", num_corp_dvz2_buffer, "\n")
  cat("Number of companies within", r/1000, "km from Development Zone 3:", num_corp_dvz3_buffer, "\n")
}

# T2 画出保定市的四个开发区，以及开发区内的企业
map_dvz <- ggplot() +
  geom_sf(data = dv_zone1, fill = "blue") +
  geom_sf(data = dv_zone2, fill = "green") +
  geom_sf(data = dv_zone3, fill = "yellow") +
  scale_fill_manual(values = c("blue", "green", "yellow"), name = "Zone") +
  theme_minimal()
print(map_dvz)

map_with_corp <- map_dvz +
  geom_sf(data = hefei_sf, color = "red", size = 1) +
  labs(title = "Map of Hefei with Development Zones and Corporates") +
  theme_minimal()
print(map_with_corp)


# T4 自学制作中国地图，将保定市的所有企业都着点在“保定市”地图上
# 读取合肥市地图
hefei_map <- st_read("合肥市.json")
# 绘制合肥市地图
hefei_plot <- ggplot() +
  geom_sf(data = hefei_map) +
  theme_minimal()
print(hefei_plot)
# 将企业添加到地图上
hefei_plot_with_corp <- hefei_plot +
  geom_sf(data = hefei_sf, color = "red", size = 1) +
  labs(title = "Map of Hefei with Corporates")
print(hefei_plot_with_corp)
