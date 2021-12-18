# 사용 라이브러리
library(dplyr)
library(ggplot2) 
library(tidyverse) 
library(lubridate) 
library(patchwork) 
library(plotly) 
library(ggmap) 
library(tidyr) 
library(rhdfs) 
library(rmr2)

# 데이터 불러오기
rmr.options(backend = "hadoop")
files <- hdfs.ls("/data/taxi/combined")$file

mr <- mapreduce(input = files[1], input.format =
make.input.format(format = "csv", sep=",", stringsAsFactors=F)
)
res <- from.dfs(mr) ress <- values(res)
colnames.tmp <- as.character(ress[,1]) class.tmp <- as.character(ress[,2])
colnames <- colnames.tmp[-1] class <- class.tmp[-1]
class[c(6,8,9,10)] <- "numeric"
input.format <- make.input.format(
format = "csv", sep = ",", stringsAsFactors = F,
col.names = colnames, colClasses = class )
files <- files[-1]
data <- mapreduce(input = files, input.format = input.format) res.data <- from.dfs(data)
taxi <- res.data$val
dat <- taxi[,-12] # store_and_fwd_flag 변수 삭제

# 데이터 전처리
## 1. rate code
n_of_obs <- dat %>% group_by(rate_code) %>% summarize(count=n())
ggplot(n_of_obs,
aes(x = as.factor(rate_code), y = count, fill
= rate_code)) +
geom_bar(stat = "identity") + labs(x = "rate code",
y = "count",
title = "rate code") + geom_text(aes(label = count), vjust=0) + theme_bw()

## rate_code 변수가 1~6의 값이 아닌 것 삭제 
dat <- dat[-which(dat$rate_code==0 | dat$rate_code == 210 | dat$rate_code == 7 | dat$rate_code == 9 | dat$rate_code == 8),]

## 2. trip_distance(mile)
dat %>% ggplot(aes(x = "data", y = trip_distance)) +
geom_boxplot() + labs(x = "taxi data",
y = "trip distance",
title = "trip distance", subtitle = "box plot") +
theme_bw() +
annotate("text", x = 1.3, y = 10000000, size = 5,
label = paste0("range = ", paste(as.character(range(dat$trip_distance)[1]),as.c haracter(range(dat$trip_distance)[2]), sep= " to ")))

## 뉴욕주 최장거리로 횡단 시 60 mile, 0 mile 이하의 값이나 100 mile 이상의 값 삭제
dat <- dat[-which(dat$trip_distance <= 0 | dat$trip_distance >= 100),] summary(dat$trip_distance)

## 3. trip_time_in_secs
dat %>% ggplot(aes(x = "data", y = trip_time_in_secs)) +
geom_boxplot() + labs(x = "taxi data",
y = "trip time",
title = "trip time",
subtitle = "box plot(sec)") +
theme_bw() +
annotate("text", x = 1.3, y = 4000000, size = 5,
label = paste0("range = ", paste(as.character(range(dat$trip_time_in_secs)[1]), as.character(range(dat$trip_time_in_secs)[2]), sep= " to ")))

## trip_time_in_secs이 60 이하이거나 4,000,000 이상인 것들 삭제
dat <- dat[-which(dat$trip_time_in_secs <= 60 | dat$trip_time_in_secs >= 4000000), ]

## 4. passenger_count
n_of_obs <- dat %>% group_by(passenger_count) %>% summarize(count=n())
ggplot(n_of_obs,
aes(x = as.factor(passenger_count), y =
count, fill = as.factor(passenger_count))) + geom_bar(stat = "identity") +
labs(x = "passenger count",
y = "count",
title = "passenger count", fill = "passenger count") +
geom_text(aes(label = count), vjust=0) + theme_bw()

## 승객 수가 0명 이하인 것 제외
dat <- dat[-which(dat$passenger_count<= 0) ,]

## 5. longitude, latitude
g1 <- dat %>% ggplot(aes(x = pickup_longitude, y = pickup_latitude)) +
geom_hex(bins=100) + scale_fill_continuous(type = "viridis") + labs(title = "승차 경도 및 위도") + theme_bw()
g2 <- dat %>% ggplot(aes(x = dropoff_longitude, y = dropoff_latitude)) +
geom_hex(bins=100) + scale_fill_continuous(type = "viridis") + labs(title = "하차 경도 및 위도") + theme_bw()
g1 + g2

## 뉴욕주 경도 범위 -74.2 ~ -73.5
## 뉴욕주 위도 범위 40.45 ~ 40.95
## 뉴욕주 위도 경도에서 벗어나는 점들 제외
dat <- dat[-which(dat$pickup_longitude <= -74.2 | dat$pickup_longitude >= -73.5) ,]
dat <- dat[-which(dat$pickup_latitude <= 40.45 | dat$pickup_latitude >= 40.95) ,]
dat <- dat[-which(dat$dropoff_longitude <= -74.2 | dat$dropoff_longitude >= -73.5) ,]
dat <- dat[-which(dat$dropoff_latitude <= 40.45 | dat$dropoff_latitude >= 40.95) ,]

ggmap::register_google(key = "API Key") ggmap::has_google_key()
NY <- ggmap::get_map("New York, NY, USA", maptype = "terrain")
g3 <- ggmap(NY) + geom_point(data = dat, aes(x =
pickup_longitude, y = pickup_latitude, color = rate_code), size =.03, alpha =.1) +
labs(title = "Taxi PickUp location") +
facet_wrap(~rate_code) g3
g4 <- ggmap(NY) + geom_point(data = dat, aes(x =
dropoff_longitude, y = dropoff_latitude, color= rate_code), size =.05) +
labs(title = "Taxi dropoff location") +
facet_wrap(~rate_code) g4
g3 + g4

## 6. tip_amount
dat %>% ggplot(aes(x = "data", y = tip_amount, fill=payment_type, color= payment_type)) +
geom_boxplot(outlier.size = 1, outlier.alpha = .3, notchwidth = 0.01) +
labs(x = "taxi data",
y = "tip amount",
title = "tip amount(dollar)",
subtitle = "types of payment") + theme_bw() +
facet_wrap(~payment_type)

## tip이 음수인 것 제외
dat <- dat[-which(dat$tip_amount < 0), ]

## 7. total_amount
dat %>% ggplot(aes(x = "data", y = total_amount, fill=payment_type, color= payment_type)) +
geom_boxplot(outlier.size = 1, outlier.alpha = .3, lwd=0.1, notch = T) +
labs(x = "taxi data",
y = "Total amount",
title = "Total_amount",
subtitle = "box plot(dollar)") + theme_bw() +
facet_wrap(~payment_type)

## 총 금액이 0 이하인 것 제외
dat <- dat[-which(dat$total_amount <= 0), ]

## 8. 속력 변수 km/h
dat.res <- dat %>% select(trip_distance, trip_time_in_secs) %>% mutate(km_per_hour = 5793.64*trip_distance/trip_time_in_secs)
dat.res %>% ggplot(aes(x = "data", y = km_per_hour)) +
geom_boxplot(outlier.size = 1, outlier.alpha = .3, lwd=0.1, notch = T) +
labs(x = "taxi data", y = "Km/h",
title = "Speed") + theme_bw()

## 5 이하 150 이상 제거
dat <- dat[-which(dat.res$km_per_hour <= 5 | dat.res$km_per_hour >= 150), ]
