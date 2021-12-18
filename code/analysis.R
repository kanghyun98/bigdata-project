# K-means 함수 사용
max.iter <- 1:10
for(i in seq_along(max.iter)){ 
  mr <- from.dfs(mapreduce( to.dfs(dat[,c("trip_time _in_secs", "trip_distance", "total_amount")]), map = kmeans.map))
  C <- aggregate(x = mr$val, by = list(mr$key), FUN = mean)[,-c(1,2)]; C 
}

# 데이터 클러스터 번호 열 추가
s.mr <- arrange(mr$val, trip_time_in_secs, total_amount, trip_distance)
s.dat <- arrange(dat, trip_time_in_secs,total_amount,trip_distance)
clusterNum <- s.mr %>% select(nearest)
taxiWithGroup <- s.dat %>% mutate(cluster = clusterNum[,1])

# 데이터 클러스터 별로 나누기
taxiG1 <- taxiWithGroup[which(taxiWithGroup$cluster == 3), ] %>% select(-cluster) # 단거리 
taxiG2 <- taxiWithGroup[which(taxiWithGroup$cluster == 1), ] %>% select(-cluster) # 중거리 
taxiG3 <- taxiWithGroup[which(taxiWithGroup$cluster == 2), ] %>% select(-cluster) # 장거리

# 단거리 분석

## 중선형회귀 P=2
reg.result <- values(from.dfs(mapreduce(to.dfs(taxiG1), map = regmap, reduce = regreduce, combine = T)))
reg.summary <- fun.summary(reg.result)

## 잔차가 큰 값 선택
## 디자인 행렬
taxiDesingMat <- model.matrix(object = total_amount ~ trip_distance + trip_time_in_secs, data = taxiG1[,c("trip_distance", "trip_time_in_secs", "total_amount")])

## 추정치 계산
fittedFare <- t(reg.summary$stat$beta.hat) %*% t(taxiDesingMat)
fittedFare <- t(fittedFare)
resi <- taxiG1$total_amount - fittedFare[,1] # 잔차
BigRes <- sort(resi, decreasing = T) %>% names() # 잔차 INDEX
BigResIDX <- BigRes[1:ceiling(length(BigRes)*0.001)] # 잔 차 INDEX 중 상위 0.1%

## 실제낸돈-추정한정상금액==잔차 
taxiG1[BigResIDX,] %>% mutate(estimateFare = fittedFare[,1][BigResIDX], residual = resi[BigResIDX]) %>% select(total_amount, estimateFare, residual) %>% head() taxiG1[BigResIDX,] %>%
mutate(estimateFare = fittedFare[,1][BigResIDX], residual = resi[BigResIDX]) %>% select(total_amount, estimateFare, residual) %>% tail()
# 변수가 다 있는 데이터로 돌아옴 
selectedDataG1 <- taxiG1[BigResIDX, ]

#가설1
# VendorID : 특정 업체에서 유독 많다 
selectedDataG1 %>% select(vendor_id) %>% table()
nOfObs <- selectedDataG1 %>% group_by(vendor_id) %>% summarize(count = n())

ggplot(nOfObs, aes(x = as.factor(vendor_id), y = count, fill = vendor_id)) + geom_bar(stat = "identity") + labs(x = "vendor ID",
y = "count", title = "vendor ID", subtitle = "rate code 1, short distance") + geom_text(aes(label = count), vjust=0) + theme_bw() # CMT사가 VTS 사에 비해 많다.

#가설2
# 덤티기를 씌우는 기사들은 특징이 있을 것이다.
BadDrivers <- selectedDataG1 %>% select(hack_license, total_amount) %>% mutate(estimateFare = fittedFare[,1][BigResIDX]) %>% group_by(hack_license) %>% summarize(totalSum = sum(total_amount), estimateSum = sum(estimateFare), count = n()) %>% mutate(ratio = totalSum/estimateSum) %>% arrange(desc(count))
BadDrivers %>% arrange(desc(ratio))
allDrivingFreqG1 <- taxiG1 %>% select(hack_license) %>% group_by(hack_license) %>%
summarize(count = n()) bigResDrivingFreaG1 <- selectedDataG1 %>% select(hack_license) %>% group_by(hack_license) %>% summarize(count=n())
count <- inner_join(allDrivingFreqG1, bigResDrivingFreaG1, by = "hack_license", suffix = c("All", "Outlier")) %>% arrange(desc(countOutlier)) BadDriversDone <- bind_cols(BadDrivers, count[,"countAll"])[, c("hack_license","totalSum", "estimateSum", "ratio", "count", "countAll")]
BadDriversDone %>% arrange(desc(count)) %>% head(20)
pivotBadDrivers <- BadDriversDone %>% select(hack_license, totalSum, estimateSum, ratio) %>% arrange(desc(ratio)) %>% head(10) %>% select(-ratio) %>% pivot_longer(cols = !hack_license, names_to = "sumType", values_to = "value")
ggplot(pivotBadDrivers, aes(x = hack_license, y = value, fill = sumType)) +
geom_col(position = "dodge") + theme(axis.text.x = element_blank()) + labs(title = "Top 10 billing rate")

# datetime : 특정시간대인지
pickupHourG1 <- hour(selectedDataG1$pickup_datetime) %> % table() %>% as.data.frame() names(pickupHourG1) <- c("Hour", "count")
dropoffHourG1 <- hour(selectedDataG1$dropoff_datetime) %> % table() %>% as.data.frame() names(dropoffHourG1) <- c("Hour", "count")
hourG1 <- inner_join(pickupHourG1, dropoffHourG1, by="Hour", suffix = c("Pickup","Dropoff"))
hourG1 <- hourG1 %>% pivot_longer(cols = !Hour, names_to = "hourType", values_to = "value")

# Change line colors by groups ggplot(hourG1, aes(x=Hour, y=value, group=hourType)) +
geom_line(aes(color=hourType), size = 1) + geom_point(aes(color=hourType), size = 2)

# Passenger count : 특정 승객수인지 
NofObs <- selectedDataG1 %>% group_by(passenger_count) %>% summarize(count=n())
ggplot(NofObs, aes(x = as.factor(passenger_count), y = count, fill = as.factor(passenger_count))) + geom_bar(stat = "identity") + labs(x = "passenger count", y = "count", title = "passenger count", fill = "passenger count") +
geom_text(aes(label = count), vjust=0) + theme_bw() # 1명일때가 많다.

# 특정 승하차 위치 
ggmap::register_google(key = "API KEY") 
ggmap::has_google_key()
NYMAP <- ggmap::get_map("New York, NY, USA", maptype = "terrain")
g3 <- ggmap(NYMAP) + geom_point(data = selectedDataG1, aes(x =pickup_longitude, y = pickup_latitude), size =.03, alpha =.05) + labs(title = "Taxi PickUp location")
g4 <- ggmap(NYMAP) + geom_point(data = selectedDataG1, aes(x = dropoff_longitude, y = dropoff_latitude), size =.03, alpha=.05) + labs(title = "Taxi dropoff location") g3 + g4

# 밀집된 위경도
# (1)
# 40.77411263879798, -73.87177711031461 East Elmhurst (지역)
# 뉴욕 라과디아 공항 + 몇 유명 호텔

# (2)
# 40.778186, -73.925555
# 관광명소
# kenedy bridge + 에스토리아 공원

# (3)
# 40.7489662320195, -73.98564147146332
# 관광명소
# 엠파이어 스테이트 빌딩

# (4)
# 73.96316396457098
# 관광명소
# 메트로폴리탄 미술관, 솔로몬 R, 구겐하 임 미술관, 센트럴파크

# (5)
# 40.81007194617472, -73.95006688207894 # 관광명소
# 아폴로 영화관, 알로프트 할렘, 마르쿠스 가비공원

# Payment type : 특정 지불 방식인지 
NofObs <- selectedDataG1 %>% group_by(payment_type) %>% summarize(count=n())
ggplot(NofObs, aes(x = as.factor(payment_type), y = count, fill = as.factor(payment_type))) + geom_bar(stat = "identity") +
labs(x = "payment type", y = "count", title = "payment type", fill = "payment type") + geom_text(aes(label = count), vjust=0) + theme_bw()

ggplot(NofObs, aes(x = as.factor(payment_type), y = count, fill = as.factor(payment_type))) + geom_bar(stat = "identity") +
labs(x = "payment type", y = "count", title = "payment type", fill = "payment type") + geom_text(aes(label = count), vjust=0) + theme_bw()

# 대부분이 카드 데이터에 있다.
# 아마 현금으로 결제한 덤티기에 대해서 는 입력을 기사가 일부러 안할 수도 있다

# TIp amount : 팁이 비싼지? 
selectedDataG1[- which(selectedDataG1$tip_amount == 0),] %>% mutate(tipratio = tip_amount / total_amount) %>% select(tipratio) %>% summary()

# cluster 1 (전체데이터) 에서 팁의 비율 (= 일반적인 팁의 비율, 약 15%) 
taxiG1[-which(taxiG1$tip_amount == 0),] %>% mutate(tipratio = tip_amount / total_amount) %>% select(tipratio) %>% summary()

# 전체 데이터에서의 tip Ratio가 15%인데, 이상치로 판별된 데이터 셋의 tip Ratio는 15%보다 한참 큰 34.3% 이다(평균, tip=0 제외).
# total amount에는 tip이 포함되지 않았음 에도 불구하고.
# 미터기 금액도 많이 내고, 팁도 많이 내고

# 중거리, 장거리 분석
# 위의 단거리 분석에서 변수명만 수정해서 사용 가능 000G1 -> 000G2 or G3