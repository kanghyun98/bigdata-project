### 변수명

- VendorID: TPEP 제공 업체 코드
  - 1: Creative Mobile Technologies, LLC
  - 2: VeriFone Inc.
- **tpep pickup datetime**: 승차 datetime(날짜, 시간)
- **tpep dropoff datetime**: 하차 datetime
- **Passenger count**: 승객 수
- **Trip distance**: 이동 거리(mile)
- Trip-time: 여행 시간 (sec)
- Pickup longitude: 승차 경도
- Pickup latitude: 승차 위도
- RateCodeID: 도착지에 따른 비용 코드
  - 1: Standard rate
  - 2: JFK (공항)
  - 3: Newark (공항)
  - 4: Nassau or Westchester (공항) 
  - 5: Negotiated fare
  - 6: Group ride
- Store and fwd flag
  - Y: store and forward trip
  - N: not a store and forward trip
- Dropoff longitude: 하차 경도
- Dropoff latitude: 하차 위도
- **Payment type**: 비용 지불 방식
  - 1: Credit card
  - 2: Cash
  - 3: No charge
  - 4: Dispute (분쟁)
  - 5: Unknown
  - 6: Voided trip
- **Fare amount**: 미터기로 계산된 시간 및 거리에 다른 요금
- Extra: 추가 요금 (교통 혼잡 시간대 또는 야간 시간대)
- MTA tax: MTA 세금
- Improvement surcharge: 개선 할증료? (교수님도 모르겟다하심)
- **Tip amount**: 팁 (신용카드로 결제했을 때의 팁)
- Tolls amount: 통행료 총액
- Total amount: 팁을 포함하지 않은 총 지불 금액

![image-20211128211839352](https://user-images.githubusercontent.com/70627979/146629522-4905ef59-42e4-4032-a2df-b9c33e3216a1.png)
