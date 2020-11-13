rm(list = ls())
gc(reset = T)


#### packages ####
library(data.table)
library(stringr)
library(dplyr)


#### data ####
gathering_2018 = fread('C:/Users/SOYOUNG/Desktop/집회2018.csv', header = TRUE, encoding = 'UTF-8', stringsAsFactors = FALSE)
gathering_2018 = as.data.frame(gathering_2018)

location_2018 = gathering_2018[, 3] # 집회장소

loc_split = gsub('<|>|〈|〉', '', location_2018)
loc_split = strsplit(loc_split, '\n') # 집회장소와 동 자르기

split1 = lapply(loc_split, function(x) x[1]) %>% unlist # 집회장소
split2 = lapply(loc_split, function(x) x[2]) %>% unlist # 동
split1 = gsub('앞|인도|차도|안도', '', split1)
split2 = gsub('앞|인도', '', split2)
split1 = if (which(str_sub(split1, -1, -1) == '등')) split1 = gsub('등', '', split1)
split2 = if (which(str_sub(split2, -1, -1) == '등')) split2 = gsub('등', '', split2)

location = lapply(loc_split, function(x) ifelse(is.na(x[2]), paste0('', x[1]), paste(x[2], x[1]))) %>% unlist # 동 + 집회장소
# lapply(loc_split, function(x) if_else(length(x) == 1, paste0("", x[1]), paste(x[2], x[1]))) %>% unlist
# test1 = loc %>% mutate(location2 = ifelse(is.na(split2), paste0("", split1), paste(split2, split1)))
# test1$location2

loc = data.frame(location_2018, split1, split2, location)
which(loc$location_2018 == '주요집회 없습니다') # 1, 384, 388, 3172, 3173
loc = loc[loc$location_2018 != '주요집회 없습니다', ]

loc = loc %>% mutate(split1 = str_trim(split1), split2 = str_trim(split2))

## 전처리가 더 필요한 데이터
pre = loc[which(str_sub(loc$split2, -1, -1) != '동'), ] # 00동
pre = pre[which(str_sub(pre$split2, -1, -1) != '도'), ] # 여의도
pre = pre[which(str_sub(pre$split2, -1, -1) != '로'), ] # 00로
pre = pre[which(str_sub(pre$split2, -1, -1) != '가'), ] # 00가
pre = pre[which(str_sub(pre$split1, -1, -1) != '동'), ] # 
pre = pre[which(str_sub(pre$split1, -1, -1) != '도'), ] # 
pre = pre[which(str_sub(pre$split1, -1, -1) != '로'), ] # 
pre = pre[which(str_sub(pre$split1, -1, -1) != '가'), ] # 

## 데이터 자르기
idx_pre = as.numeric(rownames(pre))
loc_a = loc[- idx_pre, ]
str(loc_a)
loc_a = loc_a %>% mutate(location = as.character(location))

# loc_d = loc_d[order(as.numeric(rownames(loc_d))), ] # 행 인덱스 순서에 맞게 정렬, 숫자가 이어져있지는 않음


# ## 동 데이터 : 2904개
# d_a = loc[which(str_sub(loc$split2, -1, -1) == '동'), ]
# d_b = loc[which(str_sub(loc$split1, -1, -1) == '동'), ]
# d_b = d_b[, -4]
# d_b = d_b %>% mutate(location = ifelse(is.na(split2), paste0("", split1), paste(split2, split2)))
# 
# loc_d = rbind(d_a, d_b) # 행 인덱스 순서가 안맞음
# str(loc_d)
# loc_d = loc_d %>% mutate(location = as.character(location))


# loc$split1[str_detect(str_trim(loc$split1), '[[가-힣]*?동)]')]
# loc$split2[str_detect(str_trim(loc$split2), '[가-힣]*?동')]
# 
# a = loc$split2
# head(a, 100)
# aa = a[str_detect(a, '[가-힣]동')]
# head(a, 100)
# str_extract(a, '[가-힣]{1,}[0-9]{0,}동')
# a[is.na(str_extract(a, '[가-힣]{1,}[0-9]{0,}동'))]
# 
# 
# b = str_trim(loc$split1)
# head(b, 100)
# b[is.na(str_extract(b, '[가-힣]{1,}[0-9]{0,}동'))]



#### google api ####
# library(googleway)
library(ggmap)
library(googleway)

api_key = 'AIzaSyCjYNMP-5v-sTvX5OX6gUiN3Vv0cSqzWlg'
register_google(key = api_key)

# test_geocode = geocode(strsplit(loc_address[35, 2], ',')[[1]][1], source = 'google')
# test_address = google_places(search_string = strsplit(loc_address[35, 2], ',')[[1]][1], location = c(test_geocode$lat, test_geocode$lon),
#                              key = api_key, language = 'ko')
# 
# place_name(test_address)
# test_address$results$formatted_address[1]


place_a = loc_a[, 4] # 동 데이터 집회장소
place_a = gsub('앞|인도|\\(.*?\\)', '', place_a)
place_a = gsub('및', ', ', place_a)

first_a = c() # 동 데이터의 첫번째 집회장소 
place_split_a = strsplit(place_a, ',')

for (i in 1:length(place_a))
  first_a[i] = place_split_a[[i]][1]

which(is.na(first_a))
first_a[3943] = '안국동 우체국'
first_a[3953] = '고용노동청'
# coord_a = geocode(first_a) # 분할된 집회 장소 중 첫번째 들의 위경도

# which(is.na(coord_a$lon))
# sum(is.na(coord_a$lon)) # 위경도 안뽑힌 데이터 개

coord_a = as.data.frame(matrix(nrow = length(place_a), ncol = 2))
colnames(coord_a) = c('google_lon', 'google_lat')
address_a = character(length(place_a))

for (i in 3954:dim(coord_a)[1])
{
  a = geocode(first_a[i], source = 'google')
  coord_a[i, 1] = a$lon
  coord_a[i, 2] = a$lat
  b = google_places(search_string = first_a[i], location = c(a$lat, a$lon), key = api_key, language = 'ko')
  
  # google_places(location = c(a$lat, a$lon), key = api_key, language = 'ko', radius = 1e-5) ## 반경
  
  if (length(b$results$formatted_address[1]) == 0) next
  
  address_a[i] = b$results$formatted_address[1]
}

sum(is.na(coord_a$google_lat)) # 위경도 안뽑힌 데이터 292개
length(which(address_a == '')) # 주소 안뽑힌 데이터 737개
length(which(address_a == '' & is.na(coord_a[, 1]))) # 위경도, 주소 모두 안뽑힌 데이터 88개
data_a = data.frame(loc_a$location_2018, loc_a$split2, loc_a$location, coord_a, address_a)
colnames(data_a) = c('location2018', 'split2', 'location', 'google_lon', 'google_lat', 'google_address')

# test = geocode('효자동 BH사랑채 얖')
# idx = which(str_sub(data_a$location, 1, 9) == '효자동 BH사랑채')
# data_a$google_lon[idx] = test$lon
# data_a$google_lat[idx] = test$lat

## 좌표변환 ## -보류
# library(sp)
# library(rgdal)
# 
# coord_a_kakao = coord_a[is.na(coord_a$google_lon) == FALSE, ]
# long = coord_a[1, 1]; lat = coord_a[1, 2]
# 
# convertCoordSystem <- function(long, lat, from.crs, to.crs){
#   xy <- data.frame(long=long, lat=lat)
#   # if (is.na(xy$long) & is.na(xy$lat)) next
#   coordinates(xy) <- ~long+lat
#   
#   from.crs <- CRS(from.crs)
#   from.coordinates <- SpatialPoints(xy, proj4string=from.crs)
#   
#   to.crs <- CRS(to.crs)
#   changed <- as.data.frame(SpatialPoints(spTransform(from.coordinates, to.crs)))
#   names(changed) <- c("long", "lat")
#   
#   return(changed)
# }
# 
# head(coord_a)
# 
# from.crs = '+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs' # google
# 
# to.crs = '+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000 +ellps=GRS80 +units=m +no_defs' # kakao
# 
# coord_a_kakao <- cbind(coord_a_kakao, convertCoordSystem(coord_a_kakao$google_lon, coord_a_kakao$google_lat, from.crs, to.crs))




# geocode로 얻은 좌표를 카카오 좌표계에 맞게 변환한 후 행정동 주소로 바꾸기..?
#### 밑에는 좌표 변환 ####
library(httr)
# aa  = c(124.8506, 33.4781)
# 
kakao_key = "8b452a64e99ec67c5a3d30a8efcc89bd"
url_api = "https://dapi.kakao.com/v2/local/geo/coord2regioncode.json?"

for (i in 1: dim(data_a)[1])
{
  if (is.na(data_a$google_lat[i])) 
  {
    data_a$kakao_adress[i] = 'NA'
    data_a$kakao_dong[i] = 'NA'
  } else if (data_a$google_lon[i] < 0){
    data_a$kakao_adress[i] = 'NA'
    data_a$kakao_dong[i] = 'NA'
  } else{
  query1 = str_c("x=",data_a$google_lon[i],"&y=",data_a$google_lat[i],"&input_coord=WGS84")
  result = GET(str_c(url_api, query1), add_headers("Authorization" = str_c("KakaoAK ", kakao_key)))
  con2 = content(result)
  data_a$kakao_adress[i] = con2$documents[[1]]$address_name # 전체 주소
  data_a$kakao_dong[i] = con2$documents[[1]]$region_3depth_name # 동 이름
  }
}






