# Парсинг данных по показателю из базы Всемирного банка
# Парсинг данных с Портала открытых данных РФ

# Библиотеки
library('httr')
library('jsonlite')
library('XML')
library('RCurl')
library('WDI')
library('data.table')

# WDI
# Показатель: Денежные переводы, полученные (в долларах США)
# Ссылка: https://data.worldbank.org/indicator/BX.TRF.PWKR.CD.DT?view=chart

# Индикатор показателя
indicator.code <- 'BX.TRF.PWKR.CD.DT'

# Парсим данные с WDI
data <- data.table(WDI(indicator = indicator.code, start = 2019, end = 2019))

# Загружаем данные в csv файл
write.csv(data, file = './data/money_transfers_WDI.csv', row.names = F)


# Портал открытых данные РФ

# API ключ для работы с порталом
API.key <- '848a504c1b70c1da4a87ff2744ece95c'
# Ссылка для работы с API
URL.base <- 'http://data.gov.ru/api/'

# Функция для работы с API портала открытых данных РФ
getOpenDataRF <- function(api.params, url.base = URL.base, api.key = API.key){
  par <- paste0(api.params, collapse = '/')
  url <- paste0(url.base, par, '/?access_token=', api.key)
  message(paste0('Загружаем ', url, ' ...'))
  resp <- GET(url)
  fromJSON(content(resp, 'text'))
}

# Используем id из задания, чтобы получить нужные нам данные
dataset_id <- '3460012716-zhkhregistryoverhaul'

# Задаем параметры и получаем данные
params <- c('dataset', dataset_id)
dataset <- getOpenDataRF(params)

# Количество версий таблицы
params <- c(params, 'version')
versions <- getOpenDataRF(params)

nrow(versions)

# Загружаем последнюю версию в объект doc
mrv <- versions[nrow(versions), 1]
params <- c(params, mrv)
content <- c(params, 'content')
doc <- getOpenDataRF(content)

# Оставляем только те данные в которых присутствует поселок Пурпе
doc <- doc[grep('г. Жирновск', doc$Address), c('Year2', 'Wall', 'TotalArea1', 'Address')]

# Находим координаты с помощь имеющихся адрессов через API Yandex карт
# Ключ API для работы с Яндекс картами
API.key <- 'd75fbda2-a2ee-464f-be91-1bd0c2349bff'
URL.base <- 'https://geocode-maps.yandex.ru/1.x/'

# Функция для работы с API Yandex Карт
getYandexMaps <- function(api.params, url.base = URL.base, api.key = API.key){
  par <- paste0(api.params, collapse = '&')
  url <- paste0(url.base, '?format=xml&apikey=', api.key, par)
  message(paste0('Загружаем ', url, ' ...'))
  doc.ya <- content(GET(url), 'text', encoding = 'UTF-8')
  rootNode <- xmlRoot(xmlTreeParse(doc.ya, useInternalNodes = TRUE))
  coords <- xpathSApply(rootNode, "//*[name()='Envelope']/*", xmlValue)
  coords <- lapply(strsplit(coords, ' '), as.numeric)
  coords <- c((coords[[1]][1] + coords[[2]][1])/2, (coords[[1]][2] + coords[[2]][2])/2)
  names(coords) <-c('lat', 'long')
  coords
}

params <-paste0('&geocode=', gsub(pattern =' ', replacement ='+',
                                  curlEscape(doc$Address[1])))

# Парсим координаты
coords <- sapply(as.list(doc$Address), function(x){
  params <- paste0('&geocode=', gsub(curlEscape(x), pattern = ' ',
                                     replacement = '+'))
  getYandexMaps(params)
})

df.coords <- as.data.frame(t(coords))
colnames(df.coords) <- c('long', 'lat')

#Добавляем координаты в основной фрейм данных
doc <- cbind(doc, df.coords)
# Сохраняем данные в файл
write.csv2(doc, file = './data/Zhirnovsk_portal_RF.csv', row.names = F)
