library('shiny')
library('RCurl')

DF <- read.csv('./DATA_v2.csv', header = TRUE, sep = ',')

filter.1 <- as.character(unique(DF$Trade.Flow))
names(filter.1) <- filter.1
filter.1 <- as.list(filter.1)

shinyUI(
  pageWithSidebar(
    headerPanel("График разброса Netweight..kg. относительно Trade.Value..US.."),
    sidebarPanel(
      # Выбор кода продукции
      selectInput('sp.to.plot',
                  'Выберите код продукта',
                  list('Картофель' = '701', 'Помидоры' = '702', 'Лук, чеснок' = '703', 'Капуста' = '704', 'Салат и цикорий' = '705'),
                  selected = '701'),
      # Выбор экпорт/импорт
      selectInput('trade.to.plot',
                  'Выберите экспорт или импорт',
                  filter.1),
      # Период, по годам
      sliderInput('year.range', 'Года:',
                  min = 2010, max = 2020, value = c(2010, 2020),
                  width = '100%', sep = '')
    ),
    mainPanel(
      plotOutput('sp.ggplot')
    )
  )
)