#Пакеты --------------------------------------------------------
{
  library(PortfolioAnalytics)
  library(PerformanceAnalytics)
  library(ROI)
  library(ROI.plugin.glpk)
  library(DEoptim)
  library(ROI.plugin.quadprog)
  library(quantmod)
  library(ggplot2)
  library(corrplot)
  library(pastecs)
  library(nortest)
  library(stats)
  library(car)
  library(lubridate)
  library(stargazer)
  library(dplyr)
  library(rugarch)
  library(fGarch)
  library(FactoMineR)
  library(factoextra)
  library(AER)
  library(gamlss)
  library(gamlss.dist)
  library(gamlss.add)
  library(tseries)
  library(psych)
  library(urca)
  library(forecast)
  library(lmtest)
  library(xlsx)
  library(VGAM)
  library(gridExtra)
  library(plm)
  library(texreg)
  library(glmnet)
  library(caret)
  library(panelvar)
  library(randomForest)
  library(tidyr)
  library(randomForestExplainer)
}
#робастные ошибки
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}
# Загрузка данных ----------------------------------------------------
setwd("~/Учеба/8 семестр/ДИПЛОМ/Даты")
data <- read.xlsx("data_первый эшелон — годовые.xlsx", sheetName = "Лист4")
summary(data)

#описательные статистики
data_describe <- data[, c("TRAIL_12M_NET_SALES", "EBITDA", "Number_Year", "CUR_MKT_CAP",
                          "TRAIL_12M_NET_INC", "TRAIL_12M_COM_DVD", "INV", "DEBT", 
                          "MB_ratio", "RISK", "Leverage", "Size", "CASH", "GROWTH", "TAX", 
                          "CR", "FIX", "Life.Cycle_Stage", "ROA", "Dividend_payout_ratio")]
str(data_describe)
describe <- describe(data_describe) 
View(describe)
write.xlsx(describe, file = "describe.xlsx")

names(data)
data1 <- data[, c("NAME", "TRAIL_12M_NET_SALES", "DVD", "EBITDA", "Number_Year", "CUR_MKT_CAP",
                  "TRAIL_12M_NET_INC")]%>% na.omit() %>% data.frame()
View(data1)
# Загружаем котировки ------------------------------------------------------
#Загрузим дату
AFKS <- getSymbols('AFKS.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
AFLT <- getSymbols('AFLT.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
ALRS <- getSymbols('ALRS.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
CBOM <- getSymbols('CBOM.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
CHMF <- getSymbols('CHMF.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
DSKY <- getSymbols('DSKY.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
FEES <- getSymbols('FEES.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
FIVE <- getSymbols('FIVE.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
FIXP <- getSymbols('FIXP.IL', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
GAZP <- getSymbols('GAZP.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
GMKN <- getSymbols('GMKN.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
HHRU <- getSymbols('HHR', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
HYDR <- getSymbols('HYDR.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
IRAO <- getSymbols('IRAO.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
LKOH <- getSymbols('LKOH.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
LSRG <- getSymbols('LSRG.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
MAGN <- getSymbols('MAGN.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
MGNT <- getSymbols('MGNT.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
MOEX <- getSymbols('MOEX.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
MTSS <- getSymbols('MTSS.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
NLMK <- getSymbols('NLMK.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
NVTK <- getSymbols('NVTK.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
OZON <- getSymbols('OZON', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
PHOR <- getSymbols('PHOR.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
PIKK <- getSymbols('PIKK.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
PLZL <- getSymbols('PLZL.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
POGR <- getSymbols('POGR.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
POLY <- getSymbols('POLY.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
ROSN <- getSymbols('ROSN.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
RTKM <- getSymbols('RTKM.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
RUAL <- getSymbols('RUAL.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
SBER <- getSymbols('SBER.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
SBERP <- getSymbols('SBERP.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
SNGS <- getSymbols('SNGS.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
SNGSP <- getSymbols('SNGSP.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
TATN <- getSymbols('TATN.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
TATNP <- getSymbols('TATNP.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
TCSG <- getSymbols('TCSG.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
TRNFP <- getSymbols('TRNFP.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
VKCO <- getSymbols('VKCO.IL', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
VTBR <- getSymbols('VTBR.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
YNDX <- getSymbols('YNDX.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)
IMOEX <- getSymbols('IMOEX.ME', from = '2010-01-01', to = '2021-12-31', auto.assign = FALSE)

AFKS.m <- to.monthly(AFKS)
AFLT.m <- to.monthly(AFLT)
ALRS.m <- to.monthly(ALRS)
CBOM.m <- to.monthly(CBOM)
CHMF.m <- to.monthly(CHMF)
FEES.m <- to.monthly(FEES)
GAZP.m <- to.monthly(GAZP)
GMKN.m <- to.monthly(GMKN)
HYDR.m <- to.monthly(HYDR)
IRAO.m <- to.monthly(IRAO)
LKOH.m <- to.monthly(LKOH)
LSRG.m <- to.monthly(LSRG)
MAGN.m <- to.monthly(MAGN)
MGNT.m <- to.monthly(MGNT)
MOEX.m <- to.monthly(MOEX)
MTSS.m <- to.monthly(MTSS)
NLMK.m <- to.monthly(NLMK)
NVTK.m <- to.monthly(NVTK)
PHOR.m <- to.monthly(PHOR)
PIKK.m <- to.monthly(PIKK)
PLZL.m <- to.monthly(PLZL)
POLY.m <- to.monthly(POLY)
ROSN.m <- to.monthly(ROSN)
RTKM.m <- to.monthly(RTKM)
RUAL.m <- to.monthly(RUAL)
SBER.m <- to.monthly(SBER)
SBERP.m <- to.monthly(SBERP)
SNGS.m <- to.monthly(SNGS)
SNGSP.m <- to.monthly(SNGSP)
TATN.m <- to.monthly(TATN)
TATNP.m <- to.monthly(TATNP)
TRNFP.m <- to.monthly(TRNFP)
VKCO.m <- to.monthly(VKCO)
VTBR.m <- to.monthly(VTBR)
YNDX.m <- to.monthly(YNDX)
IMOEX.m <- to.monthly(IMOEX)

prices <- merge(AFKS.m$AFKS.Adjusted,	AFLT.m$AFLT.Adjusted,	ALRS.m$ALRS.Adjusted,	CBOM.m$CBOM.Adjusted,	
                CHMF.m$CHMF.Adjusted,	FEES.m$FEES.Adjusted,	GAZP.m$GAZP.Adjusted,	GMKN.m$GMKN.Adjusted,
                HYDR.m$HYDR.Adjusted,	IRAO.m$IRAO.Adjusted,	LKOH.m$LKOH.Adjusted,	
                LSRG.m$LSRG.Adjusted,	MAGN.m$MAGN.Adjusted,	MGNT.m$MGNT.Adjusted,	MOEX.m$MOEX.Adjusted,	
                MTSS.m$MTSS.Adjusted,	NLMK.m$NLMK.Adjusted,	NVTK.m$NVTK.Adjusted,	
                PHOR.m$PHOR.Adjusted,	PIKK.m$PIKK.Adjusted,	PLZL.m$PLZL.Adjusted,	
                POLY.m$POLY.Adjusted,	ROSN.m$ROSN.Adjusted,	RTKM.m$RTKM.Adjusted,	RUAL.m$RUAL.Adjusted,	
                SBER.m$SBER.Adjusted,	SBERP.m$SBERP.Adjusted,	SNGS.m$SNGS.Adjusted,	SNGSP.m$SNGSP.Adjusted,	
                TATN.m$TATN.Adjusted,	TATNP.m$TATNP.Adjusted,	TRNFP.m$TRNFP.Adjusted,	
                VKCO.m$VKCO.Adjusted,	VTBR.m$VTBR.Adjusted,	YNDX.m$YNDX.Adjusted)
prices_IMOEX <- IMOEX.m$IMOEX.Adjusted

return <- Return.calculate(prices, method = "log") %>% na.omit()

market_return <- Return.calculate(prices_IMOEX, method = "log") %>% na.omit()
market_return <- market_return[c(29:105),] 
rf <- rep(0.0763/12, 77)
colnames(market_return) <- 'market_return'
colnames(return) <- c('AFKS',	'AFLT',	'ALRS',	'CBOM',	'CHMF',	'FEES',
                      'GAZP',	'GMKN', 'HYDR',	'IRAO',	'LKOH',	'LSRG',	'MAGN',
                      'MGNT',	'MOEX',	'MTSS',	'NLMK',	'NVTK',	'PHOR',	'PIKK',	'PLZL',
                      'POLY',	'ROSN',	'RTKM',	'RUAL',	'SBER',	'SBERP',	'SNGS',	'SNGSP',	'TATN',	
                      'TATNP',	'TRNFP',	'VKCO',	'VTBR',	'YNDX')
View(return)

# Доходности портфелей FamaFrench и риск -------------------------------------------------------
prices1 <- merge(AFKS.m$AFKS.Adjusted,	AFLT.m$AFLT.Adjusted,	ALRS.m$ALRS.Adjusted,	CBOM.m$CBOM.Adjusted,	
                 CHMF.m$CHMF.Adjusted, DSKY.m$DSKY.Adjusted,	FEES.m$FEES.Adjusted,
                 FIVE.m$FIVE.Adjusted, FIXP.m$FIXP.Adjusted,	GAZP.m$GAZP.Adjusted,	GMKN.m$GMKN.Adjusted, HHRU.m$HHRU.Adjusted,
                 HYDR.m$HYDR.Adjusted,	IRAO.m$IRAO.Adjusted,	LKOH.m$LKOH.Adjusted,	
                 LSRG.m$LSRG.Adjusted,	MAGN.m$MAGN.Adjusted,	MGNT.m$MGNT.Adjusted,	MOEX.m$MOEX.Adjusted,	
                 MTSS.m$MTSS.Adjusted,	NLMK.m$NLMK.Adjusted,	NVTK.m$NVTK.Adjusted, OZON.m$OZON.Adjusted,	
                 PHOR.m$PHOR.Adjusted,	PIKK.m$PIKK.Adjusted,	PLZL.m$PLZL.Adjusted,	
                 POLY.m$POLY.Adjusted,	ROSN.m$ROSN.Adjusted,	RTKM.m$RTKM.Adjusted,	RUAL.m$RUAL.Adjusted,	
                 SBER.m$SBER.Adjusted,	SBERP.m$SBERP.Adjusted,	SNGS.m$SNGS.Adjusted,	SNGSP.m$SNGSP.Adjusted,	
                 TATN.m$TATN.Adjusted,	TATNP.m$TATNP.Adjusted, TCSG.m$TCSG.Adjusted,	TRNFP.m$TRNFP.Adjusted,	
                 VKCO.m$VKCO.Adjusted,	VTBR.m$VTBR.Adjusted,	YNDX.m$YNDX.Adjusted)

return1 <- Return.calculate(prices1, method = "log")
#годовые стандартные отклонения
sd_monthly <- apply.yearly(x = return, FUN =  StdDev)
View(sd_monthly)
colnames(sd_monthly) <- c('AFKS',	'AFLT',	'ALRS',	'CBOM',	'CHMF', 'DSKY',	'FEES', "FIVE", "FIXP",
                          'GAZP',	'GMKN', 'HHRU', 'HYDR',	'IRAO',	'LKOH',	'LSRG',	'MAGN',
                          'MGNT',	'MOEX',	'MTSS',	'NLMK',	'NVTK', 'OZON',	'PHOR',	'PIKK',	'PLZL',
                          'POLY',	'ROSN',	'RTKM',	'RUAL',	'SBER',	'SBERP',	'SNGS',	'SNGSP',	'TATN',	
                          'TATNP', 'TCSG',	'TRNFP',	'VKCO',	'VTBR',	'YNDX')
write.xlsx(sd_monthly, "sd_monthly.xlsx")

# Анализ гос участия в компаниях ---------------------------------------------
#сколько компаний имеют гос участие
aggregate(GOV_PART ~ Number_Year, data = data, FUN = function(x) sum(x == 1))

# Анализ выплат компаний --------------------------------------------------
#Cколько компаний платит дивидендов из года в год
data5 <- data[, c("NAME", "DVD", "Number_Year", "TRAIL_12M_COM_DVD")] %>% na.omit() %>% data.frame()

a <- aggregate(DVD ~ Number_Year, data = data5, FUN = function(x) sum(x == 1))
b <- table(data5$Number_Year) %>% t()
c <- a[2]/b[1:12]
c <- cbind(a[1], c)
c

ggplot(data5, aes(x = Number_Year, fill = factor(DVD))) + 
  geom_bar(position = "fill") + scale_fill_manual(values = c("#38A8A7", "#000000"), 
                                                  labels = c("Неплательшики", "Плательщики")) + 
  labs(title = "", x = "", y = "Соотношение плательщиков и неплательщиков дивидендов", 
       fill = "Тип отношения к дивидендам") +
  theme(plot.title = element_text(size = 32, hjust = 0.5)) + theme_bw(base_size = 20)


# Анализ финансовых показателей плательщиков и неплательщиков -------------
Table1 <- data1 %>% group_by(Number_Year, DVD) %>% summarise(Sales = median(TRAIL_12M_NET_SALES),
                                                    EBITDA = median(EBITDA), Net_Income = median(TRAIL_12M_NET_INC),
                                                    Capitalization = median(CUR_MKT_CAP)) %>% ungroup

Table1$status <- ifelse(Table1$DVD == "0", "NonPayers", "Payers")
Chart1 <- ggplot(Table1, aes(fill=status, y=Sales, x=Number_Year)) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() +
  scale_fill_manual(labels = c("Неплательщики", "Плательщики"),
  values=c("#38A8A7", "#000000")) + theme(legend.position = "right") + ylab("Выручка") + 
  xlab("Год") + labs(fill = "Тип отношения к дивидендам")
Chart2 <- ggplot(Table1, aes(fill=status, y=EBITDA, x=Number_Year)) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() + 
  scale_fill_manual(labels = c("Неплательщики", "Плательщики"), 
                    values=c("#38A8A7", "#000000")) + theme(legend.position = "right") + 
  ylab("EBITDA") + xlab("Год") + labs(fill = "Тип отношения к дивидендам")

Chart3 <- ggplot(Table1, aes(fill=status, y=Net_Income, x=Number_Year)) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() + 
  scale_fill_manual(labels = c("Неплательщики", "Плательщики"), 
  values=c("#38A8A7", "#000000")) + theme(legend.position = "right") + 
  ylab("Чистая прибыль") + xlab("Год") + labs(fill = "Тип отношения к дивидендам")

grid.arrange(Chart1, Chart2, Chart3, nrow = 3)

Chart4 <- ggplot(Table1, aes(fill=status, y=Capitalization, x=Number_Year)) + 
  geom_bar(position="dodge", stat="identity", color = "black") + theme_bw() + 
  scale_fill_manual(labels = c("Неплательщики", "Плательщики"), 
                    values=c("#38A8A7", "#000000")) + theme(legend.position = "bottom") +
  ylab("Капитализация") + xlab("Год") + labs(fill = "Тип отношения к дивидендам")
Chart4
#медианная структура баланса BS_DISCLOSED_INTANGIBLES, TANGIBLE_ASSETS, BS_TOT_ASSET
names(data)
data2 <- data[, c("DVD", "BS_TOT_ASSET", "Number_Year", "BS_DISCLOSED_INTANGIBLES", 
                  "TANGIBLE_ASSETS")]%>% na.omit() %>% data.frame()

#соотношение Intangibles и Tangibles
Table2 <- data2 %>% group_by(Number_Year, DVD) %>% summarise(Intangible_Assets = median(BS_DISCLOSED_INTANGIBLES), 
                                                             Tangible_Assets = median(TANGIBLE_ASSETS)) %>% ungroup
Table2$status <- ifelse(Table2$DVD == "0", "Неплательщики", "Плательщики")
Chart21 <- ggplot(Table2, aes(fill=status, y=Tangible_Assets, x=Number_Year)) + 
  geom_bar(position="dodge", stat="identity") + theme_bw() + 
  scale_fill_manual(labels = c("Неплательщики", "Плательщики"), 
                    values=c("#38A8A7", "#000000")) + theme(legend.position = "right") + 
  ylab("Материальные активы") + xlab("Год") + labs(fill = "Тип отношения к дивидендам")
Chart21

Table3 <- Table2 %>% pivot_longer(Intangible_Assets:Tangible_Assets, 
                                  names_to = "type_of_assets", values_to = "valuation")
Chart31 <- ggplot(Table3,aes(x=status,y=valuation,fill=type_of_assets)) + theme_bw() + 
  geom_bar(stat="identity",position="stack") + facet_grid(~Number_Year) + 
  theme(legend.position = "right", axis.text.x = element_text(angle=90, hjust=1), 
  strip.background =element_rect(fill="#182932"), strip.text = element_text(colour = 'white')) + 
  scale_fill_manual(labels = c("Нематериальные активы", "Материальные активы"), 
        values=c("#38A8A7", "#000000")) + xlab("") + ylab("Общая величина материальных и 
  нематериальных активов") + labs(fill = "Тип активов") + 
  scale_y_continuous(labels = scales::comma) 

Chart32 <- ggplot(Table3,aes(x=status,y=valuation,fill=type_of_assets)) + theme_bw() + 
  geom_bar(stat="identity",position="fill") +
  facet_grid(~Number_Year) + theme(legend.position = "right", 
                                   axis.text.x = element_text(angle=90, hjust=1), 
                                   strip.background =element_rect(fill="#182932"),
                                   strip.text = element_text(colour = 'white')) + 
  scale_fill_manual(labels = c("Нематериальные активы", "Материальные активы"), 
  values=c("#38A8A7", "#000000")) + xlab("") + 
  ylab("Процентное соотношение материальных и нематериальных активов") + 
  labs(fill = "Тип активов") 

grid.arrange(Chart31, Chart32)

#соотношение краткосрочных и долгосрочных заимствований
data3 <- data[, c("DVD", "BS_TOT_ASSET", "Number_Year", "BS_ST_BORROW", 
                  "BS_LT_BORROW")] %>% na.omit() %>% data.frame()
Table4 <- data3 %>% group_by(Number_Year, DVD) %>% summarise(BS_ST_Borrow = median(BS_ST_BORROW), BS_LT_Borrow = median(BS_LT_BORROW)) %>% ungroup

Table4$status <- ifelse(Table4$DVD == "0", "Неплательщики", "Плательщики")
Table5 <- Table4 %>% pivot_longer(BS_ST_Borrow:BS_LT_Borrow, names_to = "type_of_borrowings", 
                                  values_to = "valuation")
Chart41 <- ggplot(Table5,aes(x=status,y=valuation,fill=type_of_borrowings)) + theme_bw() + 
  geom_bar(stat="identity",position="stack") +
  facet_grid(~Number_Year) + theme(legend.position = "right", 
                                   axis.text.x = element_text(angle=90, hjust=1), 
                                   strip.background =element_rect(fill="#182932"), 
                                   strip.text = element_text(colour = 'white')) + 
  scale_fill_manual(labels = c("LR заимствования", "SR заимствования"), 
  values=c("#38A8A7", "#000000")) +xlab("")+ylab("Абсолютное соотношение 
  SR и LR заимствований") + 
  labs(fill = "Тип заимствований")

Chart42 <- ggplot(Table5,aes(x=status,y=valuation,fill=type_of_borrowings)) + theme_bw() + 
  geom_bar(stat="identity",position="fill") +
  facet_grid(~Number_Year) + theme(legend.position = "right", axis.text.x = element_text(angle=90, hjust=1), 
                                   strip.background =element_rect(fill="#182932"), 
                                   strip.text = element_text(colour = 'white')) + 
  scale_fill_manual(labels = c("LR заимствования", "SR заимствования"), 
  values=c("#38A8A7", "#182932")) +xlab("")+ylab("% cоотношение 
  SR и LR заимствований") + 
  labs(fill = "Тип заимствований")

grid.arrange(Chart41,Chart42)

#сравним CASH_RATIO, QUICK_RATIO и BS_CASH_NEAR_CASH_ITEM + WORKING_CAPITAL
data4 <- data[, c("DVD", "Number_Year", "QUICK_RATIO", "CASH_RATIO", "WORKING_CAPITAL", 
                  "BS_CASH_NEAR_CASH_ITEM")] %>% na.omit() %>% data.frame()

Table6 <- data4 %>% group_by(Number_Year, DVD) %>% transmute(Cash_Ratio = median(CASH_RATIO), 
                    Quick_Ratio = median(QUICK_RATIO), Cash = median(BS_CASH_NEAR_CASH_ITEM), 
                    Working_Capital = median(WORKING_CAPITAL))
Table6$status <- ifelse(Table6$DVD == "0", "Неплательщики", "Плательщики")
Chart51 <- ggplot(data = Table6, aes(x = Number_Year, y = Cash_Ratio, group = status, 
  color = status)) + geom_line() + geom_point() + theme_bw() + 
  scale_color_manual(labels = c("Неплательщики", "Плательщики"), 
  values=c("#38A8A7", "#000000")) + theme(legend.position="bottom") + 
  xlab("Год") +ylab("Коэффициент абсолютной ликвидности") + 
  labs(fill = "Тип отношения к дивидендам")
Chart52 <- ggplot(data = Table6, aes(x = Number_Year, y = Quick_Ratio, group = status, 
  color = status)) + geom_line() + geom_point() + theme_bw() + 
  scale_color_manual(labels = c("Неплательщики", "Плательщики"), 
  values=c("#38A8A7", "#000000")) + theme(legend.position="bottom") + 
  xlab("Год") + ylab("Коэффициент срочной ликвидности")+ 
  labs(fill = "Тип отношения к дивидендам")
Chart53 <- ggplot(data = Table6, aes(x = Number_Year, y = Cash, group = status,
                                     color = status)) + geom_line() + geom_point() + 
  theme_bw() + scale_color_manual(labels = c("Неплательщики", "Плательщики"), 
                                  values=c("#38A8A7", "#000000")) + 
  theme(legend.position="bottom") + xlab("Год") + ylab("Денежные средства") + 
  labs(fill = "Тип отношения к дивидендам")
Chart54 <- ggplot(data = Table6, aes(x = Number_Year, y = Working_Capital, group = status, 
                                     color = status)) + geom_line() + geom_point() + 
  theme_bw() + scale_color_manual(labels = c("Неплательщики", "Плательщики"), 
                                  values=c("#38A8A7", "#000000")) + 
  theme(legend.position="bottom") + xlab("Год") + ylab("Оборотный капитал") + 
  labs(fill = "Тип отношения к дивидендам")
grid.arrange(Chart51, Chart52, Chart53, Chart54, ncol = 2)

# FamaFrench --------------------------------------------------------------
FamaFrench <- data[, c("NAME", "DATE", "CUR_MKT_CAP", "MB_ratio", "TRAIL_12M_COM_DVD", "D_increase", "D_decrease")] %>% na.omit()


#разбиваем капитализацию по медиане
FamaFrench1 <- subset(FamaFrench, DATE >= as.Date('2021-12-31'))
FamaFrench1$crit_cap <- ifelse(FamaFrench1$CUR_MKT_CAP>median(FamaFrench1$CUR_MKT_CAP), "Big", "Small")
table(FamaFrench1$crit_cap) #16 больших; 16 маленьких
quantile(FamaFrench1$MB_ratio, seq(0,1, by = 0.1))
quantile(FamaFrench1$MB_ratio, 0.3)
quantile(FamaFrench1$MB_ratio, 0.7)
quantile(FamaFrench1$MB_ratio, 1)

FamaFrench1$criteria_MBratio <- ifelse(FamaFrench1$MB_ratio>quantile(FamaFrench1$MB_ratio, 0.7), "High", 
                                       ifelse(FamaFrench1$MB_ratio>quantile(FamaFrench1$MB_ratio, 0.3), "Neutral", "Low"))

FamaFrench1$group <- paste0(FamaFrench1$crit_cap, " " , FamaFrench1$criteria_MBratio) 
table(FamaFrench1$group)
data_SmallLow <- subset(FamaFrench1, FamaFrench1$group == "Small Low")
data_SmallNeutral <- subset(FamaFrench1, FamaFrench1$group == "Small Neutral")
data_SmallHigh <- subset(FamaFrench1, FamaFrench1$group == "Small High")
data_BigLow <- subset(FamaFrench1, FamaFrench1$group == "Big Low")
data_BigNeutral <- subset(FamaFrench1, FamaFrench1$group == "Big Neutral")
data_BigHigh <- subset(FamaFrench1, FamaFrench1$group == "Big High")

#построение портфелей по ФФ

#1- SmallLow

return_assets_SmallLow <- return[, colnames(return) %in% data_SmallLow$NAME]
returns_port4 <- Return.portfolio(R = return_assets_SmallLow, weights = c(rep(1/dim(return_assets_SmallLow)[2],dim(return_assets_SmallLow)[2])))
round(Return.annualized(returns_port4),3)
round(sd.annualized(returns_port4),3)

#2 - SmallNeutral
return_assets_SmallNeutral <- return[, colnames(return) %in% data_SmallNeutral$NAME]
returns_port5 <- Return.portfolio(R = return_assets_SmallNeutral, weights = c(rep(1/dim(return_assets_SmallNeutral)[2],dim(return_assets_SmallNeutral)[2])))
round(Return.annualized(returns_port5),3)
round(sd.annualized(returns_port5),3)

#3 - SmallHigh
return_assets_SmallHigh <- return[, colnames(return) %in% data_SmallHigh$NAME]
returns_port6 <- Return.portfolio(R = return_assets_SmallHigh, weights = c(rep(1/dim(return_assets_SmallHigh)[2],dim(return_assets_SmallHigh)[2])))
round(Return.annualized(returns_port6),3)
round(sd.annualized(returns_port6),3)

#4 - BigLow
return_assets_BigLow <- return[, colnames(return) %in% data_BigLow$NAME]
returns_port7 <- Return.portfolio(R = return_assets_BigLow, weights = c(rep(1/dim(return_assets_BigLow)[2],dim(return_assets_BigLow)[2])))
round(Return.annualized(returns_port7),3)
round(sd.annualized(returns_port7),3)

#5 - BigNeutral
return_assets_BigNeutral <- return[, colnames(return) %in% data_BigNeutral$NAME]
returns_port8 <- Return.portfolio(R = return_assets_BigNeutral, weights = c(rep(1/dim(return_assets_BigNeutral)[2],dim(return_assets_BigNeutral)[2])))
round(Return.annualized(returns_port8),3)
round(sd.annualized(returns_port8),3)

#6 - BigHigh
return_assets_BigHigh <- return[, colnames(return) %in% data_BigHigh$NAME]
returns_port9 <- Return.portfolio(R = return_assets_BigHigh, weights = c(rep(1/dim(return_assets_BigHigh)[2],dim(return_assets_BigHigh)[2])))
round(Return.annualized(returns_port9),3)
round(sd.annualized(returns_port9),3)

FF_return_portfolio <- data.frame(returns_port4, returns_port5, returns_port6, returns_port7, returns_port8, returns_port9)
colnames(FF_return_portfolio) <- c("SmallLow", "SmallNeutral", "SmallHigh", "BigLow", "BigNeutral", "BigHigh")

#строим регрессию с SMB и HML
data_SMB_Big <- subset(FamaFrench1, FamaFrench1$crit_cap == "Big")
data_SMB_Small <- subset(FamaFrench1, FamaFrench1$crit_cap == "Small")

data_HML_High <- subset(FamaFrench1, FamaFrench1$criteria_MBratio == "High")
data_HML_Low <- subset(FamaFrench1, FamaFrench1$criteria_MBratio == "Low")

#7 - Big
return_assets_Big <- return[, colnames(return) %in% data_SMB_Big$NAME]
returns_port10 <- Return.portfolio(R = return_assets_Big, weights = c(rep(1/dim(return_assets_Big)[2],dim(return_assets_Big)[2])))

#8 - Small
return_assets_Small <- return[, colnames(return) %in% data_SMB_Small$NAME]
returns_port11 <- Return.portfolio(R = return_assets_Small, weights = c(rep(1/dim(return_assets_Small)[2],dim(return_assets_Small)[2])))

#9 - High
return_assets_High <- return[, colnames(return) %in% data_HML_High$NAME]
returns_port12 <- Return.portfolio(R = return_assets_High, weights = c(rep(1/dim(return_assets_High)[2],dim(return_assets_High)[2])))

#10 - Low
return_assets_Low <- return[, colnames(return) %in% data_HML_Low$NAME]
returns_port13 <- Return.portfolio(R = return_assets_Low, weights = c(rep(1/dim(return_assets_Low)[2],dim(return_assets_Low)[2])))


SMB <- as.data.frame(returns_port11 - returns_port10)
HML <- as.data.frame(returns_port12 - returns_port13)
ERP <- as.data.frame(market_return-rf)
data2 <- cbind(SMB, HML, ERP, return)
colnames(data2) <- c("SMB", "HML", "ERP", 'AFKS',	'AFLT',	'ALRS',	'CBOM',	'CHMF',	'FEES',
                     'GAZP',	'GMKN', 'HYDR',	'IRAO',	'LKOH',	'LSRG',	'MAGN',
                     'MGNT',	'MOEX',	'MTSS',	'NLMK',	'NVTK',	'PHOR',	'PIKK',	'PLZL',
                     'POLY',	'ROSN',	'RTKM',	'RUAL',	'SBER',	'SBERP',	'SNGS',	'SNGSP',	'TATN',	
                     'TATNP',	'TRNFP',	'VKCO',	'VTBR',	'YNDX')
data2$date <- row.names(data2)
data2$date <- as.Date(paste0("01", data2$date), format = "%d %b %Y")
data1 <- read.xlsx("data_первый эшелон — famafrench.xlsx", sheetName = "Лист3")
str(data2)
merged_data <- merge(data2, data1, by = "date")

#модели "AFLT_dec", "MOEX_inc", "MTSS_dec", "PHOR_dec", "PLZL_dec"
model1 <- lm(data = merged_data, AFKS - rf  ~ ERP + SMB + HML + AFKS_D_increase)
summary(model1)
mod1 <- lm(data = merged_data, AFKS - rf  ~ ERP + SMB + HML + AFKS_D_decrease)
summary(mod1)
model2 <- lm(data = merged_data, AFLT - rf  ~ ERP + SMB + HML + AFLT_D_increase)
summary(model2)
mod2 <- lm(data = merged_data, AFLT - rf  ~ ERP + SMB + HML + AFLT_D_decrease)
summary(mod2) #-
model3 <- lm(data = merged_data, ALRS - rf  ~ ERP + SMB + HML + ALRS_D_increase)
summary(model3)
mod3 <- lm(data = merged_data, ALRS - rf  ~ ERP + SMB + HML + ALRS_D_decrease)
summary(mod3)
model4 <- lm(data = merged_data, CBOM - rf  ~ ERP + SMB + HML + CBOM_D_increase)
summary(model4)
mod4 <- lm(data = merged_data, CBOM - rf  ~ ERP + SMB + HML + CBOM_D_decrease)
summary(mod4)
model5 <- lm(data = merged_data, CHMF - rf  ~ ERP + SMB + HML + CHMF_D_increase)
summary(model5)
mod5 <- lm(data = merged_data, CHMF - rf  ~ ERP + SMB + HML + CHMF_D_decrease)
summary(mod5)
model6 <- lm(data = merged_data, FEES - rf  ~ ERP + SMB + HML + FEES_D_increase)
summary(model6)
mod6 <- lm(data = merged_data, FEES - rf  ~ ERP + SMB + HML + FEES_D_decrease)
summary(mod6)
model7 <- lm(data = merged_data, GAZP - rf  ~ ERP + SMB + HML + GAZP_D_increase)
summary(model7)
mod7 <- lm(data = merged_data, GAZP - rf  ~ ERP + SMB + HML + GAZP_D_decrease)
summary(mod7)
model13 <- lm(data = merged_data, GMKN - rf  ~ ERP + SMB + HML + GMKN_D_increase)
summary(model13)
mod13 <- lm(data = merged_data, GMKN - rf  ~ ERP + SMB + HML + GMKN_D_decrease)
summary(mod13)
model14 <- lm(data = merged_data, HYDR - rf  ~ ERP + SMB + HML + HYDR_D_increase)
summary(model14)
mod14 <- lm(data = merged_data, HYDR - rf  ~ ERP + SMB + HML + HYDR_D_decrease)
summary(mod14)
model15 <- lm(data = merged_data, IRAO - rf  ~ ERP + SMB + HML + IRAO_D_increase)
summary(model15)
mod15 <- lm(data = merged_data, IRAO - rf  ~ ERP + SMB + HML + IRAO_D_decrease)
summary(mod15)
model16 <- lm(data = merged_data, LKOH - rf  ~ ERP + SMB + HML + LKOH_D_increase)
summary(model16)
mod16 <- lm(data = merged_data, LKOH - rf  ~ ERP + SMB + HML + LKOH_D_decrease)
summary(mod16)
model17 <- lm(data = merged_data, LSRG - rf  ~ ERP + SMB + HML + LSRG_D_increase)
summary(model17)
mod17 <- lm(data = merged_data, LSRG - rf  ~ ERP + SMB + HML + LSRG_D_decrease)
summary(mod17)
model18 <- lm(data = merged_data, MAGN - rf  ~ ERP + SMB + HML + MAGN_D_increase)
summary(model18)
mod18 <- lm(data = merged_data, MAGN - rf  ~ ERP + SMB + HML + MAGN_D_decrease)
summary(mod18)
model19 <- lm(data = merged_data, MGNT - rf  ~ ERP + SMB + HML + MGNT_D_increase)
summary(model19)
mod19 <- lm(data = merged_data, MGNT - rf  ~ ERP + SMB + HML + MGNT_D_decrease)
summary(mod19)
model25 <- lm(data = merged_data, MOEX - rf  ~ ERP + SMB + HML + MOEX_D_increase)
summary(model25) #+
mod25 <- lm(data = merged_data, MOEX - rf  ~ ERP + SMB + HML + MOEX_D_decrease)
summary(mod25) #-
model26 <- lm(data = merged_data, MTSS - rf  ~ ERP + SMB + HML + MTSS_D_increase)
summary(model26) #+
mod26 <- lm(data = merged_data, MTSS - rf  ~ ERP + SMB + HML + MTSS_D_decrease)
summary(mod26) #-
model27 <- lm(data = merged_data, NLMK - rf  ~ ERP + SMB + HML + NLMK_D_increase)
summary(model27)
mod27 <- lm(data = merged_data, NLMK - rf  ~ ERP + SMB + HML + NLMK_D_decrease)
summary(mod27)
model28 <- lm(data = merged_data, NVTK - rf  ~ ERP + SMB + HML + NVTK_D_increase)
summary(model28)
mod28 <- lm(data = merged_data, NVTK - rf  ~ ERP + SMB + HML + NVTK_D_decrease)
summary(mod28)
model29 <- lm(data = merged_data, PHOR - rf  ~ ERP + SMB + HML + PHOR_D_increase)
summary(model29) #+
mod29 <- lm(data = merged_data, PHOR - rf  ~ ERP + SMB + HML + PHOR_D_decrease)
summary(mod29) #-
model30 <- lm(data = merged_data, PIKK - rf  ~ ERP + SMB + HML + PIKK_D_increase)
summary(model30)
mod30 <- lm(data = merged_data, PIKK - rf  ~ ERP + SMB + HML + PIKK_D_decrease)
summary(mod30)
model31 <- lm(data = merged_data, PLZL - rf  ~ ERP + SMB + HML + PLZL_D_increase)
summary(model31)
mod31 <- lm(data = merged_data, PLZL - rf  ~ ERP + SMB + HML + PLZL_D_decrease)
summary(mod31) #-
model36 <- lm(data = merged_data, POLY - rf  ~ ERP + SMB + HML + POLY_D_increase)
summary(model36)
mod36 <- lm(data = merged_data, POLY - rf  ~ ERP + SMB + HML + POLY_D_decrease)
summary(mod36)
model37 <- lm(data = merged_data, ROSN - rf  ~ ERP + SMB + HML + ROSN_D_increase)
summary(model37)
mod37 <- lm(data = merged_data, ROSN - rf  ~ ERP + SMB + HML + ROSN_D_decrease) 
summary(mod37)
model38 <- lm(data = merged_data, RTKM - rf  ~ ERP + SMB + HML + RTKM_D_increase)
summary(model38)
mod38 <- lm(data = merged_data, RTKM - rf  ~ ERP + SMB + HML + RTKM_D_decrease)
summary(mod38)
model39 <- lm(data = merged_data, RUAL - rf  ~ ERP + SMB + HML + RUAL_D_increase)
summary(model39)
mod39 <- lm(data = merged_data, RUAL - rf  ~ ERP + SMB + HML + RUAL_D_decrease)
summary(mod39)
model40 <- lm(data = merged_data, SBER - rf  ~ ERP + SMB + HML + SBER_D_increase)
summary(model40)
mod40 <- lm(data = merged_data, SBER - rf  ~ ERP + SMB + HML + SBER_D_decrease)
summary(mod40)
model41 <- lm(data = merged_data, SBERP - rf  ~ ERP + SMB + HML + SBERP_D_increase)
summary(model41)
mod41 <- lm(data = merged_data, SBERP - rf  ~ ERP + SMB + HML + SBERP_D_decrease)
summary(mod41)
model42 <- lm(data = merged_data, SNGS - rf  ~ ERP + SMB + HML + SNGS_D_increase)
summary(model42)
mod42 <- lm(data = merged_data, SNGS - rf  ~ ERP + SMB + HML + SNGS_D_decrease)
summary(mod42)
model47 <- lm(data = merged_data, SNGSP - rf  ~ ERP + SMB + HML + SNGSP_D_increase)
summary(model47)
mod47 <- lm(data = merged_data, SNGSP - rf  ~ ERP + SMB + HML + SNGSP_D_decrease)
summary(mod47)
model48 <- lm(data = merged_data, TATN - rf  ~ ERP + SMB + HML + TATN_D_increase)
summary(model48)
mod48 <- lm(data = merged_data, TATN - rf  ~ ERP + SMB + HML + TATN_D_decrease) #"TATN_dec"
summary(mod48)
model49 <- lm(data = merged_data, TATNP - rf  ~ ERP + SMB + HML + TATNP_D_increase)
summary(model49)
mod49 <- lm(data = merged_data, TATNP - rf  ~ ERP + SMB + HML + TATNP_D_decrease)
summary(mod49)
model50 <- lm(data = merged_data, TRNFP - rf  ~ ERP + SMB + HML + TRNFP_D_increase)
summary(model50)
mod50 <- lm(data = merged_data, TRNFP - rf  ~ ERP + SMB + HML + TRNFP_D_decrease)
summary(mod50)
model51 <- lm(data = merged_data, VKCO - rf  ~ ERP + SMB + HML + VKCO_D_increase)
summary(model51)
mod51 <- lm(data = merged_data, VKCO - rf  ~ ERP + SMB + HML + VKCO_D_decrease)
summary(mod51)
model52 <- lm(data = merged_data, VTBR - rf  ~ ERP + SMB + HML + VTBR_D_increase)
summary(model52)
mod52 <- lm(data = merged_data, VTBR - rf  ~ ERP + SMB + HML + VTBR_D_decrease)
summary(mod52) #-
model53 <- lm(data = merged_data, YNDX - rf  ~ ERP + SMB + HML + YNDX_D_increase)
summary(model53)
mod53 <- lm(data = merged_data, YNDX - rf  ~ ERP + SMB + HML + YNDX_D_decrease)
summary(mod53)

stargazer(mod2, mod25, mod26, mod29, mod31, mod52,  
          title="Fama French", type="html", 
          column.labels=c("AFLT", "MOEX", "MTSS", "PHOR", "PLZL", "VTBR"),
          out = "fama-french.html")
stargazer(mod2, mod25, mod26, mod29, mod31, mod52,  
          title="Fama French", type="text", 
          column.labels=c("AFLT", "MOEX", "MTSS", "PHOR", "PLZL", "VTBR"))
##Сигнальная теория##----
#Гипотеза: Изменение дивидендов компании прогнозирует изменение в будущем денежном потоке компании
data_INC <- data[, c("NAME", "DATE", "INC_0", "INC_1", "INC_2", "D_0", "D_1", "D_2")]%>% na.omit() %>% data.frame()
summary(data_INC)
mod_signal <- lm(data = data_INC, INC_0 ~ D_1 + D_2)
summary(mod_signal)
cook <- cooks.distance(mod_signal)
plot(cook,type="h", main = "Cook distance")
which.max(cook)
which(cook>0.05)

data_INC2 <- data_INC[-c(24, 54, 61, 142),]
mod_signal2 <- lm(data = data_INC2, INC_0 ~ D_1 + D_2)
summary(mod_signal2)

# mod: pooling
plm_mod_signal1 <- plm(data = data_INC, INC_0 ~ D_1 + D_2,
                       index = c("NAME","DATE"), model = "pooling")
summary(plm_mod_signal1)
# mod2: FE
plm_mod_signal2 <- plm(data = data_INC, INC_0 ~ D_1 + D_2,
                       index = c("NAME","DATE"),effect = "time", model = "within")
summary(plm_mod_signal2)
#RE
plm_mod_signal3 = plm(data = data_INC, INC_0 ~ D_1 + D_2,
                      index = c("NAME","DATE"), model="random")
summary(plm_mod_signal3)

#Тест для сравнения pooled регрессии и регрессии с фиксированными эффектами: если H0, то pooled
pooltest(plm_mod_signal1,plm_mod_signal2) #FE
#Тест Хаусмана для сравнения FE и RE моделей: если H0, то RE
phtest(plm_mod_signal2, plm_mod_signal3) #FE
#Тест Бреуша - Пагана для сравнения pooled и RE моделей: если H0, то pooled
plmtest(plm_mod_signal1,type="bp") #pooled

mod_signal_test <- lm(data = data_INC, INC_0 ~ INC_1 + INC_2)
summary(mod_signal_test)
corrplot(cor(data_INC[,-c(1:2)]))

stargazer(mod_signal,se=list(cse(mod_signal)), 
          title="Сигнальная теория", type="html", 
          column.labels=c("Связь дивидендных выплат и будущей прибыли"),
          df=FALSE, digits=1, out = "Сигнальная теория.html")
stargazer(mod_signal,se=list(cse(mod_signal)), 
          title="Сигнальная теория", type="text", 
          column.labels=c("Связь дивидендных выплат и будущей прибыли"),
          df=FALSE, digits=1)
stargazer(plm_mod_signal1, plm_mod_signal2, plm_mod_signal3,
          se=list(cse(plm_mod_signal1), cse(plm_mod_signal2), cse(plm_mod_signal3)), 
          title="Сигнальная теория", type="text", 
          column.labels=c("Связь дивидендных выплат и будущей прибыли"),
          df=FALSE, digits=1)
stargazer(plm_mod_signal1, plm_mod_signal2, plm_mod_signal3,
          se=list(cse(plm_mod_signal1), cse(plm_mod_signal2), cse(plm_mod_signal3)), 
          title="Сигнальная теория", type="html", 
          column.labels=c("Связь дивидендных выплат и будущей прибыли"),
          df=FALSE, digits=3, out = "Сигнальная теория2.html")

# DIVCHG ---
data_DIVCHG <- data[, c("NAME", "DATE", "DIVCHG", "delta_ROA", "delta_ROA_3", "GOV_PART", "delta_RISK")]%>% na.omit() %>% data.frame()
crPlots(mod3)
str(data_DIVCHG)
View(data_DIVCHG)
mod3 <- lm(data = data_DIVCHG, DIVCHG ~ delta_ROA + delta_ROA_3 + delta_RISK)
summary(mod3)
crPlots(mod3)

#ожидалось, что изменение в трех-месячных ROA и риск будут значимы с отрицательным знаком, в следствие чего можно было бы сделать вывод,
#что увеличения дивидендов у компаний в среднем снижается систематический риск, требуемую доходность на капитал и рентабельность
#это будет подтверждать агентскую теорию

data_DIVCHG_GOV <- subset(data_DIVCHG, data_DIVCHG$GOV_PART == 1)
mod4 <- plm(data = data_DIVCHG_GOV, DIVCHG ~ delta_ROA + delta_ROA_3 + delta_RISK,
            index = c("NAME","DATE"), model = "pooling")
summary(mod4)

data_DIVCHG_NGOV <- subset(data_DIVCHG, data_DIVCHG$GOV_PART == 0)
mod5 <- plm(data = data_DIVCHG_NGOV, DIVCHG ~ delta_ROA + delta_ROA_3 + delta_RISK,
           index = c("NAME","DATE"), model = "pooling")
summary(mod5)
stargazer(mod3, mod4, mod5, 
          se=list(cse(mod3),cse(mod4)),
          title="Рентабельность и выплаты дивидендов c выбросами", type="html", 
          column.labels=c("Общая выборка", "Компании с гос участием", "Компании без гос участия"),
          df=FALSE, digits=3, out = "data_DIVCHG.html")
stargazer(mod3, mod4, mod5, 
          se=list(cse(mod3),cse(mod4), cse(mod5)),
          title="Рентабельность и выплаты дивидендов", type="text", 
          column.labels=c("Общая выборка", "Компании с гос участием", "Компании без гос участия"),
          df=FALSE, digits=3)

#присутствует разница в знаке между гос и не гос компаниями. Компаниями с гос участием увеличивают дивиденды при увеличении рентабильности активов

##Агентская теория##----------
#Гипотеза: Дивидендная политика компаний положительно связана с наличием крупного инвестора 
data <- read.xlsx("data_первый эшелон — годовые.xlsx", sheetName = "Лист4")
data_DIVE <- data[, c("NAME", "DATE", "ROA", "Industry", "GOV_PART", "Dividend_payout_ratio",
                      "Size", "INV", "DEBT", "RISK")]%>% na.omit() %>% data.frame()
str(data_DIVE)
data_DIVE$DATE <- as.character(data_DIVE$DATE)
data_DIVE2 <- subset(data_DIVE, Dividend_payout_ratio < 1)
summary(data_DIVE2$Dividend_payout_ratio)

mod <- vglm(Dividend_payout_ratio ~ GOV_PART + ROA + Size + INV + DEBT + RISK + Industry + DATE, data = data_DIVE2, 
            tobit(Upper = Inf, Lower = 0)) 
summary(mod)

mod2 <- vglm(Dividend_payout_ratio ~ GOV_PART + ROA + Size + INV + RISK + Industry + DATE, data = data_DIVE2, 
             tobit(Upper = 1, Lower = 0)) 
summary(mod2)

mod3 <- vglm(Dividend_payout_ratio ~ GOV_PART + ROA + Industry + DATE, data = data_DIVE2, 
             tobit(Upper = 1, Lower = 0)) 
summary(mod3)

htmlreg(mod, file = "GOV_PART1.html")
htmlreg(mod2, file = "GOV_PART2.html")
htmlreg(mod3, file = "GOV_PART3.html")
#наличие государства в компании как мажоритарного акционера увеличивает дивидендные выплаты 
#проверка без тобита
mod4 <- lm(Dividend_payout_ratio ~ GOV_PART + ROA + Size + INV + RISK + Industry + DATE, data = data_DIVE) 
summary(mod4)

mod5 <- vglm(Dividend_payout_ratio ~ GOV_PART + ROA, data = data_DIVE2, 
             tobit(Upper = 1, Lower = 0)) 
summary(mod5)

# модель catering ---------------------------------------------------------
###с полугодовыми данными
####
data <- read.xlsx("data_первый эшелон — полугодовые.xlsx", sheetName = "Sheet1")
data <- data[c(1:841),]
data$DVD <- ifelse(data$IS_TOT_CASH_COM_DVD == 0, "0", "1")
data_D <- data[, c("NA.","NAME", "DATE", "P_B", "DVD")]
data_D <- subset(data_D, DVD == 1)
View(data_D)
avg_p_b_D <- aggregate(P_B ~ DATE, data = data_D, FUN = mean)
avg_p_b_D
#
data_ND <- data[, c("NA.","NAME", "DATE", "P_B", "DVD")]
data_ND <- subset(data_ND, DVD != 1)
data_ND <- na.omit(data_ND)
data_ND <- data_ND[-201,]
View(data_ND)
avg_p_b_ND <- aggregate(P_B ~ DATE, data = data_ND, FUN = mean)
avg_p_b_ND

P_D_DN <- log(avg_p_b_D$P_B[-2])-log(avg_p_b_ND$P_B[-2])
P_D_DN1 <- cbind(avg_p_b_D[-2,],avg_p_b_ND[-2,],P_D_DN)
P_D_DN1 <- P_D_DN1[,-3]
colnames(P_D_DN1) <- c("DATE", "P_B_D", "P_B_ND", "P_D_DN")
View(P_D_DN1)
#лишь во втром полугодии 11 года более привлекатлеьными были компании не выплачивающие дивиденды
ggplot(data = P_D_DN1, aes(x = DATE, y = P_B_D)) +
  geom_line(color = "blue") +
  geom_line(aes(x = DATE, y = P_B_ND), color = "blue", linetype = "dotted")

#регерессии
data <- subset(data, DATE!="2010-06-30")
data_Cont <- data %>% 
  group_by(DATE) %>% summarise(count = sum(Payer == 'Continue'))
data_Init <- data %>% 
  group_by(DATE) %>% summarise(count = sum(Payer == 'Initiate'))
data_Non <- data %>% 
  group_by(DATE) %>% summarise(count = sum(Payer == 'NonPayer'))
data_Non
dplyr::lag(data_Non)
data_Init

#функция lag работает так как нужно
r <- data.frame(data_Non, lag(data_Non$count), data_Init$count)
b <- data_Init$count/data_Non$count
c <- data_Init$count/dplyr::lag(data_Non$count)
r <- data.frame(r, b,c)
View(r)
#
Continue <- (data_Cont$count)/(data_Init$count+data_Cont$count+data_Non$count)
data_catering <- data.frame(rev(data$DATE[c(1:23)]), Continue, P_D_DN1$P_D_DN)
colnames(data_catering) <-  c("DATE", 'Continue', 'P_D_ND')
View(data_catering)

#Continue
mod_Cont <- lm(Continue ~ P_D_ND, data_catering)
summary(mod_Cont)

cor.test(data_catering$Continue, data_catering$P_D_ND)

stargazer(mod_Cont,se=list(cse(mod_Cont)), 
          title="кейтеринг теория", type="html", 
          column.labels=c("Кейтеринг"),
          df=FALSE, digits=1, out = "Кейтеринг.html")
  
ggplot(data_catering, aes(x = DATE)) + geom_line(aes(y = data_catering$P_D_ND), color = "#38A8A7") +
  geom_line(aes(y = data_catering$Continue*10), color = "#000000") + 
  theme_bw() + 
  theme(legend.position="bottom") + xlab("Год") + scale_y_continuous( name = "Continue", 
  sec.axis = sec_axis(~./10, name = "P_D_ND")) + scale_color_manual(values=c("#38A8A7", "#000000"))

####Модель Линтнера###-----
#панель
data <- read.xlsx("data_первый эшелон — годовые.xlsx", sheetName = "Лист4")
str(Data_Lintner)
Data_Lintner <- data[, c("NAME", "DATE", "TRAIL_12M_NET_INC", "TRAIL_12M_COM_DVD", "TRAIL_12M_COM_DVD_1")] %>% na.omit()
View(Data_Lintner)
#
# mod1: pooled
plm_mod1 <-  plm(data = Data_Lintner, TRAIL_12M_COM_DVD ~ TRAIL_12M_NET_INC + TRAIL_12M_COM_DVD_1,
                 index = c("NAME","DATE"), model="pooling")
summary(plm_mod1)
# mod2: FE
plm_mod2 <- plm(data = Data_Lintner, TRAIL_12M_COM_DVD ~ TRAIL_12M_NET_INC + TRAIL_12M_COM_DVD_1,
                index = c("NAME","DATE"),effect = "time", model = "within")
summary(plm_mod2)
#Тест для сравнения pooled регрессии и регрессии с фиксированными эффектами: если H0, то pooled
pooltest(plm_mod1,plm_mod2) #pooled
#RE
plm_mod3 = plm(data = Data_Lintner, TRAIL_12M_COM_DVD ~ TRAIL_12M_NET_INC + TRAIL_12M_COM_DVD_1,
               index = c("NAME","DATE"), model="random")
summary(plm_mod3)
#Тест Хаусмана для сравнения FE и RE моделей: если H0, то RE
phtest(plm_mod2, plm_mod3) #RE
#Тест Бреуша - Пагана для сравнения pooled и RE моделей: если H0, то pooled
plmtest(plm_mod1,type="bp") #pooled
stargazer(plm_mod1, plm_mod2, plm_mod3,   
          se=list(cse(plm_mod1),cse(plm_mod2), cse(plm_mod3)), 
          title="Сглаживание дивидендов", type="html", 
          column.labels=c("pooled", "FE", "RE"),
          df=FALSE, digits=3, out = "Lintner на грязных данных.html")
stargazer(plm_mod1, plm_mod2, plm_mod3,   
          se=list(cse(plm_mod1),cse(plm_mod2), cse(plm_mod3)), 
          title="Сглаживание дивидендов", type="text", 
          column.labels=c("pooled", "FE", "RE"),
          df=FALSE, digits=3)
#показатели модели линтнера
#c - скорость корректировки дивидендов Бета_див_1=(1-с)
#r - размер целевого коэфициента выплат: Бета_прибыль=c*r
c <- 1 - plm_mod3$coefficients[3]
r <- plm_mod3$coefficients[2]/c*100
c
r
####Модель Линтнера в реальных ценах###----
inf <- c(1, 1.061,	1.0658,	1.0645,	1.1136,	1.1291,	1.0538,	1.0252,	1.0427,	1.0305,	1.0491,	1.0839)
b <- numeric(length(inf))
b[1] <- inf[1]
for (i in 2:length(inf)) {
  b[i] <- b[i-1] * inf[i]
}
c <- rev(b)
c
discount_data <- data.frame(Data_Lintner[1:11,], c[-12])
discount_data <- data.frame(discount_data$DATE, discount_data$c)
colnames(discount_data) <- c("DATE", "c")
joined_df <- inner_join(discount_data, Data_Lintner, by = "DATE")
# переводим в реальные цены
real_Data_Lintner <- joined_df %>% 
  mutate(
    TRAIL_12M_NET_INC = TRAIL_12M_NET_INC / c,
    TRAIL_12M_COM_DVD = TRAIL_12M_COM_DVD / c,
    TRAIL_12M_COM_DVD_1 = TRAIL_12M_COM_DVD_1 / c
  )
#прогоняем регрессии
# mod1: pooled
real_plm_mod1 <-  plm(data = real_Data_Lintner, TRAIL_12M_COM_DVD ~ TRAIL_12M_NET_INC + TRAIL_12M_COM_DVD_1,
                      index = c("NAME","DATE"), model="pooling")
summary(real_plm_mod1)
# mod2: FE
real_plm_mod2 <- plm(data = real_Data_Lintner, TRAIL_12M_COM_DVD ~ TRAIL_12M_NET_INC + TRAIL_12M_COM_DVD_1,
                     index = c("NAME","DATE"), effect = "time", model = "within")
summary(real_plm_mod2)
#Тест для сравнения pooled регрессии и регрессии с фиксированными эффектами: если H0, то pooled
pooltest(real_plm_mod1,real_plm_mod2) #pooled
#RE
real_plm_mod3 = plm(data = real_Data_Lintner, TRAIL_12M_COM_DVD ~ TRAIL_12M_NET_INC + TRAIL_12M_COM_DVD_1,
                    index = c("NAME","DATE"), model="random")
summary(real_plm_mod3)
#Тест Хаусмана для сравнения FE и RE моделей: если H0, то RE
phtest(real_plm_mod2, real_plm_mod3) #RE
#Тест Бреуша - Пагана для сравнения pooled и RE моделей: если H0, то pooled
plmtest(real_plm_mod1,type="bp") #pooled
stargazer(real_plm_mod1, real_plm_mod2, real_plm_mod3,   
          se=list(cse(real_plm_mod1),cse(real_plm_mod2), cse(real_plm_mod3)), 
          title="Сглаживание дивидендов в реальных ценах", type="html", 
          column.labels=c("pooled", "FE", "RE"),
          df=FALSE, digits=3, out = "real_Lintner грязь.html")
stargazer(real_plm_mod1, real_plm_mod2, real_plm_mod3,   
          se=list(cse(real_plm_mod1),cse(real_plm_mod2), cse(real_plm_mod3)), 
          title="Сглаживание дивидендов в реальных ценах", type="text", 
          column.labels=c("pooled", "FE", "RE"),
          df=FALSE, digits=3)
#показатели модели линтнера
#c - скорость корректировки дивидендов Бета_див_1=(1-с)
#r - размер целевого коэфициента выплат: Бета_прибыль=c*r
c <- 1 - real_plm_mod3$coefficients[3]
r <- real_plm_mod3$coefficients[2]/c*100
c
r
#без реальных цен
stargazer(plm_mod1, plm_mod2, plm_mod3,   
          se=list(cse(plm_mod1),cse(plm_mod2), cse(plm_mod3)), 
          title="Сглаживание дивидендов", type="text", 
          column.labels=c("pooled", "FE", "RE"),
          df=FALSE, digits=3)
#
c <- 1 - plm_mod2$coefficients[2]
r <- plm_mod2$coefficients[1]/c*100
c
r
# Линтнер на очишенных данных ---------------------------------------------
Data_Lintner_free <- read.xlsx("data_первый эшелон — годовые.xlsx", sheetName = "Lintner")
Data_Lintner_free <- Data_Lintner_free[, c("NAME", "DATE", "TRAIL_12M_NET_INC",
                        "TRAIL_12M_COM_DVD", "TRAIL_12M_COM_DVD_1")] %>% na.omit()
# mod1: pooled
plm_mod1 <-  plm(data = Data_Lintner_free, TRAIL_12M_COM_DVD ~ TRAIL_12M_NET_INC + TRAIL_12M_COM_DVD_1,
                 index = c("NAME","DATE"), model="pooling")
summary(plm_mod1)
# mod2: FE
plm_mod2 <- plm(data = Data_Lintner_free, TRAIL_12M_COM_DVD ~ TRAIL_12M_NET_INC + TRAIL_12M_COM_DVD_1,
                index = c("NAME","DATE"),effect = "time", model = "within")
summary(plm_mod2)
#Тест для сравнения pooled регрессии и регрессии с фиксированными эффектами: если H0, то pooled
pooltest(plm_mod1,plm_mod2) #pooled
#RE
plm_mod3 = plm(data = Data_Lintner_free, TRAIL_12M_COM_DVD ~ TRAIL_12M_NET_INC + TRAIL_12M_COM_DVD_1,
               index = c("NAME","DATE"), model="random")
summary(plm_mod3)
#Тест Хаусмана для сравнения FE и RE моделей: если H0, то RE
phtest(plm_mod2, plm_mod3) #RE
#Тест Бреуша - Пагана для сравнения pooled и RE моделей: если H0, то pooled
plmtest(plm_mod1,type="bp") #RE
stargazer(plm_mod1, plm_mod2, plm_mod3,   
          se=list(cse(plm_mod1),cse(plm_mod2), cse(plm_mod3)), 
          title="Сглаживание дивидендов суперочищенные данные", type="html", 
          column.labels=c("pooled", "FE", "RE"),
          df=FALSE, digits=3, out = "Lintner_free.html")
stargazer(plm_mod1, plm_mod2, plm_mod3,   
          se=list(cse(plm_mod1),cse(plm_mod2), cse(plm_mod3)), 
          title="Сглаживание дивидендов суперочищенные данные", type="text", 
          column.labels=c("pooled", "FE", "RE"),
          df=FALSE, digits=3)
#показатели модели линтнера
#c - скорость корректировки дивидендов Бета_див_1=(1-с)
#r - размер целевого коэфициента выплат: Бета_прибыль=c*r
c <- 1 - plm_mod2$coefficients[2]
r <- plm_mod2$coefficients[1]/c*100
c
r
####Модель Линтнера в реальных ценах###--
inf <- c(1, 1.061,	1.0658,	1.0645,	1.1136,	1.1291,	1.0538,	1.0252,	1.0427,	1.0305,	1.0491,	1.0839)
b <- numeric(length(inf))
b[1] <- inf[1]
for (i in 2:length(inf)) {
  b[i] <- b[i-1] * inf[i]
}
c <- rev(b)
c
discount_data <- data.frame(Data_Lintner_free[1:11,], c[-12])
discount_data <- data.frame(discount_data$DATE, discount_data$c)
colnames(discount_data) <- c("DATE", "c")
joined_df <- inner_join(discount_data, Data_Lintner_free, by = "DATE")
# переводим в реальные цены
real_Data_Lintner <- joined_df %>% 
  mutate(
    TRAIL_12M_NET_INC = TRAIL_12M_NET_INC / c,
    TRAIL_12M_COM_DVD = TRAIL_12M_COM_DVD / c,
    TRAIL_12M_COM_DVD_1 = TRAIL_12M_COM_DVD_1 / c
  )
#прогоняем регрессии
# mod1: pooled
real_plm_mod1 <-  plm(data = real_Data_Lintner, TRAIL_12M_COM_DVD ~ TRAIL_12M_NET_INC + TRAIL_12M_COM_DVD_1,
                      index = c("NAME","DATE"), model="pooling")
summary(real_plm_mod1)
# mod2: FE
real_plm_mod2 <- plm(data = real_Data_Lintner, TRAIL_12M_COM_DVD ~ TRAIL_12M_NET_INC + TRAIL_12M_COM_DVD_1,
                     index = c("NAME","DATE"), effect = "time", model = "within")
summary(real_plm_mod2)
#Тест для сравнения pooled регрессии и регрессии с фиксированными эффектами: если H0, то pooled
pooltest(real_plm_mod1,real_plm_mod2) #pooled
#RE
real_plm_mod3 = plm(data = real_Data_Lintner, TRAIL_12M_COM_DVD ~ TRAIL_12M_NET_INC + TRAIL_12M_COM_DVD_1,
                    index = c("NAME","DATE"), model="random")
summary(real_plm_mod3)
#Тест Хаусмана для сравнения FE и RE моделей: если H0, то RE
phtest(real_plm_mod2, real_plm_mod3) #RE
#Тест Бреуша - Пагана для сравнения pooled и RE моделей: если H0, то pooled
plmtest(real_plm_mod1,type="bp") #RE
stargazer(real_plm_mod1, real_plm_mod2, real_plm_mod3,   
          se=list(cse(real_plm_mod1),cse(real_plm_mod2), cse(real_plm_mod3)), 
          title="Сглаживание дивидендов в реальных ценах суперочищенные данные", type="html", 
          column.labels=c("pooled", "FE", "RE"),
          df=FALSE, digits=3, out = "real_Lintner_free.html")
stargazer(real_plm_mod1, real_plm_mod2, real_plm_mod3,   
          se=list(cse(real_plm_mod1),cse(real_plm_mod2), cse(real_plm_mod3)), 
          title="Сглаживание дивидендов в реальных ценах суперочищенные данные", type="text", 
          column.labels=c("pooled", "FE", "RE"),
          df=FALSE, digits=3)
#показатели модели линтнера
#c - скорость корректировки дивидендов Бета_див_1=(1-с)
#r - размер целевого коэфициента выплат: Бета_прибыль=c*r
c <- 1 - real_plm_mod3$coefficients[3]
r <- real_plm_mod3$coefficients[2]/c*100
c
r
#без реальных цен
stargazer(plm_mod1, plm_mod2, plm_mod3,
          se=list(cse(plm_mod1),cse(plm_mod2), cse(plm_mod3)), 
          title="Сглаживание дивидендов  очищенные данные", type="text", 
          column.labels=c("pooled", "FE", "RE", "pooled", "FE", "RE"),
          df=FALSE, digits=3)
#
c <- 1 - plm_mod2$coefficients[2]
r <- plm_mod2$coefficients[1]/c*100
c
r
# LASSO -------------------------------------------------------------------
data_LASSO <- data[, c("MB_ratio", "RISK", "Leverage", "Size", "CASH", "GROWTH", "TAX", "CR", "FIX", 
                       "Life.Cycle_Stage", "ROA", "Dividend_payout_ratio")]%>% na.omit() %>% data.frame()

x <- as.matrix(data_LASSO[,-dim(data_LASSO)[2]])
y <- data_LASSO$Dividend_payout_ratio


# подбор лямбды
fit <- cv.glmnet(x, y, alpha = 1)
?cv.glmnet

# пезультат кросс-валидации
plot(fit)

# cберем оптимальную лямбду
opt_lambda1 <- fit$lambda.min
opt_lambda2 <- fit$lambda.1se


# LASSO
lasso_model1 <- glmnet(x, y, alpha = 1, lambda = opt_lambda1)
coef(lasso_model1)
lasso_model2 <- glmnet(x, y, alpha = 1, lambda = 0.03)
coef(lasso_model2)
lasso_model3 <- glmnet(x, y, alpha = 1, lambda = opt_lambda2)
coef(lasso_model3)
lasso_model4 <- glmnet(x, y, alpha = 1, lambda = 0.053)
coef(lasso_model4)
lasso_model5 <- glmnet(x, y, alpha = 1, lambda = 0.07)
coef(lasso_model5)
lasso_model6 <- glmnet(x, y, alpha = 1, lambda = 0.08)
coef(lasso_model6)

model <- lm(data = data_LASSO, Dividend_payout_ratio ~ .)
summary(model)
mod111 <- stepAIC(model)
summary(mod111)
# Random forest ----
data_forest <- data[, c("MB_ratio", "RISK", "Leverage", "Size", "CASH", "GROWTH", "TAX", "CR", "FIX", 
                       "Life.Cycle_Stage", "Dividend_payout_ratio", "ROA")]%>% na.omit() %>% data.frame()
model_rf <- randomForest(data = data_forest, Dividend_payout_ratio ~ MB_ratio + Leverage + ROA + 
                           RISK + Size + CASH + GROWTH + TAX + CR + FIX + Life.Cycle_Stage) 
library(viridis) 
library(randomForestExplainer) 
library(randomForest) 
library(ggplot2)
plot(model_rf) 
importance(model_rf) 
varImpPlot(model_rf) 


importance_df <- data.frame(importance(model_rf))
importance_df <- importance_df %>% arrange(desc(IncNodePurity))
importance_df <- data.frame(rownames(importance_df), importance_df)
names(importance_df) <- c("variable", "importance")

importance_df <- importance_df[order(importance_df$importance, decreasing = TRUE),]

ggplot(importance_df, aes(x = variable, y = importance, fill = importance)) + 
  geom_bar(stat = "identity", color = "black") + 
  scale_fill_gradient(low = "#38A8A7", high = "#000000", name = c("Важность")) +
  theme_minimal() +
  labs(x = "Переменная", y = "Важность") +
  coord_flip()

##pvar ----
data_var <- data[, c("Size", "ROA", "Leverage", "FIX",
                     "Dividend_payout_ratio", "MB_ratio", "NAME", "DATE")]%>% na.omit() %>% data.frame()

# Panel VAR
S <- table(data_var$NAME) >= 10
A <- names(table(data_var$NAME))[S]
data_var2 <- dplyr::filter(data_var, NAME %in% A)

fixed_model <- pvarfeols(dependent_vars = c("Dividend_payout_ratio", "MB_ratio"),
                         lags = 1,
                         exog_vars = c("Size", "ROA", "Leverage", "FIX"),
                         data = data_var2,
                         panel_identifier = c("NAME", "DATE"))
summary(fixed_model)
R_2 <- 1- sum(fixed_model$residuals^2)/sum((data_var$Dividend_payout_ratio-mean(data_var$Dividend_payout_ratio))^2)
R_2 
#модель финансовых показателей для для гос и не гос компаний ----
#
#
data_fin_gov <- data[, c("NAME", "DATE", "ROA", "GOV_PART", "FIX", "Dividend_payout_ratio",
                         "Size", "Leverage", "MB_ratio")]%>% na.omit() %>% data.frame()
data_fin_gov1 <- subset(data_fin_gov, GOV_PART == 1)
# mod1: pooled
plm_mod1 <-  plm(data = data_fin_gov1, Dividend_payout_ratio ~ ROA + FIX + Size + Leverage + MB_ratio,
                 index = c("NAME","DATE"), model="pooling")
summary(plm_mod1)
# mod2: FE
plm_mod2 <- plm(data = data_fin_gov1, Dividend_payout_ratio ~ ROA + FIX + Size + Leverage + MB_ratio,
                index = c("NAME","DATE"),effect = "twoways", model = "within")
summary(plm_mod2)
#RE
plm_mod3 = plm(data = data_fin_gov1, Dividend_payout_ratio ~ ROA + FIX + Size + Leverage + MB_ratio,
               index = c("NAME","DATE"), model="random")
summary(plm_mod3)
#Тест для сравнения pooled регрессии и регрессии с фиксированными эффектами: если H0, то pooled
pooltest(plm_mod1,plm_mod2) #FE
#Тест Хаусмана для сравнения FE и RE моделей: если H0, то RE
phtest(plm_mod2, plm_mod3) #RE
#Тест Бреуша - Пагана для сравнения pooled и RE моделей: если H0, то pooled
plmtest(plm_mod1,type="bp") #RE

stargazer(plm_mod1, plm_mod2, plm_mod3,   
          se=list(cse(plm_mod1),cse(plm_mod2), cse(plm_mod3)), 
          title="Зависимость дивидендных выплат от финансовых показателей для КГУ", type="html", 
          column.labels=c("pooled", "FE", "RE"),
          df=FALSE, digits=3, out = "fin_gov1.html")

stargazer(plm_mod1, plm_mod2, plm_mod3,   
          se=list(cse(plm_mod1),cse(plm_mod2), cse(plm_mod3)), 
          title="Зависимость дивидендных выплат от финансовых показателей для КГУ", type="text", 
          column.labels=c("pooled", "FE", "RE"),
          df=FALSE)

#частные
data_fin_gov0 <- subset(data_fin_gov, GOV_PART == 0)
# mod1: pooled
plm_mod01 <-  plm(data = data_fin_gov0, Dividend_payout_ratio ~ ROA + FIX + Size + Leverage + MB_ratio,
                  index = c("NAME","DATE"), model="pooling")
summary(plm_mod01)
# mod2: FE
plm_mod02 <- plm(data = data_fin_gov0, Dividend_payout_ratio ~ ROA + FIX + Size + Leverage + MB_ratio,
                 index = c("NAME","DATE"),effect = "time", model = "within")
summary(plm_mod02)
#RE
plm_mod03 = plm(data = data_fin_gov0, Dividend_payout_ratio ~ ROA + FIX + Size + Leverage + MB_ratio,
                index = c("NAME","DATE"), model="random")
summary(plm_mod03)
#Тест для сравнения pooled регрессии и регрессии с фиксированными эффектами: если H0, то pooled
pooltest(plm_mod01,plm_mod02) #FE
#Тест Хаусмана для сравнения FE и RE моделей: если H0, то RE
phtest(plm_mod02, plm_mod03) #FE
#Тест Бреуша - Пагана для сравнения pooled и RE моделей: если H0, то pooled
plmtest(plm_mod01,type="bp") #RE

stargazer(plm_mod01, plm_mod02, plm_mod03,   
          se=list(cse(plm_mod01),cse(plm_mod02), cse(plm_mod03)), 
          title="Зависимость дивидендных выплат от финансовых показателей для частных компаний", type="html", 
          column.labels=c("pooled", "FE", "RE"),
          df=FALSE, digits=3, out = "fin_gov0.html")

stargazer(plm_mod01, plm_mod02, plm_mod03,   
          se=list(cse(plm_mod01),cse(plm_mod02), cse(plm_mod03)), 
          title="Зависимость дивидендных выплат от финансовых показателей для частных компаний", type="text", 
          column.labels=c("pooled", "FE", "RE"),
          df=FALSE, digits=3)

#гос
stargazer(plm_mod1, plm_mod2, plm_mod3,   
          se=list(cse(plm_mod1),cse(plm_mod2), cse(plm_mod3)), 
          title="Зависимость дивидендных выплат от финансовых показателей для КГУ", type="text", 
          column.labels=c("pooled", "FE", "RE"),
          df=FALSE)

#гос и не гос
stargazer(plm_mod1, plm_mod2, plm_mod3, plm_mod01, plm_mod02, plm_mod03, 
          se=list(cse(plm_mod1),cse(plm_mod2), cse(plm_mod3), cse(plm_mod01),cse(plm_mod02)), 
          title="Зависимость дивидендных выплат от финансовых показателей", type="text", 
          column.labels=c("pooled", "FE", "RE", "pooled", "FE", "RE"),
          df=FALSE, digits=6)

#гос и не гос
stargazer(plm_mod1, plm_mod2, plm_mod3, plm_mod01, plm_mod02, plm_mod03, 
          se=list(cse(plm_mod1),cse(plm_mod2), cse(plm_mod3), cse(plm_mod01),cse(plm_mod02)), 
          title="Зависимость дивидендных выплат от финансовых показателей", type="html", 
          column.labels=c("pooled", "FE", "RE", "pooled", "FE", "RE"),
          df=FALSE, digits=6, out = "fin_gov0_priv.html")
