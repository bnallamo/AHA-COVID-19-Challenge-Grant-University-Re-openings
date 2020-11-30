library(ggplot2)
data = read.csv("..\\data\\cases2.csv")
caseByDate = aggregate(x = data$confirmed_count, by = list(data$confirmed_date), FUN=sum)
colnames(caseByDate) = c("Date", "New Cases")
caseByDate$Date = as.Date(caseByDate$Date)

caseByDate$"7-day Average" = 0

for (i in 7:nrow(caseByDate)){
  caseByDate$`7-day Average`[i] = mean(caseByDate$`New Cases`[(i-6):i])
}

caseByDate = caseByDate[caseByDate$Date >= "2020-06-01",]
#ggplot(caseByDate, aes(x = Date)) + geom_col(aes(y = `New Cases`), fill = "#de2d26", alpha = 0.6) + geom_line(aes(y=`7-day Average`, lty = "7-day Average"), color = "#de2d26", size = 2) + coord_cartesian(ylim=c(18000, 90000)) + scale_x_date(date_breaks = "15 days", date_labels = "%m/%d") + label(lty = "")
ggplot(caseByDate, aes(x = Date)) + geom_col(aes(y = `New Cases`, size = "New Cases"), fill = "#de2d26", alpha = 0.6) + geom_line(aes(y=`7-day Average`, lty = "7-day Average"), color = "#de2d26", size = 2) + coord_cartesian(ylim=c(18000, 90000)) + scale_x_date(date_breaks = "15 days", date_labels = "%m/%d") + labs(lty = "") + theme(legend.position = c(0.9,0.9), legend.title = element_blank(), legend.spacing = unit(0, "mm"), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.background = element_blank(), axis.text = element_text(size = 30), axis.title = element_text(size = 30), legend.text = element_text(size = 30)) + labs(y = "New Confirmed COVID-19 Cases")
