data3 = fread('../data/data_final_final.csv')

model1 = lm(totalnow ~ August_proportion + age_group_0_19 + 
              age_group_65 + enrollment_proportion + 
              +minority+MEDHHINC_2018+test_rate, data = data3)
result1 = data.table(summary(model1)$coefficients, keep.rownames = TRUE)
fwrite(result1,'../results/totalnow.csv')
model3 = lm(totalnow ~ August_proportion + age_group_0_19 + 
              age_group_65 + enrollment_proportion + 
              +minority+MEDHHINC_2018+population_density+test_rate, data = data3)
fwrite(result3,'../results/totalnow_with_density.csv')
result3 = data.table(summary(model3)$coefficients, keep.rownames = TRUE)

model13 = lm(totalnow_30 ~ August_proportion + age_group_0_19 + 
               age_group_65 + enrollment_proportion + 
               +minority+MEDHHINC_2018+test_rate, data = data3)
result13 = data.table(summary(model13)$coefficients, keep.rownames = TRUE)
fwrite(result13,'../results/totalnow_30.csv')

data3$enrollment_binary = (data3$enrollment_proportion != 0)
model16 = lm(totalnow ~ August_proportion + age_group_0_19 + 
               age_group_65 + enrollment_binary + 
               +minority+MEDHHINC_2018+test_rate, data = data3)
result16 = data.table(summary(model16)$coefficients, keep.rownames = TRUE)
fwrite(result16,'../results/totalnow_binary.csv')

data3$all_enrollment_proportion = data3$inperson+data3$Hybrid+data3$online
model17 = lm(totalnow ~ August_proportion + age_group_0_19 + 
               age_group_65 + all_enrollment_proportion + 
               +minority+MEDHHINC_2018+test_rate, data = data3)
result17 = data.table(summary(model17)$coefficients, keep.rownames = TRUE)
fwrite(result17,'../results/totalnow_all_enrollment.csv')

data_pro = data3[,]
ss1 = data_pro$inperson / (data_pro$inperson + data_pro$Hybrid+data_pro$online)
ss2 = data_pro$Hybrid / (data_pro$inperson + data_pro$Hybrid+data_pro$online)
ss3 = data_pro$online / (data_pro$inperson + data_pro$Hybrid+data_pro$online)
data_pro$inperson = ss1
data_pro$Hybrid = ss2
data_pro$online = ss3
data_pro = data_pro[!is.na(inperson)]

model5 = lm(totalnow ~ August_proportion + age_group_0_19 + 
              age_group_65 + online +Hybrid + 
              +minority+MEDHHINC_2018+test_rate, data = data_pro)
result5 = data.table(summary(model5)$coefficients, keep.rownames = TRUE)
fwrite(result5,'../results/totalnow_online_Hybrid.csv')

model14 = lm(totalnow_30 ~ August_proportion + age_group_0_19 + 
               age_group_65 + online +Hybrid + 
               +minority+MEDHHINC_2018+test_rate, data = data_pro)
result14 = data.table(summary(model14)$coefficients, keep.rownames = TRUE)
fwrite(result14,'../results/totalnow_30_online_Hybrid.csv')

model15 = lm(totalnow ~ August_proportion + age_group_0_19 + 
               age_group_65 + online +Hybrid + 
               +minority+MEDHHINC_2018+population_density+test_rate, data = data_pro)
result15 = data.table(summary(model15)$coefficients, keep.rownames = TRUE)
fwrite(result15,'../results/totalnow_with_density_online_Hybrid.csv')
data4 = data3[,]
data5 = data3[,]
names(data5)[which(names(data5) == 'total_oct')] = 't'
names(data4)[which(names(data4) == 'September_proportion')] = 't'
rbind(data5, data4, fill = TRUE)
data5$OCT = 1
data4$OCT = 0
data5$SEP = 0
data4$SEP = 1
data6 = rbind(data5, data4, fill =T)
model19 = lm(t ~ OCT + August_proportion + August_proportion:OCT + age_group_0_19
             + age_group_65 + enrollment_proportion:SEP + enrollment_proportion:OCT
             + minority + MEDHHINC_2018 + test_rate, data = data6)
result19 = data.table(summary(model19)$coefficients, keep.rownames = TRUE)
fwrite(result19, '../results/compare.csv')


data3 = fread('../data/data_without_large_city_final.csv')

model7 = lm(totalnow ~ August_proportion + age_group_0_19 + 
              age_group_65 + enrollment_proportion + 
              +minority+MEDHHINC_2018+test_rate, data = data3)
result7 = data.table(summary(model7)$coefficients, keep.rownames = TRUE)
fwrite(result7,'../results/totalnow_no_large_city.csv')



data_pro = data3[,]
ss1 = data_pro$inperson / (data_pro$inperson + data_pro$Hybrid+data_pro$online)
ss2 = data_pro$Hybrid / (data_pro$inperson + data_pro$Hybrid+data_pro$online)
ss3 = data_pro$online / (data_pro$inperson + data_pro$Hybrid+data_pro$online)
data_pro$inperson = ss1
data_pro$Hybrid = ss2
data_pro$online = ss3
data_pro = data_pro[!is.na(inperson)]
model11 = lm(totalnow ~ August_proportion + age_group_0_19 + 
               age_group_65 + online +Hybrid + 
               +minority+MEDHHINC_2018+test_rate, data = data_pro)
result11 = data.table(summary(model11)$coefficients, keep.rownames = TRUE)
fwrite(result11,'../results/totalnow_online_Hybrid_no_large_city.csv')
