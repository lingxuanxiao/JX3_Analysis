#Program JX3_Husky
#Author: Xiaohua_Ye
#======================================================================================
#剑网三技能循环分析：天策
#计算，分析和出图
#模拟的核函数来自Husky_Core.R文件
#======================================================================================
#模拟计算
#计算对象：分技能分风虎技能数统计
#计算指标：技能总数（不含Dot）；等效技能数；直接技能比例；特定技能风虎覆盖率；Dot构成
#自变量：加速；时间
#加速-帧数换算表
#加速  0    5  157  208  318  434  489  678  876   945  1092
#GCD  24   23   23   22   22   21   21   20   20    19    19
#DOT  32   31   30   30   29   29   28   27   26    26    25
#加速考虑所有可能的帧数组合
#Jiasu = rbind(c(24, 23, 23, 22, 22, 21, 21, 20, 20, 19, 19), c(32, 31, 30, 30, 29, 29, 28, 27, 26, 26, 25))
#时间以半分钟为单位，计算从1min到10min的战斗
#Time = c(2:20/2*60*16)
#模拟结果输出
#set.seed(1234)
#Out = list()
#Result = matrix()
#for (i in 1:19){
#	Out[[i]] = list()
#	for (j in 1:11){
#		Out[[i]][[j]] = list()
#		Result = Simulation(Jiasu[1, j], Jiasu[2, j], Time[i])
#		Out[[i]][[j]][['Detail']] = Result
#	}
#}
#O = matrix(0, 19*11*4, 11)
#for (i in 1:19){
#	for (j in 1:11){
#		for (k in 1:4){
#			l = (i-1)*44+(j-1)*4+k
#			O[l, 1] = Time[i]
#			O[l, 2] = Jiasu[1, j]
#			O[l, 3] = Jiasu[2, j]
#			O[l, 4:11] = Out[[i]][[j]]$Detail[k,]
#		}
#	}
#}
#write.csv(O, file = "~Desktop/Husky_Simu1000_Seed1234.csv", row.names = F, quote = F)
#对csv文件进行基本整理（修改变量名，添加状态描述等）
#======================================================================================
#计算等效技能一
#将风虎的20%伤害折合进技能本身
#Husky = read.csv('~/Desktop/Husky_Simu1000_Seed1234.csv')
#ZTotal = numeric(418)
#for (i in 1:418){ZTotal[i] = sum(Husky[(i-1)*2+1, 4:9], Husky[(i-1)*2+2, 4:9])}
#DTotal = numeric(418)
#for (i in 1:418){DTotal[i] = sum(Husky[(i-1)*2+1, 10:11], Husky[(i-1)*2+2, 10:11])}
#write.csv(Husky, file = "~Desktop/Husky_Aggregate_1.csv", row.names = F, quote = F)
#对csv文件进行基本整理（修改变量名，添加状态描述等）
#======================================================================================
#计算等效技能二
#将直接技能拆解为附带武器伤害部分和技能伤害部分，分别以梅花枪法与穿云作为标准衡量，赋予不同技能不同权重，其中技能伤害部分的权重以技能参数为准
#确定权重时不计秘籍收益，不计奇穴收益，不计致残、致伤带来的收益，部分数据取的是经验权重
#计算技能参数：认为技能附带攻击与面板攻击呈正比例关系
#Jigong = c(1021, 1679, 2048, 2420, 2920, 3440, 3786)
#MIE = c(1187, 1952, 2381, 2813, 3395, 3999, 4401)
#CHUAN = c(1110, 1826, 2227, 2632, 3176, 3741, 4117)
#ZHAN = c(1059, 1742, 2125, 2511, 3030, 3569, 3928)
#YA = c(1621, 2665, 3251, 3842, 4636, 5461, 6010)
#YIN = c(1275, 2099, 2560, 3025, 3650, 4300, 4733)
#分析结果
#> lm(MIE ~ Jigong)
#Call:
#lm(formula = MIE ~ Jigong)
#Coefficients:
#(Intercept)       Jigong  
#      0.239        1.162  
#> lm(CHUAN ~ Jigong)
#Call:
#lm(formula = CHUAN ~ Jigong)
#Coefficients:
#(Intercept)       Jigong  
#    -0.1482       1.0876  
#> lm(ZHAN ~ Jigong)
#Call:
#lm(formula = ZHAN ~ Jigong)
#Coefficients:
#(Intercept)       Jigong  
#    -0.1298       1.0376  
#> lm(YA ~ Jigong)
#Call:
#lm(formula = YA ~ Jigong)
#Coefficients:
#(Intercept)       Jigong  
#  -0.007441     1.587504  
#> lm(YIN ~ Jigong)
#Call:
#lm(formula = YIN ~ Jigong)
#Coefficients:
#(Intercept)       Jigong  
#     -1.044        1.250 
#考虑到四舍五入，结果基本符合我们的预期
#权重：依次是龙吟，龙牙，破风，穿云，战八方，灭；标准技能：穿云
#weight = c(1.250, 1.587504, 0, 1.0876, 1.0376 * 1.5, 1.162)/1.0876
#Husky = read.csv('~/Desktop/Husky_Simu1000_Seed1234.csv')
#计算等效技能二
#DENGXIAO = 1.2 * Husky[which(Husky$Fenghu), 4:11] + Husky[which(!Husky$Fenghu), 4:11]
#Meihua = numeric(418)
#DChuanyun = numeric(418)
#for (i in 1:418){Meihua[i] = sum(DENGXIAO[i, 1:6]); DChuanyun[i] = sum(DENGXIAO[i, 1:6] * weight)}
#DDot = numeric(418)
#流血和破血的技能换算系数取的是经验权重，数据来源于叶承平大战太原98级木桩之后的DPS统计，只计算了命中伤害平均值的比，四舍五入保留一位
#for (i in 1:418){DDot[i] = DENGXIAO[i, 7] + 0.4 * DENGXIAO[i, 8]}
#write.csv(cbind(Husky, Meihua, DChuanyun, DDot), file = "~Desktop/Husky_Aggregate_2.csv", row.names = F, quote = F)
#对csv文件进行基本整理（修改变量名，添加状态描述等）
#合并两个名称中含有Husky_Aggregate的文件为Husky_Aggregate.csv，并计算次生变量（时间覆盖率），对不同时间下技能数求平均，将结果保存为Husky_show.csv文件
#======================================================================================
#画图
#Husky = read.csv('~/Desktop/Husky_show.csv')
#考虑到Husky_show给的是加速断点，这里用一个函数将其与连续的加速等级进行映射
#check = function(J, plan, x){
#	H = Husky[which(Husky$Xunhuan == plan),]
#	for (i in 1:11){if (J >= H[i, 1] & J < H[i+1, 1]){break}}
#	return(H[i, x])
#}
#绘制函数图像，加速取全部有意义的加速等级（0-1180，即不计算debuff情况下的加速下限至加速上限）
#X = c(0:1180)
#Y = numeric(1181)
#Z = numeric(1181)
#pdf('~Desktop/Husky.pdf')
#加速对每24帧释放技能数的影响
#for (i in 1:1181){Y[i] = check(X[i], 'A', 5); Z[i] = check(X[i], 'B', 5)}
#Dz = Y - Z
#plot(X, Y, 'l', ylim = c(0.9, 1.3), lty = 1, col = 'dark red', sub = 'The average number of release skills in each 24 frames', xlab = 'haste rate level', ylab = 'release skills')
#lines(X, Z, lty = 2, col = 'dark blue')
#legend("bottomright", legend = c('Plan A: don\'t wait CD', 'Plan B: wait CD'), lty = c(1, 2), col = c('dark red', 'dark blue'))
#加速对每24帧Dot跳数的影响
#for (i in 1:1181){Y[i] = check(X[i], 'A', 6) * 4 * 0.75; Z[i] = check(X[i], 'B', 6) * 4 * 0.75}
#Dd = Y - Z
#plot(X, Y, 'l', ylim = c(2.5, 3.7), lty = 1, col = 'dark red', sub = 'The average number of dots in each 24 frames', xlab = 'haste rate level', ylab = 'dots')
#lines(X, Z, lty = 2, col = 'dark blue')
#legend("bottomright", legend = c('Plan A: don\'t wait CD', 'Plan B: wait CD'), lty = c(1, 2), col = c('dark red', 'dark blue'))
#等效DPS：每24帧给出梅花枪法
#for (i in 1:1181){Y[i] = check(X[i], 'A', 7); Z[i] = check(X[i], 'B', 7)}
#Dm = Y - Z
#plot(X, Y, 'l', ylim = c(1, 1.4), lty = 1, col = 'dark red', sub = 'The average number of meihua in each 24 frames', xlab = 'haste rate level', ylab = 'meihuaqiangfa')
#lines(X, Z, lty = 2, col = 'dark blue')
#legend("bottomright", legend = c('Plan A: don\'t wait CD', 'Plan B: wait CD'), lty = c(1, 2), col = c('dark red', 'dark blue'))
#等效DPS：每24帧给出等效穿云
#for (i in 1:1181){Y[i] = check(X[i], 'A', 8); Z[i] = check(X[i], 'B', 8)}
#Dc = Y - Z
#plot(X, Y, 'l', ylim = c(1.3, 1.8), lty = 1, col = 'dark red', sub = 'The average number of chuanyun in each 24 frames', xlab = 'haste rate level', ylab = 'chuanyun')
#lines(X, Z, lty = 2, col = 'dark blue')
#legend("bottomright", legend = c('Plan A: don\'t wait CD', 'Plan B: wait CD'), lty = c(1, 2), col = c('dark red', 'dark blue'))
#等效DPS：每24帧给出流血
#for (i in 1:1181){Y[i] = check(X[i], 'A', 9) * 0.75; Z[i] = check(X[i], 'B', 9) * 0.75}
#Dl = Y - Z
#plot(X, Y, 'l', ylim = c(1.4, 2.1), lty = 1, col = 'dark red', sub = 'The average number of liuxue in each 24 frames', xlab = 'haste rate level', ylab = 'liuxue')
#lines(X, Z, lty = 2, col = 'dark blue')
#legend("bottomright", legend = c('Plan A: don\'t wait CD', 'Plan B: wait CD'), lty = c(1, 2), col = c('dark red', 'dark blue'))
#技能对时间的利用：比较每24帧可以打出的直接技能和Dot技能
#plot(X, Dz, 'l', ylim = c(-0.20, 0.13), lty = 1, col = 'dark red', sub = 'The difference in time use between Plan A and B (A - B)', xlab = 'haste rate level', ylab = 'Delta skills')
#lines(X, Dd, lty = 2, col = 'dark blue')
#legend("bottomleft", legend = c('Skills', 'Dots'), lty = c(1, 2), col = c('dark red', 'dark blue'))
#abline(h = 0, lty = 3)
#技能的DPS比较：比较每24帧可以打出的等效技能
#plot(X, Dm, 'l', ylim = c(-0.20, 0.16), lty = 1, col = 'dark red', sub = 'The difference in skill per second between Plan A and B (A - B)', xlab = 'haste rate level', ylab = 'Delta skills')
#lines(X, Dc, lty = 2, col = 'dark blue')
#lines(X, Dl, lty = 4, col = 'dark green')
#legend("bottomleft", legend = c('Meihua', 'Chuanyun', 'Liuxue'), lty = c(1, 2, 4), col = c('dark red', 'dark blue', 'dark green'))
#abline(h = 0, lty = 3)
#综合等效DPS比较：技能换算系数取的是经验权重，数据来源于叶承平大战太原98级木桩之后的DPS统计，只计算了命中伤害平均值的比，四舍五入保留一位
#D = Dm + 4.0 * Dc + 1.3 * Dl
#plot(X, D, 'l', ylim = c(-0.10, 0.70), lty = 1, col = 'dark blue', sub = 'The difference in skill per second between Plan A and B (A - B; measure in Meihua)', xlab = 'haste rate level', ylab = 'meihua')
#abline(h = 0, lty = 3)
#dev.off()