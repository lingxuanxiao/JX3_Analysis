#Program JX3_Husky
#Author: Xiaohua_Ye
#======================================================================================
#剑网三技能循环分析：天策
#主体方法：Monte-Carlo法
#======================================================================================
#Basic Assumptions:
#1. 剑三的时间单位是帧，每秒等价于16帧，且所有的时间判定以帧为单位
#2. 加速阈值理论，参见楚玄枫，泠落，南宫临风等人的研究
#3. 所有技能/奇穴按照其对应文字描述的方式生效
#4. 不考虑网络延迟
#5. 不考虑甩枪（突）的损失以及牧云的收益
#6. 不考虑Miss，识破导致的循环异常
#======================================================================================
#|------------------------------------------------------------------------------------|
#|主体函数说明：                                                                      |
#|Input:                                                                              |
#|该函数接受四个传入参数：GCD, DOT, TIME, S                                           |
#|GCD表示技能公共CD，单位是帧，允许值为[19, 24]中的整数                               |
#|DOT表示DOT技能生效时间间隔，单位是帧，允许值为[25, 32]中的整数                      |
#|TIME表示模拟循环时间，单位是帧，允许值为正整数，但考虑到模拟时间不宜过短，因此建议使|
#|用较大的数值                                                                        |
#|S表示模拟次数，单位是次，允许值为正整数，S默认为10000，建议至少超过1000             |
#|Output:                                                                             |
#|该函数输出两种不同方案的多次模拟的技能数平均值，格式为4*8的矩阵                     |
#|四行分别为：方案A无风虎；方案A有风虎；方案B无风虎；方案B有风虎                      |
#|八列分别为：龙吟，龙牙，破风，穿云，战八方，灭，流血（Dot跳数），破血（Dot跳数，多层|
#|Dot每跳视为多个单层Dot同时跳）                                                      |
#|------------------------------------------------------------------------------------|
Simulation = function(GCD, DOT, TIME, S = 1000){
	#----------------------------------------------------------------------------------
	#第一部分：指示物设定
	#第一类：技能CD，按顺序分别为龙吟，战八方，灭，公共CD
	CD = numeric(4)
	#第二类：自己
	Fenghu = 0				#自己身上只用考虑风虎的问题
	#第三类：目标
	Shang = 0				#目标身上的致伤
	Can = FALSE				#目标身上的致残
	Dot = numeric(2)		#目标身上的持续伤害剩余时间
	Poxue = 0				#目标身上可叠加Dot（破血）的层数
	#第四类：技能指示器，按顺序分别为：龙吟，龙牙，破风，穿云，战八方，灭，流血，破血；第一行是没有风虎，第二行是有风虎
	Jineng = matrix(0, 2, 8)
	Jineng_sum_1 = matrix(0, 2, 8)
	Jineng_sum_2 = matrix(0, 2, 8)
	#----------------------------------------------------------------------------------
	#第二部分：技能描绘
	#龙吟：CD7s；触发GCD；使目标致残；使自身下三次技能伤害提高（风虎）
	Longyin = function(n = 1){
		#技能统计部分
		if (Fenghu > 0){Fenghu <<- Fenghu - 1; Jineng[2, n] <<- Jineng[2, n] + 1}else{Jineng[1, n] <<- Jineng[1, n] + 1}
		#CD部分
		CD[1] <<- 112		#技能进入为时112帧（7s）的CD
		CD[4] <<- GCD		#技能触发公共CD
		#效果部分
		Can <<- TRUE		#技能致残
		Fenghu <<- 3		#技能触发风虎（3层）
	}
	#龙牙：无CD；触发GCD；消耗一层致残；刷新流血持续时间；有25%的概率刷新龙吟的CD
	Longya = function(n = 2){
		#技能统计部分
		if (Fenghu > 0){Fenghu <<- Fenghu - 1; Jineng[2, n] <<- Jineng[2, n] + 1}else{Jineng[1, n] <<- Jineng[1, n] + 1}
		#CD部分
		CD[4] <<- GCD						#技能触发公共CD
		#效果部分
		if (runif(1) >= 0.3){Can <<- FALSE} #技能有30%的概率不消耗消耗致残
		if (Dot[1] > 0){Dot[1] <<- 6 * DOT}	#技能刷新流血的持续时间，流血每30帧一跳，一共6跳
		if (runif(1) <= 0.25){CD[1] <<- 0}	#技能有25%的概率刷新龙吟的CD
	}
	#破风：无CD；触发GCD；叠加流血Dot（刷新持续时间）
	Pofeng = function(n = 3){
		#技能统计部分
		if (Fenghu > 0){Fenghu <<- Fenghu - 1; Jineng[2, n] <<- Jineng[2, n] + 1}else{Jineng[1, n] <<- Jineng[1, n] + 1}
		#CD部分
		CD[4] <<- GCD						#技能触发公共CD
		#效果部分
		Dot[1] <<- 6 * DOT					#技能刷新流血的持续时间，流血每30帧一跳，一共6跳
	}
	#穿云：无CD；触发GCD；叠加一层致伤
	Chuanyun = function(n = 4){
		#技能统计部分
		if (Fenghu > 0){Fenghu <<- Fenghu - 1; Jineng[2, n] <<- Jineng[2, n] + 1}else{Jineng[1, n] <<- Jineng[1, n] + 1}
		#CD部分
		CD[4] <<- GCD								#技能触发公共CD
		#效果部分
		Shang <<- Shang + 1							#叠加致伤
		if (Shang == 3){Shang <<- 0; Can <<- TRUE}	#判断致残
	}
	#战八方：CD7s；触发GCD；叠加一层致伤；叠加破血Dot（刷新持续时间以及堆叠层数，最多三层）
	Zhanbafang = function(n = 5){
		#技能统计部分
		if (Fenghu > 0){Fenghu <<- Fenghu - 1; Jineng[2, n] <<- Jineng[2, n] + 1}else{Jineng[1, n] <<- Jineng[1, n] + 1}
		#CD部分
		CD[2] <<- 112								#技能进入为时112帧（7s）的CD
		CD[4] <<- GCD								#技能触发公共CD
		#效果部分
		Shang <<- Shang + 1							#叠加致伤
		if (Shang == 3){Shang <<- 0; Can <<- TRUE}	#判断致残
		Dot[2] <<- 8 * DOT							#技能刷新破血的持续时间，破血每30帧一跳，一共8跳
		Poxue <<- min(3, Poxue + 1)					#计算破血层数
	}
	#灭：无CD；触发GCD
	Mie = function(n = 6){
		#技能统计部分
		if (Fenghu > 0){Fenghu <<- Fenghu - 1; Jineng[2, n] <<- Jineng[2, n] + 1}else{Jineng[1, n] <<- Jineng[1, n] + 1}
		#CD部分
		CD[3] <<- 112						#技能进入为时112帧（7s）的CD
		CD[4] <<- GCD						#技能触发公共CD
	}
	#流血：
	Liuxue = function(n = 7){
		#技能统计部分
		if (Dot[1] != 6*DOT & Dot[2]%%DOT == 0){Jineng[1, n] <<- Jineng[1, n] + 1}
	}
	#破血：
	Po = function(n = 8){
		#技能统计部分
		if (Dot[2] != 8*DOT & Dot[2]%%DOT == 0){Jineng[1, n] <<- Jineng[1, n] + Poxue}
	}
	#----------------------------------------------------------------------------------
	#第三部分：模拟
	#初始化函数
	Initialize = function(){
		CD <<- numeric(4)
		Fenghu <<- 0
		Shang <<- 0
		Can <<- FALSE
		Dot <<- numeric(2)
		Poxue <<- 0
		Jineng <<- matrix(0, 2, 8)
		Time <<- TIME
	}
	Time = TIME
	for (i in 1:S){
		Initialize()
		set.seed(1234 + i)
		#循环1（正常打技能）
		while (Time > 0){
			#执行技能判定：
			if (CD[4] == 0){
				#第一优先级：破风
				if (Dot[1] == 0){Pofeng()}else{
					#第二优先级：战八方补破血
					if (CD[2] == 0 & Dot[2] < 48){Zhanbafang()}else{
						#第三优先级：龙牙
						if (Can){Longya()}else{
							#第四优先级：龙吟
							if (CD[1] == 0){Longyin()}else{
								#第五优先级：战八方
								if (CD[2] == 0){Zhanbafang()}else{
									#第六优先级：灭
									if (CD[3] == 0 & Fenghu == 0 & Shang == 2){Mie()}else{
										#最低优先级：穿云
										Chuanyun()
									}
								}
							}
						}
					}
				}
			}
			#Dot判定：
			Liuxue()
			Po()
			#时间流逝：
			Time = Time - 1
			CD = CD - 1
			Dot = Dot - 1
			for (i in 1:4){CD[i] = max(CD[i], 0)}
		}
		Jineng_sum_1 = Jineng_sum_1 + Jineng
		Initialize()
		#set.seed(1234 + i)
		#循环2（等龙吟CD好）
		while (Time > 0){
			#执行技能判定：
			if (CD[4] == 0){
				#第零优先级：等龙吟CD
				if (CD[1] > 0 & CD[1] < GCD){}else{
					#第一优先级：破风
					if (Dot[1] == 0){Pofeng()}else{
						#第二优先级：战八方补破血
						if (CD[2] == 0 & Dot[2] < 90){Zhanbafang()}else{
							#第三优先级：龙牙
							if (Can){Longya()}else{
								#第四优先级：龙吟
								if (CD[1] == 0){Longyin()}else{
									#第五优先级：战八方
									if (CD[2] == 0){Zhanbafang()}else{
										#最低优先级：穿云
										Chuanyun()
									}
								}
							}
						}
					}
				}
			}
			#Dot判定：
			Liuxue()
			Po()
			#时间流逝：
			Time = Time - 1
			CD = CD - 1
			Dot = Dot - 1
			for (i in 1:4){CD[i] = max(CD[i], 0)}
		}
		Jineng_sum_2 = Jineng_sum_2 + Jineng
	}
	return(rbind(Jineng_sum_1, Jineng_sum_2)/S)
}
#======================================================================================
#程序样例
set.seed(8888)							#初始化随机数发生器
Simulation(22, 30, 4800, 1000)			#208加速等级，时长5min，模拟1000次
#输出结果
#       [,1]   [,2] [,3]   [,4]   [,5]   [,6]    [,7]    [,8]
#[1,] 31.086 31.670    1 18.891  7.793 13.784 174.997 421.606
#[2,]  9.481 59.239    0 24.132 21.924  0.000   0.000   0.000
#[3,] 33.704 35.883    1 16.168  7.260  0.000 171.060 431.214
#[4,]  9.196 59.852    0 26.927 26.199  0.000   0.000   0.000
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
#		#Out[[i]][[j]][['Total']] = c(sum(Result[1:2, 1:6]), sum(Result[3:4, 1:6]))
#		#Out[[i]][[j]][['Equal']] = matrix(c(Result[1, ] + 1.2 * Result[2, ], Result[3, ] + 1.2 * Result[4, ]), 2)
#		#Out[[i]][[j]][['Compose']] = matrix(c((Result[1, 1:6] + Result[2, 1:6])/sum(Result[1:2, 1:6]), (Result[3, 1:6] + Result[4, 1:6])/sum(Result[3:4, 1:6])), 2)
#		#Out[[i]][[j]][['CoverRate']] = matrix(c(Result[2, 1:6]/(Result[1, 1:6] + Result[2, 1:6]), Result[4, 1:6]/(Result[3, 1:6] + Result[4, 1:6])), 2)
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
#write.csv(O, file = "C:/Users/Feixiao_L/Desktop/Husky.csv", row.names = F, quote = F)
