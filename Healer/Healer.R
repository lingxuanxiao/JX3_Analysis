#Program JX3_HealerAnalysis
#Author: Xiaohua_Ye
#======================================================================================
#剑网三治疗装备绿字属性最优化的分析与可视化
#======================================================================================
#Basic Assumptions:
#1. 加速阈值理论，参见楚玄枫，泠落，南宫临风等人的研究
#2. 所有技能/奇穴/心法按照其对应文字描述的方式生效
#3. 不考虑装备插孔、五彩石、附魔带来的影响（因为他们破坏了绿字会心和会效之间单纯的关系）
#4. 假定比较对象是同一个职业的同一个技能（抵消了技能参数带来的影响）
#5. 注意到公共CD的不同，因此不考虑加速带来的HPS提升，只考虑加速带来的可行域的变化（即主
#   要比较的是同职业，同附魔/五彩石/插孔，同加速情况下的纯绿字属性差异带来的影响，更一
#   般的推论参见讨论部分）
#======================================================================================
#有关绿字属性的最优化分析
#1. 目标函数给出：
#-------------------------------------------------------------------------------------|
#说明：                                                                               |
#Input:                                                                               |
#Zhiliao: 装备绿字治疗量                                                              |
#Shuanghui: 装备绿字会心                                                              |
#Gengu: 装备根骨                                                                      |
#Buff: 职业常驻基础治疗量加成                                                         |
#Xinfa: 职业心法加成，格式[基础治疗成效, 每点根骨治疗成效, 每点根骨会心, 每点根骨会效]|
#Qita: 其他来源的所有增益的总和                                                       |
#Output:                                                                              |
#理论单技能给出的治疗量                                                               |
#-------------------------------------------------------------------------------------|
Analysis = function(Zhiliao, Shuanghui, Gengu, Buff = 0, Xinfa, Qita = c(0, 0, 0)){
	#计算函数
	foo = function(x1, x2, x3){return(round(x1 * (1 + x2 / 4145 * (0.75 + x3 / 1507))))}
	#常数：
	#心法加成：来源于心法说明
	#根骨天然加成：来源于红尘宝典
	Linggen = c(0, 0.3, 0.15)
	#计算实体属性
	#第一部分：根骨附加属性
	Zhiye = floor(Gengu * Linggen) + floor(Gengu * Xinfa[2:4])
	#第二部分：实际属性
	#最终治疗量 = (装备绿字治疗量 + 心法基础治疗成效加成 + 其他基础治疗增益) * (1 + 常驻加成) + 心法每点根骨治疗成效加成
	#最终会心 = 装备绿字会心 + 根骨加成 + 其他
	#最终会效 = 装备绿字会效 + 根骨加成 + 其他
	x1 = round((Zhiliao + Xinfa[1] + Qita[1]) * (1 + Buff)) + Zhiye[1]
	x2 = Zhiye[2] + Shuanghui + Qita[2]
	x3 = Zhiye[3] + round(Shuanghui / 2.2) + Qita[3]
	return(foo(x1, x2, x3))
}
#2. 绘制目标函数等高图
#定义域确定：
#Shuanghui：1000品不穿会心到1160穿全会心：[0, 1600]
#Zhiliao：1000品不穿纯治疗到1160穿全纯治疗：[2000, 5000]
#定义更详细的场景：常驻治疗小药（五生盘 = 74，白信丹 = 111，汉宫棋 = 27，补心丹 = 41，烧尾宴 = 39）
#				   常驻沐风（20%会效），常驻破苍穹（5%会心，10%会效），常驻袖气（37）
#				   附魔：会心 = 49/68，会效 = 49/68，治疗量 = 39/53，武器 = 39治疗，衣服腰带 = 19 
#				   五彩石：会会疗 41, 81, 138
#分析目标：1080品装备，1160品装备；无加成，有加成（默认1160加成是风骨附魔，1080是剑胆附魔）
#额外属性总计：
#根骨：
Extra_Int = 144
#其他：
Extra_1080 = c(215, 534, 631)
Extra_Fumo_1080 = c(215, 139, 179)
Extra_Fumo_1080_SP = c(293, 139, 81)
Extra_1160 = c(215, 572, 669)
Extra_Fumo_1160 = c(215, 177, 217)
Extra_Fumo_1160_SP = c(321, 177, 81)
#可行域确定：
#全部1080装备，不计插孔，基础根骨：1016（计算了33点基础根骨），绿字治疗量：4318，本底治疗：257
#全部1160装备，不计插孔，基础根骨：1089（计算了33点基础根骨），绿字治疗量：4634，本底治疗：276
#治疗可行域计算：round((绿字治疗量 - 本底治疗量) * 5/9) + 本底治疗量，平行坐标轴(1080: 2513; 1160: 2697)
#				 会心 = 13/16 * (绿字治疗量 - 当前治疗量) - 加速等级 * 11/16
feasible = function(x, lvzi, jiasu){return(13/16 * (lvzi - x) - jiasu * 11/16)}
#目标函数绘制：
#定义参考线颜色：
Mycol = '#004000FF'
#生成格点：
x = seq(2250, 5000, length.out = 276)
xx = seq(2250, 5000, length.out = 2751)
y = seq(0, 1650, length.out = 166)
z = matrix(0, nrow = 276, ncol = 166)
#组织输出
pdf('~/Desktop/optimization.pdf')
#1) 五毒
#--------------------------------------------------------------------------------------
#1080光板
for (i in 1:276){for (j in 1:166){z[i, j] = Analysis(Zhiliao = x[i], Shuanghui = y[j], Gengu = 1016, Buff = 0, Xinfa = c(1432, 1.75, 0, 0))}}
#绘图并添加参考线
filled.contour(x, y, z, color = terrain.colors, xlab = 'Spell power', ylab = 'Critical', zlim = c(6000, 18000), main = 'HPSkill of Wudu without Buff, 1080', key.title = title('HPSkill'),
	plot.axes = {axis(1); axis(2); abline(v = 2513, col = Mycol); points(xx, feasible(xx, 4318, 0), 'l', col = Mycol); points(xx, feasible(xx, 4318, 475), 'l', col = Mycol); points(xx, feasible(xx, 4318, 1051), 'l', col = Mycol)}
)
#1080附魔加成Buff
for (i in 1:276){for (j in 1:166){z[i, j] = Analysis(Zhiliao = x[i], Shuanghui = y[j], Gengu = 1016, Buff = 0, Xinfa = c(1432, 1.75, 0, 0), Qita = Extra_Fumo_1080)}}
#绘图并添加参考线
filled.contour(x, y, z, color = terrain.colors, xlab = 'Spell power', ylab = 'Critical', zlim = c(6000, 18000), main = 'HPSkill of Wudu with Fumo, 1080', key.title = title('HPSkill'),
	plot.axes = {axis(1); axis(2); abline(v = 2513, col = Mycol); points(xx, feasible(xx, 4318, 0), 'l', col = Mycol); points(xx, feasible(xx, 4318, 475), 'l', col = Mycol); points(xx, feasible(xx, 4318, 1051), 'l', col = Mycol)}
)
#1080附魔加成Buff(治疗)
for (i in 1:276){for (j in 1:166){z[i, j] = Analysis(Zhiliao = x[i], Shuanghui = y[j], Gengu = 1016, Buff = 0, Xinfa = c(1432, 1.75, 0, 0), Qita = Extra_Fumo_1080_SP)}}
#绘图并添加参考线
filled.contour(x, y, z, color = terrain.colors, xlab = 'Spell power', ylab = 'Critical', zlim = c(6000, 18000), main = 'HPSkill of Wudu with Fumo(SP), 1080', key.title = title('HPSkill'),
	plot.axes = {axis(1); axis(2); abline(v = 2513, col = Mycol); points(xx, feasible(xx, 4318, 0), 'l', col = Mycol); points(xx, feasible(xx, 4318, 475), 'l', col = Mycol); points(xx, feasible(xx, 4318, 1051), 'l', col = Mycol)}
)
#1080全加成Buff（小药附魔五彩石收益）
for (i in 1:276){for (j in 1:166){z[i, j] = Analysis(Zhiliao = x[i], Shuanghui = y[j], Gengu = 1016 + Extra_Int, Buff = 0, Xinfa = c(1432, 1.75, 0, 0), Qita = Extra_1080)}}
#绘图并添加参考线
filled.contour(x, y, z, color = terrain.colors, xlab = 'Spell power', ylab = 'Critical', zlim = c(6000, 18000), main = 'HPSkill of Wudu with All Buff, 1080', key.title = title('HPSkill'),
	plot.axes = {axis(1); axis(2); abline(v = 2513, col = Mycol); points(xx, feasible(xx, 4318, 0), 'l', col = Mycol); points(xx, feasible(xx, 4318, 475), 'l', col = Mycol); points(xx, feasible(xx, 4318, 1051), 'l', col = Mycol)}
)
#--------------------------------------------------------------------------------------
#1160光板
for (i in 1:276){for (j in 1:166){z[i, j] = Analysis(Zhiliao = x[i], Shuanghui = y[j], Gengu = 1089, Buff = 0, Xinfa = c(1432, 1.75, 0, 0))}}
#绘图并添加参考线
filled.contour(x, y, z, color = terrain.colors, xlab = 'Spell power', ylab = 'Critical', zlim = c(6000, 18000), main = 'HPSkill of Wudu without Buff, 1160', key.title = title('HPSkill'),
	plot.axes = {axis(1); axis(2); abline(v = 2697, col = Mycol); points(xx, feasible(xx, 4634, 0), 'l', col = Mycol); points(xx, feasible(xx, 4634, 475), 'l', col = Mycol); points(xx, feasible(xx, 4634, 1051), 'l', col = Mycol)}
)
#1160附魔加成Buff
for (i in 1:276){for (j in 1:166){z[i, j] = Analysis(Zhiliao = x[i], Shuanghui = y[j], Gengu = 1089, Buff = 0, Xinfa = c(1432, 1.75, 0, 0), Qita = Extra_Fumo_1080)}}
#绘图并添加参考线
filled.contour(x, y, z, color = terrain.colors, xlab = 'Spell power', ylab = 'Critical', zlim = c(6000, 18000), main = 'HPSkill of Wudu with Fumo, 1160', key.title = title('HPSkill'),
	plot.axes = {axis(1); axis(2); abline(v = 2697, col = Mycol); points(xx, feasible(xx, 4634, 0), 'l', col = Mycol); points(xx, feasible(xx, 4634, 475), 'l', col = Mycol); points(xx, feasible(xx, 4634, 1051), 'l', col = Mycol)}
)
#1160附魔加成Buff(治疗)
for (i in 1:276){for (j in 1:166){z[i, j] = Analysis(Zhiliao = x[i], Shuanghui = y[j], Gengu = 1089, Buff = 0, Xinfa = c(1432, 1.75, 0, 0), Qita = Extra_Fumo_1080_SP)}}
#绘图并添加参考线
filled.contour(x, y, z, color = terrain.colors, xlab = 'Spell power', ylab = 'Critical', zlim = c(6000, 18000), main = 'HPSkill of Wudu with Fumo(SP), 1160', key.title = title('HPSkill'),
	plot.axes = {axis(1); axis(2); abline(v = 2697, col = Mycol); points(xx, feasible(xx, 4634, 0), 'l', col = Mycol); points(xx, feasible(xx, 4634, 475), 'l', col = Mycol); points(xx, feasible(xx, 4634, 1051), 'l', col = Mycol)}
)
#1160全加成Buff（小药附魔五彩石收益）
for (i in 1:276){for (j in 1:166){z[i, j] = Analysis(Zhiliao = x[i], Shuanghui = y[j], Gengu = 1089 + Extra_Int, Buff = 0, Xinfa = c(1432, 1.75, 0, 0), Qita = Extra_1080)}}
#绘图并添加参考线
filled.contour(x, y, z, color = terrain.colors, xlab = 'Spell power', ylab = 'Critical', zlim = c(6000, 18000), main = 'HPSkill of Wudu with All Buff, 1160', key.title = title('HPSkill'),
	plot.axes = {axis(1); axis(2); abline(v = 2697, col = Mycol); points(xx, feasible(xx, 4634, 0), 'l', col = Mycol); points(xx, feasible(xx, 4634, 475), 'l', col = Mycol); points(xx, feasible(xx, 4634, 1051), 'l', col = Mycol)}
)
#--------------------------------------------------------------------------------------
#2. 七秀
#--------------------------------------------------------------------------------------
#1080光板
for (i in 1:276){for (j in 1:166){z[i, j] = Analysis(Zhiliao = x[i], Shuanghui = y[j], Gengu = 1117, Buff = 0.45, Xinfa = c(1400, 1.70, 0.06, 0.02))}}
#绘图并添加参考线
filled.contour(x, y, z, color = terrain.colors, xlab = 'Spell power', ylab = 'Critical', zlim = c(6000, 18000), main = 'HPSkill of Qixiu without Buff, 1080', key.title = title('HPSkill'),
	plot.axes = {axis(1); axis(2); abline(v = 2513, col = Mycol); points(xx, feasible(xx, 4318, 318), 'l', col = Mycol); points(xx, feasible(xx, 4318, 678), 'l', col = Mycol); points(xx, feasible(xx, 4318, 1092), 'l', col = Mycol)}
)
#1080附魔加成Buff
for (i in 1:276){for (j in 1:166){z[i, j] = Analysis(Zhiliao = x[i], Shuanghui = y[j], Gengu = 1117, Buff = 0.45, Xinfa = c(1400, 1.70, 0.06, 0.02), Qita = Extra_Fumo_1080)}}
#绘图并添加参考线
filled.contour(x, y, z, color = terrain.colors, xlab = 'Spell power', ylab = 'Critical', zlim = c(6000, 18000), main = 'HPSkill of Qixiu with Fumo, 1080', key.title = title('HPSkill'),
	plot.axes = {axis(1); axis(2); abline(v = 2513, col = Mycol); points(xx, feasible(xx, 4318, 318), 'l', col = Mycol); points(xx, feasible(xx, 4318, 678), 'l', col = Mycol); points(xx, feasible(xx, 4318, 1092), 'l', col = Mycol)}
)
#1080附魔加成Buff(治疗)
for (i in 1:276){for (j in 1:166){z[i, j] = Analysis(Zhiliao = x[i], Shuanghui = y[j], Gengu = 1117, Buff = 0.45, Xinfa = c(1400, 1.70, 0.06, 0.02), Qita = Extra_Fumo_1080_SP)}}
#绘图并添加参考线
filled.contour(x, y, z, color = terrain.colors, xlab = 'Spell power', ylab = 'Critical', zlim = c(6000, 18000), main = 'HPSkill of Qixiu with Fumo(SP), 1080', key.title = title('HPSkill'),
	plot.axes = {axis(1); axis(2); abline(v = 2513, col = Mycol); points(xx, feasible(xx, 4318, 318), 'l', col = Mycol); points(xx, feasible(xx, 4318, 678), 'l', col = Mycol); points(xx, feasible(xx, 4318, 1092), 'l', col = Mycol)}
)
#1080全加成Buff（小药附魔五彩石收益）
for (i in 1:276){for (j in 1:166){z[i, j] = Analysis(Zhiliao = x[i], Shuanghui = y[j], Gengu = 1276, Buff = 0.45, Xinfa = c(1400, 1.70, 0.06, 0.02), Qita = Extra_1080)}}
#绘图并添加参考线
filled.contour(x, y, z, color = terrain.colors, xlab = 'Spell power', ylab = 'Critical', zlim = c(6000, 18000), main = 'HPSkill of Qixiu with All Buff, 1080', key.title = title('HPSkill'),
	plot.axes = {axis(1); axis(2); abline(v = 2513, col = Mycol); points(xx, feasible(xx, 4318, 318), 'l', col = Mycol); points(xx, feasible(xx, 4318, 678), 'l', col = Mycol); points(xx, feasible(xx, 4318, 1092), 'l', col = Mycol)}
)
#--------------------------------------------------------------------------------------
#1160光板
for (i in 1:276){for (j in 1:166){z[i, j] = Analysis(Zhiliao = x[i], Shuanghui = y[j], Gengu = 1197, Buff = 0.45, Xinfa = c(1400, 1.70, 0.06, 0.02))}}
#绘图并添加参考线
filled.contour(x, y, z, color = terrain.colors, xlab = 'Spell power', ylab = 'Critical', zlim = c(6000, 18000), main = 'HPSkill of Qixiu without Buff, 1160', key.title = title('HPSkill'),
	plot.axes = {axis(1); axis(2); abline(v = 2697, col = Mycol); points(xx, feasible(xx, 4634, 318), 'l', col = Mycol); points(xx, feasible(xx, 4634, 678), 'l', col = Mycol); points(xx, feasible(xx, 4634, 1092), 'l', col = Mycol)}
)
#1160附魔加成Buff
for (i in 1:276){for (j in 1:166){z[i, j] = Analysis(Zhiliao = x[i], Shuanghui = y[j], Gengu = 1197, Buff = 0.45, Xinfa = c(1400, 1.70, 0.06, 0.02), Qita = Extra_Fumo_1080)}}
#绘图并添加参考线
filled.contour(x, y, z, color = terrain.colors, xlab = 'Spell power', ylab = 'Critical', zlim = c(6000, 18000), main = 'HPSkill of Qixiu with Fumo, 1160', key.title = title('HPSkill'),
	plot.axes = {axis(1); axis(2); abline(v = 2697, col = Mycol); points(xx, feasible(xx, 4634, 318), 'l', col = Mycol); points(xx, feasible(xx, 4634, 678), 'l', col = Mycol); points(xx, feasible(xx, 4634, 1092), 'l', col = Mycol)}
)
#1160附魔加成Buff(治疗)
for (i in 1:276){for (j in 1:166){z[i, j] = Analysis(Zhiliao = x[i], Shuanghui = y[j], Gengu = 1197, Buff = 0.45, Xinfa = c(1400, 1.70, 0.06, 0.02), Qita = Extra_Fumo_1080_SP)}}
#绘图并添加参考线
filled.contour(x, y, z, color = terrain.colors, xlab = 'Spell power', ylab = 'Critical', zlim = c(6000, 18000), main = 'HPSkill of Qixiu with Fumo(SP), 1160', key.title = title('HPSkill'),
	plot.axes = {axis(1); axis(2); abline(v = 2697, col = Mycol); points(xx, feasible(xx, 4634, 318), 'l', col = Mycol); points(xx, feasible(xx, 4634, 678), 'l', col = Mycol); points(xx, feasible(xx, 4634, 1092), 'l', col = Mycol)}
)
#1160全加成Buff（小药附魔五彩石收益）
for (i in 1:276){for (j in 1:166){z[i, j] = Analysis(Zhiliao = x[i], Shuanghui = y[j], Gengu = 1356, Buff = 0.45, Xinfa = c(1400, 1.70, 0.06, 0.02), Qita = Extra_1080)}}
#绘图并添加参考线
filled.contour(x, y, z, color = terrain.colors, xlab = 'Spell power', ylab = 'Critical', zlim = c(6000, 18000), main = 'HPSkill of Qixiu with All Buff, 1160', key.title = title('HPSkill'),
	plot.axes = {axis(1); axis(2); abline(v = 2697, col = Mycol); points(xx, feasible(xx, 4634, 318), 'l', col = Mycol); points(xx, feasible(xx, 4634, 678), 'l', col = Mycol); points(xx, feasible(xx, 4634, 1092), 'l', col = Mycol)}
)
#--------------------------------------------------------------------------------------
#3. 长歌
#--------------------------------------------------------------------------------------
#1080光板
for (i in 1:276){for (j in 1:166){z[i, j] = Analysis(Zhiliao = x[i], Shuanghui = y[j], Gengu = 1016, Buff = 0.1, Xinfa = c(1462, 1.65, 0.1, 0.04))}}
#绘图并添加参考线
filled.contour(x, y, z, color = terrain.colors, xlab = 'Spell power', ylab = 'Critical', zlim = c(6000, 18000), main = 'HPSkill of Changge without Buff, 1080', key.title = title('HPSkill'),
	plot.axes = {axis(1); axis(2); abline(v = 2513, col = Mycol); points(xx, feasible(xx, 4318, 0), 'l', col = Mycol); points(xx, feasible(xx, 4318, 443), 'l', col = Mycol); points(xx, feasible(xx, 4318, 710), 'l', col = Mycol)}
)
#1080附魔加成Buff
for (i in 1:276){for (j in 1:166){z[i, j] = Analysis(Zhiliao = x[i], Shuanghui = y[j], Gengu = 1016, Buff = 0.1, Xinfa = c(1462, 1.65, 0.1, 0.04), Qita = Extra_Fumo_1080)}}
#绘图并添加参考线
filled.contour(x, y, z, color = terrain.colors, xlab = 'Spell power', ylab = 'Critical', zlim = c(6000, 18000), main = 'HPSkill of Changge with Fumo, 1080', key.title = title('HPSkill'),
	plot.axes = {axis(1); axis(2); abline(v = 2513, col = Mycol); points(xx, feasible(xx, 4318, 0), 'l', col = Mycol); points(xx, feasible(xx, 4318, 443), 'l', col = Mycol); points(xx, feasible(xx, 4318, 710), 'l', col = Mycol)}
)
#1080附魔加成Buff(治疗)
for (i in 1:276){for (j in 1:166){z[i, j] = Analysis(Zhiliao = x[i], Shuanghui = y[j], Gengu = 1016, Buff = 0.1, Xinfa = c(1462, 1.65, 0.1, 0.04), Qita = Extra_Fumo_1080_SP)}}
#绘图并添加参考线
filled.contour(x, y, z, color = terrain.colors, xlab = 'Spell power', ylab = 'Critical', zlim = c(6000, 18000), main = 'HPSkill of Changge with Fumo(SP), 1080', key.title = title('HPSkill'),
	plot.axes = {axis(1); axis(2); abline(v = 2513, col = Mycol); points(xx, feasible(xx, 4318, 0), 'l', col = Mycol); points(xx, feasible(xx, 4318, 443), 'l', col = Mycol); points(xx, feasible(xx, 4318, 710), 'l', col = Mycol)}
)
#1080全加成Buff（小药附魔五彩石收益）
for (i in 1:276){for (j in 1:166){z[i, j] = Analysis(Zhiliao = x[i], Shuanghui = y[j], Gengu = 1016 + Extra_Int, Buff = 0.1, Xinfa = c(1462, 1.65, 0.1, 0.04), Qita = Extra_1080)}}
#绘图并添加参考线
filled.contour(x, y, z, color = terrain.colors, xlab = 'Spell power', ylab = 'Critical', zlim = c(6000, 18000), main = 'HPSkill of Changge with All Buff, 1080', key.title = title('HPSkill'),
	plot.axes = {axis(1); axis(2); abline(v = 2513, col = Mycol); points(xx, feasible(xx, 4318, 0), 'l', col = Mycol); points(xx, feasible(xx, 4318, 443), 'l', col = Mycol); points(xx, feasible(xx, 4318, 710), 'l', col = Mycol)}
)
#--------------------------------------------------------------------------------------
#1160光板
for (i in 1:276){for (j in 1:166){z[i, j] = Analysis(Zhiliao = x[i], Shuanghui = y[j], Gengu = 1089, Buff = 0.1, Xinfa = c(1462, 1.65, 0.1, 0.04))}}
#绘图并添加参考线
filled.contour(x, y, z, color = terrain.colors, xlab = 'Spell power', ylab = 'Critical', zlim = c(6000, 18000), main = 'HPSkill of Changge without Buff, 1160', key.title = title('HPSkill'),
	plot.axes = {axis(1); axis(2); abline(v = 2697, col = Mycol); points(xx, feasible(xx, 4634, 0), 'l', col = Mycol); points(xx, feasible(xx, 4634, 443), 'l', col = Mycol); points(xx, feasible(xx, 4634, 710), 'l', col = Mycol)}
)
#1160附魔加成Buff
for (i in 1:276){for (j in 1:166){z[i, j] = Analysis(Zhiliao = x[i], Shuanghui = y[j], Gengu = 1089, Buff = 0.1, Xinfa = c(1462, 1.65, 0.1, 0.04), Qita = Extra_Fumo_1080)}}
#绘图并添加参考线
filled.contour(x, y, z, color = terrain.colors, xlab = 'Spell power', ylab = 'Critical', zlim = c(6000, 18000), main = 'HPSkill of Changge with Fumo, 1160', key.title = title('HPSkill'),
	plot.axes = {axis(1); axis(2); abline(v = 2697, col = Mycol); points(xx, feasible(xx, 4634, 0), 'l', col = Mycol); points(xx, feasible(xx, 4634, 443), 'l', col = Mycol); points(xx, feasible(xx, 4634, 710), 'l', col = Mycol)}
)
#1160附魔加成Buff(治疗)
for (i in 1:276){for (j in 1:166){z[i, j] = Analysis(Zhiliao = x[i], Shuanghui = y[j], Gengu = 1089, Buff = 0.1, Xinfa = c(1462, 1.65, 0.1, 0.04), Qita = Extra_Fumo_1080_SP)}}
#绘图并添加参考线
filled.contour(x, y, z, color = terrain.colors, xlab = 'Spell power', ylab = 'Critical', zlim = c(6000, 18000), main = 'HPSkill of Changge with Fumo(SP), 1160', key.title = title('HPSkill'),
	plot.axes = {axis(1); axis(2); abline(v = 2697, col = Mycol); points(xx, feasible(xx, 4634, 0), 'l', col = Mycol); points(xx, feasible(xx, 4634, 443), 'l', col = Mycol); points(xx, feasible(xx, 4634, 710), 'l', col = Mycol)}
)
#1160全加成Buff（小药附魔五彩石收益）
for (i in 1:276){for (j in 1:166){z[i, j] = Analysis(Zhiliao = x[i], Shuanghui = y[j], Gengu = 1089 + Extra_Int, Buff = 0.1, Xinfa = c(1462, 1.65, 0.1, 0.04), Qita = Extra_1080)}}
#绘图并添加参考线
filled.contour(x, y, z, color = terrain.colors, xlab = 'Spell power', ylab = 'Critical', zlim = c(6000, 18000), main = 'HPSkill of Changge with All Buff, 1160', key.title = title('HPSkill'),
	plot.axes = {axis(1); axis(2); abline(v = 2697, col = Mycol); points(xx, feasible(xx, 4634, 0), 'l', col = Mycol); points(xx, feasible(xx, 4634, 443), 'l', col = Mycol); points(xx, feasible(xx, 4634, 710), 'l', col = Mycol)}
)
#--------------------------------------------------------------------------------------
#4. 万花
#--------------------------------------------------------------------------------------
#1080光板
for (i in 1:276){for (j in 1:166){z[i, j] = Analysis(Zhiliao = x[i], Shuanghui = y[j], Gengu = 1016, Buff = 0.15, Xinfa = c(1369, 1.60, 0.15, 0.05))}}
#绘图并添加参考线
filled.contour(x, y, z, color = terrain.colors, xlab = 'Spell power', ylab = 'Critical', zlim = c(6000, 18000), main = 'HPSkill of Wanhua without Buff, 1080', key.title = title('HPSkill'),
	plot.axes = {axis(1); axis(2); abline(v = 2513, col = Mycol); points(xx, feasible(xx, 4318, 678), 'l', col = Mycol); points(xx, feasible(xx, 4318, 945), 'l', col = Mycol); points(xx, feasible(xx, 4318, 1092), 'l', col = Mycol)}
)
#1080附魔加成Buff
for (i in 1:276){for (j in 1:166){z[i, j] = Analysis(Zhiliao = x[i], Shuanghui = y[j], Gengu = 1016, Buff = 0.15, Xinfa = c(1369, 1.60, 0.15, 0.05), Qita = Extra_Fumo_1080)}}
#绘图并添加参考线
filled.contour(x, y, z, color = terrain.colors, xlab = 'Spell power', ylab = 'Critical', zlim = c(6000, 18000), main = 'HPSkill of Wanhua with Fumo, 1080', key.title = title('HPSkill'),
	plot.axes = {axis(1); axis(2); abline(v = 2513, col = Mycol); points(xx, feasible(xx, 4318, 678), 'l', col = Mycol); points(xx, feasible(xx, 4318, 945), 'l', col = Mycol); points(xx, feasible(xx, 4318, 1092), 'l', col = Mycol)}
)
#1080附魔加成Buff(治疗)
for (i in 1:276){for (j in 1:166){z[i, j] = Analysis(Zhiliao = x[i], Shuanghui = y[j], Gengu = 1016, Buff = 0.15, Xinfa = c(1369, 1.60, 0.15, 0.05), Qita = Extra_Fumo_1080_SP)}}
#绘图并添加参考线
filled.contour(x, y, z, color = terrain.colors, xlab = 'Spell power', ylab = 'Critical', zlim = c(6000, 18000), main = 'HPSkill of Wanhua with Fumo(SP), 1080', key.title = title('HPSkill'),
	plot.axes = {axis(1); axis(2); abline(v = 2513, col = Mycol); points(xx, feasible(xx, 4318, 678), 'l', col = Mycol); points(xx, feasible(xx, 4318, 945), 'l', col = Mycol); points(xx, feasible(xx, 4318, 1092), 'l', col = Mycol)}
)
#1080全加成Buff（小药附魔五彩石收益）
for (i in 1:276){for (j in 1:166){z[i, j] = Analysis(Zhiliao = x[i], Shuanghui = y[j], Gengu = 1016 + Extra_Int, Buff = 0.15, Xinfa = c(1369, 1.60, 0.15, 0.05), Qita = Extra_1080)}}
#绘图并添加参考线
filled.contour(x, y, z, color = terrain.colors, xlab = 'Spell power', ylab = 'Critical', zlim = c(6000, 18000), main = 'HPSkill of Wanhua with All Buff, 1080', key.title = title('HPSkill'),
	plot.axes = {axis(1); axis(2); abline(v = 2513, col = Mycol); points(xx, feasible(xx, 4318, 678), 'l', col = Mycol); points(xx, feasible(xx, 4318, 945), 'l', col = Mycol); points(xx, feasible(xx, 4318, 1092), 'l', col = Mycol)}
)
#--------------------------------------------------------------------------------------
#1160光板
for (i in 1:276){for (j in 1:166){z[i, j] = Analysis(Zhiliao = x[i], Shuanghui = y[j], Gengu = 1089, Buff = 0.15, Xinfa = c(1369, 1.60, 0.15, 0.05))}}
#绘图并添加参考线
filled.contour(x, y, z, color = terrain.colors, xlab = 'Spell power', ylab = 'Critical', zlim = c(6000, 18000), main = 'HPSkill of Wanhua without Buff, 1160', key.title = title('HPSkill'),
	plot.axes = {axis(1); axis(2); abline(v = 2697, col = Mycol); points(xx, feasible(xx, 4634, 678), 'l', col = Mycol); points(xx, feasible(xx, 4634, 945), 'l', col = Mycol); points(xx, feasible(xx, 4634, 1092), 'l', col = Mycol)}
)
#1160附魔加成Buff
for (i in 1:276){for (j in 1:166){z[i, j] = Analysis(Zhiliao = x[i], Shuanghui = y[j], Gengu = 1089, Buff = 0.15, Xinfa = c(1369, 1.60, 0.15, 0.05), Qita = Extra_Fumo_1080)}}
#绘图并添加参考线
filled.contour(x, y, z, color = terrain.colors, xlab = 'Spell power', ylab = 'Critical', zlim = c(6000, 18000), main = 'HPSkill of Wanhua with Fumo, 1160', key.title = title('HPSkill'),
	plot.axes = {axis(1); axis(2); abline(v = 2697, col = Mycol); points(xx, feasible(xx, 4634, 678), 'l', col = Mycol); points(xx, feasible(xx, 4634, 945), 'l', col = Mycol); points(xx, feasible(xx, 4634, 1092), 'l', col = Mycol)}
)
#1160附魔加成Buff(治疗)
for (i in 1:276){for (j in 1:166){z[i, j] = Analysis(Zhiliao = x[i], Shuanghui = y[j], Gengu = 1089, Buff = 0.15, Xinfa = c(1369, 1.60, 0.15, 0.05), Qita = Extra_Fumo_1080_SP)}}
#绘图并添加参考线
filled.contour(x, y, z, color = terrain.colors, xlab = 'Spell power', ylab = 'Critical', zlim = c(6000, 18000), main = 'HPSkill of Wanhua with Fumo(SP), 1160', key.title = title('HPSkill'),
	plot.axes = {axis(1); axis(2); abline(v = 2697, col = Mycol); points(xx, feasible(xx, 4634, 678), 'l', col = Mycol); points(xx, feasible(xx, 4634, 945), 'l', col = Mycol); points(xx, feasible(xx, 4634, 1092), 'l', col = Mycol)}
)
#1160全加成Buff（小药附魔五彩石收益）
for (i in 1:276){for (j in 1:166){z[i, j] = Analysis(Zhiliao = x[i], Shuanghui = y[j], Gengu = 1089 + Extra_Int, Buff = 0.15, Xinfa = c(1369, 1.60, 0.15, 0.05), Qita = Extra_1080)}}
#绘图并添加参考线
filled.contour(x, y, z, color = terrain.colors, xlab = 'Spell power', ylab = 'Critical', zlim = c(6000, 18000), main = 'HPSkill of Wanhua with All Buff, 1160', key.title = title('HPSkill'),
	plot.axes = {axis(1); axis(2); abline(v = 2697, col = Mycol); points(xx, feasible(xx, 4634, 678), 'l', col = Mycol); points(xx, feasible(xx, 4634, 945), 'l', col = Mycol); points(xx, feasible(xx, 4634, 1092), 'l', col = Mycol)}
)
#--------------------------------------------------------------------------------------
dev.off()
#======================================================================================
#一般性配装理论
#1. 三维切片图
#核心函数：
foo = function(x1, x2, x3){return(round(x1 * (1 + x2 * x3)))}
#注意：x3是会效减1
#值域：
#治疗量：从万花1000品最低治疗量起始到五毒1160品开全爆发最高治疗量：(4500, 17000)
#会心：从五毒1000品最低会心起始到万花1160品最高会心：(6%, 76%)
#会效：从五毒1000品最低会心起始到万花1160品最高会心：(180%, 300%)
#关键区域选择（大多数正常人的区间范畴）：
#治疗成效[6000, 12000]; 会心几率[20%, 45%]; 会心效果[190%, 280%]
#压缩曲线模型：f(x) = x^alpha + kx
#压缩曲线：
fzip = function(x = 0:100/50, alpha = 5, k = 1, left = -1, minimum, maximum){
	x = x + left
	x = x ^ alpha + k * x
	x = x / (max(x) - min(x)) * (maximum - minimum)
	x = x + minimum - min(x)
	#plot(0:100/100, x)
	return(x)
}
#会心压缩参数：alpha = 5, k = 1.15, left = -0.9, minimum = 6, maximum = 76
#会效压缩参数：alpha = 9, k = 5.5, left = -1.05, minimum = 180, maximum = 300
#治疗成效压缩参数：x = 0:200/100, alpha = 7, k = 4, left = -0.85, minimum = 4500, maximum = 17000
#生成格点：
Zhiliao = round(fzip(x = 0:200/100, alpha = 7, k = 4, left = -0.85, minimum = 4500, maximum = 17000))
Huixin = round(fzip(alpha = 5, k = 1.15, left = -0.9, minimum = 6, maximum = 76), 2) / 100
Huixiao = round(fzip(alpha = 9, k = 5.5, left = -1.05, minimum = 180, maximum = 300), 2) / 100 - 1
#数据切片：以会效为准
Result = list()
for (i in 1:101){
	Result[[i]] = matrix(0, nrow = 101, ncol = 201)
	for (j in 1:101){
		for (k in 1:201){
			Result[[i]][j, k] = foo(Zhiliao[k], Huixin[j], Huixiao[i])
		}
	}
}
#输出切片数据图像
pdf('~/Desktop/Heals.pdf')
for (i in 1:101){filled.contour(Zhiliao, Huixin * 100, t(Result[[i]]), color = terrain.colors, zlim = c(4700, 43000), xlab = 'Spell power', ylab = 'Critical rate', main = paste0('HPSkill with ', Huixiao[i] * 100 + 100, '% of Critical Damage'), key.title = title('HPSkill'))}
dev.off()
#数据切片：以会心为准
Result = list()
for (i in 1:101){
	Result[[i]] = matrix(0, nrow = 101, ncol = 201)
	for (j in 1:101){
		for (k in 1:201){
			Result[[i]][j, k] = foo(Zhiliao[k], Huixin[i], Huixiao[j])
		}
	}
}
#输出切片数据图像
pdf('~/Desktop/Heals.pdf')
for (i in 1:101){filled.contour(Zhiliao, Huixiao * 100 + 100, t(Result[[i]]), color = terrain.colors, zlim = c(4700, 43000), xlab = 'Spell power', ylab = 'Critical damage', main = paste0('HPSkill with ', Huixin[i] * 100, '% of Critical Rate'), key.title = title('HPSkill'))}
dev.off()
#数据切片：以治疗量为准
Result = list()
for (i in 1:201){
	Result[[i]] = matrix(0, nrow = 101, ncol = 101)
	for (j in 1:101){
		for (k in 1:101){
			Result[[i]][j, k] = foo(Zhiliao[i], Huixin[j], Huixiao[k])
		}
	}
}
#输出切片数据图像
pdf('~/Desktop/Heals.pdf')
for (i in 1:201){filled.contour(Huixiao * 100 + 100, Huixin * 100, t(Result[[i]]), color = terrain.colors, zlim = c(4700, 43000), xlab = 'Critical damage', ylab = 'Critical rate', main = paste0('HPSkill with ', Zhiliao[i], ' of Spell Power'), key.title = title('HPSkill'))}
dev.off()
#--------------------------------------------------------------------------------------
#2. 三维等势面图
#核心函数：
equipotential = function(k, x2, x3){return(k / (1 + x2 * x3))}
#注意：x3是会效减1
#值域：
#治疗量：从万花1000品最低治疗量起始到五毒1160品开全爆发最高治疗量：(4500, 17000)
#会心：从五毒1000品最低会心起始到万花1160品最高会心：(6%, 76%)
#会效：从五毒1000品最低会心起始到万花1160品最高会心：(180%, 300%)
#生成格点：
Huixin = seq(0.06, 0.76, length.out = 100)
Huixiao = seq(0.8, 2, length.out = 100)
K = seq(6, 42, 2) * 1000
Result = list()
for (i in 1:19){
	Result[[i]] = matrix(0, 100, 100)
	for (j in 1:100){
		for (k in 1:100){
			if (equipotential(K[i], Huixin[k], Huixiao[j]) > 17000 | equipotential(K[i], Huixin[k], Huixiao[j]) < 4500){Result[[i]][j, k] = NA}else{Result[[i]][j, k] = equipotential(K[i], Huixin[k], Huixiao[j])}
		}
	}
}
#画图
library(rgl)
color = terrain.colors(19)
persp3d(Huixiao * 100 + 100, Huixin * 100, Result[[1]] / 1000, alpha=0.3, front = 'lines', back = 'lines', zlim = c(4500, 17000) / 1000, xlab = 'Critical damage', ylab = 'Critical rate', zlab = 'Spell power', col = color[1])
for (i in 2:19){surface3d(Huixiao * 100 + 100, Huixin * 100, Result[[i]] / 1000, alpha=0.3, front = 'lines', back = 'lines', col = color[i])}
movie3d(spin3d(axis = c(0, 0, 1), rpm = 4), duration = 15, fps = 16, dir = '~/Desktop/dir', convert = FALSE)
#======================================================================================
#附：其他有关参数
#精6系数	Jing = 0.075
#附魔		Fumo = matrix(c(49, 56, 68, 39, 43, 53), 2, 3, TRUE)
#心法加成	Xinfa = matrix(c(1432, 1.75, 0, 0, 1400, 1.70, 0.06, 0.02, 1462, 1.65, 0.1, 0.04, 1369, 1.60, 0.15, 0.05), 4, 4, TRUE)







