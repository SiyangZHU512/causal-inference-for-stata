/*RDD*/
/*首先RDD的情况不能采用PSM进行处理，原因很简单，在RDD当中处理与否直接由进行分组的xi这一连续性变量进行决定。一致xi的情况下肯定知道处理与否--那么处理与否跟其他控制变量无关*/
/*精确断点回归（在那个点之后获得处理的概率直接从0到1） 和模糊断点回归（非参数回归）--需要选择核函数和最优带宽*/
/*精确断点：如果个体知道断点的位置（在实验之前），并且自身可以根据自己的努力争取到处理，那么出现内生问题。*/
/*稳健性检验：1.分别汇报三角核和矩形核的回归结果 2.分别汇报不同带宽结果 3.分别汇报含协变量和不含协变量情况 4.进行模型设定检验*/
/*rd y D x,z0(real) strinep mbw(numlist) graph bdep oxline kernel(rectangle) cov(varlist) x(varlist)*/
/*y结果变量 D 处理变量 strineq严格根据不等式处理变量。x大于z0则为1否则为0. mbw(50 100 200) 0.5倍 1倍 2倍带宽。graph 根据所选每一带宽绘制局部线性回归图像。 bdep 画图考察断点回归对于带宽的依赖性。 oxline 默认最优带宽上画一条线以便识别 kernel（）用什么核函数估计。 x（varlist）检验这些协变量是否在断点有跳跃。*/
/*生成随机数据*/
clear all
	global dir d:/RDDStata
	capture mkdir $dir
	cd $dir
	
	set obs 4000
	set seed 123
	
	gen x = runiform()     //分配变量
	gen xc = x-0.5  //分配变量去中心化

	gen e = rnormal()/5    // noise
	gen z1 = rnormal()*0.5  //控制变量
	gen z2=1+3*invnormal(uniform())+sin(x*5)/3+e  //另一个控制变量
	
	gen T=0               
	replace T=1 if x>0.5   //treatment 
	
	gen g0 = 0 + 3*log(x+1) + sin(x*6)/3
	gen g1 = T + 3*log(x+1) + sin(x*6)/3
	gen y1 = g1 + 0.5*z1 +0.3*z2+ e   // outcome vaiable，with cutoff effect
	gen y0 = g0 + 0.5*z1 +0.3*z2+ e  // outcome variable, without cutoff effect

	label var y1 "Outcome variable (y)"
    label var y0 "Outcome variable (y)"
	label var x  "Assignment variable (x)"
	label var xc "Centered Assignment variable (x-c)"
	label var T  "T=1 for x>0.5, T=0 otherwise"
	
	drop e g* 
	
	save "RDD_simu_data0.dta", replace  //保存一份数据以备后用
	
	use "RDD_simu_data0.dta", clear

twoway (scatter y0 xc, msymbol(+) msize(*0.4) mcolor(black*0.3))  ,   title("无断点")
graph save y0,  replace
twoway (scatter y1 xc, msymbol(+) msize(*0.4) mcolor(black*0.3))  ,   title("有断点")
graph save y1, replace

graph  combine y0.gph y1.gph, row(1)

use "RDD_simu_data0.dta", clear

twoway (scatter y1 xc, msymbol(+) msize(*0.4) mcolor(black*0.3)),   title("散点图")
graph save scatter.gph,  replace
rdplot y1 xc, c(0) p(1) graph_options(title(线性拟合)) // 线性拟合图
graph save rd1,  replace
rdplot y1 xc, c(0) p(2) graph_options(title(二次型拟合))//二次型拟合图
graph save rd2,  replace
rdplot y1 xc, c(0) p(3) graph_options(title(三次型拟合))//二次型拟合图
graph save rd3,  replace
graph  combine scatter.gph  rd1.gph rd2.gph rd3.gph
/*another way for RDD*/
binscatter y1 xc if inrange(xc,-0.5,0.5), rd(0) line(lfit) 
graph save rdd1
binscatter y1 xc if inrange(xc,-0.5,0.5), rd(0) line(qfit) 
graph save rdd2
graph combine rdd1.gph rdd2.gph 

/*政策效应估计：使用局部线性回归法，是假定在断点邻域中的处理效应为线性，通过在左右两侧邻域分别进行线性回归并比较两侧回归系数差异来进行识别。局部回归检验的一个重要环节在于断点邻域大小的选择，也即 RDD 分析里带宽选择 （bandwidth selection） 的权衡问题。这是因为带宽越大，则意味着有越多的样本被纳入检验中，参数估计更准确，但也意味着样本随机性要求越难满足，内生性问题可能更严重。

本文中断点xc的邻域为 ([xc-h1,xc+h2]) ， h1 和 h2 分别为左右两侧带宽。h1和h2可以相等，也可以不等。在断点分析中，可进行局部线性断点回归的命令有 rd、rdrobust 和 rdcv 三个命令。这三个都会自动给出该命令下最优带宽。本部分相应代码如下。*/
use "RDD_simu_data0.dta", clear
set matsize 2000
set seed 135
sample 10          //随机抽取10%的观察值
rdplot y1 xc, c(0) //检测一下，看看数据特征是否发生明显变化
rd   y1 xc, c(0)
rdrobust y1 xc, c(0) p(1) 
rdcv y1 xc, thr(0) deg(1)
/*note 如果直接用 全部数据 rdcv的运算时间需要很久*/
/*第几阶多项式拟合效果比较好呢？*/
    use "RDD_simu_data0.dta", clear
    set matsize 2000
    set seed 135
    sample 10          //rdcv回归较为耗时，仅随机抽取10%的观察值来演示。
    
rdcv y1 xc, thr(0) deg(1)		
myic   
est store m1 
rdcv y1 xc, thr(0) deg(2)
myic   
est store m2
rdcv y1 xc, thr(0) deg(3)  	
myic   
est store m3
/*局部平滑性的检验:对于局部平滑假设，是指除了结果变量，所有所有其它变量在断点附近都不应该存在处理效应，也即没有出现跳跃现象。在检验方法上，我们可以利用图形直接观察，也可以将每一个协变量作为安慰剂结果变量 (placebo outcomes) ，使用断点回归方法进行检验。

*/
 use "RDD_simu_data0.dta", clear
twoway (scatter z1 xc, msymbol(+) msize(*0.4) mcolor(black*0.3)),   title("散点图")
graph save scatter.gph,  replace
rdplot z1 xc, c(0) p(1) graph_options(title(z1的线性拟合)) // 线性拟合图
graph save rd1,  replace
rdplot z1 xc, c(0) p(2) graph_options(title(z1的二次型拟合))//二次型拟合图
graph save rd2,  replace
rdplot z1 xc, c(0) p(3) graph_options(title(z1的三次型拟合))//二次型拟合图
graph save rd3,  replace
graph  combine scatter.gph  rd1.gph rd2.gph rd3.gph

twoway (scatter z2 xc, msymbol(+) msize(*0.4) mcolor(black*0.3)),   title("散点图")
graph save scatter.gph,  replace
rdplot z2 xc, c(0) p(1) graph_options(title(z2线性拟合)) // 线性拟合图
graph save rd1,  replace
rdplot z2 xc, c(0) p(2) graph_options(title(z2二次型拟合))//二次型拟合图
graph save rd2,  replace
rdplot z2 xc, c(0) p(3) graph_options(title(z2三次型拟合))//二次型拟合图
graph save rd3,  replace
graph  combine scatter.gph  rd1.gph rd2.gph rd3.gph
rdrobust z1 xc
rdrobust z2 xc
/*4.2 驱动变量不受人为控制的检验
检验的思路在于，如果不存在人为操控，那么在断点附近样本的数量应该相近，才符合随机性。我们可以用 rddensity 命令来检验断点两侧样本数量是否相近。*/
use "RDD_simu_data0.dta", clear
		
rdrobust y1 xc
local h = e(h_l)   //获取最优带宽
rddensity xc, p(1) hl(`h') hr(`h')
/*稳健性检验*/
/*1.断点的安慰剂检验
稳健性检验的一个自然而然的思路在于选择一个不同于断点的值作为安慰剂断点 (placcebo cutoff points) 。如果断点回归结果变得不显著，则表明断点的真实性。相应代码分别取真实断点两侧 20%、40%、60% 和 80% 样本分位数处作为断点。作为对比，我们也放入了真实断点在图形里。如下图所示，五 个placebo cutoffs 的回归系数都不显著异于0，从而在这些点处不存在处理效应。*/
use "RDD_simu_data0.dta", clear
 sum xc
 local xcmax=r(max)
 local xcmin= r(min)


rd y1 xc ,c(-0.2496156) mbw(50)
est sto jr1

rd y1 xc,c(-0.1664104) mbw(50)
estimates store jr2

rd y1 xc,c(-0.1248078) mbw(50)
estimates store jr3

rd y1 xc,c(-0.09984624) mbw(50)
estimates store jr4

rd y1 xc,c(0.2496156) mbw(50)
estimates store jl1

rd y1 xc,c(0.1664104) mbw(50)
estimates store jl2

rd y1 xc,c(0.1248078) mbw(50)
estimates store jl3

rd y1 xc,c(0.09984624) mbw(50)
estimates store jl4


//加上真实断点的回归结果，作为benchmark结果
rd y1 xc,c(0) mbw(50) 
estimates store jbaseline 

//输出图形
local vlist "jl1 jl2 jl3 jl4 jbaseline jr4 jr3 jr2 jr1  "
coefplot `vlist'  ,  yline(0) drop(_cons) vertical


/*带宽稳健性检验*/
rd y1 xc,c(0) mbw(50) 
estimates store h1
rd y1 xc,c(0) mbw(75) 
estimates store h2
rd y1 xc,c(0) mbw(25) 
estimates store h3
rd y1 xc,c(0) mbw(100) 
estimates store h4
rd y1 xc,c(0) mbw(125) 
estimates store h5
rd y1 xc,c(0) mbw(150) 
estimates store h6
local vlist "h1 h2 h3 h4 h5 h6  "
coefplot `vlist'  , yline(0) drop(_cons) vertical