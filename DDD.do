/*Triple Difference Method*/
/* If the parallel trend test doesn't pass, use DDD (Triple Difference Method) and also synthetic controls to do it. *Example: 65+ year olds get an insurance policy - treat=1 (to see the effect of insurance on health level), others are controls. But obviously there is a problem because the control group is much younger - then a problem arises, parallel trends in the control and experimental groups do not coincide. (Health level status develops differently at different ages). */
use "https://gitee.com/arlionn/data/raw/master/data01/tfp_DDD.dta", clear
*-Notes.
* (1) tt is the cross-multiplier term for pre- and post-pilot and treatment effects.
* (2) zcsy-lnzlb is the control variable.
* (3) SO2 ==1 indicates that the sample are all listed firms emitting SO2
/*DID*/
gen lntfp=log(open)
gen lnaj=log(aj)
gen lnlabor=log(labor)
gen lnzlb=log(zlb)
gen tt=treat*time
reg lntfp tt zcsy lf age owner sczy lnaj lnlabor i.year i.area i.ind if so2==1,cluster(area)
xtset company year
xtreg lntfp tt zcsy lf age owner sczy lnaj lnlabor i.year if so2==1,fe cluster(area)
reghdfe lntfp tt zcsy lf age owner sczy lnaj lnlabor if so2==1,absorb(area year ind company)

egen mean_y=mean(lntfp), by(year treat)
graph twoway (connect mean_y year if treat==1,sort) (connect mean_y year if treat==0,sort lpattern(dash)), ///
xline(2008,lpattern(dash) lcolor(gray)) ///
ytitle("lntfp") xtitle("year") ///
legend(label(1 "treated") label( 2 "control")) ///图例
xlabel(2004 (1) 2015)  graphregion(color(white)) //白底
/*DDD*/
gen ttt=time*treat*so2
gen treats=treat*so2
gen times=time*so2
reg lntfp ttt tt treats times so2,cluster(area)
reg lntfp ttt tt treats times so2 zcsy lf age owner sczy lnaj lnlabor,cluster(area)
reg lntfp ttt tt treats times so2 zcsy lf age owner sczy lnaj lnlabor i.year i.area i.ind,cluster(area)
xtreg lntfp ttt tt treats times so2 zcsy lf age owner sczy lnaj lnlabor i.year,fe cluster(area)
reghdfe lntfp ttt tt treats times so2 zcsy lf age owner sczy lnaj lnlabor i.year, absorb(area ind company) cluster(area)
diff lntfp,t(treat) p(time) ddd(so2)
diff lntfp,t(treat) p(time) ddd(so2) cov(zcsy lf age owner sczy lnaj lnlabor)
