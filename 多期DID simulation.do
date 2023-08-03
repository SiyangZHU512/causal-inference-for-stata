/*data simulation*/
clear all
	global dir d:/RDDStata
	capture mkdir $dir
	cd $dir
	
	set obs 400
	set seed 123
	
/*set panel*/	
 forvalues j=1(1)20{
     display"`j'"
     gen X`j'=.
	 forvalues i=`j'(20)400{
 	display"`i'"
	replace X`j'=`j' in `i'
 }
    replace X`j'=0 if X`j'==.
 }
 
gen ID=X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20

drop X*
gen index=.
forvalues i=1(1)400{
	replace index=`i' in `i'
}

gen time=.
forvalues i=1(1)20{
	replace time=`i' if inrange(index,(`i'-1)*20+1,`i'*20)
}
/*set ID=10 is the subject that get treated*/
	gen z1=rnormal()*4+10
	gen z2=rnormal()*5+8
	gen z3=rnormal()*9+2
	gen e=rnormal()
	gen treat=1 if inrange(ID,2,9)
	replace treat=0 if treat==.
	gen index2=1 if time>=10
	replace index2=0 if index2==.
	
	gen y=0.1*z1*z2+1.2*z1+0.6*z2+e+z3*2+10+10*treat*index2
save "DID_simu_data1.dta", replace  //save the data
/*short panel*/
xtset ID time
xtdes
xtline y
reg y z1 z2 z3 treat ,vce(cluster ID)
estimate sto cluster_OLS
eststo col1
xtreg y z1 z2 z3 treat,fe 
estimate sto fe
eststo col2
reg y z1 z2 z3 treat i.ID,vce(cluster ID)
eststo LSDV
tab time,gen(time)
xtreg y z1 z2 z3 treat i.time,r
eststo time_fixed
xtreg y z1 z2 z3 treat i.time,fe r
eststo two_way
xtreg y z1 z2 z3 treat,re
eststo RE
xttest0
hausman fe RE,constant sigmamore
/*DID*/
gen DID=treat*index2
xtreg y DID z1 z2 z3 treat i.time,fe r

panelview y DID, i(ID) t(time) type(treat) ylabdist(20) /// 
            bytiming xtitle("time") ytitle("firm")
			
egen mean_y=mean(y),by(time treat)
graph twoway (connect mean_y time if treat==1,sort) (connect mean_y time if treat==0,sort lpattern(dash)), ///
xline(10,lpattern(dash) lcolor(gray)) ///
ytitle("Y") xtitle("year") ///
legend(label(1 "treated") label( 2 "control")) ///
xlabel(1 (2) 20)  graphregion(color(white)) //
graph export "Ex ante test for parallel trends.png",replace


gen year=time
gen pd=year-10
forvalues i=10(-1)1{
	gen pre_`i'=(pd==-`i'&treat==1)
}
gen current=(pd==0&treat==1)
forvalues j=1(1)10{
	gen las_`j'=(pd==`j'&treat==1)
}
xtset ID year
drop pre_9
xtreg y z1 z2 z3 year treat pre_* current las_* i.year,fe
est sto reg
coefplot reg,keep(pre_* current  las_*) vertical recast(connect) yline(0) xline(9,lp(dash)) ytitle("regression coefficient"，size(small)) xtitle("When the policy was introduced"，size(small)) xlabel(, alternate labsize(small)) 
/*Look to see if the full 95% CI contains 0 before the policy happens?*/
reghdfe y z1 z2 z3 year treat pre_* current las_*,absorb(i.year ID)
est sto regfe
coefplot regfe,keep(pre_* current  las_*) vertical recast(connect) yline(0) xline(9,lp(dash)) coeflabels(pre_3=2010 pre_2=2011 pre_1=2013 current=2013 las_1=2014 las_2=2015) ytitle("回归系数"，size(small)) xtitle("政策实行时间"，size(small)) xlabel(, alternate labsize(small)) 

/*fictitious control group*/

reghdfe y DID ,absorb(year ID)
cap erase "simulation.dta"
permute DID beta=_b[DID] se=_se[DID] df=e(df_r),reps(500) rseed(777) saving("simulation.dta"):reghdfe y DID ,absorb(year ID)
/*Not significant == good results*/
use"simulation.dta",clear
gen t_value=beta/se
gen p_value=2*ttail(df,abs(beta/se))/*ttail 2 tails test*/
dpplot beta,xline(0,lp(dash))xtitle("Estimator")ytitle("Density") 
/*Hopefully the overlap is high*/
dpplot t_value,xtitle("T_value") ytitle("Density")
/*VS 0*/
dpplot p_value,xtitle("P_value")ytitle("Density")
/*Look at the mean value of P vs 0.05*/
twoway(scatter p_value beta)(kdensity beta,yaxis(2)) 
