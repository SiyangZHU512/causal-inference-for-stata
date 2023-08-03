/*data simluation*/
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
	gen e=rnormal()/5
	gen x=runiform()
	gen index1=1 if ID==10
	replace index1=0 if index1==.
	gen index2=1 if time>=10
	replace index2=0 if index2==.
	gen y=x*10+1.2*z1+0.6*z2+e+10+25*index1*index2
save "SCM_simu_data1.dta", replace  //save the data
use"SCM_simu_data1.dta",clear
sort time ID 
xtset ID time
synth y z1 z2 x y(5) y(15), trunit(10) trperiod(10) figure nested keep(simu_syn,replace)

use "simu_syn.dta", clear 
/*Define the treatment effect as the variable effect, where "_Y_treated" and "_Y_synthetic" denote the treatment area and synthetic control outcome variables, respectively*/
gen effect= _Y_treated - _Y_synthetic

label variable _time "year"
label variable effect "gap in per-capita cigarette sales (in packs)"
*Draw an image of the treatment effect
line effect _time, xline(10,lp(dash)) yline(0,lp(dash))




clear all
use"SCM_simu_data1.dta",clear
sort time ID
xtset ID time
tempname resmat  //Set up a temporary matrix called resmat
        forvalues i = 1/20 { //The loop here is to do a composite control of each of the other objects separately
        qui synth y z1 z2 x y(5) y(15), trunit(`i') trperiod(10) figure nested keep(simu_syn_`i',replace)
        matrix `resmat' = nullmat(`resmat') \ e(RMSPE)  //The temporary matrix is equal to the rmspe value for each object doing processing for synthesis control
        local names `"`names' `"`i'"'"'  //Set the temporary element names to 1 2 3 4 ''' 35
        }
        mat colnames `resmat' = "RMSPE"  //The column names of the temporary matrix are defined as RMSPE
        mat rownames `resmat' = `names' // The rows of the temporary matrix are named names
        matlist `resmat' , row("Treated Unit") //Show the temporary matrix and denote it at the header of the rows as"treated unit"

  
forval i=1/20{
 use simu_syn_`i', clear
 rename _time years
 gen tr_effect_`i' = _Y_treated - _Y_synthetic
 keep years tr_effect_`i'
 drop if missing(years)
 save simu_syn_`i', replace
}


  use simu_syn_1, clear
forval i=2/20{
qui merge 1:1 years using simu_syn_`i', nogenerate
}
  //**drop 
  drop tr_effect_12
 
  /*Delete anything close to 10.*/
 //**Retained Objects Drawing Imag
  local lpl
  forval i=1/9{
  	local lp1 `lp1' line tr_effect_`i' years,lpattern(dash) lcolor(gs8)||
  }
    local lp2
  forval i=11/11{
  	local lp2 `lp2' line tr_effect_`i' years,lpattern(dash) lcolor(gs8)||
  }
    local lp3
  forval i=13/20{
  	local lp3 `lp3' line tr_effect_`i' years,lpattern(dash) lcolor(gs8)||
  }

  twoway `lp1' `lp2' `lp3' || line tr_effect_10 years,lcolor(black) legend(off) xline(10,lpattern(dash)) yline(0,lp(dash)) 

