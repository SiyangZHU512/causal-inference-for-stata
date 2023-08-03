/*PSM+DID*/ 
/*psm*/
clear all
	global dir d:/RDDStata
	capture mkdir $dir
	cd $dir
	
	set obs 200
	set seed 123
		/*set panel*/	
gen ID=.
forvalue j=1(1)100{
    forvalues i=`j'(2)200{
	replace ID=`j' in `i'
}
}
gen index=.
forvalues i=1(1)4000{
	replace index=`i' in `i'
}

gen year=.
forvalues i=1(1)10{
	replace year=`i' if inrange(index,(`i'-1)*400+1,`i'*400)
}
	gen e=rnormal()
	gen x=rnormal()*10
	gen x0=x+e*2
    gen income1=rnormal()*10+121
	gen x1=rnormal()
	gen income2=income1*(1+x1*0.1)
	gen income3=income2*(1+x1*0.1)
    gen w=rnormal()*3
	gen w0=e+w
	gen h=rnormal()*2
	gen h0=e+rnormal()
	gen mari=1 if w0>2.2
	replace mari=0 if mari==.
	gen black=1 if h0>1.1
	replace black=0 if black==.
	gen edu=rnormal()*5
	replace edu=-edu if edu<=0
	gen f=rnormal()*2
	gen f0=f+e*1.2
	gen fe=1 if f0<0.7
	replace fe=0 if fe==.
    gen edu1=round(edu)+6
	gen how=rnormal()*2+10
	gen zs=2*how+2*how*how+e*2.3564
	gen g=edu1-3.345*black-2.564*fe-1.54*mari-0.654*mari*fe+e*3+2*how+zs*-1.23
	gen time=1 if year>=5
	replace time=0 if time==.
    gen t=1 if g>-245.23
	replace t=0 if t==.
	
	gen income4=income3*(1+x1*0.1)+22*t*year+edu1*2.34-3.2*black-4.3*fe-3.23*mari+e*8.53+4.234*year
	
/*DID*/
gen did=t*time
reg income4 did,r
reg income4 did edu1 black mari ,r
xtset ID year
xtdes
panelview income4 did,i(ID) t(year) type(treat) ylabdist(20) /// 
            bytiming xtitle("time") ytitle("firm") displayall 
			
			
egen mean_y=mean(income4),by(year t)
graph twoway (connect mean_y year if t==1,sort) (connect mean_y year if t==0,sort lpattern(dash)), ///
xline(5,lpattern(dash) lcolor(gray)) ///
ytitle("Y") xtitle("year") ///
legend(label(1 "treated") label( 2 "control")) ///图例
xlabel(1 (2) 10)  graphregion(color(white)) //白底
graph export "平行趋势事前检验.png",replace


/*PSM+DID*/			
	diff income4,t(t)p(time) kernel id(ID) logit cov(edu1 black mari income1 income2 income3)support
	diff income4,t(t)p(time) kernel id(ID) logit cov(edu1 black mari income1 income2 income3)support test