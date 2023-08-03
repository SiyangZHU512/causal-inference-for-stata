/*psm*/
/*data simulation*/
clear all
	global dir d:/RDDStata
	capture mkdir $dir
	cd $dir
	
	set obs 4000
	set seed 123
	

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
	gen g=edu1-3.345*black-2.564*fe-1.54*mari-0.654*mari*fe+e*3
    gen t=1 if g>7
	replace t=0 if t==.
	gen income4=income3*(1+x1*0.1)+22*t+edu1*2.34-3.2*black-4.3*fe-3.23*mari+e*4.53
	
	save "PSM_simu_data1.dta",replace  //save the data
// first using ordinary regression to test
reg income4 t,r
reg income4 t edu1 black fe mari income1 income2 income3,r
set seed 10101
gen randorder=runiform()
sort randorder
psmatch2 t edu1 fe mari black income1 income2 income3 ,outcome(income4) n(1)ate ties logit common
bootstrap r(att) r(atu) r(ate),reps(500):psmatch2 t edu1 fe mari black income1 income2 income3 ,outcome(income4) n(1)ate ties logit common
qui psmatch2 t edu1 fe mari black income1 income2 income3 ,outcome(income4) n(1)ate ties logit common noreplacement
pstest edu1 black fe mari income1 income2 income3,both graph
psgraph
/*if use income as a variable in matching the result of matching is not good enough, so I delete income and remacth*/
qui psmatch2 t edu1 fe mari black income1 ,outcome(income4) n(1)ate ties logit common 
pstest edu1 black fe mari income1 ,both graph
psgraph
/*other matching method*/
/*4 Near-neighbour matching*/
psmatch2 t edu1 fe mari black income1 income2 income3 ,outcome(income4) n(4) ate ties logit common quietly 
pstest edu1 black fe mari income1 income2 income3,both graph
psgraph
/*quietly  Estimated results ignoring propensity to match scores*/
/*Near-neighbour matching within caliper*/
sum _pscore
dis 0.25*r(sd)
psmatch2 t edu1 fe mari black income1 income2 income3 ,outcome(income4) n(4) cal(0.09) ate ties logit common quietly
pstest edu1 black fe mari income1 income2 income3,both graph
psgraph
/*Value inside cal is equal to value coming out of dis*/
/*Caliper match*/
psmatch2 t edu1 fe mari black income1 income2 income3 ,outcome(income4) radius cal(0.09) ate ties logit common quietly
pstest edu1 black fe mari income1 income2 income3,both graph
psgraph
/*Kernel matching*/
psmatch2 t edu1 fe mari black income1 income2 income3 ,outcome(income4) kernel cal(0.09) ate ties logit common quietly
pstest edu1 black fe mari income1 income2 income3,both graph
psgraph
/*Local linear regression for matching*/
psmatch2 t edu1 fe mari black income1 income2 income3 ,outcome(income4) llr ate ties logit common quietly
pstest edu1 black fe mari income1 income2 income3,both graph
psgraph

