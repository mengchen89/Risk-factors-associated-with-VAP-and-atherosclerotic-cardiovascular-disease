libname vpa "D:\vpa";

/*资料整理*/
data vpa.data;
set vpa.rawdata;
label VAR1="条码号"	VAR2="姓名"	VAR3="病人性别"	VAR4="病人年龄"VAR5="年龄类型"	
VAR6="项目名称1" VAR7="TG" VAR8="参考值1" VAR9="单位1"	VAR10="试验方法1"	
VAR11="项目名称2" VAR12="RLP" VAR13="参考值2"	 VAR14="单位	2" VAR15="试验方法2"	
VAR16="项目名称3"	VAR17="中间密度脂蛋白"	VAR18="参考值3"	VAR19="单位3"	VAR20="试验方法3"	
VAR21="项目名称4"	VAR22="高密度脂蛋白2"	VAR23="参考值4"	VAR24="单位4" VAR25="试验方法4"	
VAR26="项目名称5"	VAR27="LDL-C"	VAR28="参考值5"	VAR29="单位5"	VAR30="试验方法5"
VAR31="项目名称6"	VAR32="极低密度脂蛋白3"	VAR33="参考值6"	VAR34="单位6"	VAR35="试验方法6"	
VAR36="项目名称7"	VAR37="HDL-C"	VAR38="参考值7"	VAR39="单位7"	VAR40="试验方法7"
VAR41="项目名称8"	VAR42="TC"	VAR43="参考值8"	VAR44="单位8"	VAR45="试验方法8"
VAR46="项目名称9"	VAR47="LDL密度模式"	VAR48="参考值9"	VAR49="单位9"	VAR50="试验方法9"
VAR51="项目名称10"	VAR52="LDL-P"	VAR53="参考值10"	VAR54="单位10"	VAR55="试验方法10"
VAR56="项目名称11"	VAR57="高密度脂蛋白3"	VAR58="参考值11"	VAR59="单位11"	VAR60="试验方法11"
VAR61="项目名称12"	VAR62="低密度脂蛋白"	VAR63="参考值12"	VAR64="单位12"	VAR65="试验方法12"
VAR66="项目名称13"	VAR67="极低密度脂蛋白"	VAR68="参考值13"	VAR69="单位13"	VAR70="试验方法13"
VAR71="项目名称14"	VAR72="非高密度脂蛋白胆固醇"	VAR73="参考值14"	VAR74="单位14"	VAR75="试验方法14"
VAR76="项目名称15"	VAR77="脂蛋白 a(Lp(a))"	VAR78="参考值15"	VAR79="单位15"	VAR80="试验方法15"
VAR81="结果分析"	VAR82="风险评估"	VAR83="防治建议"	VAR84="备注";
RUN;

PROC FORMAT;
VALUE FSEX 1="男" 2="女";
run;

data vpa.nmiss;
set vpa.data;
if var3^=1 and var3^=2 then delete;
if var1=999999969999 then delete;
run;

data vpa.nmiss;
set vpa.nmiss;
format var3 FSEX.;
run;

/*资料视觉化*/
proc means data=vpa.nmiss mean median std min max;
var VAR7 VAR12 VAR17 VAR22 VAR27 VAR32 VAR37 VAR42 VAR52 VAR57 VAR62 VAR67 VAR72 VAR77;
run;

/*资料处理*/
data vpa.puredata;
set vpa.nmiss;
keep VAR3 VAR4 VAR7 VAR12 VAR17 VAR22 VAR27 VAR32 VAR37 VAR42 VAR47 VAR52 VAR57 VAR62 VAR67 VAR72 VAR77 VAR82;
RUN;
  /*风险评估为null*/
data vpa.TESTDATA;
set vpa.puredata;
if VAR82="NULL" or index(var82,"由于缺少")>0;
RUN;

data vpa.pudata;
set vpa.puredata;
if var82="NULL" or index(var82,"由于缺少")>0 then delete;
if index(var82,'低危')>0 then risk='A低危';
else if index(var82,'中危')>0 then risk="B中危";
else if index(var82,'极高危')>0 then risk='D极高危';
else risk="C高危";
run;

data aa;
set vpa.pudata;
keep risk;
proc print;
run;

/*相关*/
proc corr data=vpa.pudata;
var VAR7 VAR12 VAR17 VAR22 VAR27 VAR32 VAR37 VAR42 VAR52 VAR57 VAR62 VAR67 VAR72 VAR77;
run;

/*模式筛选*/
  /*全部数值*/
proc logistic data=vpa.pudata descending plots=(effect);
class VAR47(REF="A") VAR3;
model risk=VAR3 VAR4 VAR7 VAR12 VAR17 VAR22 VAR27 VAR32 VAR37 VAR42 VAR47 VAR52 VAR57 VAR62 VAR67 VAR72 VAR77;
RUN;

proc logistic data=vpa.pudata descending plots=(effect);
class VAR47(REF="A") VAR3;
model risk=VAR3 VAR4 VAR7 VAR12 VAR17 VAR22 VAR27 VAR32 VAR37 VAR42 VAR47 VAR52 VAR57 VAR62 VAR67 VAR72 VAR77 / SELECTION=FORWARD;
RUN;

  /*全部类别*/
PROC MEANS DATA=VPA.PUDATA MEDIAN MEAN MIN MAX;
VAR VAR4;
RUN;

data jj;
set vpa.pudata;
if var52<1000 then GRLDLP='低';
else if var52<1400 then GRLDLP='中';
else if var52<2200 then GRLDLP='高';
else GRLDLP='极高';
run;


data vpa.catedata;
set jj;
if var4<50 THEN AGEGR='五十岁以下';
else AGEGR='五十岁及以上';
if var7<1.69 then TGGR='正常';
else TGGR='不正常';
if var12<0.78 then TGRLP='正常';
else TGRLP='不正常';
if var17<0.52 then TGmidzdb='正常';
else TGmidzdb='不正常';
if var22>0.26 then TGhzdb='正常';
else TGhzdb='不正常';
if var27<3.36 then GRLDLC='正常';
else GRLDLC='不正常';
if var32<0.26 then exlow3='正常';
else exlow3='不正常';
if var37>1.03 then GRHDLC='正常';
else GRHDLC='不正常';
if var42<5.17 then GRTC='正常';
else GRTC='不正常';
if var47='A' then LDLmode='正常';
else LDLmode='不正常';
if var57>0.78 then high3='正常';
else high3='不正常';
if var62<2.59 then low='正常';
else low='不正常';
if var67<0.78 then exlow='正常';
else exlow='不正常';
if var72<4.14 then nohigh='正常';
else nohigh='不正常';
if var77<300 then GRALPA='正常';
else GRALPA='不正常';
/*keep var47 agegr tggr tgrlp tgmidzdb tghzdb grldlc exlow3 grhdlc grtc ldlmode grldlp high3 low exlow nohigh gralpa risk;*/
label AGEGR='年龄组' TGGR='甘油三酯' TGRLP='脂蛋白残粒' TGmidzdb='中间密度脂蛋白' TGhzdb='高密度脂蛋白2' 
GRLDLC='低密度脂蛋白胆固醇(LDL-C)' exlow3='极低密度脂蛋白3' GRHDLC='高密度脂蛋白胆固醇(HDL-C)' GRTC='总胆固醇(TC)' 
LDLmode='LDL密度模式' GRLDLP='低密度脂蛋白颗粒(LDL-P)' high3='高密度脂蛋白3' low='低密度脂蛋白' exlow='极低密度脂蛋白'
nohigh='非高密度脂蛋白胆固醇' GRALPA='脂蛋白 a(Lp(a))';
run;

proc gchart data=vpa.pudata;
vbar var4;
run;

data mb;
set vpa.catedata;
if GRLDLC='正常';
run;
proc freq data=mb;
tables grldlp*risk/chisq nopercent nocol;
run;
proc logistic data=mb plots=effect; 
model risk(reference='A低危')=var52 / link=glogit; 
output out=aa pred=fitted_p;
run;

/*类别型相关*/
/*甘油三脂&脂蛋白残粒*/
proc freq data=vpa.catedata;
tables TGGR*TGRLP/chisq nopercent nocol;
run;/*强相关*/

/*甘油三酯&极低密度脂蛋白3*/
proc freq data=vpa.catedata;
tables TGGR*exlow3/chisq nopercent nocol;
run;/*强相关 不存在极低3正常，甘油三酯不正常者*/

/*甘油三酯&极低密度脂蛋白*/
proc freq data=vpa.catedata;
tables TGGR*exlow/chisq nopercent nocol fisher;
run;/*强相关*/

/*脂蛋白残粒&中间密度脂蛋白*/
proc freq data=vpa.catedata;
tables TGRLP*TGmidzdb/chisq nopercent nocol fisher;
run;/*强相关 不存在中间密度脂蛋白不正常，脂蛋白残粒正常者*/

/*脂蛋白残粒&极低密度脂蛋白3*/
proc freq data=vpa.catedata;
tables TGRLP*exlow3/chisq nopercent nocol fisher;
run;/*强相关 不存在脂蛋白正常，极低3不正常者*/

/*脂蛋白残粒&极低密度脂蛋白*/
proc freq data=vpa.catedata;
tables TGRLP*exlow/chisq nopercent nocol;
run;/*强相关*/

/*脂蛋白残粒&非高密度脂蛋白胆固醇*/
proc freq data=vpa.catedata;
tables TGRLP*nohigh/chisq nopercent nocol;
run;/*强相关*/

/*中间密度脂蛋白&极低密度脂蛋白3*/
proc freq data=vpa.catedata;
tables TGmidzdb*exlow3/chisq nopercent nocol fisher;
run;/*强相关 不存在极低3正常，中间密度不正常者*/

/*中间密度脂蛋白&非高密度脂蛋白胆固醇*/
proc freq data=vpa.catedata;
tables TGmidzdb*nohigh/chisq nopercent nocol;
run;/*强相关*/

/*高密度脂蛋白2&高密度脂蛋白胆固醇(HDL-C)*/
proc freq data=vpa.catedata;
tables TGhzdb*GRHDLC/chisq nopercent nocol fisher;
run;/*强相关*/

/*高密度脂蛋白2&高密度脂蛋白3*/
proc freq data=vpa.catedata;
tables TGhzdb*high3/chisq nopercent nocol;
run;/*强相关*/

/*低密度脂蛋白胆固醇(LDL-C)&总胆固醇(TC)*/
proc freq data=vpa.catedata;
tables GRLDLC*GRTC/chisq nopercent nocol fisher;
run;/*强相关*/

/*低密度脂蛋白胆固醇(LDL-C)&低密度脂蛋白颗粒(LDL-P)*/
proc freq data=vpa.catedata;
tables GRLDLC*GRLDLP/chisq nopercent nocol fisher;
run;/*强相关 不存在低密度脂蛋白颗粒低,低密度脂蛋白胆固醇不正常者*/

/*低密度脂蛋白胆固醇(LDL-C)&低密度脂蛋白*/
proc freq data=vpa.catedata;
tables GRLDLC*low/chisq nopercent nocol fisher;
run;/*强相关*/

/*低密度脂蛋白胆固醇(LDL-C)&非高密度脂蛋白胆固醇*/
proc freq data=vpa.catedata;
tables GRLDLC*nohigh/chisq nopercent nocol fisher;
run;/*强相关*/

/*极低密度脂蛋白3&极低密度脂蛋白*/
proc freq data=vpa.catedata;
tables exlow3*exlow/chisq nopercent nocol fisher;
run;/*强相关 不存在极低3正常，极低不正常者*/

/*高密度脂蛋白胆固醇(HDL-C)&高密度脂蛋白3*/
proc freq data=vpa.catedata;
tables GRHDLC*high3/chisq nopercent nocol fisher;
run;/*强相关*/

/*总胆固醇(TC)&低密度脂蛋白颗粒(LDL-P)*/
proc freq data=vpa.catedata;
tables grtc*grldlp/chisq nopercent nocol fisher;
run;/*强相关 不存在总胆固醇不正常，LDL-P低者；不存在总胆固醇正常，LDL-P极低者*/

/*总胆固醇(TC)&低密度脂蛋白*/
proc freq data=vpa.catedata;
tables grtc*low/chisq nopercent nocol fisher;
run;/*强相关*/

/*总胆固醇(TC)&非高密度脂蛋白胆固醇*/
proc freq data=vpa.catedata;
tables grtc*nohigh/chisq nopercent nocol fisher;
run;/*强相关*/

/*低密度脂蛋白颗粒(LDL-P)&低密度脂蛋白*/
proc freq data=vpa.catedata;
tables grldlp*low/chisq nopercent nocol fisher;
run;/*强相关 不存在低密度脂蛋白不正常，ldlp低者*/

/*低密度脂蛋白颗粒(LDL-P)&非高密度脂蛋白胆固醇*/
proc freq data=vpa.catedata;
tables grldlp*nohigh/chisq nopercent nocol fisher;
run;/*强相关 不存在非高密度脂蛋白胆固醇正常，ldlp极高和中者*/

/*低密度脂蛋白&非高密度脂蛋白胆固醇*/
proc freq data=vpa.catedata;
tables low*nohigh/chisq nopercent nocol fisher;
run;/*强相关*/





/*Cumulative Logistic Model (RLP) ;refuse PO assump;*/
proc logistic data=vpa.catedata;
class tgrlp(ref='正常') /param=ref;
model risk(order=data)=tgrlp/ link=clogit
aggregate scale=none;
output out = prob PREDPROBS=C I;
run;
  /*using baseline logit model with cate*/
proc logistic data=vpa.catedata order=data;
class risk tgrlp;
model risk (reference="A低危") =tgrlp
/link=glogit scale = none aggregate;
output out = prob PREDPROBS=I;
run;
proc freq data = prob;
format IP_A____ IP_B____ IP_C____ IP_D____ f6.4;
tables tgrlp*IP_A____*IP_B____*IP_C____*IP_D____
/list nocum nopercent;
run;

 /*using baseline logit model with numeric!*/
proc logistic data=vpa.catedata plots=effect; 
model risk(reference='A低危')=var12 / link=glogit; 
output out=aa pred=fitted_p;
run;


/*Cumulative Logistic Model (TGmidzdb) ;refuse PO assump*/
proc logistic data=vpa.catedata;
class TGmidzdb(ref='正常') /param=ref;
model risk(order=data)=TGmidzdb/ link=clogit
aggregate scale=none;
output out = prob PREDPROBS=C I;
run;
/*using baseline logit model with category*/
proc logistic data=vpa.catedata order=data;
class risk TGmidzdb;
model risk (reference="A低危") =TGmidzdb
/link=glogit scale = none aggregate;
output out = prob PREDPROBS=I;
run;
/*using baseline logit model with numeric!*/
proc logistic data=vpa.catedata plots=effect; 
model risk(reference='A低危')=var17 / link=glogit; 
output out=aa pred=fitted_p;
run;


/*Cumulative Logistic Model (GRLDLC) ;refuse PO assump*/
proc logistic data=vpa.catedata;
class GRLDLC(ref='正常') /param=ref;
model risk(order=internal)=GRLDLC/ link=clogit
aggregate scale=none;
output out = prob PREDPROBS=C I;
run;
/*using baseline logit model with cate*/
proc logistic data=vpa.catedata order=data;
class risk GRLDLC;
model risk (reference="A低危") =GRLDLC
/link=glogit scale = none aggregate;
output out = prob PREDPROBS=I;
run;
/*using baseline logit model with numeric*/
proc logistic data=vpa.catedata plots=effect; 
model risk(reference='A低危')=var27 / link=glogit; 
output out=aa pred=fitted_p;
run;
data aa;
set vpa.catedata;
if GRLDLC="不正";
keep GRLDLC risk;
proc print;
run;


/*Cumulative Logistic Model (GRHDLC)*/
proc logistic data=vpa.catedata;
class GRHDLC(ref='正常') /param=ref;
model risk(order=internal)=GRHDLC/ link=clogit
aggregate scale=none;
output out = prob PREDPROBS=C I;
run;
/*baseline logit with numeric*/
proc logistic data=vpa.catedata plots=effect; 
model risk(reference='A低危')=var37 / link=glogit; 
output out=aa pred=fitted_p;
run;



/*Cumulative Logistic Model (GRTC);refuse PO assump*/
proc logistic data=vpa.catedata;
class GRTC(ref='正常') /param=ref;
model risk(order=internal)=GRTC/ link=clogit
aggregate scale=none;
output out = prob PREDPROBS=C I;
run;
/*using baseline logit model with cate*/
proc logistic data=vpa.catedata order=data;
class risk GRTC;
model risk (reference="A低危") =GRTC
/link=glogit scale = none aggregate;
output out = prob PREDPROBS=I;
run;
/*using baseline logit model with numeric*/
proc logistic data=vpa.catedata plots=effect; 
model risk(reference='A低危')=var42 / link=glogit; 
output out=aa pred=fitted_p;
run;


/*Cumulative Logistic Model (LDLmode);slightly rufuse PO assump*/
proc logistic data=vpa.catedata;
class LDLmode(ref='正常') /param=ref;
model risk(order=internal)=LDLmode/ link=clogit
aggregate scale=none;
output out = prob PREDPROBS=C I;
run;
  /*using primitive cate;**optimal*/
proc logistic data=vpa.catedata;
class var47(ref='A') /param=ref;
model risk(order=internal)=var47/ link=clogit
aggregate scale=none;
output out = prob PREDPROBS=C I;
run;
proc freq data = prob;
format IP_A____ IP_B____ IP_C____ IP_D____ f6.4;
tables VAR47*IP_A____*IP_B____*IP_C____*IP_D____
/list nocum nopercent;
run;

/**/
proc logistic data=vpa.catedata plots=effect; 
class var47(ref='A');
model risk(reference='A低危')=var47 / link=glogit; 
output out=aa pred=fitted_p;
run;





/*Cumulative Logistic Model (LDLP);refuse PO assump*/
proc logistic data=vpa.catedata;
class GRLDLP(ref='低') /param=ref;
model risk(order=internal)=GRLDLP/ link=clogit
aggregate scale=none;
output out = prob PREDPROBS=C I;
run;
/*using baseline logit model with cate*/
proc logistic data=vpa.catedata order=data;
class risk GRLDLP(ref='低');
model risk (reference="A低危") =GRLDLP
/link=glogit scale = none aggregate;
output out = prob PREDPROBS=I;
run;
/*using baseline logit model with numeric*/
proc logistic data=vpa.catedata plots=effect; 
model risk(reference='A低危')=var52 / link=glogit; 
output out=aa pred=fitted_p;
run;



/*Cumulative Logistic Model (ALPA)*/
proc logistic data=vpa.catedata;
class GRALPA(ref='正常') /param=ref;
model risk(order=internal)=GRALPA/ link=clogit
aggregate scale=none;
output out = prob PREDPROBS=C I;
run;
proc freq data = prob;
format IP_A____ IP_B____ IP_C____ IP_D____ f6.4;
tables GRALPA*IP_A____*IP_B____*IP_C____*IP_D____
/list nocum nopercent;
run;

/*baseline logit with numeric*/
proc logistic data=vpa.catedata plots=effect; 
model risk(reference='A低危')=var77 / link=glogit; 
output out=aa pred=fitted_p;
run;


/*var77 alpa*/
proc logistic data=vpa.catedata descending plots=(effect); 
model risk=var77/clparm=both covb clodds=both; 
output out=bb p=pihat lower=pihat_lcl upper=pihat_ucl; 
run;

/*var52 ldl-p 救不了*/
proc logistic data=vpa.catedata descending plots=(effect); 
model risk=var52/clparm=both covb clodds=both; 
output out=bb p=pihat lower=pihat_lcl upper=pihat_ucl; 
run;

proc logistic data=vpa.catedata descending; 
class GRLDLP(ref='低')/param=ref;
model risk=GRLDLP; 
run;

data jjj;
set vpa.pudata;
if var52<1000 then cid=0;
else if var52<1400 then cid=10;
else if var52<2200 then cid=14;
else cid=22;
keep risk var52 cid;
run;

proc logistic data=jjj descending plots=(effect);
model risk=cid/clparm=both covb clodds=both;
run;

/*var47 ldlmode 没救*/
proc logistic data=vpa.catedata descending; 
class var47(ref='A')/param=ref;
model risk=var47; 
run;

proc logistic data=vpa.catedata descending; 
class ldlmode(ref='正常')/param=ref;
model risk=ldlmode; 
run;

/*var27 LDLC*/
proc logistic data=vpa.catedata descending plots=(effect); 
model risk=var27/clparm=both covb clodds=both; 
output out=bb p=pihat lower=pihat_lcl upper=pihat_ucl; 
run;

proc logistic data=vpa.catedata descending; 
class grldlc(ref='正常')/param=ref;
model risk=grldlc; 
run;


/*独立性检定*/
proc freq data=vpa.catedata;
tables tgrlp * risk /nocol nopercent chisq; 
run;
proc freq data=vpa.catedata;
tables TGmidzdb * risk /nocol nopercent chisq fisher; 
run;
proc freq data=vpa.catedata;
tables GRLDLC * risk /nocol nopercent chisq fisher; 
run;
proc freq data=vpa.catedata;
tables GRHDLC * risk /nocol nopercent chisq fisher; 
run;
proc freq data=vpa.catedata;
tables GRTC * risk /nocol nopercent chisq fisher; 
run;
proc freq data=vpa.catedata;
tables var47 * risk /nocol nopercent chisq fisher; 
run;
proc freq data=vpa.catedata;
tables GRLDLP * risk /nocol nopercent chisq; 
run;
proc freq data=vpa.catedata;
tables GRALPA * risk /nocol nopercent chisq; 
run;
data vpa.catedata;
set vpa.catedata;
if var4<=39 then agegr='青中年';
else if var4<=49 then agegr='中年';
else agegr='中老年';
run;
proc freq data=vpa.catedata;
tables agegr* risk /nocol nopercent chisq;
run;
proc freq data=vpa.catedata;
tables var3* risk /nocol nopercent chisq fisher;
run;
proc logistic data=vpa.catedata plots=effect; 
model risk(reference='A低危')=var4 / link=glogit; 
output out=aa pred=fitted_p;
run;
proc logistic data=vpa.catedata plots=effect; 
class var3(ref='男');
model risk(reference='A低危')=var3 / link=glogit; 
output out=aa pred=fitted_p;
run;



