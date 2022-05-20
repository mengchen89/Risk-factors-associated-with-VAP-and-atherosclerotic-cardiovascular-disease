libname vpa "D:\vpa";

/*��������*/
data vpa.data;
set vpa.rawdata;
label VAR1="�����"	VAR2="����"	VAR3="�����Ա�"	VAR4="��������"VAR5="��������"	
VAR6="��Ŀ����1" VAR7="TG" VAR8="�ο�ֵ1" VAR9="��λ1"	VAR10="���鷽��1"	
VAR11="��Ŀ����2" VAR12="RLP" VAR13="�ο�ֵ2"	 VAR14="��λ	2" VAR15="���鷽��2"	
VAR16="��Ŀ����3"	VAR17="�м��ܶ�֬����"	VAR18="�ο�ֵ3"	VAR19="��λ3"	VAR20="���鷽��3"	
VAR21="��Ŀ����4"	VAR22="���ܶ�֬����2"	VAR23="�ο�ֵ4"	VAR24="��λ4" VAR25="���鷽��4"	
VAR26="��Ŀ����5"	VAR27="LDL-C"	VAR28="�ο�ֵ5"	VAR29="��λ5"	VAR30="���鷽��5"
VAR31="��Ŀ����6"	VAR32="�����ܶ�֬����3"	VAR33="�ο�ֵ6"	VAR34="��λ6"	VAR35="���鷽��6"	
VAR36="��Ŀ����7"	VAR37="HDL-C"	VAR38="�ο�ֵ7"	VAR39="��λ7"	VAR40="���鷽��7"
VAR41="��Ŀ����8"	VAR42="TC"	VAR43="�ο�ֵ8"	VAR44="��λ8"	VAR45="���鷽��8"
VAR46="��Ŀ����9"	VAR47="LDL�ܶ�ģʽ"	VAR48="�ο�ֵ9"	VAR49="��λ9"	VAR50="���鷽��9"
VAR51="��Ŀ����10"	VAR52="LDL-P"	VAR53="�ο�ֵ10"	VAR54="��λ10"	VAR55="���鷽��10"
VAR56="��Ŀ����11"	VAR57="���ܶ�֬����3"	VAR58="�ο�ֵ11"	VAR59="��λ11"	VAR60="���鷽��11"
VAR61="��Ŀ����12"	VAR62="���ܶ�֬����"	VAR63="�ο�ֵ12"	VAR64="��λ12"	VAR65="���鷽��12"
VAR66="��Ŀ����13"	VAR67="�����ܶ�֬����"	VAR68="�ο�ֵ13"	VAR69="��λ13"	VAR70="���鷽��13"
VAR71="��Ŀ����14"	VAR72="�Ǹ��ܶ�֬���׵��̴�"	VAR73="�ο�ֵ14"	VAR74="��λ14"	VAR75="���鷽��14"
VAR76="��Ŀ����15"	VAR77="֬���� a(Lp(a))"	VAR78="�ο�ֵ15"	VAR79="��λ15"	VAR80="���鷽��15"
VAR81="�������"	VAR82="��������"	VAR83="���ν���"	VAR84="��ע";
RUN;

PROC FORMAT;
VALUE FSEX 1="��" 2="Ů";
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

/*�����Ӿ���*/
proc means data=vpa.nmiss mean median std min max;
var VAR7 VAR12 VAR17 VAR22 VAR27 VAR32 VAR37 VAR42 VAR52 VAR57 VAR62 VAR67 VAR72 VAR77;
run;

/*���ϴ���*/
data vpa.puredata;
set vpa.nmiss;
keep VAR3 VAR4 VAR7 VAR12 VAR17 VAR22 VAR27 VAR32 VAR37 VAR42 VAR47 VAR52 VAR57 VAR62 VAR67 VAR72 VAR77 VAR82;
RUN;
  /*��������Ϊnull*/
data vpa.TESTDATA;
set vpa.puredata;
if VAR82="NULL" or index(var82,"����ȱ��")>0;
RUN;

data vpa.pudata;
set vpa.puredata;
if var82="NULL" or index(var82,"����ȱ��")>0 then delete;
if index(var82,'��Σ')>0 then risk='A��Σ';
else if index(var82,'��Σ')>0 then risk="B��Σ";
else if index(var82,'����Σ')>0 then risk='D����Σ';
else risk="C��Σ";
run;

data aa;
set vpa.pudata;
keep risk;
proc print;
run;

/*���*/
proc corr data=vpa.pudata;
var VAR7 VAR12 VAR17 VAR22 VAR27 VAR32 VAR37 VAR42 VAR52 VAR57 VAR62 VAR67 VAR72 VAR77;
run;

/*ģʽɸѡ*/
  /*ȫ����ֵ*/
proc logistic data=vpa.pudata descending plots=(effect);
class VAR47(REF="A") VAR3;
model risk=VAR3 VAR4 VAR7 VAR12 VAR17 VAR22 VAR27 VAR32 VAR37 VAR42 VAR47 VAR52 VAR57 VAR62 VAR67 VAR72 VAR77;
RUN;

proc logistic data=vpa.pudata descending plots=(effect);
class VAR47(REF="A") VAR3;
model risk=VAR3 VAR4 VAR7 VAR12 VAR17 VAR22 VAR27 VAR32 VAR37 VAR42 VAR47 VAR52 VAR57 VAR62 VAR67 VAR72 VAR77 / SELECTION=FORWARD;
RUN;

  /*ȫ�����*/
PROC MEANS DATA=VPA.PUDATA MEDIAN MEAN MIN MAX;
VAR VAR4;
RUN;

data jj;
set vpa.pudata;
if var52<1000 then GRLDLP='��';
else if var52<1400 then GRLDLP='��';
else if var52<2200 then GRLDLP='��';
else GRLDLP='����';
run;


data vpa.catedata;
set jj;
if var4<50 THEN AGEGR='��ʮ������';
else AGEGR='��ʮ�꼰����';
if var7<1.69 then TGGR='����';
else TGGR='������';
if var12<0.78 then TGRLP='����';
else TGRLP='������';
if var17<0.52 then TGmidzdb='����';
else TGmidzdb='������';
if var22>0.26 then TGhzdb='����';
else TGhzdb='������';
if var27<3.36 then GRLDLC='����';
else GRLDLC='������';
if var32<0.26 then exlow3='����';
else exlow3='������';
if var37>1.03 then GRHDLC='����';
else GRHDLC='������';
if var42<5.17 then GRTC='����';
else GRTC='������';
if var47='A' then LDLmode='����';
else LDLmode='������';
if var57>0.78 then high3='����';
else high3='������';
if var62<2.59 then low='����';
else low='������';
if var67<0.78 then exlow='����';
else exlow='������';
if var72<4.14 then nohigh='����';
else nohigh='������';
if var77<300 then GRALPA='����';
else GRALPA='������';
/*keep var47 agegr tggr tgrlp tgmidzdb tghzdb grldlc exlow3 grhdlc grtc ldlmode grldlp high3 low exlow nohigh gralpa risk;*/
label AGEGR='������' TGGR='��������' TGRLP='֬���ײ���' TGmidzdb='�м��ܶ�֬����' TGhzdb='���ܶ�֬����2' 
GRLDLC='���ܶ�֬���׵��̴�(LDL-C)' exlow3='�����ܶ�֬����3' GRHDLC='���ܶ�֬���׵��̴�(HDL-C)' GRTC='�ܵ��̴�(TC)' 
LDLmode='LDL�ܶ�ģʽ' GRLDLP='���ܶ�֬���׿���(LDL-P)' high3='���ܶ�֬����3' low='���ܶ�֬����' exlow='�����ܶ�֬����'
nohigh='�Ǹ��ܶ�֬���׵��̴�' GRALPA='֬���� a(Lp(a))';
run;

proc gchart data=vpa.pudata;
vbar var4;
run;

data mb;
set vpa.catedata;
if GRLDLC='����';
run;
proc freq data=mb;
tables grldlp*risk/chisq nopercent nocol;
run;
proc logistic data=mb plots=effect; 
model risk(reference='A��Σ')=var52 / link=glogit; 
output out=aa pred=fitted_p;
run;

/*��������*/
/*������֬&֬���ײ���*/
proc freq data=vpa.catedata;
tables TGGR*TGRLP/chisq nopercent nocol;
run;/*ǿ���*/

/*��������&�����ܶ�֬����3*/
proc freq data=vpa.catedata;
tables TGGR*exlow3/chisq nopercent nocol;
run;/*ǿ��� �����ڼ���3����������������������*/

/*��������&�����ܶ�֬����*/
proc freq data=vpa.catedata;
tables TGGR*exlow/chisq nopercent nocol fisher;
run;/*ǿ���*/

/*֬���ײ���&�м��ܶ�֬����*/
proc freq data=vpa.catedata;
tables TGRLP*TGmidzdb/chisq nopercent nocol fisher;
run;/*ǿ��� �������м��ܶ�֬���ײ�������֬���ײ���������*/

/*֬���ײ���&�����ܶ�֬����3*/
proc freq data=vpa.catedata;
tables TGRLP*exlow3/chisq nopercent nocol fisher;
run;/*ǿ��� ������֬��������������3��������*/

/*֬���ײ���&�����ܶ�֬����*/
proc freq data=vpa.catedata;
tables TGRLP*exlow/chisq nopercent nocol;
run;/*ǿ���*/

/*֬���ײ���&�Ǹ��ܶ�֬���׵��̴�*/
proc freq data=vpa.catedata;
tables TGRLP*nohigh/chisq nopercent nocol;
run;/*ǿ���*/

/*�м��ܶ�֬����&�����ܶ�֬����3*/
proc freq data=vpa.catedata;
tables TGmidzdb*exlow3/chisq nopercent nocol fisher;
run;/*ǿ��� �����ڼ���3�������м��ܶȲ�������*/

/*�м��ܶ�֬����&�Ǹ��ܶ�֬���׵��̴�*/
proc freq data=vpa.catedata;
tables TGmidzdb*nohigh/chisq nopercent nocol;
run;/*ǿ���*/

/*���ܶ�֬����2&���ܶ�֬���׵��̴�(HDL-C)*/
proc freq data=vpa.catedata;
tables TGhzdb*GRHDLC/chisq nopercent nocol fisher;
run;/*ǿ���*/

/*���ܶ�֬����2&���ܶ�֬����3*/
proc freq data=vpa.catedata;
tables TGhzdb*high3/chisq nopercent nocol;
run;/*ǿ���*/

/*���ܶ�֬���׵��̴�(LDL-C)&�ܵ��̴�(TC)*/
proc freq data=vpa.catedata;
tables GRLDLC*GRTC/chisq nopercent nocol fisher;
run;/*ǿ���*/

/*���ܶ�֬���׵��̴�(LDL-C)&���ܶ�֬���׿���(LDL-P)*/
proc freq data=vpa.catedata;
tables GRLDLC*GRLDLP/chisq nopercent nocol fisher;
run;/*ǿ��� �����ڵ��ܶ�֬���׿�����,���ܶ�֬���׵��̴���������*/

/*���ܶ�֬���׵��̴�(LDL-C)&���ܶ�֬����*/
proc freq data=vpa.catedata;
tables GRLDLC*low/chisq nopercent nocol fisher;
run;/*ǿ���*/

/*���ܶ�֬���׵��̴�(LDL-C)&�Ǹ��ܶ�֬���׵��̴�*/
proc freq data=vpa.catedata;
tables GRLDLC*nohigh/chisq nopercent nocol fisher;
run;/*ǿ���*/

/*�����ܶ�֬����3&�����ܶ�֬����*/
proc freq data=vpa.catedata;
tables exlow3*exlow/chisq nopercent nocol fisher;
run;/*ǿ��� �����ڼ���3���������Ͳ�������*/

/*���ܶ�֬���׵��̴�(HDL-C)&���ܶ�֬����3*/
proc freq data=vpa.catedata;
tables GRHDLC*high3/chisq nopercent nocol fisher;
run;/*ǿ���*/

/*�ܵ��̴�(TC)&���ܶ�֬���׿���(LDL-P)*/
proc freq data=vpa.catedata;
tables grtc*grldlp/chisq nopercent nocol fisher;
run;/*ǿ��� �������ܵ��̴���������LDL-P���ߣ��������ܵ��̴�������LDL-P������*/

/*�ܵ��̴�(TC)&���ܶ�֬����*/
proc freq data=vpa.catedata;
tables grtc*low/chisq nopercent nocol fisher;
run;/*ǿ���*/

/*�ܵ��̴�(TC)&�Ǹ��ܶ�֬���׵��̴�*/
proc freq data=vpa.catedata;
tables grtc*nohigh/chisq nopercent nocol fisher;
run;/*ǿ���*/

/*���ܶ�֬���׿���(LDL-P)&���ܶ�֬����*/
proc freq data=vpa.catedata;
tables grldlp*low/chisq nopercent nocol fisher;
run;/*ǿ��� �����ڵ��ܶ�֬���ײ�������ldlp����*/

/*���ܶ�֬���׿���(LDL-P)&�Ǹ��ܶ�֬���׵��̴�*/
proc freq data=vpa.catedata;
tables grldlp*nohigh/chisq nopercent nocol fisher;
run;/*ǿ��� �����ڷǸ��ܶ�֬���׵��̴�������ldlp���ߺ�����*/

/*���ܶ�֬����&�Ǹ��ܶ�֬���׵��̴�*/
proc freq data=vpa.catedata;
tables low*nohigh/chisq nopercent nocol fisher;
run;/*ǿ���*/





/*Cumulative Logistic Model (RLP) ;refuse PO assump;*/
proc logistic data=vpa.catedata;
class tgrlp(ref='����') /param=ref;
model risk(order=data)=tgrlp/ link=clogit
aggregate scale=none;
output out = prob PREDPROBS=C I;
run;
  /*using baseline logit model with cate*/
proc logistic data=vpa.catedata order=data;
class risk tgrlp;
model risk (reference="A��Σ") =tgrlp
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
model risk(reference='A��Σ')=var12 / link=glogit; 
output out=aa pred=fitted_p;
run;


/*Cumulative Logistic Model (TGmidzdb) ;refuse PO assump*/
proc logistic data=vpa.catedata;
class TGmidzdb(ref='����') /param=ref;
model risk(order=data)=TGmidzdb/ link=clogit
aggregate scale=none;
output out = prob PREDPROBS=C I;
run;
/*using baseline logit model with category*/
proc logistic data=vpa.catedata order=data;
class risk TGmidzdb;
model risk (reference="A��Σ") =TGmidzdb
/link=glogit scale = none aggregate;
output out = prob PREDPROBS=I;
run;
/*using baseline logit model with numeric!*/
proc logistic data=vpa.catedata plots=effect; 
model risk(reference='A��Σ')=var17 / link=glogit; 
output out=aa pred=fitted_p;
run;


/*Cumulative Logistic Model (GRLDLC) ;refuse PO assump*/
proc logistic data=vpa.catedata;
class GRLDLC(ref='����') /param=ref;
model risk(order=internal)=GRLDLC/ link=clogit
aggregate scale=none;
output out = prob PREDPROBS=C I;
run;
/*using baseline logit model with cate*/
proc logistic data=vpa.catedata order=data;
class risk GRLDLC;
model risk (reference="A��Σ") =GRLDLC
/link=glogit scale = none aggregate;
output out = prob PREDPROBS=I;
run;
/*using baseline logit model with numeric*/
proc logistic data=vpa.catedata plots=effect; 
model risk(reference='A��Σ')=var27 / link=glogit; 
output out=aa pred=fitted_p;
run;
data aa;
set vpa.catedata;
if GRLDLC="����";
keep GRLDLC risk;
proc print;
run;


/*Cumulative Logistic Model (GRHDLC)*/
proc logistic data=vpa.catedata;
class GRHDLC(ref='����') /param=ref;
model risk(order=internal)=GRHDLC/ link=clogit
aggregate scale=none;
output out = prob PREDPROBS=C I;
run;
/*baseline logit with numeric*/
proc logistic data=vpa.catedata plots=effect; 
model risk(reference='A��Σ')=var37 / link=glogit; 
output out=aa pred=fitted_p;
run;



/*Cumulative Logistic Model (GRTC);refuse PO assump*/
proc logistic data=vpa.catedata;
class GRTC(ref='����') /param=ref;
model risk(order=internal)=GRTC/ link=clogit
aggregate scale=none;
output out = prob PREDPROBS=C I;
run;
/*using baseline logit model with cate*/
proc logistic data=vpa.catedata order=data;
class risk GRTC;
model risk (reference="A��Σ") =GRTC
/link=glogit scale = none aggregate;
output out = prob PREDPROBS=I;
run;
/*using baseline logit model with numeric*/
proc logistic data=vpa.catedata plots=effect; 
model risk(reference='A��Σ')=var42 / link=glogit; 
output out=aa pred=fitted_p;
run;


/*Cumulative Logistic Model (LDLmode);slightly rufuse PO assump*/
proc logistic data=vpa.catedata;
class LDLmode(ref='����') /param=ref;
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
model risk(reference='A��Σ')=var47 / link=glogit; 
output out=aa pred=fitted_p;
run;





/*Cumulative Logistic Model (LDLP);refuse PO assump*/
proc logistic data=vpa.catedata;
class GRLDLP(ref='��') /param=ref;
model risk(order=internal)=GRLDLP/ link=clogit
aggregate scale=none;
output out = prob PREDPROBS=C I;
run;
/*using baseline logit model with cate*/
proc logistic data=vpa.catedata order=data;
class risk GRLDLP(ref='��');
model risk (reference="A��Σ") =GRLDLP
/link=glogit scale = none aggregate;
output out = prob PREDPROBS=I;
run;
/*using baseline logit model with numeric*/
proc logistic data=vpa.catedata plots=effect; 
model risk(reference='A��Σ')=var52 / link=glogit; 
output out=aa pred=fitted_p;
run;



/*Cumulative Logistic Model (ALPA)*/
proc logistic data=vpa.catedata;
class GRALPA(ref='����') /param=ref;
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
model risk(reference='A��Σ')=var77 / link=glogit; 
output out=aa pred=fitted_p;
run;


/*var77 alpa*/
proc logistic data=vpa.catedata descending plots=(effect); 
model risk=var77/clparm=both covb clodds=both; 
output out=bb p=pihat lower=pihat_lcl upper=pihat_ucl; 
run;

/*var52 ldl-p �Ȳ���*/
proc logistic data=vpa.catedata descending plots=(effect); 
model risk=var52/clparm=both covb clodds=both; 
output out=bb p=pihat lower=pihat_lcl upper=pihat_ucl; 
run;

proc logistic data=vpa.catedata descending; 
class GRLDLP(ref='��')/param=ref;
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

/*var47 ldlmode û��*/
proc logistic data=vpa.catedata descending; 
class var47(ref='A')/param=ref;
model risk=var47; 
run;

proc logistic data=vpa.catedata descending; 
class ldlmode(ref='����')/param=ref;
model risk=ldlmode; 
run;

/*var27 LDLC*/
proc logistic data=vpa.catedata descending plots=(effect); 
model risk=var27/clparm=both covb clodds=both; 
output out=bb p=pihat lower=pihat_lcl upper=pihat_ucl; 
run;

proc logistic data=vpa.catedata descending; 
class grldlc(ref='����')/param=ref;
model risk=grldlc; 
run;


/*�����Լ춨*/
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
if var4<=39 then agegr='������';
else if var4<=49 then agegr='����';
else agegr='������';
run;
proc freq data=vpa.catedata;
tables agegr* risk /nocol nopercent chisq;
run;
proc freq data=vpa.catedata;
tables var3* risk /nocol nopercent chisq fisher;
run;
proc logistic data=vpa.catedata plots=effect; 
model risk(reference='A��Σ')=var4 / link=glogit; 
output out=aa pred=fitted_p;
run;
proc logistic data=vpa.catedata plots=effect; 
class var3(ref='��');
model risk(reference='A��Σ')=var3 / link=glogit; 
output out=aa pred=fitted_p;
run;



