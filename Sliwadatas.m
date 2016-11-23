(* ::Package:: *)

(* Mathematica Package  *)
(* Created by Adan Cabello, Zhen-peng Xu and etc.*)

(* :Title: Sliwadatas *)
(* :Author: Adan Cabello, Zhen-peng Xu and etc. <xuzp1009@gmail.com> *)
(* :Context: Sliwadatas` *)
(* :Version: 0.1 *)
(* :Date: 2016-09-14 *)

(* :Mathematica Version: 11 *)
(* :Copyright: (c) 2016 Adan Cabello *)

(* This package stores the datas of Sliwa inequalities, totally 46*)
If[$VersionNumber < 10,
  Print["MaTeX requires Mathematica 10 or later."];
  Abort[]
]

BeginPackage["Sliwadatas`"]

Sliexpression::usage = "Sliexpression[i] will present the string expression of the i-th inequality."

Slisolution::usage = "Slisolution[i] will give the optimal measurements and states for the i-th inequality."

Slistatus::usage = "Slistatus[i] shows the status of the i-th inequality is analytical (1) or numerical (0)."

Slilocal::usage = "Slilocal[i] will tell you the local bound of the i-th inequality."

Sliquantum::usage = "Sliquantum[i] will tell you the quantum bound of the i-th inequality."

Sliaf::usage = "Sliaf[i] will tell you the fractional packing number of the i-th inequality."

ClearMaTeXCache::usage = "ClearMaTeXCache[] will clear MaTeX's cache."

`Developer`$Version = "0.1 (Sep 14, 2016)";

Begin["`Private`"] (* Begin Private Context *)
(*solution needs to be update*)


Sliexpression[i_]:={"E(A1)+E(B1)-E(A1B1)+E(C1)-E(A1C1)-E(B1C1)+E(A1B1C1)","E(A1B1C1)+E(A2B2C1)+E(A2B1C2)-E(A1B2C2)","E(A1B1C1)+E(A2B1C1)+E(A1B2C2)-E(A2B2C2)","2E(A1)+E(B1C1)-E(A1B1C1)+E(B2C1)-E(A1B2C1)+E(B1C2)-E(A1B1C2)-E(B2C2)+E(A1B2C2)","E(A1)+E(B1)+E(A2B1)+E(A1B2)-E(A2B2)+E(C1)+E(A2C1)-E(A1B1C1)-E(A2B1C1)+E(B2C1)-E(A1B2C1)+E(A1C2)-E(A2C2)+E(B1C2)-E(A1B1C2)-E(B2C2)+E(A2B2C2)","E(A1)+E(B1)+E(A1B1)+E(C1)+E(A2C1)-E(A1B1C1)-E(A2B1C1)+E(B2C1)-E(A1B2C1)+E(A1C2)-E(A2C2)-E(B1C2)+E(A2B1C2)+E(B2C2)-E(A1B2C2)","3E(A1B1C1)+E(A2B1C1)+E(A1B2C1)-E(A2B2C1)+E(A1B1C2)-E(A2B1C2)-E(A1B2C2)+E(A2B2C2)","E(A1B1)+E(A2B1)+E(A1B2)+E(A2B2)+2E(A1B1C1)-2E(A2B2C1)+E(A1B1C2)-E(A2B1C2)-E(A1B2C2)+E(A2B2C2)","E(A1B1)+E(A2B1)+E(A1B2)+E(A2B2)+2E(A1B1C1)-2E(A1B2C1)+E(A1B1C2)-E(A2B1C2)+E(A1B2C2)-E(A2B2C2)","E(A1B1)+E(A2B1)+E(A1B2)+E(A2B2)+E(A1C1)-E(A2C1)+E(B1C1)+E(A1B1C1)-E(B2C1)-E(A2B2C1)+E(A1C2)-E(A2C2)-E(B1C2)+E(A2B1C2)+E(B2C2)-E(A1B2C2)","2E(A1B1)+2E(A2B2)+E(A1B1C1)+E(A2B1C1)-E(A1B2C1)-E(A2B2C1)+E(A1B1C2)-E(A2B1C2)+E(A1B2C2)-E(A2B2C2)","2E(A1B1)+2E(A2B2)+E(A1C1)+E(A2C1)-E(B1C1)+E(A2B1C1)-E(B2C1)-E(A1B2C1)+E(A1C2)+E(A2C2)-E(B1C2)-E(A2B1C2)-E(B2C2)+E(A1B2C2)","2E(A1B1)+2E(A2B1)+E(A1B1C1)-E(A2B1C1)+E(A1B2C1)-E(A2B2C1)+E(A1B1C2)-E(A2B1C2)-E(A1B2C2)+E(A2B2C2)","2E(A1B1)+2E(A2B1)+E(A1C1)-E(A2C1)+E(A1B2C1)-E(A2B2C1)+E(A1C2)-E(A2C2)-E(A1B2C2)+E(A2B2C2)","2E(A1B1)+2E(A2B1)+E(A1C1)+E(A2C1)-2E(B1C1)+E(A1B2C1)-E(A2B2C1)+E(A1C2)+E(A2C2)-2E(B1C2)-E(A1B2C2)+E(A2B2C2)","E(A1)+E(A2)+E(A1B1)+E(A2B1)+E(A1C1)+E(A2C1)-2E(A2B1C1)+E(A1B2C1)-E(A2B2C1)+E(A1B1C2)-E(A2B1C2)-E(A1B2C2)+E(A2B2C2)","E(A1)+E(A2)+E(A1B1)+E(A2B1)+E(A1C1)+E(A2C1)-E(A1B1C1)-E(A2B1C1)+2E(A1B2C2)-2E(A2B2C2)","E(A1)+E(A2)+E(A1B1)+E(A2B1)+E(A1C1)+E(A2C1)-2E(B1C1)+E(A1B2C1)-E(A2B2C1)+E(A1B1C2)-E(A2B1C2)+2E(B2C2)-E(A1B2C2)-E(A2B2C2)","E(A1)+E(A2)+E(A1B1)+E(A2B1)+E(A1C1)+E(A2C1)-2E(B1C1)+2E(B2C1)-E(A1B2C1)-E(A2B2C1)+E(A1B1C2)-E(A2B1C2)+E(A1B2C2)-E(A2B2C2)","E(A1)+E(A2)+E(A1B1)-E(A2B1)+E(A1B2)-E(A2B2)+E(A1C1)-E(A2C1)-E(B1C1)+E(A1B1C1)+E(A2B1C1)-E(B2C1)+E(A1B2C1)+E(A2B2C1)+E(B1C2)-E(A1B1C2)-E(A2B1C2)-E(B2C2)+E(A1B2C2)+E(A2B2C2)","E(A1)+E(A2)+E(B1)+E(A1B1)+E(B2)-E(A2B2)+E(A1C1)+E(A2C1)+E(B1C1)-2E(A1B1C1)-E(A2B1C1)+E(B2C1)-E(A1B2C1)+E(A1B1C2)-E(A2B1C2)-E(A1B2C2)+E(A2B2C2)","E(A1)+E(A2)+E(B1)+E(A1B1)+E(B2)-E(A2B2)+E(C1)+E(A1C1)+E(B1C1)-2E(A1B1C1)-E(A2B1C1)-E(A1B2C1)+E(A2B2C1)+E(C2)-E(A2C2)-E(A1B1C2)+E(A2B1C2)-E(B2C2)+E(A1B2C2)","E(A1)+E(A2)+E(B1)-E(A1B1)-E(A2B1)+E(B2)-E(A1B2)-E(A2B2)+E(A1C1)-E(A2C1)-E(A1B1C1)+E(A2B1C1)-E(A1B2C1)+E(A2B2C1)+E(B1C2)-E(A1B1C2)-E(A2B1C2)-E(B2C2)+E(A1B2C2)+E(A2B2C2)","E(A1)+E(B1)+E(A2B1)+E(A1B2)+E(A2B2)+E(C1)+E(A2C1)-E(B1C1)+2E(A1B1C1)-E(A2B1C1)-2E(A2B2C1)+E(A1C2)+E(A2C2)-2E(A2B1C2)-E(A1B2C2)+E(A2B2C2)","E(A1)+E(B1)+E(A2B1)+E(A1B2)+E(A2B2)+E(C1)+E(A2C1)-E(B1C1)+2E(A1B1C1)-E(A2B1C1)-2E(A2B2C1)+E(A1C2)+E(A2C2)-2E(A1B1C2)+E(A1B2C2)-E(A2B2C2)","E(A1)+E(B1)+E(A1B1)+2E(A2B2)+E(C1)+E(A1C1)+E(B1C1)-E(A1B1C1)-2E(A2B2C1)+2E(A2C2)-2E(A2B1C2)-2E(B2C2)+2E(A1B2C2)","2E(A1)+E(A2)+E(B1)-E(A1B1)+E(A1B2)+E(A2B2)+E(C1)-E(A1C1)+2E(A1B1C1)-2E(A2B1C1)+E(B2C1)-E(A1B2C1)+E(A1C2)+E(A2C2)+E(B1C2)-E(A1B1C2)+E(B2C2)-2E(A1B2C2)-E(A2B2C2)","E(A1)+E(A2)+E(A1B1)-E(A2B1)+E(A1C1)-E(A2C1)-E(B1C1)+2E(A1B1C1)+E(A2B1C1)+E(B2C1)-E(A1B2C1)-2E(A2B2C1)+E(B1C2)-E(A1B1C2)-2E(A2B1C2)+E(B2C2)-3E(A1B2C2)","E(A1)+E(A2)+E(A1B1)-E(A2B1)+E(A1C1)-E(A2C1)-E(B1C1)+2E(A1B1C1)+E(A2B1C1)+E(B2C1)-E(A1B2C1)-2E(A2B2C1)+E(B1C2)-3E(A1B1C2)+E(B2C2)-E(A1B2C2)-2E(A2B2C2)","E(A1)+E(A2)+2E(A1B1)-2E(A2B1)+E(A1B2)-E(A2B2)+E(A1C1)-E(A2C1)-E(B1C1)+2E(A1B1C1)+E(A2B1C1)-E(B2C1)+E(A1B2C1)+2E(A2B2C1)+E(B1C2)-2E(A1B1C2)-E(A2B1C2)-E(B2C2)+2E(A1B2C2)+E(A2B2C2)","E(A1)+E(A2)+E(B1)-E(A2B1)+E(B2)-E(A1B2)+E(A1C1)-E(A2C1)+2E(A2B1C1)-E(A1B2C1)+3E(A2B2C1)+E(B1C2)-2E(A1B1C2)-E(A2B1C2)-E(B2C2)+2E(A1B2C2)+E(A2B2C2)","E(A1)+E(A2)+E(B1)-E(A2B1)+E(B2)-E(A1B2)+2E(A1C1)-2E(A2C1)+2E(A2B1C1)+2E(A2B2C1)+E(A1C2)-E(A2C2)-E(B1C2)+2E(A1B1C2)+E(A2B1C2)+E(B2C2)-E(A1B2C2)-2E(A2B2C2)","E(A1)+E(A2)+E(B1)-E(A2B1)+E(B2)-E(A1B2)+E(C1)-E(A2C1)+2E(A2B1C1)-E(B2C1)+2E(A1B2C1)+E(A2B2C1)+E(C2)-E(A1C2)-E(B1C2)+2E(A1B1C2)+E(A2B1C2)+E(A1B2C2)-3E(A2B2C2)","E(A1)+E(A2)+E(B1)-E(A2B1)+E(B2)-E(A1B2)+E(C1)-E(A2C1)-E(B1C1)-2E(A1B1C1)+E(A2B1C1)-2E(B2C1)+2E(A1B2C1)+2E(A2B2C1)+E(C2)-E(A1C2)-2E(B1C2)-E(B2C2)+E(A1B2C2)-2E(A2B2C2)","E(A1)+E(A2)+E(B1)-E(A1B1)-2E(A2B1)+E(B2)-2E(A1B2)-E(A2B2)+E(A1C1)-E(A2C1)-E(A1B1C1)+E(A2B1C1)-2E(A1B2C1)+2E(A2B2C1)+E(B1C2)-2E(A1B1C2)-E(A2B1C2)-E(B2C2)+2E(A1B2C2)+E(A2B2C2)","2E(A1)+E(A1B1)+E(A2B1)+E(A1B2)+E(A2B2)+E(A1C1)+E(A2C1)+E(B1C1)-2E(A1B1C1)+E(A2B1C1)-E(B2C1)+E(A1B2C1)-2E(A2B2C1)+E(A1C2)+E(A2C2)-E(B1C2)+E(A1B1C2)-2E(A2B1C2)-E(B2C2)+2E(A1B2C2)-E(A2B2C2)","2E(A1)+E(A1B1)+E(A2B1)+E(A1B2)+E(A2B2)+E(A1C1)+E(A2C1)+E(B1C1)-3E(A1B1C1)-E(B2C1)+2E(A1B2C1)-E(A2B2C1)+E(A1C2)+E(A2C2)-E(B1C2)+2E(A1B1C2)-E(A2B1C2)-E(B2C2)+E(A1B2C2)-2E(A2B2C2)","2E(A1)+2E(A1B1)+2E(A2B1)+E(A1C1)+E(A2C1)-E(B1C1)+E(A1B1C1)-2E(A2B1C1)+E(B2C1)-2E(A1B2C1)+E(A2B2C1)+E(A1C2)+E(A2C2)-E(B1C2)+E(A1B1C2)-2E(A2B1C2)-E(B2C2)+2E(A1B2C2)-E(A2B2C2)","2E(A1)+2E(B1)-E(A1B1)+E(A2B1)+E(A1B2)+E(A2B2)+2E(C1)-E(A1C1)+E(A2C1)-E(B1C1)+2E(A1B1C1)-E(A2B1C1)+E(B2C1)-E(A1B2C1)-2E(A2B2C1)+E(A1C2)+E(A2C2)+E(B1C2)-E(A1B1C2)-2E(A2B1C2)+E(B2C2)-2E(A1B2C2)+E(A2B2C2)","2E(A1)+2E(A2)+2E(B1)-E(A1B1)-E(A2B1)+E(A1B2)+E(A2B2)+E(A1C1)+E(A2C1)+2E(B1C1)-E(A1B1C1)-E(A2B1C1)+2E(B2C1)-2E(A1B2C1)-2E(A2B2C1)+E(A1C2)-E(A2C2)-2E(A1B1C2)+2E(A2B1C2)+E(A1B2C2)-E(A2B2C2)","E(A1)+E(B1)+E(A1B1)+E(C1)+E(A2C1)-3E(A1B1C1)-E(A2B1C1)+E(B2C1)-E(A1B2C1)-2E(A2B2C1)+E(A1C2)-E(A2C2)+E(B1C2)-4E(A1B1C2)+E(A2B1C2)-E(B2C2)+E(A1B2C2)+2E(A2B2C2)","E(A1)+E(A2)+E(B1)+E(A1B1)+E(B2)-E(A2B2)+E(A1C1)-E(A2C1)+E(B1C1)-2E(A1B1C1)-E(A2B1C1)-E(B2C1)-E(A1B2C1)+4E(A2B2C1)+2E(A2C2)-E(A1B1C2)-3E(A2B1C2)+2E(B2C2)-3E(A1B2C2)-E(A2B2C2)","2E(A1)+2E(B1)-E(A1B1)+E(A2B1)+E(A1B2)-E(A2B2)+E(A1C1)+E(A2C1)+E(B1C1)-2E(A1B1C1)-3E(A2B1C1)-E(B2C1)+E(A1B2C1)+2E(A2B2C1)+E(A1C2)-E(A2C2)+E(B1C2)-3E(A1B1C2)+E(B2C2)-4E(A1B2C2)+E(A2B2C2)","2E(A1)+2E(A2)+2E(A1B1)-2E(A2B1)+E(A1C1)-E(A2C1)-2E(B1C1)+2E(A1B1C1)+2E(A2B1C1)+2E(B2C1)-E(A1B2C1)-3E(A2B2C1)+E(A1C2)-E(A2C2)-2E(B1C2)+2E(A1B1C2)+2E(A2B1C2)-2E(B2C2)+3E(A1B2C2)+E(A2B2C2)","3E(A1)+E(A2)+2E(A1B1)-2E(A2B1)+E(A1B2)-E(A2B2)+2E(A1C1)-2E(A2C1)-2E(B1C1)+2E(A1B1C1)+2E(A2B1C1)-2E(B2C1)+2E(A1B2C1)+2E(A2B2C1)+E(A1C2)-E(A2C2)-2E(B1C2)+2E(A1B1C2)+2E(A2B1C2)+2E(B2C2)-3E(A1B2C2)-E(A2B2C2)","3E(A1)+E(A2)+3E(B1)-2E(A1B1)-E(A2B1)+E(B2)-E(A1B2)-2E(A2B2)+2E(A1C1)-2E(A2C1)+E(B1C1)-3E(A1B1C1)+4E(A2B1C1)+E(B2C1)-E(A1B2C1)+2E(A2B2C1)+E(A1C2)+E(A2C2)+2E(B1C2)-3E(A1B1C2)-E(A2B1C2)-2E(B2C2)+4E(A1B2C2)+2E(A2B2C2)"}[[i]]


Slistatus[i_]:={1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,0,0,1,0,0,0,0,0,1,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,1,1,1,0}[[i]]
Slisolution[i_]:={{{1/2,1/2,1/2,-(1/4)},{0,0,0,0,0,0}},{{0,0,0,0},{1/2,0,1/2,0,-(1/2),0}},{{1/2,2/3,ArcSec[Sqrt[3]]/\[Pi],1/4},{1/2,0,1/4,3/4,1,0}},{{1/2,1/2,1/2,3/4},{1/2,1/2,1/2,0,3/4,1/4}},{{ArcCos[-Sqrt[-(3/2)+7/(2 Sqrt[5])]]/\[Pi],ArcCos[Sqrt[1/38 (9+Sqrt[5])]]/\[Pi],ArcCos[-Sqrt[1/22 (7+Sqrt[5])]]/\[Pi],ArcTan[1/2 (-1+Sqrt[5])]/\[Pi]},{-(ArcCos[-17+8 Sqrt[5]]/(2 \[Pi])),-(1/2),1+ArcCos[-17+8 Sqrt[5]]/(2 \[Pi]),3/2,-(ArcCos[-17+8 Sqrt[5]]/(2 \[Pi])),-(1/2)}},{{1/2,1/2,1/4,1/2},{1/4,3/4,1/2,1/2,1/2,0}},{{1/2,1/3,ArcSec[Sqrt[3]]/\[Pi],3/4},{1/2,-((2 ArcTan[3-2 Sqrt[2]])/\[Pi]),-((2 ArcTan[3-2 Sqrt[2]])/\[Pi]),1/2,-((2 ArcTan[3-2 Sqrt[2]])/\[Pi]),1/2}},{{1/2,ArcCos[-(1/Sqrt[3])]/\[Pi],1/3,ArcTan[Sqrt[2]]/\[Pi]},{1/2,-((2 ArcTan[3-2 Sqrt[2]])/\[Pi]),1+(2 ArcTan[3-2 Sqrt[2]])/\[Pi],1/2,0,1/2}},{{1/2,1/4,1/2,-(1/2)},{1/4,3/4,1/2,0,-(1/2),1/2}},{{1/2,1/2,1/4,0},{0,0,0,0,0,0}},{{1/2,3/4,1/2,1/2},{1/4,3/4,1/2,0,-(1/2),1/2}},{{1/2,3/4,1/2,1/2},{1/4,3/4,1/2,0,-(1/2),1/2}},{{1/4,1/2,1/2,1/2},{1/4,3/4,1/2,0,0,1}},{{3/4,1/2,1/2,1/2},{1/4,3/4,1/2,1,0,1}},{{1/4,1/2,1/2,1/2},{0,1/6,0,0,0,-(1/6)}},{{0.4382256248237981`,0.6685598897669347`,0.3009735998801832`,-0.647583617600129`},{0.9031464870072489`,0.260156912676533`,1/2,-0.07901060321590694`,1/2,1.079010603215907`}},{{1/4,1/2,1/2,1/2},{3/4,1/4,1/2,1,1/2,0}},{{1/2,0.7207386206817643`,0.8123387017126025`,-0.1813056640904313`},{1.10212`,-0.102119`,1/2,0,1/2,0}},{{0.7449727695678602`,1/2,0.40737583448398523`,1/2},{0.8655323984397627`,0.1344676015602373`,-0.08389783632945129`,0.3295332250713658`,1/2,0}},{{1/2,1/2,1/2,3/4},{1/2,1/2,1/2,0,-(1/4),1/4}},{{0.4857411118325263`,0.3019376593613017`,0.2546667671699728`,0.5056901470535111`},{1.2252856999610875`,0.6297193119705218`,0.8259586337419411`,1.421525021847049`,0.7700212784062254`,0.37819472620759476`}},{{0.4484192791767503`,0.35960149030402144`,0.34349815475334705`,0.6798876716736572`},{0.41847300359003886`,0.9887281937895358`,0.5815269964099612`,0.011271806210464197`,0.5815269964099612`,0.011271806210464197`}},{{0.5550102386493011`,1/2,0.7538707303108314`,0.`},{1/2,0.07035242156738376`,0.4374855232241503`,0.8671331024196022`,1/2,1/2}},{{0.4848660615862685`,0.3395721163798783`,0.6859467966400267`,0.3440313596462456`},{0.5716943568621075`,-0.015884540397759765`,0.03372922871375662`,0.5374548205237515`,0.9662707712862434`,0.4625451794762485`}},{{0.6283670000000001`,0.367501`,0.6602820000000001`,0.317626`},{1.02807`,0.347576`,-0.0912219`,0.540945`,0.37556`,0.967902`}},{{1/2,ArcCos[-(1/Sqrt[6])]/\[Pi],ArcSec[Sqrt[5]]/\[Pi],1/3},{1/2,0,1/2,1,1/2,0}},{{0.2524`,0.53443`,0.465363`,-0.5937450000000001`},{0.283268`,0.869088`,0.0227767`,0.58978`,0.977223`,0.41022`}},{{0.462884`,0.626838609865468`,0.6383197351012238`,0.772927`},{0.530614`,0.190233`,0.927289`,0.398764`,0.927289`,0.398764`}},{{1/2,1/2,1/2,3/4},{1/2,1/2,0,1/2,1/4,3/4}},{{1/2,1/2,1/2,3/4},{1/2,1/2,0,-(1/2),1/4,3/4}},{{0.51731`,0.36123400000000017`,0.709339`,0.30073600000000006`},{0.915577`,1.21171`,1.64904`,0.241058`,0.218587`,0.925373`}},{{0.5528599999999999`,0.363019`,0.3014720000000001`,0.630833`},{0.22329`,-0.124725`,0.8469`,1.30228`,0.328932`,0.915226`}},{{0.448713`,0.3280970000000001`,0.2953680000000001`,-0.230526`},{1.02618`,0.559014`,1.02618`,0.559014`,1.02618`,0.559014`}},{{1/2,0.707734`,0.776935`,-0.375329`},{1.17931`,-0.17931000000000008`,0.913785`,0.375686`,0.624314`,0.08621500000000004`}},{{1/2,0.337463`,0.748188`,0.04793639999999998`},{1.47196`,0.940352`,0.940352`,1.47196`,0.171529`,-0.171529`}},{{0.471586`,0.28098400000000007`,0.807187`,0.3188500000000001`},{-0.350063`,0.397018`,0.115381`,0.607539`,0.884619`,0.39246099999999995`}},{{1/2,1/2,1/2,3/4},{1/2,-(1/2),0,-(1/2),3/4,-(1/4)}},{{1/2,1/2,1/2,3/4},{1/2,-(1/2),0,1/2,1/4,-(1/4)}},{{0.48894506119885606`,0.6434209375848072`,0.3392713194517376`,0.31358245549401403`},{0.8024879477429815`,0.29254174967965363`,0.8024879477429815`,0.29254174967965363`,0.1975120522570185`,0.7074582503203464`}},{{0.2312241042506523`,1/2,0.45589236686291035`,1/2},{0.8135040458595003`,0.1864959541404997`,0.05211195394377428`,0.6492070373045898`,1/2,0}},{{0.553055688466618`,0.4119514200183806`,0.6668318054266901`,0.30381689383260696`},{0.45419232961540124`,0.13000630029661855`,0.5458076693133053`,0.8699936955934217`,0.8276636161302874`,0.3065640784531188`}},{{0.5220215495526518`,0.35568539041204467`,0.6620833450980943`,0.3114504881594524`},{0.5070441026776511`,0.06955100184702256`,0.4929558795727512`,0.9304489802064244`,0.4698995429627252`,0.9115240081809809`}},{{1/2,1/4,1/2,1/2},{0,0,-(1/4),1/2,1/2,1/2}},{{1/2,1/2,1/2,3/4},{1/2,1/2,0,-(1/2),-(1/4),1/4}},{{1/2,1/2,1/2,3/4},{1/2,1/2,0,1/2,-(1/4),1/4}},{{1/2,0.2271310838394335`,1/2,-(1/2)},{-0.6347985003641508`,0.9068746211365594`,-0.6347985003641508`,0.9068746211365594`,1/2,1/2}}}[[i]]
Slilocal[i_]:={1,2,2,2,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,6,6,6,6,6,6,6,6,6,6,6,6,6,7,8,8,8,8,10}[[i]]
Sliquantum[i_]:={1,4,2 Sqrt[2],-2+4 Sqrt[2],-13+8 Sqrt[5],-1+4 Sqrt[2],20/3,20/3,4 Sqrt[2],4,4 Sqrt[2],4 Sqrt[2],4 Sqrt[2],4 Sqrt[2],6,6.128869370280891`,4 Sqrt[2],-2 (-7+Sqrt[17]),5.782911753203689`,-2+6 Sqrt[2],5.9554569186959245`,6.1979587791753055`,3/2 (-1+Sqrt[17]),7.940109805858052`,6.824261583579066`,1+4 Sqrt[3],6.95465583289284`,9.909761578303707`,-2+8 Sqrt[2],-2+8 Sqrt[2],7.804265863932002`,8.151561762001537`,9.78985331827268`,8.251497774083449`,7.85527773162123`,9.461431152434017`,-2+8 Sqrt[2],-2+8 Sqrt[2],9.325340165014`,8.129787242088113`,10.367746725495849`,13.047046076499523`,8 Sqrt[2],-4+12 Sqrt[2],-4+12 Sqrt[2],12.985173496317381`}[[i]]



End[] (* End Private Context *)

EndPackage[]
