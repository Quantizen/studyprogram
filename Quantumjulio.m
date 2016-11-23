(* ::Package:: *)

(* Mathematica Package  *)
(* Created by Julio Xu and etc.*)

(* :Title: Sliwadatas *)
(* :Author: Julio Xu and etc. <xuzp1009@gmail.com> *)
(* :Context: Sliwadatas` *)
(* :Version: 0.1 *)
(* :Date: 2016-09-14 *)

(* :Mathematica Version: 11 *)
(* :Copyright: (c) 2016 Julio Xu*)

(* This package stores the datas of Sliwa inequalities, totally 46*)
If[$VersionNumber < 10,
  Print["MaTeX requires Mathematica 10 or later."];
  Abort[]
]

BeginPackage["Quantumjulio`"]

exp2vec2::usage = "Sliexpression[i] will present the string expression of the i-th inequality."

list4exp::usage = "Slisolution[i] will give the optimal measurements and states for the i-th inequality."

singleout2::usage = "Slistatus[i] shows the status of the i-th inequality is analytical (1) or numerical (0)."

singleout::usage = "Slilocal[i] will tell you the local bound of the i-th inequality."

toperator::usage = "Sliquantum[i] will tell you the quantum bound of the i-th inequality."

list2op::usage = "Sliaf[i] will tell you the fractional packing number of the i-th inequality."

vec2exp::usage = "Sliaf[i] will tell you the fractional packing number of the i-th inequality."

exp4list::usage = "Sli"

ClearMaTeXCache::usage = "ClearMaTeXCache[] will clear MaTeX's cache."

`Developer`$Version = "0.1 (Nov 10, 2016)";

Begin["`Private`"] (* Begin Private Context *)
(*solution needs to be update*)


exp2vec[exp_,parties_]:=Module[{split,rules},
split=exp;
rules=((ToExpression@FromLetterNumber[#])->{0,SparseArray[#->1,parties]//Normal})&/@Range[parties];
(ToExpression@(split[[1]]<>"e")/.{ee->{1,{0,0,0}}})+(ToExpression@StringPartition[split[[2]]//StringReverse,2]/.rules//Total)
(*ee to escape the conflict with party name*)
]
(*exp2vec2: change an item into a vector. {coe,{p1,p2,p3}}, now only three parties at most. but it's very easy to add support for new parties*)
list4exp[expression_,parties_]:=Module[{expression2,split,ineqlist,cbound,qbound,aim,temp},
expression2=StringReplace[expression,{" "->"","\n"->""}]//ToLowerCase;
(*drop the blank and change to lowercase*)
split=Partition[StringSplit[expression2,{"(",")"}],2];
(*split into a set of items*)
temp=exp2vec[#,parties]&/@split;
(*change items into vector expressions thus we can use the functions defined before*)
ineqlist={temp[[All,1]],Transpose@(temp[[All,2]])/.{ConstantArray[0,Length@temp]->Nothing}}
]
(*treat each party as a list is more suitable for calculation, for example, add or remove some parties.*)


singleout2[ineq_,deleterules_]:=Module[{part0,factors,ineq1,ineq2},
part0[list_,poses_List]:=part0[list,#]&/@poses;
part0[list_,pos_]:=If[pos==0,1,Part[list,pos]];
(*because index 0 if for identity, and should take 1 when it's singled out*)
factors=part0[#[[2]],ineq[[2,#[[1]]]]]&/@deleterules;
ineq1=Thread[Times[Sequence@@factors,ineq[[1]]]];
(*put the factors in delete rules all to ineq1*)
ineq2=ineq[[2,Complement[Range[Length@ineq],deleterules[[All,1]]]]];
(*take the rest parties*)
{ineq1,ineq2}
]


singleout[ineqdata_,deleterule_]:=Module[{singleitem,part0},
part0[list_,pos__]:=If[pos==0,1,Part[list,pos]];
(*because index 0 if for identity, and should take 1 when it's singled out*)
singleitem[item_]:=Module[{newitem1,newitem2,poses},
newitem2=Drop[item[[2]],deleterule[[All,1]]];
(*part 2 of the item after singling, that is, the index of each party*)
poses=item[[2,deleterule[[All,1]]]];
(*the complement of newitem2, the rest ones in the same order of delete rules*)
newitem1=Times[item[[1]],Sequence@@(part0[deleterule[[#,2]],poses[[#]]]&/@Range[Length@poses])];
(*put the factors in delete rules all to item1*)
{newitem1,newitem2}
];
(*sigle the parties in one item in the ineqdata*)
singleitem[#]&/@ineqdata
(*single all the items*)
]
(*(*example, note that since we can give any values to the singleout parties, thus, singleout is more powerful than partial trace*)
deleterule={{2,{1,-1}},{3,{-1,-1}}};
num=21;
ineq=list4exp[Sliexpression[num]];
singleout[ineq,deleterule]
*)


toperator[s_,index_]:=Module[{dim,parties,ftemp,numsetting,vars,op,opxyz},
dim=2;
numsetting=2;
parties=Length@index;
vars=ToExpression[FromLetterNumber[#[[1]]]<>ToString[#[[2]]]]&/@SortBy[Tuples[{Range[2],Range[numsetting*parties]}],Last];
(*each operator needs two variables a,b; and the number of operators is number of settings times number of parties*)
op[a_,b_]:={Cos[a],Sin[a]Cos[b],Sin[a]Sin[b]}.(PauliMatrix[#]&/@Range[3]);
opxyz[k_Integer,i_Integer]:=If[i==0,IdentityMatrix[dim],op[vars[[2(2k+i-2)-1]],vars[[2(2k+i-2)]]]];
s KroneckerProduct[Sequence@@(Thread[ftemp[Range[Length@index],index]]/.{ftemp->opxyz})]
(*change a vector into a matrix; each party take two index: 2k-1 and 2k; and at least 2 parties*)
]

list2op[ineq_]:=toperator[ineq[[1,#]],ineq[[2,All,#]]]&/@Range[Length@(ineq[[1]])]//Total;


vec2exp[vec_]:=Module[{},
vec[[1]]"E("<>StringJoin@(If[vec[[2,#]]!=0,ToUpperCase[FromLetterNumber[#]]<>ToString[vec[[2,#]]],Nothing]&/@Range[Length@(vec[[2]])])<>")"]

exp4list[ineq_]:=With[{list={ineq[[1,#]],ineq[[2,All,#]]}&/@Range[Length@(ineq[[1]])]},StringDelete[ToString@Total[(vec2exp[#]&/@list)]," "]]
(*exp4list[ineq]*)
(*but when treat each item, another form is convient*)


End[] (* End Private Context *)

EndPackage[]
