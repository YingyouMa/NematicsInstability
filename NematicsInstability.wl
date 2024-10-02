(* ::Package:: *)

BeginPackage["MyPackage`"]


q::usage="Provide the notations of Q with derivatives and indices, q[derivatives,index]"


Begin["`Private`"]


q[derivatives_, index_]:=(
If[Length@derivatives>2,Print["Error in q[]: The highest order of derivatives is 2"];Abort[]];
If[Length@index!=2,Print["Error in q[]: Q has two indices"];Abort[]];
If[MemberQ[Flatten[{derivatives,index}],Null],Print["Error in q[]: Null in input"];
Abort[]];
If[Not@ContainsOnly[Flatten[{derivatives,index}],{1,2,3}],Print["Error in q[]: derivatives or indices not in {1,2,3}"];Abort[]];

Module[
{over=Sort@derivatives,under=Sort@index},
If[Length@derivatives==0,Underscript["Q",FromDigits@under],
Underoverscript["Q",FromDigits@under,FromDigits@over]
]
]
)


End[]


EndPackage[]
