(* ::Package:: *)

(* ::Code::Bold:: *)
BeginPackage["ListOperations`"]

ClearAll @@ Names["ListOperations`*"];

ListReshape::usage="ListReshape[list,dimensions,n] Does the same as ArrayReshape but it does not flatten the array automatically. The integer n specifies how much to flatten the list before reshaping."
SelectEquivalents::usage="This allows me to group lists by any criteria and transform them in the process.
The way it works is that a criteria function (f) tags each item in the list, each item is then transformed 
by a second supplied function (g), and the specific output is controlled by a third function (h). 
The function h accepts two arguments: a tag and a list of the collected items that have that tag."
HeadsToList::usage="Turns a nested function into a list of all the Heads."
PrintA::usage="Neatly print a list."
LocalToGlobal::usage="Removes both namespace and module number from variable."
LocalToString::usage="Removes both namespace and module number from variable and returns as string."



Begin["`Private`"]

ListReshape[list_, dims_,flatten_:0] := Fold[Partition,Flatten[list,flatten], dims[[-1;;2;;-1]]];

SelectEquivalents[x_List,f_:Identity, g_:Identity, h_:(#2&)]:=
   Reap[Sow[g[#],{f[#]}]&/@x, _, h][[2]];

SetAttributes[HeadsToList,HoldAllComplete];
IgnoreHeads ={SmallCircle,Hold,HoldComplete};
HeadsToList[H_]:=Cases[HoldComplete[H],x_/; (Depth[Hold[x]]==2&& Cases[IgnoreHeads,x]=={})->x,\[Infinity],Heads-> True ];


LocalToGlobal[x_]:=Module[ {pos,strx}, 
	strx= SymbolName[Unevaluated[x]] ;
	pos = Position[ Characters[strx],"$"];
	If[pos=={},Return[x]];
	pos = pos[[1,1]];
	strx=StringDrop[strx,{pos,strx//StringLength}];
	Return[strx//ToExpression];
];
SetAttributes[LocalToGlobal, HoldFirst];

LocalToString[x_]:=Module[ {pos,strx}, 
	strx= SymbolName[Unevaluated[x]] ;
	pos = Position[ Characters[strx],"$"];
	If[pos=={},Return[strx]];
	pos = pos[[1,1]];
	strx=StringDrop[strx,{pos,strx//StringLength}];
	Return[strx];
];
SetAttributes[LocalToGlobal, HoldFirst];

PrintA[x_]:= Module[{tmp},
	tmp=Flatten[x];
	Table[ Print[ ToString[tmp[[j]]]<>": ",tmp[[j]],", " ],{j,Length[tmp]} ];
];
End[];
EndPackage[];
