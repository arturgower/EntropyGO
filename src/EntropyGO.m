(* ::Package:: *)

(* ::Code::Bold:: *)
BeginPackage["EntropyGO`"]

Unprotect[StoneProb, StoneRadius, MaxPathLength,MaxRadius,p, CoordToBoard, listUDRL, CoordLinkerToStone ];
ClearAll @@ Names["EntropyGO`*"];

(*Needs["ListOperations`",NotebookDirectory[]<>"ListOperations.m"];*)
Get["ListOperations.m", Path-> {"../src",""}]


(* ::Code::Bold:: *)
GenerateRandomBoard::usage="GenerateRandomBoard[lenBoard_,options_:{} ] generates a random board and loads Groups, Board and BoardOfLinksUDRL}."
LoadBoard::usage="LoadBoard[board_:Board] will load the variables Board, LenBoard, Groups and BoardOfLinksUDRL. " 
DropRedudantPaths::usage=" DropRedudantPaths[pathBoard] returns a board of paths without the redudant paths in pathBoard. It is also sorts the paths in order of size. By definition if two paths are from the same group then one can not contain the other, so we just keep the smallest one. not "
BoardInfluence::usage="Return a Board of influence from the existing Groups."
BoardInfluenceFromGroup::usage="BoardInfluenceFromGroup[group,w] returns a board of influence from group."
InfluenceFromPaths::usage="InfluenceFromPaths[paths] returns the influence of paths to the same coordinate, for example paths ={{a/3,{x,y,z} },{b/2,{x,u}},{c/3,{y,z,u}}}, where the influence of the paths, (a,b,c), must be divided by the number of stones before hand."

Draw::usage="Draw[] draws the state of the board as given by Board. The drawing is oriented in the same way as the matrix Board." 
DrawLinks::usage="Takes a matrix linksUDRL and draws the links on top of Draw[]." 
DrawTentacles::usage="DrawTentacles[boardInfluence,maxRadius:MaxRadius] draws the influence from boardInfluence."

NeighborsFromGroup::usage="returns {l1,l2} where l1 is a list of coordinates of the adjacent empty neighboors and l2 is a list of groups of w stones which are adjacent to group."
NeighborGroupsFromGroup::usage=" NeighborGroupsFromGroup[{coord1,coord2,..},linkBoard,w] returns {l1,l2} where l1 is a list of coordinates of the adjacent empty neighboors and l2 is a list of groups of w stones which are adjacent to group."
Groups::usage="Groups[1] and Groups[-1] are respectively a list of the white and black groups, with each element in a group being a list of the coordinates of that group."

Board::usage="A matrix where each element is -1,1,0 which respectively represent a black, white and empty."
BoardOfLinksUDRL::usage="a matrix the size of Board where each element is link= {x,y,z,w}, which represents the connected neighboors Up, Down, Left and Right, link={1,0,-1,0} says that above is white, below is empty, left is black and right is empty;"



(* ::Code::Bold:: *)
Begin["`Private`"]

(*Internal variables used in a number of operations*)
	(*StoneProb = 1/2.2;*)
	StoneProb = 0.35;
(*StoneProb = 0.4;*)
	(*StoneProb = \[Alpha];*)
	MaxPathLength = 4;
    MaxPathLength = 5;
	MaxRadius =3; (*For DrawTentacles*) 
	p=4; (*This is the p for the p-norm applied in InfluenceFromPaths for paths that share stones*)	

	CoordToBoard = {#[[2]],LenBoard-#[[1]]+1}&;
	listUDRL = {{-1,0},{1,0},{0,1},{0,-1}};
	LenBoard; 
	Liberties; 
	StoneRadius=0.4;
	(*NoLinksBoard= ConstantArray[-2,{LenBoard,LenBoard,4}];*)
(*Create CoordLinkerToStone*)
	(*CoordLinkerToStone has elements {{a,b},c} where {a,b} is a max of 2 stones away from {0,0} and c is the position in listUDRL which gives the direction of {a,b} towards {0,0}*)
	CoordLinkerToStone=Reap[Sow[#,Length[#]]&/@(DeleteCases[#,{0,0}]&/@Flatten[
		{{#, ({0,Sign[-#[[2]]]}/.Array[ listUDRL[[#]]-> # &,4] )},{#, ({Sign[-#[[1]]],0}/.Array[ listUDRL[[#]]-> # &,4] )}}&/@(Union[Flatten [Outer[Plus,listUDRL,listUDRL,1],1]]~Join~listUDRL)
	,1]),2 ][[2,1]];

	Protect[StoneProb, StoneRadius, MaxPathLength,MaxRadius,p, CoordToBoard, listUDRL, CoordLinkerToStone ];
(*End CoordLinkerToStone*)


(* ::Code::Bold:: *)
(*End Internal variables*)

Draw[opts:OptionsPattern[]]:= Module[{grid,stones,labels},
	grid ={Thick,Black,Line[Flatten[{{{#,1},{#,LenBoard}},{{1,#},{LenBoard,#}}}&/@Range[1,LenBoard],1] ]  };
    labels = Flatten[{Inset[#,{#,0}], Inset[LenBoard - # +1,{0,#}]}&/@Range[1,LenBoard]];
	stones = {Black}~Join~(Disk[CoordToBoard@#,StoneRadius]& /@Position[Board,-1]);
	stones = stones~Join~{Darker[Yellow]}~Join~(Disk[CoordToBoard@#,StoneRadius]& /@Position[Board,1]);
	Return[Graphics[ grid~Join~labels~Join~stones,opts]]
];
DrawLinks[linkUDRL_:BoardOfLinksUDRL, opts:OptionsPattern[]]:=Module[{arrows},
	arrows ={Gray}~Join~Table[ Arrow[{CoordToBoard@#,CoordToBoard[#+listUDRL[[j]] Boole[ linkUDRL[[Sequence@@#]][[j]]!=-2]StoneRadius ]}]
	 ,{j,4}]& /@Flatten[Outer[List,Range[1,LenBoard],Range[1,LenBoard]],1];
	Return[Show[Draw[opts],Graphics[Flatten@arrows]]]
];

DrawTentacles[boardInfluence_, maxRadius_:MaxRadius]:=Module[{d,g,WSlimeMoldBoard,BSlimeMoldBoard,WTentacles,BTentacles},
	WSlimeMoldBoard= Flatten[#,1]&@Reap[Table[ If[boardInfluence[[i,j]]>0, Sow[{boardInfluence[[i,j]],CoordToBoard@{i,j}}] ],{i,LenBoard},{j,LenBoard}]][[2]];
	If[Head@WSlimeMoldBoard=!= List,WSlimeMoldBoard={}]; 
	BSlimeMoldBoard= Flatten[#,1]&@Reap[Table[ If[boardInfluence[[i,j]]<0, Sow[{boardInfluence[[i,j]],CoordToBoard@{i,j}}] ],{i,LenBoard},{j,LenBoard}]][[2]];
	If[Head@BSlimeMoldBoard=!= List,BSlimeMoldBoard={}]; 
	WTentacles = {Lighter[Red]}~Join~(Disk[#[[2]],#[[1]]StoneRadius/maxRadius]& /@WSlimeMoldBoard);
	BTentacles = {Lighter[Blue]}~Join~(Disk[#[[2]],-#[[1]] StoneRadius/maxRadius]& /@BSlimeMoldBoard);
	g=Graphics@(WTentacles~Join~BTentacles);
	d=Draw[];
	Return[Show[g,d]]
];

GenerateRandomBoard[lenBoard_,options_:{} ]:=(
	If[ And@@( NumericQ[#]&/@("ProbabilityWeights"/.options)) === True,    
		Board= RandomChoice[("ProbabilityWeights"/.options)[[1;;3]]->{0,1,-1},{lenBoard,lenBoard}];,
		Board= RandomChoice[{0.6,0.2,0.2}->{0,1,-1},{lenBoard,lenBoard}];
	];
	LoadBoard[];
	(*Print[Draw[]];*)
);
LoadBoard[board_:Board]:= Module[{NoNeighboor},
	Board =board;
	LenBoard= Length[board]; 
	NoNeighboor ={ConstantArray[{-2},LenBoard]};

(*For each element of the matrix LinkedNeighboors is a list of Neighboors {i1 ,i2,i3,i4}, where i1 says what stone is above, i2 what stone is below, i3 what stone to the right, i4 what stone to the left.*)
	BoardOfLinksUDRL= Join[NoNeighboor~Join~Drop[Map[List,Board,{2}],-1],
		Drop[Map[List,Board,{2}],1]~Join~NoNeighboor,
		Transpose[Drop[Map[List,Board,{2}]\[Transpose],1]~Join~NoNeighboor],
		Transpose[NoNeighboor~Join~ Drop[Map[List,Board,{2}]\[Transpose],-1]],3];
	{Groups[1], Groups[-1]} = FormGroups/@Reap[ Table[  Sow[{i,j},Board[[i,j]]],{i,1,LenBoard},{j,1,LenBoard}] ,{1,-1},Sequence@@#2 &][[2]];
];

AdjacentGroup[group_,coord_]:= Intersection [{#[[1]]}&/@Position[group, Alternatives@@((coord+#)&/@listUDRL),2,4]];

FormGroups[Stones_]:=Module[{group={},p},
	If[(p= AdjacentGroup[group,# ]) =={}, AppendTo[group,{#}];,
		Part[group,p[[1,1]]]={#}~Join~Flatten[Extract[group,p],1];
		group=Delete[group, Drop[p,1]];
   ]&/@Stones;
	Return[group]
];

NeighborsFromGroup[group_,linkBoard_,w_]:= Intersection[Flatten[#,1]]&/@
Reap[If[ (#[[1]]== 0)||(#[[1]]== w), Sow[#[[2]],#[[1]]]]&/@
Flatten[ Transpose[{ linkBoard[[ Sequence@@# ]],listUDRL+ConstantArray[#,4]}]&/@group ,1] 
	,{0,w}][[2]];

NeighborGroupsFromGroup[group_,linkBoard_,w_]:=
{#[[1]],Extract[Groups[w], Intersection[{Position[Groups[w],#,2,1][[1,1]]}&/@(#[[2]]) ]]}&@NeighborsFromGroup[group,linkBoard,w];

RemoveLinksToGroup[group_,linkBoard_]:=Module[{links},
	links=Outer[Plus,CoordLinkerToStone\[Transpose][[1]],group,1];
	links=Delete[links,Position[links,0|-1|-2|Alternatives@@group][[All,{1,2}]]];
	links=Flatten[(Flatten/@Thread[#])&/@Transpose[{links,CoordLinkerToStone\[Transpose][[2]]}],1];
	Return[ReplacePart[linkBoard,links->-2]]
];

VirtualPath[linkUDRL_,w_,coord_,pathDistance_,path_]:=Module[{neighboors= NeighborGroupsFromGroup[{coord},linkUDRL,w]},
Sow[If[path=={},{Liberties,{0}},{Liberties StoneProb^pathDistance/pathDistance,path} ], {coord}];
	If[pathDistance<MaxPathLength && coord!={}, 
		With[ {Links=RemoveLinksToGroup[{coord},linkUDRL] },
			VirtualPath[Links,w,#,pathDistance+1., path~Join~{coord}]&/@neighboors[[1]];
			With[{GroupLinks= RemoveLinksToGroup[#,Links ] },
				VirtualPath[GroupLinks,w,#,pathDistance+1.,path~Join~{coord} ]&/@ NeighborsFromGroup[#,GroupLinks,w][[1]];
			] &/@ neighboors[[2]];
		];
	];
];

(*
(* from version which uses AppendTo in VirtualPath*)
VirtualPath[linkUDRL_,w_,coord_,pathDistance_,path_]:=Module[{neighboors= NeighborGroupsFromGroup[{coord},linkUDRL,w]},
	AppendTo[ boardPathGroup[[Sequence@@coord]],If[path=={},{Liberties,{0}},{Liberties StoneProb^pathDistance/pathDistance,path} ]];

	If[pathDistance<MaxPathLength && coord!={}, 
		With[ {Links=RemoveLinksToGroup[{coord},linkUDRL] },
			VirtualPath[Links,w,#,pathDistance+1., path~Join~{coord}]&/@neighboors[[1]];
			With[{GroupLinks= RemoveLinksToGroup[#,Links ] },
				VirtualPath[GroupLinks,w,#,pathDistance+1.,path~Join~{coord} ]&/@ NeighborsFromGroup[#,GroupLinks,w][[1]];
			] &/@ neighboors[[2]];
		];
	];
];*)

InfluenceFromPaths= (Total[#[[1]],2]+ Apply[Plus, (Norm[#,p]&/@#[[2]]) ] )&@Reap[Reap[(Sow@@#)&/@#,0][[2]]]&;

PathsToBoardInfluence[pathBoard_]:= ArrayReshape[InfluenceFromPaths/@Flatten[pathBoard,1],{LenBoard,LenBoard}];

DropRedudantPaths[pathBoard_]:= (list=Sort[pathBoard,(Length@#1[[2]]< Length@#2[[2]] &) ] ;
 If[#!={},First@#,{}] &@Reap[ While[ list!= {}, 
list=( Sow[list[[1]]]; Select[Drop[list,1],(Intersection[list[[1,2]] ,#[[2]]]!=  list[[1,2]] & ) ] )
]][[2]]);
BoardInfluence:= (BoardInfluenceWB[1] + BoardInfluenceWB[-1]); 

BoardInfluenceWB[w_]:=(
	boardPath = ConstantArray[{},{LenBoard,LenBoard}];
	With[ {Links=RemoveLinksToGroup[#,BoardOfLinksUDRL]}, 	
Reap[(Liberties=Length[#]; VirtualPath[Links,w,#,0.,{}]&/@#) &@ NeighborGroupsFromGroup[#,Links,w][[1]];,_,
		(boardPath[[Sequence@@#1 ]]=DropRedudantPaths[#2]~Join~boardPath[[Sequence@@#1 ]] )& ];
	(*	boardPath=ListReshape[ Reap[(Sow[Extract[boardPath, #],{#}];Sow[Extract[ boardPathGroup, #],{#}])&/@ Flatten[Outer[List, Range[LenBoard],Range[LenBoard]] ,1], _,Flatten[#2,1] &][[2]],{LenBoard,LenBoard}];*)
	] &/@Groups[w];
	Return[w PathsToBoardInfluence[boardPath]];
);

(*
(* from version which uses AppendTo in VirtualPath*)
BoardInfluenceWB[w_]:=(
	boardPath = ConstantArray[{},{LenBoard,LenBoard}];
	With[ {Links=RemoveLinksToGroup[#,BoardOfLinksUDRL]}, 
		boardPathGroup = ConstantArray[{},{LenBoard,LenBoard}];
		(Liberties=Length[#]; VirtualPath[Links,w,#,0.,{}]&/@#) &@ NeighborGroupsFromGroup[#,Links,w][[1]];
		
		boardPathGroup=Map[DropRedudantPaths,boardPathGroup,{2}];
		(*(boardPath[[Sequence@@#]]=Extract[boardPath, #]~Join~Extract[ boardPathGroup, #]) &/@ Flatten[Outer[List, Range[LenBoard],Range[LenBoard]] ,1];*)
		boardPath=ListReshape[ Reap[(Sow[Extract[boardPath, #],{#}];Sow[Extract[ boardPathGroup, #],{#}])&/@ Flatten[Outer[List, Range[LenBoard],Range[LenBoard]] ,1], _,Flatten[#2,1] &][[2]],{LenBoard,LenBoard}];
	] &/@Groups[w];
	Return[w PathsToBoardInfluence[boardPath]];
);*)

BoardInfluenceFromGroup[group_,w_,options_:{}]:=(
	boardPathGroup = ConstantArray[{},{LenBoard,LenBoard}];
	Reap[With[ {Links=RemoveLinksToGroup[#,BoardOfLinksUDRL]}, 
		( Liberties=Length[#]; VirtualPath[Links,w,#,0.,{}]&/@#)&@ NeighborGroupsFromGroup[#,Links,w][[1]]; 
	] &@group,_, (boardPathGroup[[Sequence@@#1 ]]=DropRedudantPaths[#2])& ];
	If[("boardPaths"/.options)===True,
		Return[{w PathsToBoardInfluence[boardPathGroup],boardPathGroup}],
		Return[w PathsToBoardInfluence[boardPathGroup]]
	];	
);

(*
(* from version which uses AppendTo in VirtualPath*)
BoardInfluenceFromGroup[group_,w_,options_:{}]:=(
	boardPathGroup = ConstantArray[{},{LenBoard,LenBoard}];
	With[ {Links=RemoveLinksToGroup[#,BoardOfLinksUDRL]}, 
		( Liberties=Length[#]; VirtualPath[Links,w,#,0.,{}]&/@#)&@ NeighborGroupsFromGroup[#,Links,w][[1]]; 
	] &@group;
	boardPathGroup=Map[DropRedudantPaths,boardPathGroup,{2}];
	If[("boardPaths"/.options)===True,
		Return[{w PathsToBoardInfluence[boardPathGroup],boardPathGroup}],
		Return[w PathsToBoardInfluence[boardPathGroup]]
	];
	
);*)




(* ::Code::Bold:: *)
End[];

EndPackage[];
