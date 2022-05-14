(* ::Package:: *)

(* ::Text:: *)
(*Dr. Markus Tiersch and Hendrik Poulsen Nautrup*)
(*Institute for Theoretical Physics*)
(*University of Innsbruck*)


BeginPackage["OpticalElements`"];


SPDCstate::usage="SPDCstate[ max\[CapitalDelta]l] produces a down-converted state with maximum angular momentum max\[CapitalDelta]l.";

toolbox::usage="toolbox[ numberOfModes, max\[CapitalDelta]l] provides the optical elements that can be used for a given number of modes and a maximum angular momentum.";

experimentallyObtainableSchmitRankVectors::usage="experimentallyObtainableSchmitRankVectors[ initialState, experimentalSetup, coincidentCount, triggerMode] computes obtainable Schmidt-Rank Vectors given an experimental setup, initial down-converted state and trigger mode.";

experimentallyObtainableSchmitRankVectorsImproved::usage="experimentallyObtainableSchmitRankVectors[ initialState, experimentalSetup, coincidentCount, triggerMode, numberOfModes] computes obtainable Schmidt-Rank Vectors given an experimental setup, initial down-converted state and trigger mode.";

experimentallyObtainableSchmitRankVectorsImprovedNoRestriction::usage="[ initialState, experimentalSetup, coincidentCount, triggerMode, numberOfModes] computes obtainable Schmidt-Rank Vectors given an experimental setup, initial down-converted state and trigger mode without considering the good representations.";

simplifyExperimentRules::usage="simplifyExperimentRules simplifies a given expression for an optical table according to some simple rules.";

removeHoloRule::usage="removeHoloRule removes all the Holos that are zero.";

performExperiment::usage="performExperiment[ initialState, experimentalSetup] computes the state of an experiment in the mode representation."


(* ::Subchapter:: *)
(*Optical Elements*)


(* ::Section:: *)
(*Initial State*)


SPDCstate[DC_]:=(Sum[mode[1][l]mode[2][-l]+mode[3][l]mode[4][-l],{l,-DC,DC}])^2


(* ::Subsubsection::Closed:: *)
(*Test*)


(* ::Input:: *)
(*SPDCstate[1]*)


(* ::Input:: *)
(*%//Expand*)


(* ::Input:: *)
(*SPDCstate[2]*)


(* ::Section:: *)
(*Optical Elements \[Dash] State Transformation Rules*)


(* ::Subsection:: *)
(*Beam splitter*)


ruleBS[mode1_,mode2_]:={
(* assuming vertical polarisation throughout *)
mode[mode1][l_]->1/Sqrt[2] (mode[mode2][l]+I mode[mode1][-l]),
mode[mode2][l_]->1/Sqrt[2] (mode[mode1][l]+I mode[mode2][-l])
}


(* ::Subsection:: *)
(*Mirror*)


ruleRefl[mode1_]:=
(* assuming vertical polarisation *)
mode[mode1][l_]->I mode[mode1][-l]


(* ::Subsubsection::Closed:: *)
(*Test*)


(* ::Input:: *)
(*mode[a][l]/.ruleRefl[a]*)
(*%/.ruleRefl[a]*)


(* ::Subsection:: *)
(*Hologram*)


ruleHolo[mode1_,\[CapitalDelta]l_]:=mode[mode1][l_]->mode[mode1][l+\[CapitalDelta]l]


(* ::Subsubsection::Closed:: *)
(*Test*)


(* ::Input:: *)
(*SPDCstate[1]*)
(*%/.ruleHolo[1,1]*)


(* ::Subsection:: *)
(*Dove prism*)


ruleDP[mode1_,n_]:=
(* assuming vertical polarisation *)
mode[mode1][l_]->I E^(I \[Pi] l/n) mode[mode1][-l]


(* ::Subsubsection::Closed:: *)
(*Test*)


(* ::Input:: *)
(*SPDCstate[1]*)
(*%/.ruleDP[1]*)


(* ::Input:: *)
(*mode[a][x]/.ruleDP[a,n]*)
(*%/.ruleDP[a,n]*)


(* ::Subsection:: *)
(*Special Element: Parity Sorter*)


(* ::Input:: *)
(*(*ruleLI[mode1_, mode2_] := {*)
(*mode[mode1][l_]\[Rule]-Abs[Sin[l*Pi/2]]*mode[mode1][l]-\[ImaginaryI]*Abs[Cos[l*Pi/2]]*mode[mode2][-l],*)
(*mode[mode2][l_]\[Rule] Abs[Sin[l*Pi/2]]*mode[mode2][l]-\[ImaginaryI]*Abs[Cos[l*Pi/2]]mode[mode1][-l]*)
(*}*)*)


(* ::Subsubsection::Closed:: *)
(*Test*)


(* ::Input:: *)
(*(*mode[a][0]*mode[b][1]*)
(*%/.ruleLI[a,b]*)*)


(* ::Section:: *)
(*Experiment Building Blocks \[Dash] "Toolbox"*)


(* ::Text:: *)
(*Our toolbox is the collection of all concrete optical elements for a given number of modes and maximal hologram shift in the experiment.*)


toolbox[numberOfModes_,max\[CapitalDelta]l_, DPphase_]:=Join[
(* all combinations of beam splitter to any two modes *)
BS@@@Subsets[Range[numberOfModes],{2}],
(* all possible mirrors *)
Refl/@Range[numberOfModes],
(* all possible holograms *)
Outer[Holo,Range[numberOfModes],Join[#,-#]&@Range[1,max\[CapitalDelta]l]]//Flatten,
(* all possible Dove prisms *)
DP@@@Tuples[{Range[numberOfModes], Range[DPphase]}](*,
(*all possible parity checks to any two modes*)
LI@@@Subsets[Range[numberOfModes],{2}]*)
]


(* ::Text:: *)
(*The following replacement rule turns an experiment descript in terms of optical elements into the actual sequence of replacement rules of these specific optical elements.*)


generateToolboxRules={BS->ruleBS,Refl->ruleRefl,Holo->ruleHolo,DP->ruleDP(*, LI\[Rule] ruleLI*)};


(* ::Text:: *)
(*Calculate final state for a given initial state and experimental setup.*)


performExperiment[initialState_,experimentalSetup_]:=Fold[ReplaceAll,initialState,experimentalSetup/.generateToolboxRules]


(* ::Subsubsection:: *)
(*Test*)


(* ::Input:: *)
(*toolbox[4,2]*)


(* ::Text:: *)
(*A random 10-element experiment:*)


(* ::Input:: *)
(*RandomChoice[toolbox[4,2],10]*)


(* ::Input:: *)
(*%/.generateToolboxRules*)


(* ::Text:: *)
(*Final state generated in this random experiment:*)


(* ::Input:: *)
(*performExperiment[SPDCstate[1],%]*)


(* ::Section:: *)
(*State analysis*)


Begin[ "Private`"]


(* ::Subsection:: *)
(*Mode List and Count*)


modeList[state_]:=(Head/@Variables[state])//DeleteDuplicates
modeCount[state_]:=Length[modeList[state]]


(* ::Subsubsection:: *)
(*Tests*)


(* ::Input:: *)
(*modeList[%22]*)
(*modeCount[%22]*)


(* ::Subsection:: *)
(*Coincidence projection*)


(* ::Text:: *)
(*For a state in its expanded form (a sum) postselect only those terms that have the right number of modes.*)


projectCoincidences[state_,numberOfCoincidences_]:=Select[state,(((Variables[#]~DeleteDuplicatesBy~Head (* takes only "mode[number]" part *))//Length)==numberOfCoincidences)&]


(* ::Subsubsection::Closed:: *)
(*Test*)


(* ::Input:: *)
(*SPDCstate[1]*)


(* ::Input:: *)
(*SPDCstate[1]//Expand*)


(* ::Input:: *)
(*projectCoincidences[%,4]*)


(* ::Subsection:: *)
(*Trigger basis states*)


(* find all non-tivial trigger basis states in a given mode
	-- lists all OAM basis states for that mode *)
triggerSpace[state_,triggerMode_]:=Select[Variables[state],Head[#]==mode[triggerMode]&]


(* ::Subsubsection::Closed:: *)
(*Test*)


(* ::Input:: *)
(*triggerSpace[%31,1]*)


(* ::Subsection:: *)
(*Trigger states \[Dash] currently heuristic and simplified to get going*)


generateTriggerStates[triggerSpaceBasis_]:=
(* currently restricted to basis states and special superpositions *)
Plus@@@Subsets[triggerSpaceBasis,{1,2}]


(* ::Subsubsection::Closed:: *)
(*Test*)


(* ::Input:: *)
(*generateTriggerStates[%36]*)


(* ::Subsection:: *)
(*Trigger projection*)


projectTrigger[state_,triggerState_,triggerSpaceBasis_]:=Module[{
projectionRuleTriggerState,
projectionRuleRest
},
projectionRuleTriggerState=(#->Coefficient[triggerState,#]\[Conjugate]) &/@Variables[triggerState];
projectionRuleRest=Thread[triggerSpaceBasis->0];
(*Print[projectionRuleTriggerState]*);
(*Print[projectionRuleRest]*);
state/.projectionRuleTriggerState/.projectionRuleRest
]


(* ::Subsubsection::Closed:: *)
(*Test*)


(* ::Input:: *)
(*projectTrigger[*)
(*1/Sqrt[2] (mode[1][-2]mode[2][-2]+mode[1][0]mode[2][0]+mode[1][2]mode[2][1]),*)
(*1/2 mode[1][-2]-Sqrt[3/4]mode[1][0],*)
(*{mode[1][-2],mode[1][0],mode[1][2]}*)
(*]==1/(2Sqrt[2]) mode[2][-2]-Sqrt[3]/(2Sqrt[2]) mode[2][0]//Simplify*)
(**)


(* ::Input:: *)
(*projectTrigger[%34,mode[1][0],{mode[1][0],mode[1][1],mode[1][-1]}]*)


(* ::Section:: *)
(*Schmidt-rank vector*)


(* ::Subsection:: *)
(*Hilbert space vector*)


(* ::Text:: *)
(*transform state description in terms of modes to an actual column vector*)


(* ::Subsubsection::Closed:: *)
(*old implementation for reference*)


(* ::Input:: *)
(*(* earlier, not used implementation, kept for reference *)*)
(*RewriteToHilbertSpaceVector[state_]:=Module[{*)
(*basisStates=Variables[state]//Sort,*)
(*subsystems=modeList[state]//Sort,*)
(*modeNames,*)
(*subsystemCount=modeCount[state],*)
(*subsystemBasisStates,*)
(*subsystemDimensions,*)
(*subsystemBasisVectors,*)
(*OAMvariableNames,*)
(*modeToBasisVectorRule,*)
(*basisVectorToProductRule*)
(*},*)
(*OAMvariableNames=Array["l"<>ToString[#]&,subsystemCount];*)
(*modeNames=First/@subsystems;*)
(*(* the following rule assumes that all modes are present in each term *)*)
(*modeToBasisVectorRule=Dispatch[ReleaseHold[*)
(*Inner[#1@#2&,subsystems,Hold[ToExpression[#<>"_"]]&/@OAMvariableNames,Times]->Inner[basisVector,modeNames,ToExpression/@OAMvariableNames,KroneckerProduct]*)
(*]];*)
(*(*If[!FreeQ[modeToBasisVectorRule,KroneckerProduct[]],Print[state];Abort[]];*)*)
(*(*Print[modeToBasisVectorRule];*)*)
(**)
(*subsystemBasisStates=GatherBy[basisStates,Head];*)
(*subsystemDimensions=Length/@subsystemBasisStates;*)
(**)
(*(*Print[subsystemBasisStates];*)*)
(*subsystemBasisVectors=subsystemBasisStates/.{mode[m_][l_]->basisVector[m,l]};*)
(*(*Print[subsystemBasisVectors];*)*)
(*basisVectorToProductRule=Thread[*)
(*Flatten[subsystemBasisVectors,1]->Flatten[IdentityMatrix/@subsystemDimensions,1]*)
(*];*)
(*(*Print[basisVectorToProductRule];*)*)
(**)
(*{(state/.modeToBasisVectorRule/.basisVectorToProductRule)//Flatten,subsystemDimensions}*)
(*]*)


(* ::Subsubsection:: *)
(*used implementation*)


HilbertSpaceVector[state_]:=Module[{
basisStates=Variables[state]//Sort,
subsystemDimensions,
subMatrixRanges,
vector
},
(*Print[basisStates];*)
(* count how often mode[1][_], mode[2][_], ... appears in the state *)
subsystemDimensions=Tally[Head/@basisStates][[All,2]];
(*Print[subsystemDimensions];*)

(* a columns vector for the given state in mode description can be obtained *)
(* from the coefficient array of the mode description (a polynomial) as follows: *)
(* the relevant coefficient array (of highest degree) is only populated in a subarray *)
(* that subarray can be extracted as follows *)
(* for ordered variable names and subsystem dimensions d1,d2,... *)
(* take the first d1 elements, of each of those elements take the next d2 elements starting from d1+1, and so on *)
(* example: subsystem dimensions 3,2,2 give a 7\[Times]7\[Times]7 array, *)
(*          of which the part \[LeftDoubleBracket];;3,4;;5,5;;6\[RightDoubleBracket] is linear in each subsytem and thus relevant *)
(* flattening that subarray gives a vector in the usual tensor representation *)
subMatrixRanges=MovingMap[{1,0}+#&,Prepend[Accumulate@subsystemDimensions,0],1];
(*Print[subMatrixRanges];*)

vector=Take[CoefficientArrays[state,basisStates][[-1]],Sequence@@subMatrixRanges]//Flatten;
{vector,subsystemDimensions}
]


(* ::Subsubsection::Closed:: *)
(*Tests*)


(* ::Input:: *)
(*HilbertSpaceVector[mode[1][-1]mode[2][-2]-mode[1][2]mode[2][2]]//Normal*)


(* ::Input:: *)
(*HilbertSpaceVector[mode[1][0]mode[2][1]-mode[1][2]mode[2][2]]=={{1,0,0,-1},{2,2}}*)
(*HilbertSpaceVector[mode[1][0]mode[2][1]-mode[1][2]mode[2][0]]=={{0,1,-1,0},{2,2}}*)
(*HilbertSpaceVector[mode[1][0]mode[2][1]-mode[1][2](mode[2][0]+mode[2][3])//Expand]=={{0,1,0,0,0,0}-{0,0,0,1,0,1},{2,3}}*)


(* ::Input:: *)
(*HilbertSpaceVector[mode[1][-1]mode[2][-2]-mode[1][2]mode[2][2]]=={{1,0,0,-1},{2,2}}*)


(* ::Input:: *)
(*HilbertSpaceVector[*)
(*mode[1][1]mode[2][1]mode[3][1]+*)
(*mode[1][2]mode[2][2]mode[3][2]+*)
(*mode[1][3]mode[2][2]mode[3][2]]=={{1,0,0,0,0,0,0,1,0,0,0,1},{3,2,2}}*)


(* ::Input:: *)
(*HilbertSpaceVector[*)
(*mode[1][1]mode[2][1]mode[3][1]+*)
(*mode[1][2]mode[2][1]mode[3][1]+*)
(*mode[1][3]mode[2][2]mode[3][2]]=={{1,0,0,0,1,0,0,0,0,0,0,1},{3,2,2}}*)


(* ::Input:: *)
(*HilbertSpaceVector[*)
(*mode[1][1]mode[2][1]mode[3][1]+*)
(*mode[1][2]mode[2][2]mode[3][2]+*)
(*mode[1][3]mode[2][3]mode[3][3]]=={{1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1},{3,3,3}}*)


(* ::Subsection:: *)
(*Partial Trace*)


(* ::Text:: *)
(*The present implementation of the partial trace uses a nested matrix (matrix of matrices) to represent the indices of each tensor factor. The trace fuction is then applied directly to the tensor factor that should be tranced out.*)
(**)
(*TODO: Check applicability of the BlockMap function or similar variants in partialTr implementation for improved performance.*)


partialTr::usage="partialTr[squareMatrix,subsystemDimensions,subsystem] returns the matrix with the subsystem traced out.\n
partialTr[squareMatrix,subsystemDimensions,subsystemList] returns the matrix with all subsystems in subsystemList traced out.";

nestedBlockMatrix[m_,dimensions_]:=Fold[Partition[#1,#2{1,1}]&,m,Rest[dimensions]//Reverse]
nestedBlockMatrixTr[m_,level_]:=Map[Tr[#,Plus,2]&,m,{2(level-1)}]

Off[Assert] (* turn on for argument checking during evaluation, useful for debugging purposes *)

partialTr[matrix_?SquareMatrixQ,subsystemDimensions_?ListQ,subsystem_?IntegerQ]:=Module[{m},
(* check if the matrix dimensions are compatible with the Hilbert space dimentions *)
Assert[Dimensions[matrix]=={1,1}*Times@@subsystemDimensions];

m=nestedBlockMatrix[matrix,subsystemDimensions];
nestedBlockMatrixTr[m,subsystem]//ArrayFlatten
]

partialTr[matrix_?SquareMatrixQ,subsystemDimensions_?ListQ,subsystemList_?ListQ]:=Module[{m},
(* check if the matrix dimensions are compatible with the Hilbert space dimentions *)
Assert[Dimensions[matrix]=={1,1}*Times@@subsystemDimensions];
(* check if each subsystem to be traced out appears only once in the list *)
Assert[DuplicateFreeQ[subsystemList]];

m=nestedBlockMatrix[matrix,subsystemDimensions];
Fold[
nestedBlockMatrixTr,m,Sort[subsystemList,Greater]
]//ArrayFlatten
]


(* ::Subsubsection::Closed:: *)
(*Tests*)


(* ::Input:: *)
(*(Array[m,(2*3)^2]~Partition~(2*3))//MatrixForm*)
(*Dimensions[%]*)
(*partialTr[Array[m,(2*3)^2]~Partition~(2*3),{2,3},1]//MatrixForm*)
(*Dimensions[%]*)
(*partialTr[Array[m,(2*3)^2]~Partition~(2*3),{2,3},2]//MatrixForm*)
(*Dimensions[%]*)
(*partialTr[Array[m,(3*2)^2]~Partition~(3*2),{3,2},1]//MatrixForm*)
(*Dimensions[%]*)
(*partialTr[Array[m,(3*2)^2]~Partition~(3*2),{3,2},2]//MatrixForm*)
(*Dimensions[%]*)


(* ::Input:: *)
(*(Array[m,(2*2*2)^2]~Partition~(2*2*2))//MatrixForm*)
(*Dimensions[%]*)
(*partialTr[Array[m,(2*2*2)^2]~Partition~(2*2*2),{2,2,2},{1,2}]//MatrixForm*)
(*Dimensions[%]*)
(*partialTr[Array[m,(2*2*2)^2]~Partition~(2*2*2),{2,2,2},{2,3}]//MatrixForm*)
(*Dimensions[%]*)
(*%%==partialTr[Array[m,(2*2*2)^2]~Partition~(2*2*2),{2,2,2},{3,2}]//MatrixForm*)
(*partialTr[Array[m,(2*2*2)^2]~Partition~(2*2*2),{2,2,2},{1,3}]//MatrixForm*)
(*Dimensions[%]*)


(* ::Subsection:: *)
(*Reduced States*)


(* ::Text:: *)
(*All reduced density matrixes obtainable from tracing out a single subsystem:*)


reducedStates[matrix_?SquareMatrixQ,subsystemDimensions_?ListQ]:=
With[{subsystemCount=Length[subsystemDimensions]},partialTr[matrix,subsystemDimensions,#]&/@Reverse@Subsets[Range[subsystemCount],{subsystemCount-1}]
]


(* ::Subsection:: *)
(*Schmidt-rank vector*)


SchmidtRankVector[\[Psi]_?ListQ,subsystemDimensions_?ListQ]:=Module[{
\[Rho],
subsystemCount=Length[subsystemDimensions]
},
(* check if state vector \[Psi] is compatible with the Hilbert space dimension *)
Assert[Length[\[Psi]]==Times@@subsystemDimensions];

\[Rho]=KroneckerProduct[\[Psi],\[Psi]\[Conjugate]];
MatrixRank/@(partialTr[\[Rho],subsystemDimensions,#]&/@Reverse@Subsets[Range[subsystemCount],{subsystemCount-1}])
]


(* ::Subsubsection::Closed:: *)
(*Tests*)


(* ::Input:: *)
(*mode[1][0]mode[2][0]+mode[1][1]mode[2][1]*)
(*HilbertSpaceVector[%]//Normal*)
(*SchmidtRankVector@@%*)


(* ::Input:: *)
(*mode[1][0]mode[2][0]+mode[1][1]mode[2][1]-mode[1][2]mode[2][-2]*)
(*HilbertSpaceVector[%]//Normal*)
(*SchmidtRankVector@@%*)


(* ::Input:: *)
(*mode[1][0]mode[2][0]+mode[1][1]mode[2][1]+mode[1][0]mode[2][2]*)
(*HilbertSpaceVector[%]//Normal*)
(*SchmidtRankVector@@%*)


(* ::Input:: *)
(*mode[1][0]mode[2][0]mode[3][0]+mode[1][1]mode[2][1]mode[3][1]+mode[1][2]mode[2][2]mode[3][2]*)
(*HilbertSpaceVector[%]//Normal*)
(*SchmidtRankVector@@%*)


(* ::Input:: *)
(*mode[1][0]mode[2][0]mode[3][0]+mode[1][1]mode[2][1]mode[3][1]+mode[1][0]mode[2][2]mode[3][2]*)
(*HilbertSpaceVector[%]//Normal*)
(*SchmidtRankVector@@%*)


(* ::Section:: *)
(*Experiment Simplification Rules*)


(* ::Subsection:: *)
(*Combine Holograms*)


simplifyHoloRule={
{x___,PatternSequence[Holo[a_,l1_],Holo[a_,l2_]],y___}->{x,Holo[a,l1+l2],y}
};

removeHoloRule={
{x___, Holo[a_,0], y___}->{x, y}
};


(* ::Subsubsection::Closed:: *)
(*Test*)


(* ::Input:: *)
(*{BS[a,b],Holo[a,2],Holo[a,3],BS[a,c]}/.simplifyHoloRule*)
(*{BS[a,b],Holo[a,2],Holo[b,3],BS[a,c]}/.simplifyHoloRule*)


(* ::Subsection:: *)
(*Combine Beam Splitters*)


simplifyBSRule={{x___,PatternSequence[BS[a_,b_],BS[a_,b_]],y___}->{x,y}};


(* ::Subsubsection::Closed:: *)
(*Test*)


(* ::Input:: *)
(*{BS[a,b],Holo[2,2],BS[a,b],BS[a,b],Holo[2,3],BS[a,c]}/.simplifyBSRule*)
(*{BS[a,b],Holo[2,2],BS[a,b],BS[a,b],Holo[2,3],BS[a,c]}//.Join[simplifyHoloRule,simplifyBSRule]*)


(* ::Subsection:: *)
(*Bring elements into canonical order*)


(* ::Text:: *)
(*In an optical experiment individual element are placed into beam paths. Within one beam path the order of elements matters, in general. There is, however, no order between elements in different beam paths. Here, when using a string to list the order of elements in the experiment we simply order elements by mode number whenever their order is not fixed.*)


canonicalOrderRules={
(* 2 one-mode elements in different modes are sorted by their mode number *)
{x___,PatternSequence[element1_[a_,p___],element2_[b_,q___]],y___}/;
(a>b&&MatchQ[element1,Refl|Holo|DP]&&MatchQ[element2,Refl|Holo|DP])
->{x,element2[b,q],element1[a,p],y},
(* one-mode element and beam splitter in different modes are sorted by the (first) mode number *)
{x___,PatternSequence[BS[a_,b_],element2_[c_,p___]],y___}/;
(a>c&&b!=c&&MatchQ[element2,Refl|Holo|DP])
->{x,element2[c,p],BS[a,b],y},
{x___,PatternSequence[element1_[a_,p___],BS[b_,c_]],y___}/;
(a>b&&a!=c&&MatchQ[element1,Refl|Holo|DP])
->{x,BS[b,c],element1[a,p],y},
(* two beam splitters in disjoint set of modes the lower mode number *)
{x___,PatternSequence[BS[a_,b_],BS[c_,d_]],y___}/;
(Intersection[{a,b},{c,d}]=={}&&Min[a,b]>Min[c,d])
->{x,BS[c,d],BS[a,b],y}
};


(* ::Subsubsection::Closed:: *)
(*Tests*)


(* ::Input:: *)
(*{Refl[1],Refl[2],Refl[3]}=={Refl[3],Refl[2],Refl[1]}//.canonicalOrderRules*)
(*{Holo[1,2],Holo[2,-2],Holo[3,2]}=={Holo[3,+2],Holo[2,-2],Holo[1,+2]}//.canonicalOrderRules*)
(*{DP[1,2],DP[2],DP[3]}=={DP[3],DP[2],DP[1,2]}//.canonicalOrderRules*)


(* ::Input:: *)
(*{BS[1,2],Holo[3,+2],DP[2,-2],Refl[1],DP[3],Holo[1,-3]}//.canonicalOrderRules*)


(* ::Input:: *)
(*{Holo[3,-2],DP[2,2],BS[2,3],Refl[1],Holo[1,1]}//.canonicalOrderRules*)


(* ::Input:: *)
(*{BS[3,4],BS[1,2],BS[4,3],BS[2,1],BS[2,4],BS[3,1]}//.canonicalOrderRules*)


(* ::Subsection:: *)
(*All Simplifciation Rule*)


simplifyExperimentRules=Join[simplifyBSRule,simplifyHoloRule,removeHoloRule,canonicalOrderRules];


(* ::Section:: *)
(*Experiment Analysis*)


(* ::Subsection:: *)
(*Representation testing*)


(* ::Text:: *)
(*When written in the OAM product basis a good state has coefficients with equal moduli.*)


(* ::Input:: *)
(*(* old implementation based on polynome analysis *)(* goodRepresentation[state_]:=Equal@@Abs[CoefficientRules[state,Sort@Variables@state]\[LeftDoubleBracket]All,2\[RightDoubleBracket]] *)*)


(*goodRepresentation[state_, SRV_]:=And[Equal@@Abs[(List@@(Expand@state))/.mode[_][_]->1], Length[MonomialList@state]== Max[SRV]]*)
goodRepresentationFast[state_, triggerMode_, numberOfModes_]:= And[Equal@@Abs[(List@@(Expand@state))/.mode[_][_]->1], Length[MonomialList@state]==Max@Table[state//Cases[#, _ mode[a][x_]-> x]&//DeleteDuplicates//Length,{a, Range[numberOfModes]//DeleteCases[#, triggerMode]&}]]
goodRepresentation[state_, SRV_]:=Length[MonomialList@state]== Max[SRV]


(* ::Subsubsection:: *)
(*Tests*)


(* ::Input:: *)
(*goodRepresentation[-mode[1][1]mode[2][1]mode[3][1]+E^(-I \[Pi]/2) mode[1][2]mode[2][1]mode[3][1]+2mode[1][3]mode[2][2]mode[3][2], {3,3,1}]==False*)


(* ::Input:: *)
(*goodRepresentation[mode[1][1]mode[2][1]mode[3][1]+E^(-I \[Pi]/2) mode[1][2]mode[2][1]mode[3][1]-1/Sqrt[2] mode[1][3]mode[2][2]mode[3][2]+I/Sqrt[2] mode[1][3]mode[2][2]mode[3][2], {3,3,1}]==True*)


(* ::Subsection:: *)
(*Calculate Schmidt rank vectors*)


SchmidtRankClasses[vectors_]:=(Sort[Delete[#, Position[#, Null]]&/@vectors,Greater])//DeleteDuplicates;



experimentallyObtainableSchmitRankVectors[initialState_,experimentalSetup_,coincidentCount_,triggerMode_]:=Module[{
finalState,
triggerBasisStates,
triggerStates,
triggeredFinalStates,
srvList,
goodRepresentationList,
goodTriggeredFinalStates
},
finalState=performExperiment[initialState,experimentalSetup]//Expand;
(*Print[finalState];*)(*this has weights*)
finalState=projectCoincidences[finalState,coincidentCount];
(*Print[finalState];*)
triggerBasisStates=triggerSpace[finalState,triggerMode];
triggerStates=generateTriggerStates[triggerBasisStates]; (* this one is a heavily simplified function *)

triggeredFinalStates=(projectTrigger[finalState,#,triggerBasisStates]&/@triggerStates)~DeleteCases~0;
(*Print[triggeredFinalStates];*)

goodTriggeredFinalStates=Select[triggeredFinalStates, Equal@@Abs[(List@@(Expand@#))/.mode[_][_]->1]&];


srvList=(SchmidtRankVector@@Normal@HilbertSpaceVector[#])&/@goodTriggeredFinalStates;
(*Print[srvList];*)

goodRepresentationList=Select[{goodTriggeredFinalStates,srvList}//Transpose,goodRepresentation@@#&][[All,2]];
(*Print[goodRepresentationList];*)

(*{*)SchmidtRankClasses[goodRepresentationList(*[[All,2]]*)](*, SchmidtRankClasses[srvList]}[[1]]*)
](*place SRV with triggeredfinalstates*)


experimentallyObtainableSchmitRankVectorsImproved[initialState_,experimentalSetup_,coincidentCount_,triggerMode_, numberOfModes_]:=Module[{
finalState,
triggerBasisStates,
triggerStates,
triggeredFinalStates,
srvList,
goodRepresentationList,
goodTriggeredFinalStates
},
finalState=performExperiment[initialState,experimentalSetup]//Expand;
(*Print[finalState];*)(*this has weights*)
finalState=projectCoincidences[finalState,coincidentCount];
(*Print[finalState];*)
triggerBasisStates=triggerSpace[finalState,triggerMode];
triggerStates=generateTriggerStates[triggerBasisStates]; (* this one is a heavily simplified function *)

triggeredFinalStates=(projectTrigger[finalState,#,triggerBasisStates]&/@triggerStates)~DeleteCases~0;
(*Print[triggeredFinalStates];*)

goodTriggeredFinalStates=Select[triggeredFinalStates, goodRepresentationFast[#, triggerMode, numberOfModes]&];
(*Print[goodTriggeredFinalStates];*)
(*Print[goodTriggeredFinalStates\[Equal]triggeredFinalStates];*)

srvList=(SchmidtRankVector@@Normal@HilbertSpaceVector[#])&/@goodTriggeredFinalStates;
(*Print[srvList];*)

goodRepresentationList=Select[{goodTriggeredFinalStates,srvList}//Transpose,goodRepresentation@@#&][[All,2]];
(*Print[goodRepresentationList];*)

(*{*)SchmidtRankClasses[goodRepresentationList(*[[All,2]]*)](*, SchmidtRankClasses[srvList]}[[1]]*)
](*place SRV with triggeredfinalstates*)


experimentallyObtainableSchmitRankVectorsImprovedNoRestriction[initialState_,experimentalSetup_,coincidentCount_,triggerMode_, numberOfModes_]:=Module[{
finalState,
triggerBasisStates,
triggerStates,
triggeredFinalStates,
srvList,
goodRepresentationList,
goodTriggeredFinalStates
},
finalState=performExperiment[initialState,experimentalSetup]//Expand;
(*Print[finalState];*)(*this has weights*)
finalState=projectCoincidences[finalState,coincidentCount];
(*Print[finalState];*)
triggerBasisStates=triggerSpace[finalState,triggerMode];
triggerStates=generateTriggerStates[triggerBasisStates]; (* this one is a heavily simplified function *)

triggeredFinalStates=(projectTrigger[finalState,#,triggerBasisStates]&/@triggerStates)~DeleteCases~0;
(*Print[triggeredFinalStates];*)

(*goodTriggeredFinalStates=Select[triggeredFinalStates, goodRepresentationFast[#, triggerMode, numberOfModes]&];*)
goodTriggeredFinalStates=triggeredFinalStates;
(*Print[goodTriggeredFinalStates];*)
(*Print[goodTriggeredFinalStates\[Equal]triggeredFinalStates];*)

srvList=(SchmidtRankVector@@Normal@HilbertSpaceVector[#])&/@goodTriggeredFinalStates;
(*Print[srvList];*)

(*goodRepresentationList=Select[{goodTriggeredFinalStates,srvList}//Transpose,goodRepresentation@@#&][[All,2]];*)
goodRepresentationList=srvList;
(*Print[goodRepresentationList];*)

(*{*)SchmidtRankClasses[goodRepresentationList(*[[All,2]]*)](*, SchmidtRankClasses[srvList]}[[1]]*)
](*place SRV with triggeredfinalstates*)


(* ::Subsubsection:: *)
(*Test*)


(* ::Input:: *)
(*experimentallyObtainableSchmitRankVectors[SPDCstate[1],{},4,1]*)


(* ::Input:: *)
(*experimentallyObtainableSchmitRankVectors[SPDCstate[1],{Holo[2,2],BS[2,3]},4,1]*)


(* ::Input:: *)
(*experimentallyObtainableSchmitRankVectors[SPDCstate[1],{Holo[1,2],Refl[4],DP[3],DP[3],Holo[2,1],BS[1,4],BS[1,2]},4,1]*)


(* ::Input:: *)
(*(*experimentallyObtainableSchmitRankVectors[SPDCstate[1],{LI[1,4]},4,1]*)*)


(* ::Input:: *)
(*Timing[experimentallyObtainableSchmitRankVectors[initialState,{Holo[1,-2],BS[2,4],DP[2],Refl[4],BS[2,4]},coincidentCount,triggerMode]];*)
(*Timing[experimentallyObtainableSchmitRankVectorsImproved[initialState,{Holo[1,-2],BS[2,4],DP[2],Refl[4],BS[2,4]},coincidentCount,triggerMode, numberOfModes]];*)


(* ::Subsection:: *)
(*End*)


End[];

EndPackage[];
