(* ::Package:: *)

(* ::Input::Initialization:: *)
SetDirectory[NotebookDirectory[]];
BeginPackage["GymOpticalTables`", {"OpticalElements`", "GeneralUtilities`"}];
Begin["`Private`"];


(* ::Input::Initialization:: *)
GymOpticalTables::usage="
This is a standardized gym environment for the creation of high-dimensional multipartite entanglement in simulated optical experiments.
Create the environment as `env = DeviceOpen[\"GymOpticalTables\", parameters];`
Standard parameters are {\"MaxL\" -> 2, \"DPphase\" -> 1,  \"NumberOfModes\" -> 4, \"InitialSPDC\" -> 1, \"CoincidenceCount\" -> 4, \"TriggerMode\" -> 1, \"EpisodeLength\" -> 12}.
This is built and tested on Mathematica 13.0.
";


(* ::Section:: *)
(*Environment*)


(* ::Text:: *)
(*This environment is created analogously to `SimulatedCartPole` environment, which can be found on your device here:*)


(* ::Input:: *)
(*FindFile["ReinforcementLearning`"]*)


(* ::Text:: *)
(*To go to the device registration use the following command:*)


(* ::Input:: *)
(*DeviceFramework`DeviceDriverLoad["SimulatedCartPole"]//SystemOpen*)


(* ::Subsection:: *)
(*Environment methods*)


(* ::Input::Initialization:: *)
If[!AssociationQ[$OpticalTableEnvironments], $OpticalTableEnvironments = <||>];


(* ::Subsubsection:: *)
(*Start method*)


(* ::Input::Initialization:: *)
deviceEnvCreate[uuid_] := deviceEnvCreate[uuid, {}]
deviceEnvCreate[uuid_, a__] := CatchFailureAsMessage @ Module[
{state = {},
hiddenState,
ended = False,
args = Association[a],
opts,
toolBox
	},

(*Add options*)
opts = <||>;
opts = Join[opts, args];
If[MissingQ[opts["MaxL"]] , opts["MaxL"] = 2];
If[MissingQ[opts["DPphase"]], opts["DPphase"] = 1];
If[MissingQ[opts["NumberOfModes"]], opts["NumberOfModes"] = 4];
If[MissingQ[opts["InitialSPDC"]], opts["InitialState"] = SPDCstate[1], opts["InitialState"] = SPDCstate[opts["InitialSPDC"]]];
If[MissingQ[opts["EpisodeLength"]], opts["EpisodeLength"] = 12];
If[MissingQ[opts["CoincidenceCount"]], opts["CoincidenceCount"] = 4];
If[MissingQ[opts["TriggerMode"]], opts["TriggerMode"] = 1];

toolBox = toolbox[opts["NumberOfModes"], opts["MaxL"],opts["DPphase"]];
hiddenState = opts["InitialState"];

(* all external (and internal??) environment variables *)
$OpticalTableEnvironments[uuid] = <|
"Parameters" -> <||>,
"EpisodeLength" -> opts["EpisodeLength"],
"State" -> state,
"Ended" -> ended,
"ObservedState" -> encodeObservation[state, opts["EpisodeLength"]],
"HiddenState" -> hiddenState, (*TODO: Use this to speed up SRV calculation*)
"HiddenInitialState" -> opts["InitialState"],
"CurrentStep" -> 0,
"Toolbox" -> toolBox,
"CoincidenceCount" -> opts["CoincidenceCount"],
"TriggerMode" -> opts["TriggerMode"],
"NumberOfModes" -> opts["NumberOfModes"],
"MaxL" -> opts["MaxL"],
"DPphase" -> opts["DPphase"]
|>;

(* need to return UUID else DeviceFramework`DeviceHandle doesn't work *)
uuid
]


(* ::Subsubsection:: *)
(*Close method*)


(* ::Input::Initialization:: *)
deviceEnvClose[{id_, _}] := KeyDropFrom[$OpticalTableEnvironments, id]


(* ::Subsubsection:: *)
(*Read method*)


(* ::Text:: *)
(*The observed state is an array of lists that encode optical elements. One can use the conversion methods below to convert to and from the observation encoding.*)


(* ::Input::Initialization:: *)
deviceEnvRead[{id_, _}] := <|
	"ObservedState" -> $OpticalTableEnvironments[id, "ObservedState"], 
	"Ended" -> $OpticalTableEnvironments[id, "Ended"]
|>

DeviceFramework`Devices`GymOpticalTables::invdevread := "DeviceRead only supports a single argument."
deviceEnvRead[{id_, _}, {__}] := 
	(Message[DeviceFramework`Devices`GymOpticalTables::invdevread]; $Failed)


(* ::Subsubsection:: *)
(*Reset method*)


(* ::Input::Initialization:: *)
deviceEnvExecute[{id_, _}, "Reset"] := CatchFailure @ Scope[
$OpticalTableEnvironments[id, "State"] = {};
$OpticalTableEnvironments[id, "ObservedState"] = encodeObservation[{}, env["EpisodeLength"]];
$OpticalTableEnvironments[id, "HiddenState"] = $OpticalTableEnvironments[id, "HiddenInitialState"] ;
$OpticalTableEnvironments[id, "Ended"] = False;
$OpticalTableEnvironments[id, "CurrentStep"] = 0;
	<|"ObservedState" -> $OpticalTableEnvironments[id, "ObservedState"]|>
]


(* ::Subsubsection:: *)
(*Step method*)


(* ::Input::Initialization:: *)
DeviceFramework`Devices`GymOpticalTables::invaction := "Invalid action ``."

deviceEnvExecute[{id_, _}, "Step", arg__] := CatchFailure @ Scope[

  $OpticalTableEnvironments[id, "CurrentStep"] ++;
	env = Lookup[$OpticalTableEnvironments, id];
	
(*Check for valid action and make arg into an integer/list of integers,
	 as appropriate*)
 action = arg + 1; (*Mathematica counts from 1 instead of 0.*)
	If[!MemberQ[Range[Length[$OpticalTableEnvironments[id, "Toolbox"]]], action], 
		ThrowFailure[DeviceFramework`Devices`GymOpticalTables::invaction, arg]
	];

	(*Update state and get reward*)
  update = opticalTablesUpdate[env, action];
	(*need to do the following to each entry of the list*)
	newState = update["State"];
  observedState = encodeObservation[newState, env["EpisodeLength"]];
	reward = update["Reward"];
	info = <|"SRVs" -> update["Info"]|>;
  
 (*Check whether max episode length is reached.*)
  If[env["CurrentStep"] >= env["EpisodeLength"], done = True, done = False];

	(* update state *)
         $OpticalTableEnvironments[id, "State"] = newState;
	$OpticalTableEnvironments[id, "Ended"] = done;
  $OpticalTableEnvironments[id, "ObservedState"] = observedState;
	<|"ObservedState" -> observedState, "Ended" -> done, "Reward" -> reward, "Info" -> info|>
]


(* ::Subsubsection:: *)
(*Get random action method*)


deviceEnvExecute[{id_, _}, "RandomAction"] := Module[{
	randomAction, 
	allActions = Range[Length[$OpticalTableEnvironments[id, "Toolbox"]]]-1
	},
	randomAction = RandomChoice[allActions];
	randomAction
]


(* ::Subsubsection:: *)
(*Conversion methods*)


(* ::Text:: *)
(*Convert observation to optical elements and vice versa. Also, encodes actions into a form analogous to observations.*)


(* ::Input::Initialization:: *)
deviceEnvExecute[{id_, _}, "DecodeObservation", observation__] := CatchFailure @ Scope[
decodeObservation[observation]
]

deviceEnvExecute[{id_, _}, "EncodeObservation", input__] := CatchFailure @ Scope[
encodeObservation[input, Lookup[$OpticalTableEnvironments, id]["EpisodeLength"]]
]

deviceEnvExecute[{id_, _}, "EncodeActions", actions__] := CatchFailure @ Scope[
 encodeObservation[Lookup[$OpticalTableEnvironments, id]["Toolbox"][[actions+1]],Lookup[$OpticalTableEnvironments, id]["EpisodeLength"]]
]


(* ::Subsubsection:: *)
(*Get properties*)


(* ::Text:: *)
(*Properties should be readable by external programs. (For example, "Toolbox" is output as a list of strings.)*)


DeviceFramework`Devices`GymOpticalTables::notimplementedprop := "Not implemented property `1`."

opticalTablesGetProperty[dev_, "ActionSpace"] := Range[Length[Lookup[$OpticalTableEnvironments[DeviceFramework`DeviceHandle[dev]], "Toolbox"]]]-1

opticalTablesGetProperty[dev_, "ObservationSpace"] := Message[DeviceFramework`Devices`GymOpticalTables::notimplementedprop, "ObservationSpace"]

opticalTablesGetProperty[dev_, "Toolbox"] := Module[{
	toolbox = Lookup[$OpticalTableEnvironments[DeviceFramework`DeviceHandle[dev]], "Toolbox"]
	},
	toolbox = Map[ToString,toolbox];
	toolbox
]

(* for other properties, look them up in environment list *)
opticalTablesGetProperty[dev_, x_] := CatchFailure @ Scope[
	Lookup[$OpticalTableEnvironments[DeviceFramework`DeviceHandle[dev]], x]
]


(* ::Subsubsection:: *)
(*General functionalities*)


environmentCreateHandle[args___] := CreateUUID[];


(* ::Subsection:: *)
(*Helper methods*)


(* ::Text:: *)
(*These are helper methods for the top-level methods above.*)


(* ::Subsubsection:: *)
(*Update function*)


(* ::Text:: *)
(*TODO: This calculate the reward always from scratch. *)


(* ::Input::Initialization:: *)
opticalTablesUpdate[env_, action_] := Module[{
optElement = env["Toolbox"][[action]],
state = env["State"],
quantumState = env["HiddenState"] (*TODO: Use this here...*),
SRVs,
newState,
newStateSimplified,
reward
},

(*Update current experimental setup and simplify.*)
newState=Join[state, {optElement}];
     newStateSimplified = newState/.simplifyExperimentRules;
     While[!TrueQ[newStateSimplified == newState],
	newState=newStateSimplified;
	newStateSimplified=newState/.simplifyExperimentRules
      ];

(*Get SRVs of the current experiment. We exclude all SRVs with at least one Schmidt rank of 1 and permutations of (4,3,3) because this is apparently too easy to obtain.*)
SRVs=DeleteCases[experimentallyObtainableSchmitRankVectorsImproved[env["HiddenInitialState"],newState,env["CoincidenceCount"],env["TriggerMode"], env["NumberOfModes"]], a_/; IntersectingQ[{a}, Permutations[{4,3,3}]]||SubsetQ[a, {1}]];

(*Rewarded if there is any SRVs*)
If[Length[SRVs] > 0,  reward = 1, reward = 0];

<|"State" -> newState, "Reward" -> reward, "Info" -> SRVs|>  (*this is the return*)
]


(* ::Subsubsection:: *)
(*Get encoding of observation*)


(* ::Text:: *)
(*Encoding a state of an optical table consisting of a sequence of optical elements into an array. We encode as follows,*)


(* ::Item:: *)
(*Beam splitter on modes a and b: {1, a, b, 0, 0}*)


(* ::Item:: *)
(*Mirror on mode a: {2, a, 0, 0, 0}*)


(* ::Item:: *)
(*Hologram on mode a shifting by l: {3, a, 0, l, 0}*)


(* ::Item:: *)
(*Dove prism on mode a with angle e^(i \[Pi] l/n): {4, a, 0, 0, n}*)


(* ::Input::Initialization:: *)
encodeObservation[state_, maxElements_] := Module[
{encodedObservation = state
},
encodedObservation = encodedObservation /.encodingRules;
While[Length[encodedObservation] < maxElements, AppendTo[encodedObservation, ConstantArray[0, 5]]];

(*Developer`ToPackedArray@*)encodedObservation
]


(* ::Text:: *)
(*Replacement rules used above to encode a set of optical elements into an array.*)


(* ::Input::Initialization:: *)
encodeBS = {BS[a_,b_]-> {1, a, b,0, 0}};
encodeRefl = {Refl[a_] -> {2, a, 0, 0, 0}};
encodeHolo = {Holo[a_,\[CapitalDelta]l_] -> {3, a, 0, \[CapitalDelta]l, 0}};
encodeDP = {DP[a_, n_] -> {4, a, 0, 0, n}};
encodingRules = Join[encodeBS, encodeRefl, encodeHolo, encodeDP];


(* ::Subsubsection:: *)
(*Decode observation*)


(* ::Text:: *)
(*Decoding an observation array into a sequence of optical elements. Inverse of the encoding.*)


(* ::Input::Initialization:: *)
decodeObservation[observation_] := Module[
{state = observation
},
state = state /.decodingRules;
state = state/. {{0,0,0,0,0} -> Nothing}//Flatten;

state
]


(* ::Text:: *)
(*Replacement rules used above to decode an observation.*)


(* ::Input::Initialization:: *)
decodeBS = {{1, a_, b_,0, 0} -> BS[a,b]};
decodeRefl = {{2, a_, 0, 0, 0} -> Refl[a]};
decodeHolo = {{3, a_, 0, \[CapitalDelta]l_, 0} -> Holo[a,\[CapitalDelta]l]};
decodeDP = {{4, a_, 0, 0, n_} -> DP[a, n]};
decodingRules = Join[decodeBS, decodeRefl, decodeHolo, decodeDP];


(* ::Section:: *)
(*Register device*)


(* ::Text:: *)
(*Here, we register a device similar to SimulatedCartPole. This is done in accordance to the corresponding tutorial.*)


(* ::Input::Initialization:: *)
DeviceFramework`DeviceClassRegister[
"GymOpticalTables", 
"DeregisterOnClose" -> True,
"OpenFunction"-> deviceEnvCreate,
"ExecuteFunction" -> deviceEnvExecute, 
"ReadFunction" -> deviceEnvRead, 
"CloseFunction" -> deviceEnvClose,
"GetPropertyFunction" -> opticalTablesGetProperty,
"MakeManagerHandleFunction" ->  environmentCreateHandle,
"Properties" -> {"ActionSpace" -> {}, "ObservationSpace" -> {}, "Toolbox" -> {}, "EpisodeLength" -> {}, "CurrentStep" -> {}},
"DriverVersion" -> 0.1];


(* ::Section:: *)
(*End*)


(* ::Input::Initialization:: *)
End[];
EndPackage[];


(* ::Section:: *)
(*Tests*)


(* ::Text:: *)
(*Here we test some of the environment functionality.*)


(* ::Input:: *)
(*(*create device environment with standard initial conditions*)*)
(*Clear[env, output, parameters, testTools];*)
(**)
(*parameters = {"MaxL" -> 2, "DPphase" -> 1,  "NumberOfModes" -> 4, "InitialSPDC" -> 1, "CoincidenceCount" -> 4, "TriggerMode" -> 1, "EpisodeLength" -> 12};*)
(*env = DeviceOpen["GymOpticalTables", parameters];*)
(**)
(*(*reset environment to starting conditions*)*)
(*DeviceExecute[env, "Reset"];*)
(**)
(*(*run a sequence of actions which we know creates a (2,3,3)-state*)*)
(*DeviceExecute[env, "Step", 3];DeviceExecute[env, "Step", 7];DeviceExecute[env, "Step", 28];output = DeviceExecute[env, "Step", 3];*)
(**)
(*(*check results*)*)
(*Clear[test01, test02, test03, test04, testResult04]*)
(*test01 = (output["Reward"] == 1);*)
(*test02 = (MemberQ[output["Info"]["SRVs"], {2,3,3}]);*)
(*test03 = (DeviceRead[env]["Ended"] == False);*)
(*testResult04 = {BS[2,3],Refl[2],DP[3,1],BS[2,3]}/.{BS[a_,b_]-> {1, a, b,0, 0}}/.{Refl[a_] -> {2, a, 0, 0, 0}}/.{DP[a_, n_] -> {4, a, 0, 0, n}};*)
(*While[Length[testResult04] < env["EpisodeLength"], AppendTo[testResult04, ConstantArray[0, 5]]];*)
(*test04 = (DeviceRead[env]["ObservedState"] == testResult04);*)
(**)
(*(*check extra functionalities*)*)
(*Clear[test05, test06, test07]*)
(*test05 = (DeviceExecute[env, "DecodeObservation", {DeviceRead[env]["ObservedState"]}] == {BS[2,3],Refl[2],DP[3,1],BS[2,3]});*)
(*test06 = (DeviceExecute[env, "EncodeObservation",  {{BS[2,3],Refl[2],DP[3,1],BS[2,3]}}] == DeviceRead[env]["ObservedState"]);*)
(*test07 = (DeviceExecute[env, "EncodeActions",  {{3,7,28,3}}] ==  DeviceRead[env]["ObservedState"]);*)
(**)
(*(*test internal parameters*)*)
(*Clear[test08, test09, test10, testTools]*)
(*parameters = Association[parameters];*)
(*testTools = toolbox[parameters["NumberOfModes"], parameters["MaxL"],parameters["DPphase"]];*)
(*test08 = (env["Toolbox"] == Map[ToString, testTools]);*)
(*test09 = (env["ActionSpace"] == Range[Length[testTools]]-1);*)
(*test10 = (env["EpisodeLength"]== parameters["EpisodeLength"]);*)
(**)
(*(*check end and reset condition*)*)
(*Clear[test11, test12, test13]*)
(*For[i= 5, i<= parameters["EpisodeLength"],  i++, *)
(*test11 = DeviceExecute[env, "Step", DeviceExecute[env, "RandomAction"]]["Ended"];*)
(*]*)
(*test12 = (env["CurrentStep"] == parameters["EpisodeLength"]);*)
(*DeviceExecute[env, "Reset"];*)
(*test13 = (env["CurrentStep"] ==0);*)
(**)
(*(*close environment*)*)
(*DeviceClose[env];*)
(**)
(*(*output test results*)*)
(*VectorQ[{test01, test02, test03, test04, test05, test06, test07, test08, test09, test10, test11, test12, test13}, TrueQ]*)


(* ::Section:: *)
(*Info*)


(* ::Subsection:: *)
(*ToDo list*)


(* ::Text:: *)
(*There is a few pending improvements.*)


(* ::Item:: *)
(*Currently, we re-calculate the quantum state of an experiment from the initial state after every step of agent-environment interaction. Instead, we should keep the hidden quantum state in memory and only apply one optical element at a time. This requires us to update `opticalTablesUpdate` and the function `experimentallyObtainableSchmitRankVectorsImproved`.*)


(* ::Item:: *)
(*UUID assignment seems to be not properly working. That is, we have to restart the kernel for debugging unless we close the device explicitly.*)


(* ::Subsection:: *)
(*Python usage*)


(* ::Text:: *)
(*We want this package to be callable in python using Wolfram Client Library.*)


(* ::Input:: *)
(*ExternalEvaluate;*)
(*ExternalEvaluate`Private`resetCache[];*)
(*FindExternalEvaluators["Python"]*)


(* ::Text:: *)
(*If you still need to register python, use the `Target` from above in the following command.*)


(* ::CodeText:: *)
(*RegisterExternalEvaluator["Python", "Target"]*)


(* ::Subsubsection:: *)
(*GymOpticalTables environment*)


(* ::Text:: *)
(*Here some python code to run this environment to create a Leach interferometer.*)
(*TODO: Make output of this environment python readable.*)


(* ::Input:: *)
(*ExternalEvaluate["Python","*)
(**)
(*from wolframclient.evaluation import WolframLanguageSession*)
(*from wolframclient.language import wl, wlexpr*)
(**)
(*# start Mathematica session and get package*)
(*session = WolframLanguageSession()*)
(**)
(*# get parameters for environment*)
(*parameters = wlexpr('{\"MaxL\" -> 2, \"DPphase\" -> 1,  \"NumberOfModes\" -> 4, \"InitialSPDC\" -> 1, \"CoincidenceCount\" -> 4, \"TriggerMode\" -> 1, \"EpisodeLength\" -> 12}')*)
(**)
(*# open the environment as mathematica device (assuming the file is in the same folder)*)
(*session.evaluate( *)
(*	wlexpr(f'env = DeviceOpen[\"GymOpticalTables\", {parameters}]')*)
(*)*)
(**)
(*# reset environment*)
(*resetExpr = wlexpr('DeviceExecute[env, \"Reset\"]')*)
(*r = 0*)
(*done = False*)
(*o = session.evaluate( *)
(*	resetExpr*)
(*)['ObservedState']*)
(**)
(*# run environment*)
(*actions = [3,7,28,3]*)
(*for action in actions:*)
(*	stepExpr = wlexpr(f'DeviceExecute[env, \"Step\", {action}]')*)
(*	mOut = session.evaluate( *)
(*		stepExpr*)
(*	)*)
(*o = mOut['ObservedState']*)
(*r = mOut['Reward']*)
(*done = mOut['Ended']*)
(*info = mOut['Info']*)
(**)
(*# check result gives reward as expected*)
(*assert r == 1*)
(*"]*)
(**)


(* ::Subsubsection:: *)
(*OpenAI gym Cartpole environment (for comparison)*)


(* ::Text:: *)
(*This is how a prototypical python code would look like for the standardized cartpole RL environment:*)


(* ::Input:: *)
(*ExternalEvaluate["Python","*)
(**)
(*from wolframclient.evaluation import WolframLanguageSession*)
(*from wolframclient.language import wl, wlexpr*)
(**)
(*# start Mathematica session and get package*)
(*session = WolframLanguageSession()*)
(*session.evaluate( *)
(*    wl.Needs('ReinforcementLearning`')*)
(*)*)
(**)
(*# open the environment as mathematica device*)
(*session.evaluate( *)
(*	wlexpr('env = DeviceOpen[\"SimulatedCartPole\"]')*)
(*)*)
(**)
(*# reset environment*)
(*resetExpr = wlexpr('DeviceExecute[env, \"Reset\"]')*)
(*r = 0*)
(*done = False*)
(*o = session.evaluate( *)
(*	resetExpr*)
(*)['ObservedState']*)
(**)
(*# run environment*)
(*while not done:*)
(*	action = 'Left'*)
(*	stepExpr = wlexpr(f'DeviceExecute[env, \"Step\", {action}]')*)
(*	mOut = session.evaluate( *)
(*		stepExpr*)
(*	)*)
(*	o = mOut['ObservedState']*)
(*	r = mOut['Reward']*)
(*	done = mOut['Ended']*)
(*"]*)
