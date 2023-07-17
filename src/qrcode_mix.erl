%% Copyright 2023 Erik Reitsma
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

-module(qrcode_mix).

%% Optimised encoding based on Annex H of ISO/IEC 18004:2000
%% No Kanji support.

-include("qrcode.hrl").
-include("qrcode_params.hrl").

-export([mix/2, mix_version/2]).

-record(node, {
    mode = -1, %% current mode of encoding
    mode_count = 0, %% number of elements encoded in this mode
    remaining %% total characters remaining
}).

mix(Text, ECC) when is_binary(Text) ->
    mix(binary_to_list(Text), ECC);
mix(Text, ECC) ->
    %% first try with version 40
    {Version, Modes, DataSize} = mix_attempt(Text, ECC, 40),
    {Version, split_text_by_path(Text, Modes), DataSize}.

mix_attempt(Text, ECC, Version) ->
    {Path, Size} = mix_version(Text, Version),
    case version_required(Size, ECC) of
        RequiredVersion when RequiredVersion < Version, RequiredVersion > 1 ->
            %% we could try again with an even smaller version
            mix_attempt(Text, ECC, RequiredVersion - 1);
        Version ->
            %% required is what we used, return the result
            {Version, Path, Size};
        RequiredVersion ->
            %% some other required version, use it for an attempt
            mix_attempt(Text, ECC, RequiredVersion)
    end.

mix_version(Text, Version) when is_binary(Text) ->
    mix_version(binary_to_list(Text), Version);
mix_version(Text, Version) ->
    mix_version(Version, #node{remaining = bytes_to_modes(Text)}, [], 0, #{}).

mix_version(_Version, #node{remaining = []}, CurrentPath, DistanceToCurrent, _Unhandled) ->
    {lists:reverse(CurrentPath), DistanceToCurrent};
mix_version(Version, CurrentNode, CurrentPath, DistanceToCurrent, Unhandled) ->
    Neighbours = neighbours(Version, CurrentNode),
    NewUnhandled = update_unhandled(Unhandled, Neighbours, CurrentPath, DistanceToCurrent),
    {ClosestNeighbour, {ClosestDistance, ClosestPath}} = closest_neighbour(NewUnhandled),
    mix_version(Version, ClosestNeighbour, ClosestPath, ClosestDistance, maps:remove(ClosestNeighbour, NewUnhandled)).

version_required(Size, ECC) ->
    version_required(Size, ECC, ?TABLES).

version_required(Size, ECC, [{{ECC, Version}, _ModeSizes, _ECC, _Remainder, DataWords} | _T]) when DataWords * 8 >= Size ->
    Version;
version_required(Size, ECC, [_ | T]) ->
    version_required(Size, ECC, T);
version_required(_, _, []) ->
    error.

split_text_by_path(Text, Path) ->
    split_text_by_path(Text, Path, undefined, [], []).

split_text_by_path([], [], _, [], Res) ->
    lists:reverse(Res);
split_text_by_path([], [], Mode, ModeChars, Res) ->
    lists:reverse([{Mode, lists:reverse(ModeChars)} | Res]);
split_text_by_path([C | TC], [M | MC], M, ModeChars, Res) ->
    split_text_by_path(TC, MC, M, [C | ModeChars], Res);
split_text_by_path([C | TC], [M | MC], undefined, _ModeChars, Res) ->
    split_text_by_path(TC, MC, M, [C], Res);
split_text_by_path([C | TC], [M | MC], Mode, ModeChars, Res) ->
    split_text_by_path(TC, MC, M, [C], [{Mode, lists:reverse(ModeChars)} | Res]).

update_unhandled(Unhandled, [], _Path, _Distance) ->
    Unhandled;
update_unhandled(Unhandled, [{Distance, Node} | More], Path, DistanceToCurrent) ->
    NewDistance = DistanceToCurrent + Distance,
    case maps:find(Node, Unhandled) of
        {ok, {OldDistance, _OldPath}} when NewDistance >= OldDistance ->
            %% older distance is shorter, keep it
            update_unhandled(Unhandled, More, Path, DistanceToCurrent);
        _ ->
            %% new or shorter distance found
            update_unhandled(Unhandled#{Node => {NewDistance, [Node#node.mode | Path]}}, More, Path, DistanceToCurrent)
    end.

closest_neighbour(Unhandled) ->
    UnhandledList = maps:to_list(Unhandled),
    closest_neighbour(undefined, {infinity, []}, UnhandledList).

closest_neighbour(BestNode, BestDistancePath, []) ->
    {BestNode, BestDistancePath};
closest_neighbour(_BestNode, {BestDistance, _BestPath}, [{Node, {Distance, Path}} | More]) when Distance < BestDistance ->
    closest_neighbour(Node, {Distance, Path}, More);
closest_neighbour(BestNode, BestDistancePath, [_H | More]) ->
    closest_neighbour(BestNode, BestDistancePath, More).

neighbours(Version,
    #node{
        remaining = [NextMode | _]
        } = Node) -> 
    %% we can switch to any mode >= NextMode
    NextModes = matching_modes(NextMode),
    Neighbours = [neighbour_node(Version, Node, M) || M <- NextModes],
    Neighbours.

neighbour_node(_Version, #node{mode = Mode, mode_count = ModeCount, remaining = Remaining}, Mode) ->
    %% continue with the same mode
    {size_for_extra_in_mode(Mode, ModeCount),
        #node{mode = Mode, mode_count = ModeCount + 1, remaining = tl(Remaining)}};
neighbour_node(Version, #node{remaining = Remaining}, Mode) ->
    {4 + qrcode:cci(Mode, Version) + size_for_extra_in_mode(Mode, 0), #node{mode = Mode, mode_count = 1, remaining = tl(Remaining)}}.

matching_modes(?BYTE_MODE) ->
    [?BYTE_MODE];
matching_modes(?ALPHANUMERIC_MODE) ->
    [?ALPHANUMERIC_MODE, ?BYTE_MODE];
matching_modes(?NUMERIC_MODE) ->
    [?NUMERIC_MODE, ?ALPHANUMERIC_MODE, ?BYTE_MODE].

size_for_extra_in_mode(?NUMERIC_MODE, ModeCount) ->
    case ModeCount rem 3 of
        0 ->
            4;
        1 ->
            3;
        2 ->
            3
    end;
size_for_extra_in_mode(?ALPHANUMERIC_MODE, ModeCount) ->
    case ModeCount rem 2 of
        0 ->
            6;
        1 ->
            5
    end;
size_for_extra_in_mode(?BYTE_MODE, _ModeCount) ->
    8.

bytes_to_modes(B) when is_binary(B) ->
    [ char_to_mode(C) || <<C>> <= B];
bytes_to_modes(L) when is_list(L) ->
    [ char_to_mode(C) || C <- L].

char_to_mode(C) ->
    case char_is_num(C) of
        true ->
            ?NUMERIC_MODE;
        _ ->
            case char_is_alpha(C) of
                true ->
                    ?ALPHANUMERIC_MODE;
                _ ->
                    ?BYTE_MODE
            end
    end.

char_is_num(C) when C >= $0, C =< $9 ->
	true;
char_is_num(_C) ->
	false.

char_is_alpha(C) when C >= $0, C =< $9 ->
	true;
char_is_alpha(C) when C >= $A, C =< $Z ->
	true;
char_is_alpha($ ) ->
	true;
char_is_alpha($$) ->
	true;
char_is_alpha($%) ->
	true;
char_is_alpha($*) ->
	true;
char_is_alpha($+) ->
	true;
char_is_alpha($-) ->
	true;
char_is_alpha($.) ->
	true;
char_is_alpha($/) ->
	true;
char_is_alpha($:) ->
	true;
char_is_alpha(_C) ->
	false.

