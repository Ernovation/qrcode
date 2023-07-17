%% Copyright 2011 Steve Davis <steve@simulacity.com>
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

-module(qrcode).

-include("qrcode.hrl").
-include("qrcode_params.hrl").

-export([encode/1, encode/2]).
-export([cci/2]).

%%
encode(Bin) ->
	encode(Bin, 'M').
%
encode(Bin, ECC) ->
	Params = choose_qr_params(Bin, ECC),
	Content = encode_content(Params, Params#qr_params.data),
	BlocksWithECC = generate_ecc_blocks(Params, Content),
	Codewords = interleave_blocks(BlocksWithECC),
	Matrix = qrcode_matrix:embed_data(Params, Codewords),
	MaskedMatrices = qrcode_mask:generate(Params, Matrix),
	Candidates = [qrcode_matrix:overlay_static(Params, M) || M <- MaskedMatrices],
	{MaskType, SelectedMatrix} = qrcode_mask:select(Candidates),
	Params0 = Params#qr_params{mask = MaskType},
	FMT = format_info_bits(Params0),
	VSN = version_info_bits(Params0),
	#qr_params{version = Version, dimension = Dim, ec_level = _ECC} = Params0,
	QRCode = qrcode_matrix:finalize(Dim, FMT, VSN, ?QUIET_ZONE, SelectedMatrix),
	#qrcode{version = Version, ecc = ECC, dimension = Dim + ?QUIET_ZONE * 2, data = QRCode}.

%%
choose_qr_params(Bin, ECLevel) ->
	{Version, Data, _DataSize} = qrcode_mix:mix(Bin, ECLevel),
	{ECCBlockDefs, Remainder} = version_info(Version, ECLevel),
	AlignmentCoords = alignment_patterns(Version),
	Dim = qrcode_matrix:dimension(Version),
	#qr_params{mode = mixed, version = Version, dimension = Dim, ec_level = ECLevel,
		block_defs = ECCBlockDefs, align_coords = AlignmentCoords, remainder = Remainder, data = Data}.

%%
version_info(Version, ECC) ->
	{value, {_, _, ECCBlocks, Remainder, _}} =
		lists:keysearch({ECC, Version}, 1, ?TABLES),
	{ECCBlocks, Remainder}.

%%
encode_content(#qr_params{mode = Mode, version = Version}, Bin) ->
	pad_to_bytes(encode_content(Mode, Version, Bin)).
%
encode_content(mixed, Version, List) when is_list(List) ->
	<< <<(encode_content(Type, Version, Bytes))/bits>> || {Type, Bytes} <- List>>;
encode_content(?ALPHANUMERIC_MODE, Version, Bin) ->
	encode_alphanum(Version, Bin);
encode_content(?NUMERIC_MODE, Version, Bin) ->
	encode_num(Version, Bin);
encode_content(?BYTE_MODE, Version, Bin) ->
	encode_bytes(Version, Bin).

%%
generate_ecc_blocks(#qr_params{block_defs = ECCBlockDefs}, Bin) ->
	Bin0 = pad_data(Bin, ECCBlockDefs),
	generate_ecc(Bin0, ECCBlockDefs, []).

pad_to_bytes(B) ->
	PadSize = (8 - (bit_size(B) rem 8)) rem 8,
	<<B/bits, 0:PadSize>>.

pad_data(Bin, ECCBlockDefs) ->
	DataSize = byte_size(Bin),
	TotalSize = get_ecc_size(ECCBlockDefs),
	PaddingSize = TotalSize - DataSize,
	Padding = binary:copy(<<?DATA_PAD_0, ?DATA_PAD_1>>, PaddingSize bsr 1),
	case PaddingSize band 1 of
	0 ->
		<<Bin/bits, Padding/bits>>;
	1 ->
		<<Bin/bits, Padding/bits, ?DATA_PAD_0>>
	end.


get_ecc_size(ECCBlockDefs) ->
	get_ecc_size(ECCBlockDefs, 0).
get_ecc_size([{C, _, D}|T], Acc) ->
	get_ecc_size(T, C * D + Acc);
get_ecc_size([], Acc) ->
	Acc.


generate_ecc(Bin, [{C, L, D}|T], Acc) ->
	{Result, Bin0} = generate_ecc0(Bin, C, L, D, []),
	generate_ecc(Bin0, T, [Result|Acc]);
generate_ecc(<<>>, [], Acc) ->
	lists:flatten(lists:reverse(Acc)).


generate_ecc0(Bin, Count, TotalLength, BlockLength, Acc) when byte_size(Bin) >= BlockLength, Count > 0 ->
	<<Block:BlockLength/binary, Bin0/binary>> = Bin,
	EC = qrcode_reedsolomon:encode(Block, TotalLength - BlockLength),
	generate_ecc0(Bin0, Count - 1, TotalLength, BlockLength, [{Block, EC}|Acc]);
generate_ecc0(Bin, 0, _, _, Acc) ->
	{lists:reverse(Acc), Bin}.

%%
interleave_blocks(Blocks) ->
	Data = interleave_data(Blocks, <<>>),
	interleave_ecc(Blocks, Data).

interleave_data(Blocks, Bin) ->
	Data = [X || {X, _} <- Blocks],
	interleave_blocks(Data, [], Bin).

interleave_ecc(Blocks, Bin) ->
	Data = [X || {_, X} <- Blocks],
	interleave_blocks(Data, [], Bin).

interleave_blocks([], [], Bin) ->
	Bin;
interleave_blocks([], Acc, Bin) ->
	Acc0 = [X || X <- Acc, X =/= <<>>],
	interleave_blocks(lists:reverse(Acc0), [], Bin);
interleave_blocks([<<X, Data/binary>>|T], Acc, Bin) ->
	interleave_blocks(T, [Data|Acc], <<Bin/binary, X>>).

%
encode_bytes(Version, B) ->
	Bin = list_to_binary(B),
	Size = size(Bin),
	CharacterCountBitSize = cci(?BYTE_MODE, Version),
	<<?BYTE_MODE:4, Size:CharacterCountBitSize, Bin/binary>>.

encode_alphanum(Version, A) ->
	Size = length(A),
	CharacterCountBitSize = cci(?ALPHANUMERIC_MODE, Version),
	Bits = alphanum_to_bits(A),
	<<?ALPHANUMERIC_MODE:4, Size:CharacterCountBitSize, Bits/bits>>.

alphanum_to_bits([B]) ->
	Value = alpha_value(B),
	<<Value:6>>;
alphanum_to_bits([]) ->
	<<>>;
alphanum_to_bits([B1, B2 | More]) ->
	MoreBits = alphanum_to_bits(More),
	<<((alpha_value(B1) * 45) + alpha_value(B2)):11, MoreBits/bits>>.

encode_num(Version, N) ->
	Size = length(N),
	CharacterCountBitSize = cci(?NUMERIC_MODE, Version),
	Bits = num_to_bits(N),
	<<?NUMERIC_MODE:4, Size:CharacterCountBitSize, Bits/bits>>.

num_to_bits([]) ->
	<<>>;
num_to_bits([B]) ->
	<<(B - $0):4>>;
num_to_bits([B1, B2]) ->
	N1 = B1 - $0,
	N2 = B2 - $0,
	<<(N1 * 10 + N2):7>>;
num_to_bits([B1, B2, B3 | More]) ->
	MoreBits = num_to_bits(More),
	N1 = B1 - $0,
	N2 = B2 - $0,
	N3 = B3 - $0,
	<<(N1 * 100 + N2 * 10 + N3):11, MoreBits/bits>>.
%% Table 25. Error correction level indicators
ecc('L') -> 1;
ecc('M') -> 0;
ecc('Q') -> 3;
ecc('H') -> 2.

% Table 5. Charset encoder
% NOTE: removed

%%
alignment_patterns(Version) ->
	D = qrcode_matrix:dimension(Version),
	L = element(Version, ?ALIGNMENT_COORDINATES),
	L0 = [{X, Y} || X <- L, Y <- L],
	L1 = [{X, Y} || {X, Y} <- L0, is_finder_region(D, X, Y) =:= false],
	% Change the natural sort order so that rows have greater weight than columns
	F = fun
		({_, Y}, {_, Y0}) when Y < Y0 ->
			true;
		({X, Y}, {X0, Y0}) when Y =:= Y0 andalso X =< X0 ->
			true;
		(_, _) ->
			false
		end,
	lists:sort(F, L1).
%
is_finder_region(D, X, Y)
		when (X =< 8 andalso Y =< 8)
		orelse (X =< 8 andalso Y >= D - 8)
		orelse (X >= D - 8 andalso Y =< 8) ->
	true;
is_finder_region(_, _, _) ->
	false.

%% Table 3. Number of bits in Character Count Indicator
cci(Mode, Version) when Version >= 1 andalso Version =< 40->
	{Mode, CC} = lists:keyfind(Mode, 1, ?CCI_BITSIZE),
	cci0(CC, Version).
%
cci0([X, _, _], Version) when Version =< 9 ->
	X;
cci0([_, X, _], Version) when Version =< 26 ->
	X;
cci0([_, _, X], _) ->
	X.

version_info_bits(#qr_params{version = Version}) when Version < 7 ->
	<<>>;
version_info_bits(#qr_params{version = Version}) when Version =< 40 ->
	BCH = qrcode_reedsolomon:bch_code(Version, ?VERSION_INFO_POLY),
	<<Version:6, BCH:12>>.

format_info_bits(#qr_params{ec_level = ECLevel, mask = MaskType}) ->
	Info = (ecc(ECLevel) bsl 3) bor MaskType,
	BCH = qrcode_reedsolomon:bch_code(Info, ?FORMAT_INFO_POLY),
	InfoWithEC = (Info bsl 10) bor BCH,
	Value = InfoWithEC bxor ?FORMAT_INFO_MASK,
	<<Value:15>>.

alpha_value(C) when C >= $0, C =< $9 ->
	C - $0;
alpha_value(C) when C >= $A, C =< $Z ->
	C + 10 - $A;
alpha_value($ ) ->
	36;
alpha_value($$) ->
	37;
alpha_value($%) ->
	38;
alpha_value($*) ->
	39;
alpha_value($+) ->
	40;
alpha_value($-) ->
	41;
alpha_value($.) ->
	42;
alpha_value($/) ->
	43;
alpha_value($:) ->
	44;
alpha_value(_C) ->
	-1.
