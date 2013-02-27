-module(sha).

-export([start/0, sha/1, sha1/1, sha224/1, sha256/1, sha384/1,
         sha512/1]).

start() ->
	ok.

digit_to_xchar(D) when (D >= 0) and (D < 10) ->
    D + 48;
digit_to_xchar(D) ->
    D + 87.

ints_to_rxstr([], Res) ->
    Res;
ints_to_rxstr([N | Ns], Res) ->
    ints_to_rxstr(Ns, [digit_to_xchar(N rem 16),
                       digit_to_xchar(N div 16) | Res]).

sha(Text) ->
    Bin = crypto:sha(Text),
    lists:reverse(ints_to_rxstr(binary_to_list(Bin), [])).

sha1(Text) ->
	crypto:sha(Text).

sha224(Text) ->
	crypto:hash(sha224, Text).

sha256(Text) ->
	crypto:hash(sha256, Text).

sha384(Text) ->
	crypto:hash(sha384, Text).

sha512(Text) ->
	crypto:hash(sha512, Text).

%%EOF
