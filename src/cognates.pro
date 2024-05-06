:- use_module(library(csv)).

read_wordlist :-
    csv_read_file('data/lexibank_wordlist.csv', [ _ | Words], [functor(lex)]),
    retractall(lex(_, _, _, _, _, _, _, _, _, _, _, _, _)),
    maplist(assertz, Words).

create_asjp_symbol(asjp(Symbol, FeaturesString), asjp_symbol(Symbol, Features)) :-
    atomics_to_string(Features, ' ', FeaturesString).

:- dynamic asjp_symbol/2.

read_asjp_symbols(ASJP) :-
    csv_read_file('data/asjp.tsv', Rows, [functor(asjp)]),
    maplist(create_asjp_symbol, Rows, ASJP),
    retractall(asjp_symbol(_, _)),
    maplist(assertz, ASJP).


conception(ConceptionID, ConceptionGloss) :-
    lex(_, _, _, _, _, _, _, _, ConceptionID, ConceptionGloss, _, _, _).

conceptions(ConceptionsSet) :-
    findall(
        conception(ConceptionID, ConceptionGloss),
        conception(ConceptionID, ConceptionGloss),
        Conceptions),
    sort(Conceptions, ConceptionsSet).





