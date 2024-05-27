:- use_module(library(csv)).
:- use_module(library(strings)).
:- use_module(library(yall)).

:- ['src/text_utils.pro', 'openai_utils.pro'].

:- set_prolog_flag(stack_limit, 8_589_934_592).

system_prompt(SystemPrompt) :-
    SystemPrompt = {|string||
    You are an expert in phonetics, phonology, and diachronic linguistics.  
    You are a master at applying the Comparative Method.
    |}.

:- dynamic lex/13.
:- dynamic lex_cog/13.
:- dynamic cog/2.
% db,ID,Language_ID,Parameter_ID,Segments,Glottolog_Name,Glottocode,Family,Concepticon_ID,Concepticon_Gloss,Cognateset_ID,cc,ASJP
read_wordlist :-
    csv_read_file('data/lexibank_wordlist.csv', [ _ | Words], [functor(lex)]),
    retractall(lex(_, _, _, _, _, _, _, _, _, _, _, _, _)),
    maplist(assertz, Words),

    findall(
        lex_cog(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13),
        (   lex(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13),
            \+ X11 = ''
        ),
        LexWithCognateSet),
    random_permutation(LexWithCognateSet, ShuffledLexWithCognateSet),
    maplist(assertz, ShuffledLexWithCognateSet).

% lex[_cog](DB, ID, LanguageID, ParameterID, Segments, GlottologName, GlottoCode, Family, 
% ConceptID, Gloss, CognatesetID, CC, ASJP)

create_asjp_symbol(asjp(Symbol, FeaturesString), asjp_symbol(Symbol, Features)) :-
    atomics_to_string(Features, ' ', FeaturesString).

:- dynamic asjp_symbol/2.

read_asjp_symbols(ASJP) :-
    csv_read_file('data/asjp.tsv', Rows, [functor(asjp)]),
    maplist(create_asjp_symbol, Rows, ASJP),
    retractall(asjp_symbol(_, _)),
    maplist(assertz, ASJP).

read_alignments :-
    csv_read_file('data/all_cognate_alignments.csv', Rows, [functor(alignment)]),
    retractall(alingment(_, _, _, _)),
    maplist(assertz, Rows).


concept(ConceptID, Gloss) :-
    lex(_, _, _, _, _, _, _, _, ConceptID, Gloss, _, _, _).

concepts(Concepts) :-
    findall(
        concept(ConceptID, Gloss),
        concept(ConceptID, Gloss),
        AllConcepts),
    sort(AllConcepts, Concepts).

family(Family) :-
    lex(_, _, _, _, _, _, _, Family, _, _, _, _, _).

families(Families) :-
    findall(Family, family(Family), AllFamilies),
    sort(AllFamilies, SortedFamilies),
    exclude({}/['']>>true, SortedFamilies, Families).

cognate_set_id(CognatesetID) :-
    lex_cog(_, _, _, _, _, _, _, _, _, _, CognatesetID, _, _),
    \+ CognatesetID = ''.

cognate_set_ids(CognatesetIDs) :-
    findall(CognatesetID, cognate_set_id(CognatesetID), AllCognatesetIDs),
    sort(AllCognatesetIDs, CognatesetIDs).

cognate_set_words(CognatesetID, cognate_set(CognatesetID, Words)) :-
    findall(
        word(ID, Family, LanguageID, ConceptID, Gloss, CognatesetID, Transcription),
        lex_cog(_, ID, LanguageID, _, _, _, _, Family, ConceptID, Gloss, CognatesetID, _, Transcription),
        Words).

cognate_set_transcriptions(CognatesetID, cognate_set(CognatesetID, Transcriptions)) :-
    findall(
        Transcription,
        lex_cog(_, _, _, _, _, _, _, _, _, _, CognatesetID, _, Transcription),
        Transcriptions).

all_cognate_pairs(CognatePairs) :-
    cognate_set_ids(CognateSetIDs),
    maplist(cognate_set_words, CognateSetIDs, AllCognates),
    include({}/[cognate_set(_, Words)]>>(length(Words, L), L > 1), AllCognates, MultipleCognates),
    maplist({}/[cognate_set(_, Words), Cognates]>> 
        (   pairwise_combinations(Words, Pairs),
            maplist({}/[[Cognate1, Cognate2], cog(Cognate1, Cognate2)]>>true, Pairs, Cognates)
        ), 
        MultipleCognates,
        NestedCognatePairs),
    flatten(NestedCognatePairs, CognatePairs).



concept_set_with_cognates(Family, ConceptID, ConceptSet) :-
    findall(
        word(ID, Family, LanguageID, ConceptID, Gloss, CognatesetID, Transcription),
        lex_cog(_, ID, LanguageID, _, _, _, _, Family, ConceptID, Gloss, CognatesetID, _, Transcription),
        ConceptSet).

random_concept_set_with_cognates(ConceptSet) :-
    findall(
        [Family, ConceptID], 
        lex_cog(_, _, _, _, _, _, _, Family, ConceptID, _, _, _, _),
        FamilyConcepts),
    random_member([Family, ConceptID], FamilyConcepts),
    concept_set_with_cognates(Family, ConceptID, ConceptSet).


random_cognate_pair(CognatesetID, [Word1, Word2]) :-
    cognate_set(CognatesetID, Words),
    random_member(Word1, Words),
    select(Word1, Words, RestWords),
    random_member(Word2, RestWords).

cognate_set_size(CognatesetID, Size) :-
    cognate_set(CognatesetID, Words),
    length(Words, Size).

random_cognates(Number, Cognates) :-
    cognate_set_ids(CSIDs),
    include({}/[CognatesetID]>>(cognate_set_size(CognatesetID, Size), Size > 1), CSIDs, BigCSIDs),
    sample(BigCSIDs, Number, SampledCSIDs),
    maplist(random_cognate_pair, SampledCSIDs, Cognates).

random_non_cognate_pair([CognatesetID1, CognatesetID2], [Word1, Word2]) :-
    cognate_set(CognatesetID1, Words1),
    cognate_set(CognatesetID2, Words2),
    random_member(Word1, Words1),
    random_member(Word2, Words2).

cognate_sets_in_concept(ConceptID, CognatesetIDs) :-
    findall(
        CognatesetID, 
        lex_cog(_, _, _, _, _, _, _, _, ConceptID, _, CognatesetID, _, _),
        AllCognatesetIDs),
    sort(AllCognatesetIDs, CognatesetIDs).

non_cognate_set_in_concept(CognatesetID1, CognatesetID2) :-
    lex_cog(_, _, _, _, _, _, _, _, ConceptID, _, CognatesetID1, _, _),
    cognate_sets_in_concept(ConceptID, CognatesetIDs),
    select(CognatesetID1, CognatesetIDs, RestCognatesetIDs),
    random_member(CognatesetID2, RestCognatesetIDs).


random_non_cognates(Number, NonCognates) :-
    cognate_set_ids(CSIDs),
    sample(CSIDs, Number, SampledCSIDs1),
    maplist(non_cognate_set_in_concept, SampledCSIDs1, SampledCSIDs2),
    zip_lists(SampledCSIDs1, SampledCSIDs2, SampledCSIDPairs),
    maplist(random_non_cognate_pair, SampledCSIDPairs, NonCognates).


asjp_to_markdown(ASJPMarkdownTable) :-
    findall([Symbol, Features], asjp_symbol(Symbol, Features), ASJPEntries),
    maplist({}/[[Symbol, Features], [Symbol, FeaturesString]]>>(atomics_to_string(Features, ',', FeaturesString)), ASJPEntries, FeaturesStrings),
    markdown_table(['Symbol', 'Phonetic Features'], FeaturesStrings, ASJPMarkdownTable).

:- dynamic cognate_set/2.

init :-
    read_wordlist,
    read_asjp_symbols(_),
    init_openai_key,
    garbage_collect,
    read_alignments,

    cognate_set_ids(CSIDs),
    maplist(cognate_set_words, CSIDs, CognateSets),
    retractall(cognate_set(_, _)),
    maplist(assert, CognateSets).

concepts_in_cognate_set(CognatesetID, Concepts) :-
    findall(ConceptID,
    lex_cog(_, _, _, _, _, _, _, _, ConceptID, _, CognatesetID, _, _),
    Concepts).

