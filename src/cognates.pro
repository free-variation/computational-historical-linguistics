:- use_module(library(csv)).
:- use_module(library(strings)).
:- use_module(library(yall)).

:- ['src/text_utils.pro', 'openai_utils.pro'].

:- set_prolog_flag(stack_limit, 8_589_934_592).

:- dynamic lex/8.
:- dynamic lex_cog/8.
:- dynamic concept/2.
:- dynamic glottolog/2.
:- dynamic asjp_symbol/2.

create_asjp_symbol(asjp(Symbol, FeaturesString), asjp_symbol(Symbol, Features)) :-
    atomics_to_string(Features, ' ', FeaturesString).

read_asjp_symbols(ASJP) :-
    csv_read_file('data/asjp.tsv', Rows, [functor(asjp)]),
    maplist(create_asjp_symbol, Rows, ASJP),
    retractall(asjp_symbol(_, _)),
    maplist(assertz, ASJP).


asjp_to_markdown(ASJPMarkdownTable) :-
    findall([Symbol, Features], asjp_symbol(Symbol, Features), ASJPEntries),
    maplist({}/[[Symbol, Features], [Symbol, FeaturesString]]>>(atomics_to_string(Features, ',', FeaturesString)), ASJPEntries, FeaturesStrings),
    markdown_table(['Symbol', 'Phonetic Features'], FeaturesStrings, ASJPMarkdownTable).

% db,ID,Language_ID,Parameter_ID,Segments,Glottolog_Name,Glottocode,Family,Concepticon_ID,Concepticon_Gloss,Cognateset_ID,cc,ASJP
read_wordlist :-
    csv_read_file('data/lexibank_wordlist.csv', [ _ | Words], [functor(lex)]),

    maplist({}/[
        lex(DB, ID, _, _, Segments, _, Glottocode, Family, ConceptID, _, CSID, _, ASJP),
        lex(DB, ID, Family, Glottocode, ConceptID, CSID, Segments, ASJP)
    ]>>true, Words, Lexicon),
    maplist(assertz, Lexicon),

    include({}/[lex(_, _, _, _, _, CSID, _, _)]>>(\+ CSID = ''), Lexicon, LexCogs),
    maplist(replace_functor(lex_cog), LexCogs, LexiconWithCognates),
    maplist(assertz, LexiconWithCognates),

    maplist({}/[
        lex(DB, _, _, _, _, _, _, _, ConceptID, Gloss, _, _, _),
        concept(DB, ConceptID, Gloss)
    ]>>true, Words, AllConcepts),
    sort(AllConcepts, Concepts),
    maplist(assertz, Concepts),

    maplist({}/[
        lex(_, _, _, _, _, GlottologName, Glottocode, _, _, _, _, _, _),
        glottolog(Glottocode, GlottologName)
    ]>>true, Words, AllGlottolog),
    sort(AllGlottolog, Glottolog),
    maplist(assertz, Glottolog).


compile_statistics(ConceptStats) :-
    findall(
        concept(DB, Family, ConceptID, CSID),
        lex_cog(DB, _, Family, _, ConceptID, CSID, _, _),
        Concepts
    ),
    sort(Concepts, SortedConcepts),
    maplist({}/[concept(DB, Family, ConceptID, _), concept(DB, Family, ConceptID)]>>true, SortedConcepts, PureConcepts),
    frequencies(PureConcepts, ConceptCounts),
    maplist(
        {}/[concept(DB, Family, ConceptID)-Count, stat_concept(DB, Family, ConceptID, Count)]>>true,
        ConceptCounts,
        ConceptStats).

random_words(DB, Family, ConceptID, CSID, NumWords, SelectedWords) :-
    findall(
        word(ID, Glottocode, ASJP),
        lex_cog(DB, ID, Family, Glottocode, ConceptID, CSID, _, ASJP),
        Words),
    sample(Words, NumWords, SelectedWords).

random_pairs(CognatePair, NonCognatePair) :-
    findall(
        family_concept(DB, Family, ConceptID),
        (   stat_concept(DB, Family, ConceptID, Count),
            Count > 4
        ), 
        LargeConcepts),
    random_member(family_concept(DB, Family, ConceptID), LargeConcepts),

    findall(CSID, lex_cog(DB, _, Family, _, ConceptID, CSID, _, _), CSIDs),
    frequencies(CSIDs, CognatesetCounts),
    exclude({}/[CSID-1]>>true, CognatesetCounts, LargeCognatesets),
    maplist({}/[CSID-_, CSID]>>true, LargeCognatesets, LargeCSIDs),
    length(LargeCSIDs, L),
    (   L < 2
    ->  random_pairs(CognatePair, NonCognatePair)
    ;   (   sample(LargeCSIDs, 2, [CognatesetID1, CognatesetID2]),

            random_words(DB, Family, ConceptID, CognatesetID1, 2, [Word1, Word2]),
            random_words(DB, Family, ConceptID, CognatesetID2, 1, [NonCognate]),
    
            CognatePair = cog(DB, CSID, Family, ConceptID, Word1, Word2),
            NonCognatePair = non_cog(DB, CSID, Family, ConceptID, Word1, NonCognate)
        )
    ).

create_dataset(NumTraining, NumValidation, Dataset) :-
    Total is NumTraining + NumValidation,
    range(1, Total, R),
    maplist({}/[_, [cog(ASJP1, ASJP2), non_cog(ASJP3, ASJP4)]]>>(random_pairs(
        cog(_, _, _, _, word(_, _, ASJP1), word(_, _, ASJP2)), 
        non_cog(_, _, _, _, word(_, _, ASJP3), word(_, _, ASJP4)))), 
        R, Pairs),
    
    length(TrainingSet, NumTraining),
    length(ValidationSet, NumValidation),
    append(TrainingSet, ValidationSet, Pairs),

    flatten(TrainingSet, FlatTrainingSet),
    flatten(ValidationSet, FlatValidationSet),

    Dataset = dataset(FlatTrainingSet, FlatValidationSet).

align_dataset(Dataset, Alignments) :-
    tell('dataset.csv'),
    maplist({}/[Pair]>>(Pair =.. [_, W1, W2], format('~w,~w~n', [W1, W2])), Dataset),
    told,
    
    run('julia ./pairwise-alignment/get_pairwise_alignment.jl dataset.csv', Output),
    atomics_to_string(Lines, '\n', Output),
    length(Tail, 2),
    append(AlignmentStrings, Tail, Lines),

    maplist({}/[Pair, PairType]>>(functor(Pair, PairType, 2)), Dataset, PairTypes),
    maplist({}/[PairType, AlignmentString, AlignedPair]>>
        (   atomics_to_string(Words, ',', AlignmentString),
            AlignedPair =.. [PairType | Words]
        ),
        PairTypes,
        AlignmentStrings,
        Alignments).

format_alignment(Quad, Alignment) :-
    Quad =.. [PairType, W1, W2, W3, W4],
    atom_chars(W3, CW3),
    atom_chars(W4, CW4),
    maplist({}/[C3, C4, O]>>(format(atom(O), '(~w, ~w)', [C3, C4])), CW3, CW4, AlignedCodes),
    format(string(Alignment), '| ~w | ~w | ~w | ~w |', [PairType, W1, W2, AlignedCodes]).


system_prompt(SystemPrompt) :-
    SystemPrompt = {|string||
    You are an expert in phonetics, phonology, and diachronic linguistics.  
    You are a master at applying the Comparative Method.
    |}.

init :-
    read_asjp_symbols(_),
    read_wordlist,
    compile_statistics(ConceptStats),
    maplist(assertz, ConceptStats),
    init_openai_key.

