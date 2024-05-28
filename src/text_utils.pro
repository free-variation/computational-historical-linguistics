:- use_module(library(yall)).

% extremely useful debugging utility
:- op(920, fy, *).
* _.

track :-
    leash(-all),
    visible(-all),
    visible(+fail),
    trace.

notrack :-
    leash(+all),
    visible(+all),
    notrace.

relations_to_lists(Relations, Lists) :-
    maplist({}/[Relation, List]>>(Relation =.. [_ | List]), Relations, Lists).

pair(X, Y, [X, Y]).
zip_lists(L1, L2, ZippedLists) :- 
    maplist(pair, L1, L2, ZippedLists).

chunk_list(Size, List, CurrentChunks, Chunks) :-
    length(List, Length),
    Length =< Size,
    append(CurrentChunks, [List], Chunks).

chunk_list(Size, List, CurrentChunks, Chunks) :-
    length(List, Length),
    Length > Size,
    length(Chunk, Size),
    append(Chunk, Rest, List),
    append(CurrentChunks, [Chunk], NewCurrentChunks),
    chunk_list(Size, Rest, NewCurrentChunks, Chunks).

chunk_list(Size, List, Chunks) :-
    chunk_list(Size, List, [], Chunks).

even(N) :-
    mod(N, 2) =:= 0.

chunk_list_even(NumChunks, List, Chunks) :-
    length(List, Length),
    Size is div(Length, NumChunks),
    (   even(Size)
    ->  ChunkSize = Size
    ;   ChunkSize is Size + 1
    ),
    FirstChunkSize is ChunkSize - 1,
    append(FirstChunk, Rest, List),
    length(FirstChunk, FirstChunkSize),
    chunk_list(ChunkSize, Rest, RestChunks),
    append([FirstChunk], RestChunks, Chunks).

header_cell_separator(_, '----').

header_separator(Headers, Separator) :-
    maplist(header_cell_separator, Headers, Separators),
    atomics_to_string(Separators, ' | ', Separator).

data_row(Data, Row) :-
    atomics_to_string(Data, ' | ', DataRow),
    format(atom(Row), '| ~w |~n', DataRow).

ensure_rows([], []).
ensure_rows(Data, Data) :-
    Data = [First | _],
    is_list(First).
ensure_rows(Data, Lists) :-
    Data = [First | _],
    compound(First),
    \+ is_list(First),
    rows_to_lists(Data, Lists).

markdown_table(Headers, Data, MDTable) :-
    ensure_rows(Data, Rows),
    % construct the header
    atomics_to_string(Headers, ' | ', HeaderRow),
    header_separator(Headers, Separator),
    format(atom(Header), '| ~w |~n| ~w |~n', [HeaderRow, Separator]),

    % format the data rows
    maplist(data_row, Rows, MDRows),
    atomics_to_string(MDRows, '', AllRows),

    % put it all together
    format(atom(MDTable), '~w~w', [Header, AllRows]).


write_tsv_row([]) :- !.
write_tsv_row([Datum]) :-
    format('~p~n', Datum), !.

write_tsv_row([Datum | Data]) :-
    format('~p\t', Datum),
    write_tsv_row(Data).

write_tsv(Filename, Header, Data) :-
    ensure_rows(Data, Rows),
    tell(Filename),
    write_tsv_row(Header),
    maplist(write_tsv_row, Rows),
    told.


remove_quotes(QuotedString, UnquotedString) :-
    (   string(QuotedString); atom(QuotedString) ),
    re_matchsub("^\"(.*)\"$", QuotedString, Sub),
    UnquotedString = Sub.1, !.
remove_quotes(X, X).

range(From, To, Values) :-
    findall(Value, between(From, To, Value), Values).

run(Command, Output) :-
    setup_call_cleanup(
        process_create('/bin/sh', ['-c', Command],
                       [ stdout(pipe(Out))
                       ]),
        read_string(Out, _, Output),
        close(Out)).

strip(S, S1) :-
    normalize_space(string(S1), S).

markdown_row_to_list(MarkdownRow, List) :-
    atomics_to_string(Pieces, '|', MarkdownRow),
    Pieces = [_ | RestPieces],
    reverse(RestPieces, [_ | RevPieces]),
    reverse(RevPieces, Bits),
    maplist(strip, Bits, List).

markdown_table_to_lists(MarkdownTable, Lists) :-
    atomics_to_string(MarkdownRows, '\\n', MarkdownTable),
    MarkdownRows = [_, _ | RestRows],
    exclude({}/['']>>(true), RestRows, RestRows1),
    maplist(markdown_row_to_list, RestRows1, Lists).

pairwise_combinations(List, Pairs) :-
    findall(SortedXY, (
        select(X, List, List1), 
        select(Y, List1, _), 
        msort([X, Y], SortedXY)), 
        AllPairs), 
    sort(AllPairs, Pairs).

sample(List, NumSamples, Samples) :-
    random_permutation(List, ShuffledList),
    length(Samples, NumSamples),
    append(Samples, _, ShuffledList).

replace_functor(NewFunctor, Term, NewTerm) :-
    Term =.. [_ | Args],
    NewTerm =.. [NewFunctor | Args].

frequencies(List, RunLengths) :-
    msort(List, SortedList),
    clumped(SortedList, RunLengths).




    