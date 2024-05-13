
:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).
:- use_module(library(janus)).

init_openai_key:-
   getenv('OPENAI_API_KEY',Key),
   create_prolog_flag(trace_openai, true, [type(atom)]),
   create_prolog_flag(openai_key,Key,[type(atom)]).

extract_model_ids(json(Objects), ModelIDs) :-
    member(data = Models, Objects),
    findall(ID, (member(json(ModelSpecs), Models), member(id = ID, ModelSpecs)), ModelIDs).

list_models(Models):-
   current_prolog_flag(openai_key,Key),

   http_get('https://api.openai.com/v1/models', Models,
            [authorization(bearer(Key)), application/json]).

num_tokens(Text, NumTokens) :-
    num_tokens('cl100k_base', Text, NumTokens).

num_tokens(EncodingName, Text, NumTokens) :-
    py_call(tiktoken:get_encoding(EncodingName), Encoding),
    py_call(Encoding:encode(Text), EncodedText),
    length(EncodedText, NumTokens).


create_message(Role, Content, Message) :-
    Message = json([role = Role, content = Content]).

create_system_message(Message) :-
    system_prompt(SystemPrompt),
    create_message('system', SystemPrompt, Message).

chat_with_gpt(Messages, Response) :-
    chat_with_gpt('gpt-4', Messages, [temperature = 0], 3, false, Response).

max_context('gpt-4', 8192).
max_context('gpt-4-turbo', 32000).

chat_with_gpt(Model, Messages, Options, Attempts, Trace, FinalResponse) :-
    Attempts > 0,

    current_prolog_flag(openai_key,Key),
    atom_json_term(Request, json([model = Model, messages = Messages | Options]), []),

    format(atom(FormattedRequest), '~w', Request),
    (   Trace = true
    ->  format('~n~n~w~n~n', FormattedRequest)
    ; true
    ),

    catch(
        (
            http_post(
                'https://api.openai.com/v1/chat/completions', 
                atom(application/json, FormattedRequest), 
                Result,
                [authorization(bearer(Key)), application/json]),
            Response = Result,
            (   Trace = true
            ->  format('~n~w~n', Result)
            ;   true
            )
        ),
        error(Error, Context),
        (
            format('~w ~w~n', [Error, Context]),
            sleep(10),
            OneFewerAttempts is Attempts - 1,
            chat_with_gpt(Model, Messages, Options, OneFewerAttempts, Trace, Response)
        )
    ),
    response_size(Response, NumTokens),
    max_context(Model, MaxContext),
    (   NumTokens > MaxContext
    ->  (   format('\tresponse too large: ~w~n\trequest: ~w~n~n\tresponse: ~w~n', [NumTokens, FormattedRequest, Response]),
            OneFewerAttempts is Attempts - 1,
            chat_with_gpt(Model, Messages, Options, OneFewerAttempts, Trace, FinalResponse)
        )
    ; FinalResponse = Response
    ).

response_message(json(Response), Message) :-
    member(choices = [json(Choice0) | _], Response),
    member(message = Message, Choice0).

response_content(Response, Content) :-
    response_message(Response, json(Message)),
    member(content = Content, Message).

response_size(json(Response), Size) :-
    member(usage = json(Usage), Response),
    member(total_tokens = Size, Usage).

run_CoT(Prompts, Messages, Responses) :-
    current_prolog_flag(trace_openai, Trace),
    run_CoT('gpt-4', Prompts, [temperature = 0], Trace, Messages, Responses).

run_CoT(Model, Prompts, Messages, Responses) :-
    current_prolog_flag(trace_openai, Trace),
    run_CoT(Model, Prompts, [temperature = 0], Trace, Messages, Responses).

run_CoT(Model, Prompts, Options, Trace, Messages, Responses) :-
    create_system_message(SystemMessage),
    InitialMessages = [SystemMessage],
    run_CoT(Model, Prompts, Options, Trace, InitialMessages, Messages, [], Responses).

run_CoT(_, [], _, _, Messages, Messages, Responses, Responses).

run_CoT(Model, [Prompt | Prompts], Options, Trace, CurrentMessages, Messages, CurrentResponses, Responses) :-
    create_message('user', Prompt, Message),
    append(CurrentMessages, [Message], NewMessages),

    sub_string(Prompt, 0, 64, _, PromptStart),
    format('~w', PromptStart),
    
    chat_with_gpt(Model, NewMessages, Options, 3, Trace, Response),
    
    response_message(Response, ResponseMessage),
    append(NewMessages, [ResponseMessage], NewMessages1),
    append(CurrentResponses, [Response], NewResponses),

    response_size(Response, Size),
    format('\t~w~n', [Size]),
    
    run_CoT(Model, Prompts, Options, Trace, NewMessages1, Messages, NewResponses, Responses).



