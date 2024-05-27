:- use_module(library(csv)).
:- use_module(library(strings)).
:- use_module(library(yall)).

:- ['src/text_utils.pro', 'openai_utils.pro', 'src/cognates.pro'].


create_find_alignments_prompt(Word, Words, Prompt) :-
    asjp_to_markdown(ASJPMarkdownTable),
    maplist({}/[word(_, _, _, _, _, Transcription), [Transcription]]>>true, Words, Transcriptions),
    markdown_table(['Transcription'], Transcriptions, TranscriptionsMarkdown),
    Word = word(_, _, _, _, _, Transcription),

    Prompt = {|string(Transcription, TranscriptionsMarkdown, ASJPMarkdownTable) ||

I need help aligning the phonetic segments of the word "{Transcription}" to the phonetic segments of a series of words in other languages that share the same meaning.
The words are transcribed using a special orthography in which a single alphanumeric symbol represents a set of phonetic features.   

Objective:
Align the characters of the word "{Transcription}"" to the characters of each word in the target word list in turn, so that each phonetic segment from one language corresponds as closely as possible to a segment in the other language.

Rules:

    - Respect Phonetic Closeness: Align phonetic segments to maximize the number of shared phonetic features for each pair of aligned segments.  This means, for example, that consonants are usually not aligned with vowels.
    - No Reordering: Maintain the original order of segments in both words; do not rearrange characters to force alignments.
    - No Deletions or Changes to segments.  Make sure you do not change any of the segments in a word.
    - Phonetic Transcriptions: Take special note of numbers or symbols (like '3' for schwa sounds) and treat them according to their phonetic value rather than their literal character.
    - Add Gaps to Improve Phonetic Feature Similarity.  If you find that you are aligning two segments that share few or zero phonetic features, such as a stop with a vowel, then you may add gaps, represented as '-', within one or the other word, to shift segments in one word relative to the other, to improve the phonetic match.  For example, in aligning "zun" with "khala", we probably want a gap in "zun" to align with the "h" in "khala" so that the vowel "u" aligns with "a" and the alveolar nasal "n" aligns with the liquid alveolar "l".
    - Add Gaps When Necessary: if the transcriptions of two words in an alignment pair are of different lengths, then you will need to add gaps, represented as '-', to the shorter word.  These could be added to the start of the word, within the word, or at the end.
    - Minimize Gaps: Introduce as few gaps as necessary, but use them to avoid aligning dissimilar phonetic elements.

Here is the table of phonetic symbols:
{ASJPMarkdownTable}

Please note that the feature sets in the table may contain contrastive features, meaning that the symbol in question represents multiple values for some features.  For example, 'X' can represent both voiced and unvoiced phones.
The symbols therefore underspecify complete phones.

Examples for Alignment:

    Example 1: Spanish "sangre" and Romanian "s3nje"
        Correct Alignment:
            (s, s)
            (a, 3)
            (n, n)
            (g, j)
            (r, -)
            (e, e)

    Explanation: Here, '3' represents a schwa, which is aligned with 'a' due to its vowel-like nature. The consonant 'g' in Spanish, which has no perfect match in Romanian, is forced to align with 'j', and 'r' is placed with a gap, represented by '-', due to the lack of a corresponding sound in Romanian.
    Notice that a gap '-' was inserted into the Romanian word, to shift the word-final 'e' so it matches the word-final 'e' in the Spanish word.

    Example 2: Spanish "persona" and Romanian "persoan3"
        Suggested Alignment:
            (p, p)
            (e, e)
            (r, r)
            (s, s)
            (o, o)
            (-, a)
            (n, n)
            (a, 3)

    Explanation: The alignment respects the phonetic closeness as much as possible, with '3' as schwa aligning to 'a' in Spanish, and the 'a' in Romanian being placed with a gap '-' so at to preserve the phonetic closeness of the following two segments.

    Example 3: "zun" and "draunikan"
        Suggested Alignment:
            (z, d)
            (-, r)
            (-, a)
            (u, u)
            (n, n)
            (-, i)
            (-, k)
            (-, a)
            (-, n)

        Explanation: The alignment respects the phonetic closeness as much as possible, with voiced alveolars "z" and "d" aligning.  But then note that further into "draunikan" there's a sequence "-un-", which "zun" also contains.  So we add gaps to "zun" so that "u" and "n" align.  

    Here is the list of words to be aligned to {Transcription}:
    {TranscriptionsMarkdown}

    Return your alignments as a Markdown table, with columns:
    - Aligned Word: this is just "{Transcription}"
    - Target Word: the transcription of the target word
    - Alignment: a list of tuples, e.g. (s,s) (a,3) (n,n) (g,j) (r,-) (e,e) from the example above.  There should be as many tuples as there are characters in the longest word of the alignment pair.  Pad the shorter word with '-' as needed.

    Attempt an alignment for every word in the target words table.

    Return only the Markdown table of alignments, without further commentary, introductory language, or evaluations.

    Take your time and work step by step, considering each word pair in turn.
    |}.

align(Word, Words, Alignments) :-
    append(FirstWords, _, Words),
    length(FirstWords, 10), % otherwise gpt-4 takes FOREVER ...
    create_find_alignments_prompt(Word, FirstWords, AlignPrompt),
    
    EvaluatePrompt1 = {|string||
    Evaluate and, where necessary, revise your alignments.  Did you insert gaps in places that make linguistic sense, i.e. to maximize phonetic similarity, or merely at the end of the shorter word in each pair?  
    
    It is not likely (though not impossible) for a consonant to align with a pure vowel, or a vowel with a consonant.  If you proposed alignments of this sort, see if you can insert gaps so that vowels match vowels and consonant match consonants.

    Is every character in each word represented in the alignments?  If not, add the deleted ones back into the alignment.

    Did you mistakenly change the words, altering one or more segments? If so, restore them to their exact original forms.
    Return just the newest table of revised alignments, without commentary nor extra language.
    |},

    run_CoT([AlignPrompt, EvaluatePrompt1], _, Responses),

    last(Responses, Response),
    response_content(Response, Content),
    Alignments = Content.

random_align(Alignments) :-
     random_concept_set_with_cognates([Word | Words]),
     align(Word, Words, Alignments).
