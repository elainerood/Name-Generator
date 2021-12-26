# Name Generator

I've long thought about writing a fantasy-name generation script, but always get tripped up by the linguistic side - sure, I can throw a bunch of letters or syllables together, but how do you consistently get *good* results? [Martin O'Leary's name generator](https://github.com/mewo2/naming-language) takes an interesting approach to this, and (importantly for me) has the script available for anyone to look at or use.

I thought it would be fun to translate that script from Javascript into R so that I could play with it at my leisure. Because here's a complication: I never learned any Javascript. However, using my knowledge of R and Python, along with Martin's notes on what he was trying to do, I could get a general idea of what was happening. Function documentation, experimentation (shoutout to [JSFiddle](https://jsfiddle.net/)), and StackOverflow let me figure out the rest, like language-specific quirks.

In its current form, my script is largely a literal translation, except for structural changes made to avoid modifying global variables from within functions. I did a few tweaks to function arguments to suit my own desires, but otherwise haven't had the time to do much playing with this, but I would like to do so eventually.

## Potential future updates:

-   Improve documentation (standardize format of function description, better comment contents/location, etc)

-   Review the script for portions that can be removed (e.g., `add_genitive()` and `add_definite()`)

-   Set up a `make_custom_language()` function, which would let the user specify certain language characteristics, while the rest will either remain the default or be selected randomly

    -   Will require a sanity check for syllable/word lengths

-   Add argument to language-part functions to allow generation of multiple morphemes/words/etc at once

-   Add language characteristics that allow for changing percentage of:

    -   appearance rate for optional syllables (e.g., 70% "CVC" / 30% "VC" for structure "C?VC", vs 50/50)
    -   single-word names (currently always 50%)
    -   `key` morpheme being included in a name (currently always 60%)
    -   genitive or definite articles being used in names (currently always 50% and 10%, respectively)

-   Expand to multiple syllable-structure options (e.g., randomly pick between "CVC" and "CVL?V", instead of only ever using one or the other)

    -   Further expansion of this: allow user specification of the structure, like the [Rinkworks](http://rinkworks.com/namegen/) name generator

-   Tweak limits for min/max syllable and characters-per-word

    -   If there are often 4 letters in generated syllables, the words may tend to be longer on average, but with the current system the maximum character limit for words is completely independent of syllable structure (length)

-   Consider moving from the exponent system to using a non-uniform probability distribution in the first place, to see how much that affects the outputs

    -   Languages would instead have parameter(s) for that distribution (beta?)

-   Tidyverse translation (e.g., use purrr instead of loops when it makes sense to do so, bring in dplyr for `case_when()` to replace repeated if/else)

-   Shiny app
