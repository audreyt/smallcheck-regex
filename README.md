smallcheck-regex
================

This module exports a `Matching` type that turns a type-annotated regular expression into a generator for all strings matching that regex.

For example:

    $ ghci -XDataKinds
    >>> import Test.SmallCheck.Regex (Matching)
    >>> import Test.SmallCheck.Series (list, series)
    >>> list 10 series :: [Matching "a?b?c?"]
    [,a,b,ab,c,ac,bc,abc]

# CC0 1.0 Universal

To the extent possible under law, 唐鳳 has waived all copyright
and related or neighboring rights to smallcheck-regex.

This work is published from Taiwan.

http://creativecommons.org/publicdomain/zero/1.0
