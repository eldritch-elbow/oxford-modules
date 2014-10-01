> module WebParser where
> import Char

======================================================================
Parsers written for the assignment or written for lab-based practicals
======================================================================

Parser of single digit integers. Takes the first char from the input string, verifies 
that it's in the correct range, then subtracts 48 from the ASCII value to get the 
integer value.

> intchr :: Parser Int
> intchr [] = []
> intchr (c:cs)
>       | ((ord c >= 48) && (ord c <= 57)) = [(ord c-48,cs)]
>       | otherwise = []

Int parser. Parses the input string for an optional '+' or '-' character, followed by at
least one integer character via the intchr parser. If intermediate parse is successful, 
construct result pair using makeint (below), otherwise, return an empty result list.

> integer :: Parser Int
> integer s
>       | length parsed == 0  = []
>       | otherwise           = [(makeint (fst (head parsed)), snd (head parsed))]
>     where parsed  = (((lit '-') ||| (lit '+')) `opt` '+' ... (some intchr)) s

Given a pair consisting of a character and a list of integer digits, constructs an integer
value. Uses fold left to generate an int from the list. Resulting int will be negated if
the character equals '-'. This function supports parsing of negative integers.

> makeint :: (Char, [Int]) -> Int
> makeint (c,is)
>       | c == '-'     = -result
>       | otherwise    = result
>   where result = foldl (\ x y -> (x*10) + y) 0 is


Parser of literal chars. Uses 'satisfy' with equality operator to parse the first 
character in the input string against the specified character.

> lit :: Char -> Parser Char
> lit x = satisfy (==x)

String parser. Combines 'some' and local 'isAlpha' to parse for a string containing 
alphabetic characters only. Result is a Parser of strings because 'some' is a 
Parser that creates list results. List of characters == string.

> alphastr :: Parser String
> alphastr = some (satisfy (isAlpha))
>   where isAlpha = (\ ch -> ((ord ch >= 65) && (ord ch <= 90)) ||
>                            ((ord ch >= 97) && (ord ch <= 122)))

Combinators which throw away results of either right or left hand Parser argument.
'...' creates pairs of parsed results, so 'using' fst or snd will throw away the 
first or second element of the pair.

> (..*) :: Parser a -> Parser b -> Parser a
> p1 ..* p2 = p1 ... p2 `using` fst 
> (*..) :: Parser a -> Parser b -> Parser b
> p1 *.. p2 = p1 ... p2 `using` snd 

Parser for web query elements. Expects: an alphabetic string, an equals sign,
a parameterised type, and an optional ampersand. The equals sign and the optional 
ampersand are discarded from the results which are pairs of Strings and
parameterised types.

The "opt 'a'" provides a simple way of parsing for optional, throw-away characters 
(in this case the delimiting '&' character. Whatever the result of 'opt', the 
..* throws it away anyway. 

> queryElement :: Parser a -> Parser (String, a)
> queryElement p = (((alphastr ..* (lit '=')) ... p) ..* ((lit '&') `opt` 'Z'))

Parameterised Parser for dictionaries. Combines 'many' and 'queryElement' to
create a list of "string-to-a" pairs.

> type Dict a = [(String,a)]
> dict :: Parser a -> Parser (Dict a)
> dict p = many (queryElement p)

Parameterised Dictionary lookup function. Returns the full pair from the dictionary,
this is to provide a way of knowing if the lookup failed, i.e. in case of error, a 
pair containing an empty string for the key and the second parameter value is returned.

> look :: String -> a -> Dict a -> (String, a)
> look s av [] = ("",av)
> look s av (m:ms) = if (fst m == s) then m else (look s av ms)


=======================================================================
A few parser related functions adopted from lab-based practicals. These 
functions were adopted as useful helpers in writing the above parsers
=======================================================================

Constructs a list from a pair of a and [a]

> cons :: (a,[a]) -> [a]
> cons (x,xs) = x:xs

Given a default result, returns either the result of applying "Parser a" to the input
string, or the default result if "Parser a" returned no results.

> opt :: Parser a -> a -> Parser a
> opt p v inp = [head ((p ||| succeed v) inp)]

'many' and 'some', parse for 0..n and 1..n of a respectively. 'using' cons
causes results to be constructed into a single list result. Some was actually
written as part of practical solutions but is shown here for clarity

> many, some :: Parser a -> Parser [a]
> many p = ((p ... many p) `using` cons) `opt` []
> some p = ((p ... many p) `using` cons)


====================================
Parser basics (copied from skeleton)
====================================

> type Parser a = String -> [(a,String)]

> succeed :: a -> Parser a
> succeed a s = [(a,s)]

> satisfy :: (Char -> Bool) -> Parser Char
> satisfy p [] = []
> satisfy p (c:s) = if p c then [(c,s)] else []

> (...) :: Parser a -> Parser b -> Parser (a,b)
> (p ... q) s = [ ((a,b),s'') | (a,s') <- p s, (b,s'') <- q s']

> (|||) :: Parser a -> Parser a -> Parser a
> (p ||| q) s = p s ++ q s

> using :: Parser a -> (a->b) -> Parser b
> using p f s = [ (f a, out) | (a,out) <- p s]

