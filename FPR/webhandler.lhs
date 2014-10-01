> module Main where
> import System.Environment(getEnv)
> import WebParser
> import SumService

================
Main web handler 
================

Modified from the skeleton to generate a result page from the sumservice module, which 
takes a pair of dictionaries - one of Int, one of String. These are parsed from the 
query string. 

Assumes that all necessary alphabetic query strings will be at the start of the query, 
followed by integer query strings. This is currently only enforced by the ordering of 
the fields in the HTML pages.

Considered using cookies as an alternative to URL string, however CGI specification
does not seem to support passing cookies to CGI programs.

> main = do
>    q <- getEnv "QUERY_STRING"
>    putStrLn "Content-Type: text/html; charset=utf-8"
>    putStrLn ""
>    putStrLn (show (sumservice 
>                  (fst (head (((dict alphastr) ... (dict integer)) q)))))





