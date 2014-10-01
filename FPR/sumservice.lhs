> module SumService where
> import WebParser
> import HTMLPage

=========================================
sumservice module, written for assignment
=========================================

Function generates HTML page based on dictionaries containing query input. Takes a pair 
of dictionaries, one of Ints, one of Strings, which contain the parsed parameters
from the query string. Returns an HTMLPage based on the "stagereq" param. Checks that 
correct parameters provided for both 'final' and 'middle' stages. 

If neither middle or final stages were validated but either of the input dictionaries
contain query elements, then shows an error page. Otherwise shows the page requesting 
the first value to add.

> sumservice :: (Dict String, Dict Int) -> HTMLPage
> sumservice (ds,di)
>     | (snd stagereq) == "final" && (fst m == "m") && (fst n == "n")
>                                        = sumfinal (snd m) (snd n)
>     | (snd stagereq) == "middle" && (fst m == "m")
>                                        = summiddle (snd m)
>     | length(ds) > 0 || length(di) > 0 = sumerror
>     | otherwise                        = suminitial
>    where stagereq = look "stagereq" "" ds
>          m        = look "m" 0 di
>          n        = look "n" 0 di

HTML Page definition for initial page. Next-stage request incorporated into
page as hidden field.

> suminitial :: HTMLPage
> suminitial = HTMLPage "Staged addition form (enter first term)" 
>  (HTMLBody 
>   [HTMLHeader (HTMLText [HTMLPlain "Staged addition service"]),
>    HTMLPara (HTMLText [HTMLPlain "First number:"]),
>    HTMLForm "webhandler.cgi" 
>       [HTMLInput Hidden "stagereq" "middle",
>        HTMLInput Text "m" "",
>        HTMLInput Submit "submit" "Submit first term"]])

HTML Page definition for intermediate page. Next-stage request and previously
entered value of m incorporated as hidden fields.

> summiddle :: Int-> HTMLPage
> summiddle m = HTMLPage "Staged addition intermediate page (enter second term)" 
>  (HTMLBody 
>   [HTMLPara (HTMLText [HTMLPlain ("Your first number was " ++ (show m))]),
>    HTMLPara (HTMLText [HTMLPlain "Second number:"]),
>    HTMLForm "webhandler.cgi" 
>       [HTMLInput Hidden "stagereq" "final",
>        HTMLInput Hidden "m" (show m),
>        HTMLInput Text "n" "",
>        HTMLInput Submit "submit" "Perform addition"]])

HTML Page definition for final page, just shows results of addition.

> sumfinal :: Int-> Int-> HTMLPage
> sumfinal n m = HTMLPage "Addition results page" 
>    (HTMLBody 
>      [HTMLHeader (HTMLText [HTMLPlain "Addition results"]),
>       HTMLPara   (HTMLText [HTMLPlain sumtext])])
>  where sumtext = "The sum of " ++ (show m) ++ " and " ++ (show n) ++ " is " ++ (show (m+n))

HTML page definition for an error page.

> sumerror :: HTMLPage
> sumerror = HTMLPage "Error" 
>    (HTMLBody 
>      [HTMLHeader (HTMLText [HTMLPlain "Invalid input!"]),
>       HTMLPara (HTMLText [HTMLPlain "The staged sum service received invalid input"])])


