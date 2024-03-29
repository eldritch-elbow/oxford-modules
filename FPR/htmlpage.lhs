> module HTMLPage where

===========================================================================
A datatype of HTML pages. Additions to skeleton are at the top of the file,
skeleton code that is unchanged is at the bottom of the file
=========================================================================== 

HTMLBlock data structure.

The HTMLForm variant of HTMLBlock has a 'form action' followed by a list of HTMLBlocks. 
This provides the basic form structure, i.e. form tags with an associated action, 
enclosing an arbitrary number of HTML Blocks.

The HTMLInput variant of HTMLBlock has a type, name, value. These are the constituent 
attributes of an HTML input element. Not defined as recursive in this case given 
required scenario, but it is possible that other scenarios exist that would require
enclosing of HTMLBlocks within input elements.

The structure allows for Input elements to exist outside of Form elements, and also 
allows for recursive Forms. I wasn't sure whether to allow this but Firefox seems happy 
to render the elements so I left it in as it seems to be a nice and simple solution.

> data HTMLBlock = HTMLHeader HTMLText
>                | HTMLPara HTMLText
>                | HTMLForm HTMLFormAction [HTMLBlock]
>                | HTMLInput HTMLInputType HTMLInputName HTMLInputValue


Form action, Input name, and Input value are all defined as strings. 

> type HTMLFormAction = String
> type HTMLInputName = String
> type HTMLInputValue = String

Definition of valid input types: Text input, Hidden field, and Submit button.
Derives Enum and Show to save having to write a separate instance of Show, though 
this does tie the HTMLInputType variants to being the same as the valid HTML input
type strings.

> data HTMLInputType = Text | Hidden | Submit
>   deriving (Show, Enum)

Show instance of HTMLBlock, modified to output Forms and Input fields.

Show of HTMLForm generates the HTML for the form elements using tagattr, where the 
ACTION attribute is generated from the associated HTMLFormAction. Enclosed HTML is 
generated by concatenating together a map of 'shows' of HTMLBlocks associated with  
the HTMLForm element.

Show of HTMLInput also uses tagattr, where attributes are created from HTMLInputType, 
HTMLInputName, and HTMLInputValue. HTMLInput is not a recursive data structure, so 
enclosed HTML is just an empty string.

> instance Show HTMLBlock where
>   show (HTMLHeader t)          = tag "H1" (show t)
>   show (HTMLPara t)            = tag "P" (show t)
>   show (HTMLForm a b)          = tagattr "FORM" [("ACTION",a)] (concat (map show b))
>   show (HTMLInput tp name val) = tagattr "INPUT" attribs ""
>      where attribs = [("TYPE",show tp),("NAME",name),("VALUE",val)]

===================================================================================
Unchanged skeleton code - includes data structures and Show instances for HTMLPage, 
Body, Text, and Word, as well as HTTPAttrs type, tag, and tagattr functions.
===================================================================================

Data structures...

> data HTMLPage = HTMLPage String HTMLBody
> data HTMLBody = HTMLBody [ HTMLBlock ]
> data HTMLText = HTMLText [ HTMLWord ]
> data HTMLWord = HTMLEmph HTMLText
>               | HTMLPlain String

Instances of Show...

> instance Show HTMLPage where 
>   show (HTMLPage title body) 
>     = tag "HTML" (tag "HEAD" (tag "TITLE" (unlines [title])) ++ 
>                   tag "BODY" (show body))

> instance Show HTMLBody where
>   show (HTMLBody blocks) = concat (map show blocks)

> instance Show HTMLText where
>   show (HTMLText ws) = concat (map show ws)

> instance Show HTMLWord where
>   show (HTMLEmph t) = tag "EM" (show t)
>   show (HTMLPlain s) = s ++ "\n"

Type for HTML attributes...

> type HTMLAttrs = [(String,String)]

Function to generate tag with no attributes...

> tag :: String -> String -> String
> tag t s = tagattr t [] s

Function to generate tag with attributes...

> tagattr :: String -> HTMLAttrs -> String -> String
> tagattr t attrs s = "<" ++ t ++ concat (map attr attrs) ++ ">\n" ++
>   s ++ "</" ++ t ++ ">\n"
>   where attr (n,v) = " " ++ n ++ "=\"" ++ v ++ "\""
