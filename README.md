# CustomCollapser
Haskell tool to collapse and expand general texts surrounded by custom identifiers, to navigate and view the text more easily. 

___

# install
 - stack build
 - stack install

___

# Usage:

# OPTIONS : 

| Option | Arg Spec |
|--------|-----|
| -e | list of nodes you want expanded  |
| -l | left identifier | 
| -r | right indentifier | 
| -L | outputs the text with new left identifier |
| -R | outputs the text with new right identifier |

```sh
> echo "{{{HI . {{{see this by passing [0,1]}}}. no secret message here}}}" | CustomCollapser-exe -e []
{{{0}}}

> echo "{{{HI . {{{see this by passing [0,1]}}}. no secret message here}}}" | CustomCollapser-exe -e [0]
{{{HI . {{{1}}}. no secret message here}}}

> echo "{{{HI . {{{see this by passing [0,1]}}}. no secret message here}}}" | CustomCollapser-exe -e [0,1]
{{{HI . {{{see this by passing [0,1]}}}. no secret message here}}}

> echo "{{{{{{no way {{{I let you read the message}}}}}} can {{{junk here}}} you {{{to distract you}}} even {{{from reading the message}}} read {{{wow it works}}} this ???}}}" | CustomCollapser-exe
{{{{{{1}}} can {{{3}}} you {{{4}}} even {{{5}}} read {{{6}}} this ???}}}

> echo "{{{{{{no way {{{I let you read the message}}}}}} can {{{junk here}}} you {{{to distract you}}} even {{{from reading the message}}} read {{{wow it works}}} this ???}}}" | CustomCollapser-exe -e [0,3]
{{{{{{1}}} can {{{junk here}}} you {{{4}}} even {{{5}}} read {{{6}}} this ???}}}


```

