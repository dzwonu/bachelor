# Bachelor

Expert system for the evaluation of listed companies using technical analysis.


# Grammar for parsing rules

RULE = EXPR' >> 'FACT

EXPR = '('EXPR' 'OpL' 'EXPR')' | '('WSK' 'OpA' 'NUM')' | '('WSK' 'OpA' 'WSK')' | '('FACT')'

OpL = 'AND' | 'OR'

OpA = '>' | '<' | '=='

WSK = #'[A-Z]+[0-9]*'

FACT = #'[a-z]+[0-9]*'

NUM = #'[-+]?[0-9]+[.]?[0-9]*|[0-9]+'

