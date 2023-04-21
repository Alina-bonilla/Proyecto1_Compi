import java_cup.runtime.Symbol;
import static Clases.token.*;

%%
%class Lexer
%type token

%{
  // Define aquí variables globales y funciones de Java que necesites en las acciones
%}

%public
%cup

%state IN_COMMENT

%{
  // Define aquí las acciones que se ejecutarán cuando se encuentre una determinada expresión regular
%}

---------------------------------------

signos = +|-
equal = \=
endLine = \$
arregloEstatico = "int"[\*]*|"char"[\*]*
tipoBool = "true"|"false"
tipoChar = .
tipoString =  \".*\"
formatoDato = "%d"|"%f"
identificador = [a-zA-Z][a-zA-Z0-9\_]*
operador = \+|-|\*|\/|~|\**
operadorUnario = --|++
expresionesLogicasBool =  "=="|"!="
expresionesLogicasNum = "<"|">"|"<="|">="
operadorRelacional = "^"|"#"


-----------------------------


"int" {return INT;}
"+" {return SUMA;}
"*" {return MULT;}
"==" {return ASSIGN;}
"float" {return FLOAT;}
"-" {return RESTA;}
"/" {return DIV;}
"!=" {return DIFERENTE;}
"char" {return CHAR;}
"=" {return IGUAL;}
"~" {return MODULO;}
"<" {return MENOR;}
"string" {return STRING;}
"$" {return END;}
"**" {return POTENCIA;}
">" {return MAYOR;}
"bool" {return BOOL;}
"true" {return TRUE;}
"--" {return DISMINUIR;}
"<=" {return MENORIGUAL;}
"arregloEstatico" {return LISTE;}
"false" {return FALSE;}
"++" {return AUMENTAR;}
">=" {return MAYORIGUAL;}
"^" {return CONJUCION;}
"#" {return DISYUNCION;}
"main" {return MAIN;}
"if" {return IF;}
"elif" {return ELIF;}
"else" {return ELSE;}
"do while" {return DOWHILE;}
"for" {return FOR;}
"print" {return PRINT;}
"input" {return INPUT;}
"\s" {return ESPACIO;}
"(" {return PAREN1;}
")" {return PAREN2;}
"{" {return LLAVE1;}
"}" {return LLAVE2;}
"[" {return CUADRO1;}
"]" {return CUADRO2;}
"\d" {return NUM;}
"\f" {return DECIMAL;}
"\n" {return ENTER;}
"," {return COMA;}
"!" {return NEGACION;}









---------------------------------------

<numEntero> ::= <signos>?[1-9][\d]*|0
<numFloat> ::= <signos>?[\d]+\.[\d]+

<numEntero> ::= <signos>?[1-9][\d]*|0
<numFloat> ::= <signos>?[\d]+\.[\d]+
<tiposDatos> ::= “int”|“float”|“bool”|“char”|“string”|<arregloEstatico>

-----------------------------
<expresionesLogicas> ::=<expresionesLogicasNum>|<expresionesLogicasBool>
<tiposOperandos> ::= [<identificador>|<numEntero>|<numFloat>|<identificador>\[numEntero\]]

<parametro> ::= <tiposDatos><identificador>

<operandosLogicos> ::= <identificador>|<numEntero>|<numFloat>|<identificador>\[numEntero\]|<llamadaFuncion>
<operacionLogica> ::= \!?<operandosLogicos><expresionesLogicas><operandosLogicos>

<condicionales> ::= [\!?<identificador>]|[<operacionLogica>[<operadorRelacional><operacionLogica>]*]
<ifEstructura> ::= “if”\(<condicionales>\)\{<bloque>\}[“elif”\(<condicionales>\)\{bloque\}]*[else\{bloque\}]?<endLine>
<condicionFor> ::= [int|float]<identificador><equal><operandosLogicos><endLine><condicionales><endLine><operacionUnitaria>
<forEstructura> ::= ”for”\(<condicionFor>\)\{bloque\}<endLine>
<doWhile> ::= “do”\{<bloque>\}”while”\(<condicionales>\)<endLine>
<estructurasControl> ::= <ifEstructura>|<forEstructura>|<doWhile>

<elementosBloque> ::= <sentencias>|<estructurasControl>
<bloque> ::= <elementosBloque>*

<retorno> ::= “return”[<operandosLogicos>|<tipoBool>|<tipoChar><tipoString>]<endLine>
<break> ::= “break”<endLine>

<expresionAritmetica> ::= [<numEnteros>|<numFloat>|<identificador>|<identificador>\[<numEntero>\]]<operador>[<numEnteros>|<numFloat>|<identificador>|<identificador>\[<numEntero>\]]<endLine>
<expresionBoolean> ::= <tiposOperandos>[<expresionesLogicas><tiposOperandos>]*<endLine>

<sentencia> ::= <operacionUnitaria>|<varEnteroCreaAsigOp>|<varChar>|<print>|<input>|<varString>|<varArregloEstatico>|<asignacionArreglo>|<obtenerValorArreglo>|<varFloatCreaAsigOp>|<variableCrea>
<sentencias> ::= <sentencia>|<retorno>|<break>

<variableCrea> ::= <tiposDatos><identificador>*\<endLine>
<varEnteroCreaAsigOp> ::= "int"<identificador><equal><operadorEntero>[<operador><operadorEntero>]*<endLine>
<varFloatCreaAsigOp> ::= "float"<identificador><equal><operadorFloat>[<operador><operadorFloat>]*<endLine>
<varBoolean> ::= "bool"<identificador><equals><tipoBool><endLine>
<varChar> ::= “char”<identificador><equals><operadorChar><endLine>
<varString> ::= “string”<identificador><equals><operadorString>[\+<operadorString>]*<endLine>
<varArregloEstatico> ::= [“int”|”char”]<identificador>\[<numEntero>\]<endLine> 

<operadorEntero> ::= <identificador>|<numEntero>|<identificador>\[<numEntero>\]|<llamadaFuncion>
<operadorFloat> ::= <identificador>|<numFloat>|<identificador>\[<numEntero>\]|<llamadaFuncion>
<operacionUnitaria> ::= [<identificador>|<identificador>\[numEntero\]]<operadorUnario><endLine>
<operadorChar> ::= <tipoChar>|<identificador>|<identificador>\[<numEntero>\]|<llamadaFuncion>
<operadorString> ::= <tipoString>|<identificador>|<llamadaFuncion>
<operandorArreglos> ::= <numEntero>|<tipoChar>|<identificador>\[numEntero]|identificador|expresionAritmetica|<llamadaFuncion>

<asignacionArreglo> ::=  <identificador>\[<numEntero>\]<equals><operadorArreglos><endLine> 
<obtenerValorArreglo> ::= <identificador>\[<numEntero>\]<endLine>

<print> ::= ”print”\(“.*”\)<endLine>
<input> ::= ”input”\(<formatoDato>\,<identificador>\)<endLine>

<comentarioSimple> ::= “@”.*”\n”
<comentarioMultiple> ::= “\/_” .*\s.* “ _\/”

<funcion> ::= <tiposDatos><identificador>\([<parametro>[\,<parametro>]*]?\)\{<bloque>\}<endLine>
<llamadaFuncion> ::= <identificador>\([<parametro>[\,<parametro>]*]?\)

<main> ::= “int main”\([<parametro>[\,<parametro>]*]?\)\{<bloque>\}<endLine>

<programa> ::= <sentencia>*<funcion>*<sentencia>*<main><sentencia>*<funcion><sentencia>*



%%
// Reglas para manejar los tokens
<YYINITIAL> {
    {DIGIT}+    { return new Symbol(Sym.INTEGER, Integer.parseInt(yytext())); }
    {ID}        { return new Symbol(Sym.ID, yytext()); }
    "if"        { return new Symbol(Sym.IF, yytext()); }
    "while"     { return new Symbol(Sym.WHILE, yytext()); }
    "for"       { return new Symbol(Sym.FOR, yytext()); }
    "+"         { return new Symbol(Sym.PLUS, yytext()); }
    "-"         { return new Symbol(Sym.MINUS, yytext()); }
    "*"         { return new Symbol(Sym.TIMES, yytext()); }
    "/"         { return new Symbol(Sym.DIVIDE, yytext()); }
    "("         { return new Symbol(Sym.LPAREN, yytext()); }
    ")"         { return new Symbol(Sym.RPAREN, yytext()); }
    "\n"        { /* Ignorar saltos de línea */ }
    [ \t]+      { /* Ignorar espacios en blanco */ }
    "/*"        { yybegin(IN_COMMENT); }
}

<IN_COMMENT> {
    "*/"        { yybegin(YYINITIAL); }
    [^\n]*      { /* Ignorar todo dentro de los comentarios */ }
    \n          { /* Contar saltos de línea dentro de los comentarios */ }
}

// Define aquí las constantes para los símbolos terminales (por ejemplo, Sym.INTEGER, Sym.ID, etc.)
