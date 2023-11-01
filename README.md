# llvm-for-glsl
GLSL Parser
实现一个GLSL语法分析器，要求判断输入的GLSL文件的文法是否正确。如果文法正确，返回Accept，如果文法错误，返回Reject。
LLVM Code Generator
实现面向LLVM的代码生成器。要求将特定的GLSL文件翻译为正确的LLVM IR。

GLSL语法：
toplevel :=
	version definition*

version :=
	'#version' NUMBER

definition :=
	function_definition
	global_variable_definition

function_definition :=
	function_prototype '{' sentence* '}'

function_prototype :=
	TYPE identifier '(' ')'
	TYPE identifier '(' TYPE identifier (',' TYPE identifier)* ')'

sentence :=
	'{' sentence* '}'
	expression ';'
	variable_definition ';'
	'if' '(' expression ')' sentence
	'if' '(' expression ')' sentence 'else' sentence
	'for' '(' expression ';' expression ';' expression ')' sentence
	'for' '(' variable_definition ';' expression ';' expression ')' sentence
	';'
	'return' ';'
	'return' expression ';'

variable_definition :=
	TYPE identifier '=' expression
	'const' TYPE identifier '=' expression

expression :=
	expression '.x'
	expression '.y'
	expression '.z'
	expression '.w'
	TYPE '(' expression_list ')'
	'{' expression_list '}'
	'(' expression ')'
	NUMBER
	identifier
	identifier '(' expression_list ')'
	identifier '[' expression ']'
	expression '++'
	expression '--'
	'++' expression
	'--' expression
	'+' expression
	'-' expression
	'~' expression
	'!' expression
	expression '*' expression
	expression '/' expression
	expression '%' expression
	expression '+' expression
	expression '-' expression
	expression '<<' expression
	expression '>>' expression
	expression '>=' expression
	expression '<=' expression
	expression '>' expression
	expression '<' expression
	expression '==' expression
	expression '!=' expression
	expression '&' expression
	expression '^' expression
	expression '|' expression
	expression '&&' expression
	expression '^' expression
	expression '||' expression
	expression '?' expression ':' expression
	expression '=' expression
	expression '+=' expression
	expression '-=' expression
	expression '*=' expression
	expression '/=' expression
	expression '%=' expression
	expression '<<=' expression
	expression '>>=' expression
	expression '||=' expression
	expression '&&=' expression
	expression '|=' expression
	expression '&=' expression
	expression ',' expression

expression_list :=
	expression (',' expression)*
	/* empty */

global_variable_definition :=
（为简化翻译，带有layout的全局变量，无论其相关数值如何，全部翻译为外部(external)全局变量即可）
	layout_defaults TYPE identifier ';'

layout_defaults :=
	layout_uniform_defaults
	layout_in_defaults
	layout_out_defaults
	/* empty */

layout_uniform_defaults :=
	layout_qualifier 'uniform'

layout_in_defaults :=
	layout_qualifier 'in'

layout_out_defaults :=
	layout_qualifier 'out'

layout_qualifier :=
	'layout' '(' layout_qualifier_id_list ')'

layout_qualifier_id_list :=
	layout_qualifier_id (',' layout_qualifier_id)*

layout_qualifier_id :=
	layout_identidier
	layout_identidier '=' NUMBER

layout_identidier :=
	'location'
	'binding'

TYPE包括以下类型：
void bool int float double vec2 vec3 vec4 mat2 mat3 mat4 

