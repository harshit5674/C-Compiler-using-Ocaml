type exp = Constant of int
type statement = Return of exp
type func_decl = Function of (string*statement)
type prog = Prog of func_decl
type tokens = |INT|SEMICOLON|LEFT_BRACE|RIGHT_BRACE
	|LEFT_PARA
	|RIGHT_PARA
	|RETURN



let file="main.c"

let read_file ()= 
	let ic =open_in file in
	let s=really_input_string ic (in_channel_length ic) in
	close_in ic;
	s

type tokens
let token_list:int=[]
let rec lexer s= function
	|" "^t -> lexer t;;
	|"\n"^t -> lexer t;;
	|"\t"^t -> lexer t;;
	|"int "^t -> token_list@[0];lexer t;;
	|"{"^t -> token_list@[1];lexer t;;
	|"}"^t -> token_list@[2];lexer t;;
	|"("^t -> token_list@[3];lexer t;;
	|")"^t -> token_list@[4];lexer t;;
	|"return "^t -> token_list@[5];lexer t;;
	|";"^t -> token_list@[6];lexer t;;
	|_ -> token_list@[7];lexer t;;

let rec print_list token_list=function
	|[x]-> printf "%d",x;()
	|h::t-> printf "%d",h;print_list t;;

let s=read_file ();;
lexer s;;
print_list token_list;;

