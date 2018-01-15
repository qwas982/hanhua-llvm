#包含 <stdio.h>
#包含 <stdlib.h>
#包含 <memory.h>
#包含 <string.h>

整 debug;    // print the executed instructions 打印执行的指令
整 assembly; // print out the assembly and source 打印出汇编和源代码

整 token; // current token 当前令牌

// instructions 指令
枚举 { LEA ,IMM ,JMP ,CALL,JZ  ,JNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PUSH,
       OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,
       OPEN,READ,CLOS,PRTF,MALC,MSET,MCMP,EXIT };

// tokens and classes (operators last and in precedence order) 令牌和类(操作符最后和按优先顺序排列)
// copied from c4 从c4复制
枚举 {
  Num = 128, Fun, Sys, Glo, Loc, Id,
  Char, Else, Enum, If, Int, Return, Sizeof, While,
  Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak
};

// fields of identifier 标识符字段
枚举 {Token, Hash, Name, Type, Class, Value, BType, BClass, BValue, IdSize};


// types of variable/function 变量/函数的类型
枚举 { CHAR, INT, PTR };

// type of declaration. 类型的声明。
枚举 {Global, Local};

整 *text, // text segment 文本段
    *stack;// stack 栈
整 * old_text; // for dump text segment 转储文本段
字 *data; // data segment 数据段
整 *idmain;

字 *src, *old_src;  // pointer to source code string; 指向源代码字符串的指针;
整 poolsize; // default size of text/data/stack   //文本/数据/堆栈的默认大小
整 *pc, *bp, *sp, ax, cycle; // virtual machine registers 虚拟机寄存器

整 *current_id, // current parsed ID 当前解析的ID
    *symbols,    // symbol table   符号表
    line,        // line number of source code   源代码的行数
   token_val;   // value of current token (mainly for number)    当前令牌值(主要为数字)

整 basetype;    // the type of a declaration, make it global for convenience 一个声明的类型，使它在全局范围内方便
整 expr_type;   // the type of an expression 表达式的类型

// function frame 函数框架
//
// 0: arg 1 0:参数1
// 1: arg 2 0:参数2
// 2: arg 3 0:参数3
// 3: 返回 address 3:返回地址
// 4: old bp pointer  <- index_of_bp  4：旧的bp指针< - index_of_bp
// 5: local var 1 本地变量1
// 6: local var 2 本地变量2
整 index_of_bp; // index of bp pointer on stack 栈上bp指针的索引

空 next() {
    字 *last_pos;
    整 hash;

    当 (token = *src) {
        ++src;

        如 (token == '\n') {
            如 (assembly) {
                // print compile info 打印编译信息
                printf("%d: %.*s", line, src-old_src, old_src);
                old_src = src;

                当 (old_text < text) {
                    printf("%8.4s", & "LEA ,IMM ,JMP ,CALL,JZ  ,JNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PUSH,"
                                      "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
                                      "OPEN,READ,CLOS,PRTF,MALC,MSET,MCMP,EXIT"[*++old_text * 5]);

                    如 (*old_text <= ADJ)
                        printf(" %d\n", *++old_text);
                    另
                        printf("\n");
                }
            }
            ++line;
        }
        另 如 (token == '#') {
            // skip macro, because we will not support it 跳过宏，因为我们不会支持它
            当 (*src != 0 && *src != '\n') {
                src++;
            }
        }
        另 如 ((token >= 'a' && token <= 'z') || (token >= 'A' && token <= 'Z') || (token == '_')) {

            // parse identifier 解析标识符
            last_pos = src - 1;
            hash = token;

            当 ((*src >= 'a' && *src <= 'z') || (*src >= 'A' && *src <= 'Z') || (*src >= '0' && *src <= '9') || (*src == '_')) {
                hash = hash * 147 + *src;
                src++;
            }

            // look for existing identifier, linear search 寻找现有的标识符，线性搜索
            current_id = symbols;
            当 (current_id[Token]) {
                如 (current_id[Hash] == hash && !memcmp((字 *)current_id[Name], last_pos, src - last_pos)) {
                    //found one, 返回 找到一个，返回
                    token = current_id[Token];
                    返回;
                }
                current_id = current_id + IdSize;
            }


            // store new ID 存储新的ID
            current_id[Name] = (整)last_pos;
            current_id[Hash] = hash;
            token = current_id[Token] = Id;
            返回;
        }
        另 如 (token >= '0' && token <= '9') {
            // parse number, three kinds: dec(123) hex(0x123) oct(017) 解析数，三种：十进制（123）十六进制（0x123）八进制（017）
            token_val = token - '0';
            如 (token_val > 0) {
                // dec, starts with [1-9] 十进制，从[1-9]开始
                当 (*src >= '0' && *src <= '9') {
                    token_val = token_val*10 + *src++ - '0';
                }
            } 另 {
                // starts with number 0 从数字0开始
                如 (*src == 'x' || *src == 'X') {
                    //hex 十六进制
                    token = *++src;
                    当 ((token >= '0' && token <= '9') || (token >= 'a' && token <= 'f') || (token >= 'A' && token <= 'F')) {
                        token_val = token_val * 16 + (token & 15) + (token >= 'A' ? 9 : 0);
                        token = *++src;
                    }
                } 另 {
                    // oct 八进制
                    当 (*src >= '0' && *src <= '7') {
                        token_val = token_val*8 + *src++ - '0';
                    }
                }
            }

            token = Num;
            返回;
        }
        另 如 (token == '/') {
            如 (*src == '/') {
                // skip comments 跳过注释
                当 (*src != 0 && *src != '\n') {
                    ++src;
                }
            } 另 {
                // divide operator 划分操作符
                token = Div;
                返回;
            }
        }
        另 如 (token == '"' || token == '\'') {
            // parse string literal, currently, the only supported escape 解析字符串文字，目前是唯一支持的转义
            // character is '\n', store the string literal into data. 字符是'\n'，将字符串字面值存储到数据中。
            last_pos = data;
            当 (*src != 0 && *src != token) {
                token_val = *src++;
                如 (token_val == '\\') {
                    // escape character 转义字符
                    token_val = *src++;
                    如 (token_val == 'n') {
                        token_val = '\n';
                    }
                }

                如 (token == '"') {
                    *data++ = token_val;
                }
            }

            src++;
            // 如 it is a single character, 返回 Num token 如果是单个字符，则返回Num标记
            如 (token == '"') {
                token_val = (整)last_pos;
            } 另 {
                token = Num;
            }

            返回;
        }
        另 如 (token == '=') {
            // parse '==' and '=' 解析“==”和“=”
            如 (*src == '=') {
                src ++;
                token = Eq;
            } 另 {
                token = Assign;
            }
            返回;
        }
        另 如 (token == '+') {
            // parse '+' and '++'
            如 (*src == '+') {
                src ++;
                token = Inc;
            } 另 {
                token = Add;
            }
            返回;
        }
        另 如 (token == '-') {
            // parse '-' and '--'
            如 (*src == '-') {
                src ++;
                token = Dec;
            } 另 {
                token = Sub;
            }
            返回;
        }
        另 如 (token == '!') {
            // parse '!='
            如 (*src == '=') {
                src++;
                token = Ne;
            }
            返回;
        }
        另 如 (token == '<') {
            // parse '<=', '<<' or '<' 解析“<=”，“<<”或“<”
            如 (*src == '=') {
                src ++;
                token = Le;
            } 另 如 (*src == '<') {
                src ++;
                token = Shl;
            } 另 {
                token = Lt;
            }
            返回;
        }
        另 如 (token == '>') {
            // parse '>=', '>>' or '>'
            如 (*src == '=') {
                src ++;
                token = Ge;
            } 另 如 (*src == '>') {
                src ++;
                token = Shr;
            } 另 {
                token = Gt;
            }
            返回;
        }
        另 如 (token == '|') {
            // parse '|' or '||'
            如 (*src == '|') {
                src ++;
                token = Lor;
            } 另 {
                token = Or;
            }
            返回;
        }
        另 如 (token == '&') {
            // parse '&' and '&&'
            如 (*src == '&') {
                src ++;
                token = Lan;
            } 另 {
                token = And;
            }
            返回;
        }
        另 如 (token == '^') {
            token = Xor;
            返回;
        }
        另 如 (token == '%') {
            token = Mod;
            返回;
        }
        另 如 (token == '*') {
            token = Mul;
            返回;
        }
        另 如 (token == '[') {
            token = Brak;
            返回;
        }
        另 如 (token == '?') {
            token = Cond;
            返回;
        }
        另 如 (token == '~' || token == ';' || token == '{' || token == '}' || token == '(' || token == ')' || token == ']' || token == ',' || token == ':') {
            // directly 返回 the character as token; 直接返回字符作为标记;
            返回;
        }
    }
}

空 match(整 tk) {
    如 (token == tk) {
        next();
    } 另 {
        printf("%d: expected token: %d\n", line, tk);
        exit(-1);
    }
}


空 expression(整 level) {
    // expressions have various format. 表达式有各种格式。
    // but majorly can be divided into two parts: unit and operator 主要可以分为两部分：单元和运算符
    // for example `(字) *a[10] = (整 *) func(b > 0 ? 10 : 20); 例如`（char）* a [10] =（int *）func（b> 0？10：20）;
    // `a[10]` is an unit 当 `*` is an operator. `a [10]`是一个单元，而`*`是一个运算符。
    // `func(...)` in total is an unit. ‘func（...）’总共是一个单元。
    // so we should first parse those unit and unary operators 所以我们应该首先解析这些单元和一元运算符
    // and then the binary ones 然后是二元的
    //
    // also the expression can be in the following types: 表达式可以是以下类型：
    //
    // 1. unit_unary ::= unit | unit unary_op | unary_op unit  单元_一元 ::= 单元 | 单元 一元 | 一元_op 单元 
    // 2. expr ::= unit_unary (bin_op unit_unary ...)  表达式 ::= 单元_一元 (二元_op 单元_一元 ...) 

    // unit_unary() 单元_一元()
    整 *id;
    整 tmp;
    整 *addr;
    {
        如 (!token) {
            printf("%d: unexpected token EOF of expression 表达式的意外的令牌EOF\n", line);
            exit(-1);
        }
        如 (token == Num) {
            match(Num);

            // emit code 发出代码
            *++text = IMM;
            *++text = token_val;
            expr_type = INT;
        }
        另 如 (token == '"') {
            // continous string "abc" "abc" 连续字符串“abc”“abc”


            // emit code
            *++text = IMM;
            *++text = token_val;

            match('"');
            // store the rest strings 存储其余的字符串 
            当 (token == '"') {
                match('"');
            }

            // append the end of string character '\0', all the data are default 追加字符串“\ 0”的结尾，所有的数据都是默认的
            // to 0, so just move data one position forward. 到0，所以只要将数据向前移动一个位置。
            data = (字 *)(((整)data + 求长度(整)) & (-求长度(整)));
            expr_type = PTR;
        }
        另 如 (token == Sizeof) {
            // 求长度 is actually an unary operator  sizeof实际上是一个一元运算符
            // now only `求长度(整)`, `求长度(字)` and `求长度(*...)` are 现在只有`sizeof（int）`，`sizeof（char）`和`sizeof（* ...）`是
            // supported. 支持的。
            match(Sizeof);
            match('(');
            expr_type = INT;

            如 (token == Int) {
                match(Int);
            } 另 如 (token == Char) {
                match(Char);
                expr_type = CHAR;
            }

            当 (token == Mul) {
                match(Mul);
                expr_type = expr_type + PTR;
            }

            match(')');

            // emit code 发出代码
            *++text = IMM;
            *++text = (expr_type == CHAR) ? 求长度(字) : 求长度(整);

            expr_type = INT;
        }
        另 如 (token == Id) {
            // there are several type when occurs to Id  Id出现时有几种类型
            // but this is unit, so it can only be 但这是单位，所以它只能是
            // 1. function call 1.函数调用
            // 2. Enum variable  2.枚举变量
            // 3. global/local variable  3.全局/局部变量
            match(Id);

            id = current_id;

            如 (token == '(') {
                // function call 函数调用
                match('(');

                // pass in arguments 传递参数
                tmp = 0; // number of arguments 参数个数
                当 (token != ')') {
                    expression(Assign);
                    *++text = PUSH;
                    tmp ++;

                    如 (token == ',') {
                        match(',');
                    }

                }
                match(')');

                // emit code
                如 (id[Class] == Sys) {
                    // system functions
                    *++text = id[Value];
                }
                另 如 (id[Class] == Fun) {
                    // function call
                    *++text = CALL;
                    *++text = id[Value];
                }
                另 {
                    printf("%d: bad function call\n", line);
                    exit(-1);
                }

                // clean the stack for arguments 清理栈的参数
                如 (tmp > 0) {
                    *++text = ADJ;
                    *++text = tmp;
                }
                expr_type = id[Type];
            }
            另 如 (id[Class] == Num) {
                // 枚举 variable
                *++text = IMM;
                *++text = id[Value];
                expr_type = INT;
            }
            另 {
                // variable
                如 (id[Class] == Loc) {
                    *++text = LEA;
                    *++text = index_of_bp - id[Value];
                }
                另 如 (id[Class] == Glo) {
                    *++text = IMM;
                    *++text = id[Value];
                }
                另 {
                    printf("%d: undefined variable\n", line);
                    exit(-1);
                }

                // emit code, default behaviour is to load the value of the 发出代码，默认行为是加载
                // address which is stored in `ax` 存储在“ax”中的地址的值
                expr_type = id[Type];
                *++text = (expr_type == Char) ? LC : LI;
            }
        }
        另 如 (token == '(') {
            // cast or parenthesis 铸造或括号
            match('(');
            如 (token == Int || token == Char) {
                tmp = (token == Char) ? CHAR : INT; // cast type 铸造型
                match(token);
                当 (token == Mul) {
                    match(Mul);
                    tmp = tmp + PTR;
                }

                match(')');

                expression(Inc); // cast has precedence as Inc(++) 铸造优先级为Inc（++）

                expr_type  = tmp;
            } 另 {
                // normal parenthesis 正常的括号
                expression(Assign);
                match(')');
            }
        }
        另 如 (token == Mul) {
            // dereference *<addr> 解引用操作符*（addr＞
            match(Mul);
            expression(Inc); // dereference has the same precedence as Inc(++) 取消引用与Inc（++）具有相同的优先级

            如 (expr_type >= PTR) {
                expr_type = expr_type - PTR;
            } 另 {
                printf("%d: bad dereference\n", line);
                exit(-1);
            }

            *++text = (expr_type == CHAR) ? LC : LI;
        }
        另 如 (token == And) {
            // get the address of 得到的地址
            match(And);
            expression(Inc); // get the address of 得到的地址
            如 (*text == LC || *text == LI) {
                text --;
            } 另 {
                printf("%d: bad address of\n", line);
                exit(-1);
            }

            expr_type = expr_type + PTR;
        }
        另 如 (token == '!') {
            // not
            match('!');
            expression(Inc);

            // emit code, use <expr> == 0 发出代码，使用<expr> == 0
            *++text = IMM;
            *++text = 0;
            *++text = EQ;

            expr_type = INT;
        }
        另 如 (token == '~') {
            // bitwise not 按位不是
            match('~');
            expression(Inc);

            // emit code, use <expr> XOR -1 发出代码，使用<expr> XOR -1
            *++text = PUSH;
            *++text = IMM;
            *++text = -1;
            *++text = XOR;

            expr_type = INT;
        }
        另 如 (token == Add) {
            // +var, do nothing
            match(Add);
            expression(Inc);

            expr_type = INT;
        }
        另 如 (token == Sub) {
            // -var
            match(Sub);

            如 (token == Num) {
                *++text = IMM;
                *++text = -token_val;
                match(Num);
            } 另 {

                *++text = IMM;
                *++text = -1;
                *++text = PUSH;
                expression(Inc);
                *++text = MUL;
            }

            expr_type = INT;
        }
        另 如 (token == Inc || token == Dec) {
            tmp = token;
            match(token);
            expression(Inc);
            如 (*text == LC) { 复制地址
                *text = PUSH;  // to duplicate the address 复制地址
                *++text = LC; 
            } 另 如 (*text == LI) {
                *text = PUSH;
                *++text = LI;
            } 另 {
                printf("%d: bad lvalue of pre-increment\n", line);
                exit(-1);
            }
            *++text = PUSH;
            *++text = IMM;
            *++text = (expr_type > PTR) ? 求长度(整) : 求长度(字);
            *++text = (tmp == Inc) ? ADD : SUB;
            *++text = (expr_type == CHAR) ? SC : SI;
        }
        另 {
            printf("%d: bad expression\n", line);
            exit(-1);
        }
    }

    // binary operator and postfix operators. 二元运算符和后缀运算符。
    {
        当 (token >= level) {
            // handle according to current operator's precedence 根据当前运算符的优先级进行处理
            tmp = expr_type;
            如 (token == Assign) {
                // var = expr;
                match(Assign);
                如 (*text == LC || *text == LI) {
                    *text = PUSH; // save the lvalue's pointer 保存左值的指针
                } 另 {
                    printf("%d: bad lvalue in assignment\n", line);
                    exit(-1);
                }
                expression(Assign);

                expr_type = tmp;
                *++text = (expr_type == CHAR) ? SC : SI;
            }
            另 如 (token == Cond) {
                // expr ? a : b;
                match(Cond);
                *++text = JZ;
                addr = ++text;
                expression(Assign);
                如 (token == ':') {
                    match(':');
                } 另 {
                    printf("%d: missing colon in conditional\n", line);
                    exit(-1);
                }
                *addr = (整)(text + 3);
                *++text = JMP;
                addr = ++text;
                expression(Cond);
                *addr = (整)(text + 1);
            }
            另 如 (token == Lor) {
                // logic or 逻辑或
                match(Lor);
                *++text = JNZ;
                addr = ++text;
                expression(Lan);
                *addr = (整)(text + 1);
                expr_type = INT;
            }
            另 如 (token == Lan) {
                // logic and
                match(Lan);
                *++text = JZ;
                addr = ++text;
                expression(Or);
                *addr = (整)(text + 1);
                expr_type = INT;
            }
            另 如 (token == Or) {
                // bitwise or 按位或
                match(Or);
                *++text = PUSH;
                expression(Xor);
                *++text = OR;
                expr_type = INT;
            }
            另 如 (token == Xor) {
                // bitwise xor
                match(Xor);
                *++text = PUSH;
                expression(And);
                *++text = XOR;
                expr_type = INT;
            }
            另 如 (token == And) {
                // bitwise and
                match(And);
                *++text = PUSH;
                expression(Eq);
                *++text = AND;
                expr_type = INT;
            }
            另 如 (token == Eq) {
                // equal == 等于==
                match(Eq);
                *++text = PUSH;
                expression(Ne);
                *++text = EQ;
                expr_type = INT;
            }
            另 如 (token == Ne) {
                // not equal !=
                match(Ne);
                *++text = PUSH;
                expression(Lt);
                *++text = NE;
                expr_type = INT;
            }
            另 如 (token == Lt) {
                // less than 少于
                match(Lt);
                *++text = PUSH;
                expression(Shl);
                *++text = LT;
                expr_type = INT;
            }
            另 如 (token == Gt) {
                // greater than 比...更棒
                match(Gt);
                *++text = PUSH;
                expression(Shl);
                *++text = GT;
                expr_type = INT;
            }
            另 如 (token == Le) {
                // less than or equal to 小于或等于
                match(Le);
                *++text = PUSH;
                expression(Shl);
                *++text = LE;
                expr_type = INT;
            }
            另 如 (token == Ge) {
                // greater than or equal to 大于或等于
                match(Ge);
                *++text = PUSH;
                expression(Shl);
                *++text = GE;
                expr_type = INT;
            }
            另 如 (token == Shl) {
                // shift left 向左移动
                match(Shl);
                *++text = PUSH;
                expression(Add);
                *++text = SHL;
                expr_type = INT;
            }
            另 如 (token == Shr) {
                // shift right
                match(Shr);
                *++text = PUSH;
                expression(Add);
                *++text = SHR;
                expr_type = INT;
            }
            另 如 (token == Add) {
                // add
                match(Add);
                *++text = PUSH;
                expression(Mul);

                expr_type = tmp;
                如 (expr_type > PTR) {
                    // pointer type, and not `字 *` 指针类型，而不是`字 *`
                    *++text = PUSH;
                    *++text = IMM;
                    *++text = 求长度(整);
                    *++text = MUL;
                }
                *++text = ADD;
            }
            另 如 (token == Sub) {
                // sub
                match(Sub);
                *++text = PUSH;
                expression(Mul);
                如 (tmp > PTR && tmp == expr_type) {
                    // pointer subtraction 指针减法
                    *++text = SUB;
                    *++text = PUSH;
                    *++text = IMM;
                    *++text = 求长度(整);
                    *++text = DIV;
                    expr_type = INT;
                } 另 如 (tmp > PTR) {
                    // pointer movement 指针移动
                    *++text = PUSH;
                    *++text = IMM;
                    *++text = 求长度(整);
                    *++text = MUL;
                    *++text = SUB;
                    expr_type = tmp;
                } 另 {
                    // numeral subtraction 数字减法
                    *++text = SUB;
                    expr_type = tmp;
                }
            }
            另 如 (token == Mul) {
                // multiply
                match(Mul);
                *++text = PUSH;
                expression(Inc);
                *++text = MUL;
                expr_type = tmp;
            }
            另 如 (token == Div) {
                // divide
                match(Div);
                *++text = PUSH;
                expression(Inc);
                *++text = DIV;
                expr_type = tmp;
            }
            另 如 (token == Mod) {
                // Modulo
                match(Mod);
                *++text = PUSH;
                expression(Inc);
                *++text = MOD;
                expr_type = tmp;
            }
            另 如 (token == Inc || token == Dec) {
                // postfix inc(++) and dec(--)  后缀 inc（++）和dec（ - ），
                // we will increase the value to the variable and decrease it  我们将把这个值增加到变量上，
                // on `ax` to get its original value. 并在ax上减小它以得到它的原始值。
                如 (*text == LI) {
                    *text = PUSH;
                    *++text = LI;
                }
                另 如 (*text == LC) {
                    *text = PUSH;
                    *++text = LC;
                }
                另 {
                    printf("%d: bad value in increment\n", line);
                    exit(-1);
                }

                *++text = PUSH;
                *++text = IMM;
                *++text = (expr_type > PTR) ? 求长度(整) : 求长度(字);
                *++text = (token == Inc) ? ADD : SUB;
                *++text = (expr_type == CHAR) ? SC : SI;
                *++text = PUSH;
                *++text = IMM;
                *++text = (expr_type > PTR) ? 求长度(整) : 求长度(字);
                *++text = (token == Inc) ? SUB : ADD;
                match(token);
            }
            另 如 (token == Brak) {
                // array access var[xx] 数组访问var [xx]
                match(Brak);
                *++text = PUSH;
                expression(Assign);
                match(']');

                如 (tmp > PTR) { 
                    // pointer, `not 字 *` 指针，不是char *`
                    *++text = PUSH;
                    *++text = IMM;
                    *++text = 求长度(整);
                    *++text = MUL;
                }
                另 如 (tmp < PTR) {
                    printf("%d: pointer type expected 指针类型的预期\n", line);
                    exit(-1);
                }
                expr_type = tmp - PTR;
                *++text = ADD;
                *++text = (expr_type == CHAR) ? LC : LI;
            }
            另 {
                printf("%d: compiler error, token = %d\n", line, token);
                exit(-1);
            }
        }
    }
}

空 statement() {
    // there are 8 kinds of statements here: 这里有8种陈述：
    // 1. 如 (...) <statement> [另 <statement>]
    // 2. 当 (...) <statement> 
    // 3. { <statement> }  语句
    // 4. 返回 xxx;
    // 5. <empty statement>;
    // 6. expression; (expression end with semicolon)  6.表达; （表达式以分号结尾）

    整 *a, *b; // bess for branch control 对分支机构的控制

    如 (token == If) {
        // 如 (...) <statement> [另 <statement>]
        //
        //   如 (...)           <cond>
        //                      JZ a
        //     <statement>      <statement>
        //   另:              JMP b
        // a:
        //     <statement>      <statement>
        // b:                   b:
        //
        //
        match(If);
        match('(');
        expression(Assign);  // parse condition 解析条件
        match(')');

        // emit code for 如  为if发出代码
        *++text = JZ;
        b = ++text;

        statement();         // parse statement 解析语句
        如 (token == Else) { // parse 另
            match(Else);

            // emit code for JMP B
            *b = (整)(text + 3);
            *++text = JMP;
            b = ++text;

            statement();
        }

        *b = (整)(text + 1);
    }
    另 如 (token == While) {
        //
        // a:                     a:
        //    当 (<cond>)        <cond>
        //                          JZ b
        //     <statement>          <statement>
        //                          JMP a
        // b:                     b:
        match(While);

        a = text + 1;

        match('(');
        expression(Assign);
        match(')');

        *++text = JZ;
        b = ++text;

        statement();

        *++text = JMP;
        *++text = (整)a;
        *b = (整)(text + 1);
    }
    另 如 (token == '{') {
        // { <statement> ... }
        match('{');

        当 (token != '}') {
            statement();
        }

        match('}');
    }
    另 如 (token == Return) {
        // 返回 [expression]; 返回表达式
        match(Return);

        如 (token != ';') {
            expression(Assign);
        }

        match(';');

        // emit code for 返回
        *++text = LEV;
    }
    另 如 (token == ';') {
        // empty statement
        match(';');
    }
    另 {
        // a = b; or function_call();
        expression(Assign);
        match(';');
    }
}

空 enum_declaration() {
    // parse 枚举 [id] { a = 1, b = 3, ...} 解析枚举
    整 i;
    i = 0;
    当 (token != '}') {
        如 (token != Id) {
            printf("%d: bad 枚举 identifier %d\n", line, token);
            exit(-1);
        }
        next();
        如 (token == Assign) {
            // like {a=10}
            next();
            如 (token != Num) {
                printf("%d: bad 枚举 initializer\n", line);
                exit(-1);
            }
            i = token_val;
            next();
        }

        current_id[Class] = Num;
        current_id[Type] = INT;
        current_id[Value] = i++;

        如 (token == ',') {
            next();
        }
    }
}

空 function_parameter() {
    整 type;
    整 params;
    params = 0;
    当 (token != ')') {
        // 整 name, ...
        type = INT;
        如 (token == Int) {
            match(Int);
        } 另 如 (token == Char) {
            type = CHAR;
            match(Char);
        }

        // pointer type
        当 (token == Mul) {
            match(Mul);
            type = type + PTR;
        }

        // parameter name 参数名称
        如 (token != Id) {
            printf("%d: bad parameter declaration\n", line);
            exit(-1);
        }
        如 (current_id[Class] == Loc) {
            printf("%d: duplicate parameter declaration\n", line);
            exit(-1);
        }

        match(Id);
        // store the local variable 存储本地变量
        current_id[BClass] = current_id[Class]; current_id[Class]  = Loc;
        current_id[BType]  = current_id[Type];  current_id[Type]   = type;
        current_id[BValue] = current_id[Value]; current_id[Value]  = params++;   // index of current parameter

        如 (token == ',') {
            match(',');
        }
    }
    index_of_bp = params+1;
}

空 function_body() {
    // type func_name (...) {...}
    //                   -->|   |<--

    // ... {
    // 1. local declarations 本地声明
    // 2. statements
    // }

    整 pos_local; // position of local variables on the stack. 堆栈上局部变量的位置。
    整 type;
    pos_local = index_of_bp;

    当 (token == Int || token == Char) {
        // local variable declaration, just like global ones. 局部变量声明，就像全局变量一样。
        basetype = (token == Int) ? INT : CHAR;
        match(token);

        当 (token != ';') {
            type = basetype;
            当 (token == Mul) {
                match(Mul);
                type = type + PTR;
            }

            如 (token != Id) {
                // invalid declaration 无效的声明
                printf("%d: bad local declaration\n", line);
                exit(-1);
            }
            如 (current_id[Class] == Loc) {
                // identifier exists 标识符存在
                printf("%d: duplicate local declaration\n", line);
                exit(-1);
            }
            match(Id);

            // store the local variable 存储本地变量
            current_id[BClass] = current_id[Class]; current_id[Class]  = Loc;
            current_id[BType]  = current_id[Type];  current_id[Type]   = type;
            current_id[BValue] = current_id[Value]; current_id[Value]  = ++pos_local;   // index of current parameter

            如 (token == ',') {
                match(',');
            }
        }
        match(';');
    }

    // save the stack size for local variables 保存本地变量的堆栈大小
    *++text = ENT;
    *++text = pos_local - index_of_bp;

    // statements
    当 (token != '}') {
        statement();
    }

    // emit code for leaving the sub function 发出离开子功能的代码
    *++text = LEV;
}

空 function_declaration() {
    // type func_name (...) {...}
    //               | this part

    match('(');
    function_parameter();
    match(')');
    match('{');
    function_body();
    //match('}');

    // unwind local variable declarations for all local variables. 展开所有局部变量的局部变量声明。
    current_id = symbols;
    当 (current_id[Token]) {
        如 (current_id[Class] == Loc) {
            current_id[Class] = current_id[BClass];
            current_id[Type]  = current_id[BType];
            current_id[Value] = current_id[BValue];
        }
        current_id = current_id + IdSize;
    }
}

空 global_declaration() {
    // 整 [*]id [; | (...) {...}]


    整 type; // tmp, actual type for variable tmp，变量的实际类型
    整 i; // tmp

    basetype = INT;

    // parse 枚举, this should be treated alone. 解析枚举，这应该被单独处理。
    如 (token == Enum) {
        // 枚举 [id] { a = 10, b = 20, ... }
        match(Enum);
        如 (token != '{') {
            match(Id); // skip the [id] part
        }
        如 (token == '{') {
            // parse the assign part 解析分配部分
            match('{');
            enum_declaration();
            match('}');
        }

        match(';');
        返回;
    }

    // parse type information 解析类型信息
    如 (token == Int) {
        match(Int);
    }
    另 如 (token == Char) {
        match(Char);
        basetype = CHAR;
    }

    // parse the comma seperated variable declaration. 分析逗号分隔的变量声明。
    当 (token != ';' && token != '}') {
        type = basetype;
        // parse pointer type, note that there may exist `整 ****x;` 解析指针类型，注意可能存在`整 **** x;`
        当 (token == Mul) {
            match(Mul);
            type = type + PTR;
        }

        如 (token != Id) {
            // invalid declaration
            printf("%d: bad global declaration\n", line);
            exit(-1);
        }
        如 (current_id[Class]) {
            // identifier exists 标识符存在
            printf("%d: duplicate global declaration\n", line);
            exit(-1);
        }
        match(Id);
        current_id[Type] = type;

        如 (token == '(') {
            current_id[Class] = Fun;
            current_id[Value] = (整)(text + 1); // the memory address of function 函数的内存地址
            function_declaration();
        } 另 {
            // variable declaration
            current_id[Class] = Glo; // global variable
            current_id[Value] = (整)data; // assign memory address 分配内存地址
            data = data + 求长度(整);
        }

        如 (token == ',') {
            match(',');
        }
    }
    next();
}

空 program() {
    // get next token 得到下一个令牌
    next();
    当 (token > 0) {
        global_declaration();
    }
}

整 eval() {
    整 op, *tmp;
    cycle = 0;
    当 (1) {
        cycle ++;
        op = *pc++; // get next operation code 获取下一个操作代码

        // print debug info 打印调试信息
        如 (debug) {
            printf("%d> %.4s", cycle,
                   & "LEA ,IMM ,JMP ,CALL,JZ  ,JNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PUSH,"
                   "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
                   "OPEN,READ,CLOS,PRTF,MALC,MSET,MCMP,EXIT"[op * 5]);
            如 (op <= ADJ)
                printf(" %d\n", *pc);
            另
                printf("\n");
        }
        如 (op == IMM)       {ax = *pc++;}                                     // load immediate value to ax  将即时值加载到ax
        另 如 (op == LC)   {ax = *(字 *)ax;}                               // load character to ax, address in ax   加载字符到ax，地址在ax
        另 如 (op == LI)   {ax = *(整 *)ax;}                                // load integer to ax, address in ax 将整数加载到ax，地址放在ax中
        另 如 (op == SC)   {ax = *(字 *)*sp++ = ax;}                       // save character to address, value in ax, address on stack 保存字符到地址，值在ax，堆栈上的地址
        另 如 (op == SI)   {*(整 *)*sp++ = ax;}                             // save integer to address, value in ax, address on stack 将整数保存到地址，值为ax，堆栈上的地址
        另 如 (op == PUSH) {*--sp = ax;}                                     // push the value of ax onto the stack 将ax的值推入堆栈
        另 如 (op == JMP)  {pc = (整 *)*pc;}                                // jump to the address             跳转到地址
        另 如 (op == JZ)   {pc = ax ? pc + 1 : (整 *)*pc;}                   // jump 如 ax is zero            如果ax为零，则跳转
        另 如 (op == JNZ)  {pc = ax ? (整 *)*pc : pc + 1;}                   // jump 如 ax is zero            如果ax为零，则跳转
        另 如 (op == CALL) {*--sp = (整)(pc+1); pc = (整 *)*pc;}           // call subroutine            调用子程序
        //另 如 (op == RET)  {pc = (整 *)*sp++;}                              // 返回 from subroutine;         子程序返回;
        另 如 (op == ENT)  {*--sp = (整)bp; bp = sp; sp = sp - *pc++;}      // make new stack frame             使新的堆栈框架
        另 如 (op == ADJ)  {sp = sp + *pc++;}                                // add esp, <size>                 添加esp，<size>
        另 如 (op == LEV)  {sp = bp; bp = (整 *)*sp++; pc = (整 *)*sp++;}  // restore call frame and          PC恢复呼叫帧和PC
        另 如 (op == LEA)  {ax = (整)(bp + *pc++);}                         // load address for arguments.      加载参数的地址。

        另 如 (op == OR)  ax = *sp++ | ax;
        另 如 (op == XOR) ax = *sp++ ^ ax;
        另 如 (op == AND) ax = *sp++ & ax;
        另 如 (op == EQ)  ax = *sp++ == ax;
        另 如 (op == NE)  ax = *sp++ != ax;
        另 如 (op == LT)  ax = *sp++ < ax;
        另 如 (op == LE)  ax = *sp++ <= ax;
        另 如 (op == GT)  ax = *sp++ >  ax;
        另 如 (op == GE)  ax = *sp++ >= ax;
        另 如 (op == SHL) ax = *sp++ << ax;
        另 如 (op == SHR) ax = *sp++ >> ax;
        另 如 (op == ADD) ax = *sp++ + ax;
        另 如 (op == SUB) ax = *sp++ - ax;
        另 如 (op == MUL) ax = *sp++ * ax;
        另 如 (op == DIV) ax = *sp++ / ax;
        另 如 (op == MOD) ax = *sp++ % ax;

        另 如 (op == EXIT) { printf("exit(%d)", *sp); 返回 *sp;}
        另 如 (op == OPEN) { ax = open((字 *)sp[1], sp[0]); }
        另 如 (op == CLOS) { ax = close(*sp);}
        另 如 (op == READ) { ax = read(sp[2], (字 *)sp[1], *sp); }
        另 如 (op == PRTF) { tmp = sp + pc[1]; ax = printf((字 *)tmp[-1], tmp[-2], tmp[-3], tmp[-4], tmp[-5], tmp[-6]); }
        另 如 (op == MALC) { ax = (整)malloc(*sp);}
        另 如 (op == MSET) { ax = (整)memset((字 *)sp[2], sp[1], *sp);}
        另 如 (op == MCMP) { ax = memcmp((字 *)sp[2], (字 *)sp[1], *sp);}
        另 {
            printf("unknown instruction:%d\n", op);
            返回 -1;
        }
    }
}

整 main(整 argc, 字 **argv)
{
    整 i, fd;
    整 *tmp;

    argc--;
    argv++;

    // parse arguments 解析参数
    如 (argc > 0 && **argv == '-' && (*argv)[1] == 's') {
        assembly = 1;
        --argc;
        ++argv;
    }
    如 (argc > 0 && **argv == '-' && (*argv)[1] == 'd') {
        debug = 1;
        --argc;
        ++argv;
    }
    如 (argc < 1) {
        printf("usage: xc [-s] [-d] file ...\n");
        返回 -1;
    }

    如 ((fd = open(*argv, 0)) < 0) {
        printf("could not open(%s)\n", *argv);
        返回 -1;
    }

    poolsize = 256 * 1024; // arbitrary size 任意大小
    line = 1;

    // allocate memory 分配内存
    如 (!(text = malloc(poolsize))) {
        printf("could not malloc(%d) for text area\n", poolsize);
        返回 -1;
    }
    如 (!(data = malloc(poolsize))) {
        printf("could not malloc(%d) for data area\n", poolsize);
        返回 -1;
    }
    如 (!(stack = malloc(poolsize))) {
        printf("could not malloc(%d) for stack area\n", poolsize);
        返回 -1;
    }
    如 (!(symbols = malloc(poolsize))) {
        printf("could not malloc(%d) for symbol table\n", poolsize);
        返回 -1;
    }

    memset(text, 0, poolsize);
    memset(data, 0, poolsize);
    memset(stack, 0, poolsize);
    memset(symbols, 0, poolsize);

    old_text = text;

    src = "字 另 枚举 如 整 返回 求长度 当 "
          "open read close printf malloc memset memcmp exit 空 main";

     // add keywords to symbol table 添加关键字符号表
    i = Char;
    当 (i <= While) {
        next();
        current_id[Token] = i++;
    }

    // add library to symbol table 添加库到符号表
    i = OPEN;
    当 (i <= EXIT) {
        next();
        current_id[Class] = Sys;
        current_id[Type] = INT;
        current_id[Value] = i++;
    }

    next(); current_id[Token] = Char; // handle 空 type 处理void类型
    next(); idmain = current_id; // keep track of main 跟踪主要

    如 (!(src = old_src = malloc(poolsize))) {
        printf("could not malloc(%d) for source area\n", poolsize);
        返回 -1;
    }
    // read the source file 读取源文件
    如 ((i = read(fd, src, poolsize-1)) <= 0) {
        printf("read() returned %d\n", i);
        返回 -1;
    }
    src[i] = 0; // add EOF character 添加EOF字符
    close(fd);

    program();

    如 (!(pc = (整 *)idmain[Value])) {
        printf("main() not defined\n");
        返回 -1;
    }

    // dump_text(); 转储文本
    如 (assembly) {
        // only for compile 仅用于编译
        返回 0;
    }

    // setup stack 设置堆栈
    sp = (整 *)((整)stack + poolsize);
    *--sp = EXIT; // call exit 如 main returns 如果主要返回，则调用exit
    *--sp = PUSH; tmp = sp;
    *--sp = argc;
    *--sp = (整)argv;
    *--sp = (整)tmp;

    返回 eval();
}
