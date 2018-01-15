#包含 <stdio.h>
#包含 <stdlib.h>
#包含 <memory.h>
#包含 <string.h>

整 debug;    // print the executed instructions 打印执行的指令
整 assembly; // print out the assembly and source 打印出汇编和源代码

整 标记流; // current 标记流 当前令牌

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
枚举 {标记流, Hash, Name, Type, Class, Value, BType, BClass, BValue, IdSize};


// types of variable/function 变量/函数的类型
枚举 { CHAR, INT, PTR };

// type of declaration. 类型的声明。
枚举 {Global, Local};

整 *代码段, // 代码段 segment 文本段
    *栈;// 栈 栈
整 * old_text; // for dump 代码段 segment 转储文本段
字 *数据段; // 数据段 segment 数据段
整 *idmain;

字 *src, *old_src;  // pointer to source code string; 指向源代码字符串的指针;
整 poolsize; // default size of 代码段/数据段/栈   //文本/数据/堆栈的默认大小
整 *程序计数器, *基址指针, *指针寄存器, 通用寄存器, cycle; // virtual machine registers 虚拟机寄存器

整 *current_id, // current parsed ID 当前解析的ID
    *symbols,    // symbol table   符号表
    line,        // line number of source code   源代码的行数
   token_val;   // value of current 标记流 (mainly for number)    当前令牌值(主要为数字)

整 basetype;    // the type of a declaration, make it global for convenience 一个声明的类型，使它在全局范围内方便
整 expr_type;   // the type of an 解析一个表达式 表达式的类型

// function frame 函数框架
//
// 0: arg 1 0:参数1
// 1: arg 2 0:参数2
// 2: arg 3 0:参数3
// 3: 返回 address 3:返回地址
// 4: old 基址指针 pointer  <- index_of_bp  4：旧的bp指针< - index_of_bp
// 5: local var 1 本地变量1
// 6: local var 2 本地变量2
整 index_of_bp; // index of 基址指针 pointer on 栈 栈上bp指针的索引

空 词法分析() {
    字 *last_pos;
    整 hash;

    当 (标记流 = *src) {
        ++src;

        如 (标记流 == '\n') {
            如 (assembly) {
                // print compile info 打印编译信息
                printf("%d: %.*s", line, src-old_src, old_src);
                old_src = src;

                当 (old_text < 代码段) {
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
        另 如 (标记流 == '#') {
            // skip macro, because we will not support it 跳过宏，因为我们不会支持它
            当 (*src != 0 && *src != '\n') {
                src++;
            }
        }
        另 如 ((标记流 >= 'a' && 标记流 <= 'z') || (标记流 >= 'A' && 标记流 <= 'Z') || (标记流 == '_')) {

            // parse identifier 解析标识符
            last_pos = src - 1;
            hash = 标记流;

            当 ((*src >= 'a' && *src <= 'z') || (*src >= 'A' && *src <= 'Z') || (*src >= '0' && *src <= '9') || (*src == '_')) {
                hash = hash * 147 + *src;
                src++;
            }

            // look for existing identifier, linear search 寻找现有的标识符，线性搜索
            current_id = symbols;
            当 (current_id[标记流]) {
                如 (current_id[Hash] == hash && !memcmp((字 *)current_id[Name], last_pos, src - last_pos)) {
                    //found one, 返回 找到一个，返回
                    标记流 = current_id[标记流];
                    返回;
                }
                current_id = current_id + IdSize;
            }


            // store new ID 存储新的ID
            current_id[Name] = (整)last_pos;
            current_id[Hash] = hash;
            标记流 = current_id[标记流] = Id;
            返回;
        }
        另 如 (标记流 >= '0' && 标记流 <= '9') {
            // parse number, three kinds: dec(123) hex(0x123) oct(017) 解析数，三种：十进制（123）十六进制（0x123）八进制（017）
            token_val = 标记流 - '0';
            如 (token_val > 0) {
                // dec, starts with [1-9] 十进制，从[1-9]开始
                当 (*src >= '0' && *src <= '9') {
                    token_val = token_val*10 + *src++ - '0';
                }
            } 另 {
                // starts with number 0 从数字0开始
                如 (*src == 'x' || *src == 'X') {
                    //hex 十六进制
                    标记流 = *++src;
                    当 ((标记流 >= '0' && 标记流 <= '9') || (标记流 >= 'a' && 标记流 <= 'f') || (标记流 >= 'A' && 标记流 <= 'F')) {
                        token_val = token_val * 16 + (标记流 & 15) + (标记流 >= 'A' ? 9 : 0);
                        标记流 = *++src;
                    }
                } 另 {
                    // oct 八进制
                    当 (*src >= '0' && *src <= '7') {
                        token_val = token_val*8 + *src++ - '0';
                    }
                }
            }

            标记流 = Num;
            返回;
        }
        另 如 (标记流 == '/') {
            如 (*src == '/') {
                // skip comments 跳过注释
                当 (*src != 0 && *src != '\n') {
                    ++src;
                }
            } 另 {
                // divide operator 划分操作符
                标记流 = Div;
                返回;
            }
        }
        另 如 (标记流 == '"' || 标记流 == '\'') {
            // parse string literal, currently, the only supported escape 解析字符串文字，目前是唯一支持的转义
            // character is '\n', store the string literal into 数据段. 字符是'\n'，将字符串字面值存储到数据中。
            last_pos = 数据段;
            当 (*src != 0 && *src != 标记流) {
                token_val = *src++;
                如 (token_val == '\\') {
                    // escape character 转义字符
                    token_val = *src++;
                    如 (token_val == 'n') {
                        token_val = '\n';
                    }
                }

                如 (标记流 == '"') {
                    *数据段++ = token_val;
                }
            }

            src++;
            // 如 it is a single character, 返回 Num 标记流 如果是单个字符，则返回Num标记
            如 (标记流 == '"') {
                token_val = (整)last_pos;
            } 另 {
                标记流 = Num;
            }

            返回;
        }
        另 如 (标记流 == '=') {
            // parse '==' and '=' 解析“==”和“=”
            如 (*src == '=') {
                src ++;
                标记流 = Eq;
            } 另 {
                标记流 = Assign;
            }
            返回;
        }
        另 如 (标记流 == '+') {
            // parse '+' and '++'
            如 (*src == '+') {
                src ++;
                标记流 = Inc;
            } 另 {
                标记流 = Add;
            }
            返回;
        }
        另 如 (标记流 == '-') {
            // parse '-' and '--'
            如 (*src == '-') {
                src ++;
                标记流 = Dec;
            } 另 {
                标记流 = Sub;
            }
            返回;
        }
        另 如 (标记流 == '!') {
            // parse '!='
            如 (*src == '=') {
                src++;
                标记流 = Ne;
            }
            返回;
        }
        另 如 (标记流 == '<') {
            // parse '<=', '<<' or '<' 解析“<=”，“<<”或“<”
            如 (*src == '=') {
                src ++;
                标记流 = Le;
            } 另 如 (*src == '<') {
                src ++;
                标记流 = Shl;
            } 另 {
                标记流 = Lt;
            }
            返回;
        }
        另 如 (标记流 == '>') {
            // parse '>=', '>>' or '>'
            如 (*src == '=') {
                src ++;
                标记流 = Ge;
            } 另 如 (*src == '>') {
                src ++;
                标记流 = Shr;
            } 另 {
                标记流 = Gt;
            }
            返回;
        }
        另 如 (标记流 == '|') {
            // parse '|' or '||'
            如 (*src == '|') {
                src ++;
                标记流 = Lor;
            } 另 {
                标记流 = Or;
            }
            返回;
        }
        另 如 (标记流 == '&') {
            // parse '&' and '&&'
            如 (*src == '&') {
                src ++;
                标记流 = Lan;
            } 另 {
                标记流 = And;
            }
            返回;
        }
        另 如 (标记流 == '^') {
            标记流 = Xor;
            返回;
        }
        另 如 (标记流 == '%') {
            标记流 = Mod;
            返回;
        }
        另 如 (标记流 == '*') {
            标记流 = Mul;
            返回;
        }
        另 如 (标记流 == '[') {
            标记流 = Brak;
            返回;
        }
        另 如 (标记流 == '?') {
            标记流 = Cond;
            返回;
        }
        另 如 (标记流 == '~' || 标记流 == ';' || 标记流 == '{' || 标记流 == '}' || 标记流 == '(' || 标记流 == ')' || 标记流 == ']' || 标记流 == ',' || 标记流 == ':') {
            // directly 返回 the character as 标记流; 直接返回字符作为标记;
            返回;
        }
    }
}

空 match(整 tk) {
    如 (标记流 == tk) {
        词法分析();
    } 另 {
        printf("%d: expected 标记流: %d\n", line, tk);
        exit(-1);
    }
}


空 解析一个表达式(整 level) {
    // expressions have various format. 表达式有各种格式。
    // but majorly can be divided into two parts: unit and operator 主要可以分为两部分：单元和运算符
    // for example `(字) *a[10] = (整 *) func(b > 0 ? 10 : 20); 例如`（char）* a [10] =（int *）func（b> 0？10：20）;
    // `a[10]` is an unit 当 `*` is an operator. `a [10]`是一个单元，而`*`是一个运算符。
    // `func(...)` in total is an unit. ‘func（...）’总共是一个单元。
    // so we should first parse those unit and unary operators 所以我们应该首先解析这些单元和一元运算符
    // and then the binary ones 然后是二元的
    //
    // also the 解析一个表达式 can be in the following types: 表达式可以是以下类型：
    //
    // 1. unit_unary ::= unit | unit unary_op | unary_op unit  单元_一元 ::= 单元 | 单元 一元 | 一元_op 单元 
    // 2. expr ::= unit_unary (bin_op unit_unary ...)  表达式 ::= 单元_一元 (二元_op 单元_一元 ...) 

    // unit_unary() 单元_一元()
    整 *id;
    整 tmp;
    整 *addr;
    {
        如 (!标记流) {
            printf("%d: unexpected 标记流 EOF of 解析一个表达式 表达式的意外的令牌EOF\n", line);
            exit(-1);
        }
        如 (标记流 == Num) {
            match(Num);

            // emit code 发出代码
            *++代码段 = IMM;
            *++代码段 = token_val;
            expr_type = INT;
        }
        另 如 (标记流 == '"') {
            // continous string "abc" "abc" 连续字符串“abc”“abc”


            // emit code
            *++代码段 = IMM;
            *++代码段 = token_val;

            match('"');
            // store the rest strings 存储其余的字符串 
            当 (标记流 == '"') {
                match('"');
            }

            // append the end of string character '\0', all the 数据段 are default 追加字符串“\ 0”的结尾，所有的数据都是默认的
            // to 0, so just move 数据段 one position forward. 到0，所以只要将数据向前移动一个位置。
            数据段 = (字 *)(((整)数据段 + 求长度(整)) & (-求长度(整)));
            expr_type = PTR;
        }
        另 如 (标记流 == Sizeof) {
            // 求长度 is actually an unary operator  sizeof实际上是一个一元运算符
            // now only `求长度(整)`, `求长度(字)` and `求长度(*...)` are 现在只有`sizeof（int）`，`sizeof（char）`和`sizeof（* ...）`是
            // supported. 支持的。
            match(Sizeof);
            match('(');
            expr_type = INT;

            如 (标记流 == Int) {
                match(Int);
            } 另 如 (标记流 == Char) {
                match(Char);
                expr_type = CHAR;
            }

            当 (标记流 == Mul) {
                match(Mul);
                expr_type = expr_type + PTR;
            }

            match(')');

            // emit code 发出代码
            *++代码段 = IMM;
            *++代码段 = (expr_type == CHAR) ? 求长度(字) : 求长度(整);

            expr_type = INT;
        }
        另 如 (标记流 == Id) {
            // there are several type when occurs to Id  Id出现时有几种类型
            // but this is unit, so it can only be 但这是单位，所以它只能是
            // 1. function call 1.函数调用
            // 2. Enum variable  2.枚举变量
            // 3. global/local variable  3.全局/局部变量
            match(Id);

            id = current_id;

            如 (标记流 == '(') {
                // function call 函数调用
                match('(');

                // pass in arguments 传递参数
                tmp = 0; // number of arguments 参数个数
                当 (标记流 != ')') {
                    解析一个表达式(Assign);
                    *++代码段 = PUSH;
                    tmp ++;

                    如 (标记流 == ',') {
                        match(',');
                    }

                }
                match(')');

                // emit code
                如 (id[Class] == Sys) {
                    // system functions
                    *++代码段 = id[Value];
                }
                另 如 (id[Class] == Fun) {
                    // function call
                    *++代码段 = CALL;
                    *++代码段 = id[Value];
                }
                另 {
                    printf("%d: bad function call\n", line);
                    exit(-1);
                }

                // clean the 栈 for arguments 清理栈的参数
                如 (tmp > 0) {
                    *++代码段 = ADJ;
                    *++代码段 = tmp;
                }
                expr_type = id[Type];
            }
            另 如 (id[Class] == Num) {
                // 枚举 variable
                *++代码段 = IMM;
                *++代码段 = id[Value];
                expr_type = INT;
            }
            另 {
                // variable
                如 (id[Class] == Loc) {
                    *++代码段 = LEA;
                    *++代码段 = index_of_bp - id[Value];
                }
                另 如 (id[Class] == Glo) {
                    *++代码段 = IMM;
                    *++代码段 = id[Value];
                }
                另 {
                    printf("%d: undefined variable\n", line);
                    exit(-1);
                }

                // emit code, default behaviour is to load the value of the 发出代码，默认行为是加载
                // address which is stored in `通用寄存器` 存储在“ax”中的地址的值
                expr_type = id[Type];
                *++代码段 = (expr_type == Char) ? LC : LI;
            }
        }
        另 如 (标记流 == '(') {
            // cast or parenthesis 铸造或括号
            match('(');
            如 (标记流 == Int || 标记流 == Char) {
                tmp = (标记流 == Char) ? CHAR : INT; // cast type 铸造型
                match(标记流);
                当 (标记流 == Mul) {
                    match(Mul);
                    tmp = tmp + PTR;
                }

                match(')');

                解析一个表达式(Inc); // cast has precedence as Inc(++) 铸造优先级为Inc（++）

                expr_type  = tmp;
            } 另 {
                // normal parenthesis 正常的括号
                解析一个表达式(Assign);
                match(')');
            }
        }
        另 如 (标记流 == Mul) {
            // dereference *<addr> 解引用操作符*（addr＞
            match(Mul);
            解析一个表达式(Inc); // dereference has the same precedence as Inc(++) 取消引用与Inc（++）具有相同的优先级

            如 (expr_type >= PTR) {
                expr_type = expr_type - PTR;
            } 另 {
                printf("%d: bad dereference\n", line);
                exit(-1);
            }

            *++代码段 = (expr_type == CHAR) ? LC : LI;
        }
        另 如 (标记流 == And) {
            // get the address of 得到的地址
            match(And);
            解析一个表达式(Inc); // get the address of 得到的地址
            如 (*代码段 == LC || *代码段 == LI) {
                代码段 --;
            } 另 {
                printf("%d: bad address of\n", line);
                exit(-1);
            }

            expr_type = expr_type + PTR;
        }
        另 如 (标记流 == '!') {
            // not
            match('!');
            解析一个表达式(Inc);

            // emit code, use <expr> == 0 发出代码，使用<expr> == 0
            *++代码段 = IMM;
            *++代码段 = 0;
            *++代码段 = EQ;

            expr_type = INT;
        }
        另 如 (标记流 == '~') {
            // bitwise not 按位不是
            match('~');
            解析一个表达式(Inc);

            // emit code, use <expr> XOR -1 发出代码，使用<expr> XOR -1
            *++代码段 = PUSH;
            *++代码段 = IMM;
            *++代码段 = -1;
            *++代码段 = XOR;

            expr_type = INT;
        }
        另 如 (标记流 == Add) {
            // +var, do nothing
            match(Add);
            解析一个表达式(Inc);

            expr_type = INT;
        }
        另 如 (标记流 == Sub) {
            // -var
            match(Sub);

            如 (标记流 == Num) {
                *++代码段 = IMM;
                *++代码段 = -token_val;
                match(Num);
            } 另 {

                *++代码段 = IMM;
                *++代码段 = -1;
                *++代码段 = PUSH;
                解析一个表达式(Inc);
                *++代码段 = MUL;
            }

            expr_type = INT;
        }
        另 如 (标记流 == Inc || 标记流 == Dec) {
            tmp = 标记流;
            match(标记流);
            解析一个表达式(Inc);
            如 (*代码段 == LC) { 复制地址
                *代码段 = PUSH;  // to duplicate the address 复制地址
                *++代码段 = LC; 
            } 另 如 (*代码段 == LI) {
                *代码段 = PUSH;
                *++代码段 = LI;
            } 另 {
                printf("%d: bad lvalue of pre-increment\n", line);
                exit(-1);
            }
            *++代码段 = PUSH;
            *++代码段 = IMM;
            *++代码段 = (expr_type > PTR) ? 求长度(整) : 求长度(字);
            *++代码段 = (tmp == Inc) ? ADD : SUB;
            *++代码段 = (expr_type == CHAR) ? SC : SI;
        }
        另 {
            printf("%d: bad 解析一个表达式\n", line);
            exit(-1);
        }
    }

    // binary operator and postfix operators. 二元运算符和后缀运算符。
    {
        当 (标记流 >= level) {
            // handle according to current operator's precedence 根据当前运算符的优先级进行处理
            tmp = expr_type;
            如 (标记流 == Assign) {
                // var = expr;
                match(Assign);
                如 (*代码段 == LC || *代码段 == LI) {
                    *代码段 = PUSH; // save the lvalue's pointer 保存左值的指针
                } 另 {
                    printf("%d: bad lvalue in assignment\n", line);
                    exit(-1);
                }
                解析一个表达式(Assign);

                expr_type = tmp;
                *++代码段 = (expr_type == CHAR) ? SC : SI;
            }
            另 如 (标记流 == Cond) {
                // expr ? a : b;
                match(Cond);
                *++代码段 = JZ;
                addr = ++代码段;
                解析一个表达式(Assign);
                如 (标记流 == ':') {
                    match(':');
                } 另 {
                    printf("%d: missing colon in conditional\n", line);
                    exit(-1);
                }
                *addr = (整)(代码段 + 3);
                *++代码段 = JMP;
                addr = ++代码段;
                解析一个表达式(Cond);
                *addr = (整)(代码段 + 1);
            }
            另 如 (标记流 == Lor) {
                // logic or 逻辑或
                match(Lor);
                *++代码段 = JNZ;
                addr = ++代码段;
                解析一个表达式(Lan);
                *addr = (整)(代码段 + 1);
                expr_type = INT;
            }
            另 如 (标记流 == Lan) {
                // logic and
                match(Lan);
                *++代码段 = JZ;
                addr = ++代码段;
                解析一个表达式(Or);
                *addr = (整)(代码段 + 1);
                expr_type = INT;
            }
            另 如 (标记流 == Or) {
                // bitwise or 按位或
                match(Or);
                *++代码段 = PUSH;
                解析一个表达式(Xor);
                *++代码段 = OR;
                expr_type = INT;
            }
            另 如 (标记流 == Xor) {
                // bitwise xor
                match(Xor);
                *++代码段 = PUSH;
                解析一个表达式(And);
                *++代码段 = XOR;
                expr_type = INT;
            }
            另 如 (标记流 == And) {
                // bitwise and
                match(And);
                *++代码段 = PUSH;
                解析一个表达式(Eq);
                *++代码段 = AND;
                expr_type = INT;
            }
            另 如 (标记流 == Eq) {
                // equal == 等于==
                match(Eq);
                *++代码段 = PUSH;
                解析一个表达式(Ne);
                *++代码段 = EQ;
                expr_type = INT;
            }
            另 如 (标记流 == Ne) {
                // not equal !=
                match(Ne);
                *++代码段 = PUSH;
                解析一个表达式(Lt);
                *++代码段 = NE;
                expr_type = INT;
            }
            另 如 (标记流 == Lt) {
                // less than 少于
                match(Lt);
                *++代码段 = PUSH;
                解析一个表达式(Shl);
                *++代码段 = LT;
                expr_type = INT;
            }
            另 如 (标记流 == Gt) {
                // greater than 比...更棒
                match(Gt);
                *++代码段 = PUSH;
                解析一个表达式(Shl);
                *++代码段 = GT;
                expr_type = INT;
            }
            另 如 (标记流 == Le) {
                // less than or equal to 小于或等于
                match(Le);
                *++代码段 = PUSH;
                解析一个表达式(Shl);
                *++代码段 = LE;
                expr_type = INT;
            }
            另 如 (标记流 == Ge) {
                // greater than or equal to 大于或等于
                match(Ge);
                *++代码段 = PUSH;
                解析一个表达式(Shl);
                *++代码段 = GE;
                expr_type = INT;
            }
            另 如 (标记流 == Shl) {
                // shift left 向左移动
                match(Shl);
                *++代码段 = PUSH;
                解析一个表达式(Add);
                *++代码段 = SHL;
                expr_type = INT;
            }
            另 如 (标记流 == Shr) {
                // shift right
                match(Shr);
                *++代码段 = PUSH;
                解析一个表达式(Add);
                *++代码段 = SHR;
                expr_type = INT;
            }
            另 如 (标记流 == Add) {
                // add
                match(Add);
                *++代码段 = PUSH;
                解析一个表达式(Mul);

                expr_type = tmp;
                如 (expr_type > PTR) {
                    // pointer type, and not `字 *` 指针类型，而不是`字 *`
                    *++代码段 = PUSH;
                    *++代码段 = IMM;
                    *++代码段 = 求长度(整);
                    *++代码段 = MUL;
                }
                *++代码段 = ADD;
            }
            另 如 (标记流 == Sub) {
                // sub
                match(Sub);
                *++代码段 = PUSH;
                解析一个表达式(Mul);
                如 (tmp > PTR && tmp == expr_type) {
                    // pointer subtraction 指针减法
                    *++代码段 = SUB;
                    *++代码段 = PUSH;
                    *++代码段 = IMM;
                    *++代码段 = 求长度(整);
                    *++代码段 = DIV;
                    expr_type = INT;
                } 另 如 (tmp > PTR) {
                    // pointer movement 指针移动
                    *++代码段 = PUSH;
                    *++代码段 = IMM;
                    *++代码段 = 求长度(整);
                    *++代码段 = MUL;
                    *++代码段 = SUB;
                    expr_type = tmp;
                } 另 {
                    // numeral subtraction 数字减法
                    *++代码段 = SUB;
                    expr_type = tmp;
                }
            }
            另 如 (标记流 == Mul) {
                // multiply
                match(Mul);
                *++代码段 = PUSH;
                解析一个表达式(Inc);
                *++代码段 = MUL;
                expr_type = tmp;
            }
            另 如 (标记流 == Div) {
                // divide
                match(Div);
                *++代码段 = PUSH;
                解析一个表达式(Inc);
                *++代码段 = DIV;
                expr_type = tmp;
            }
            另 如 (标记流 == Mod) {
                // Modulo
                match(Mod);
                *++代码段 = PUSH;
                解析一个表达式(Inc);
                *++代码段 = MOD;
                expr_type = tmp;
            }
            另 如 (标记流 == Inc || 标记流 == Dec) {
                // postfix inc(++) and dec(--)  后缀 inc（++）和dec（ - ），
                // we will increase the value to the variable and decrease it  我们将把这个值增加到变量上，
                // on `通用寄存器` to get its original value. 并在ax上减小它以得到它的原始值。
                如 (*代码段 == LI) {
                    *代码段 = PUSH;
                    *++代码段 = LI;
                }
                另 如 (*代码段 == LC) {
                    *代码段 = PUSH;
                    *++代码段 = LC;
                }
                另 {
                    printf("%d: bad value in increment\n", line);
                    exit(-1);
                }

                *++代码段 = PUSH;
                *++代码段 = IMM;
                *++代码段 = (expr_type > PTR) ? 求长度(整) : 求长度(字);
                *++代码段 = (标记流 == Inc) ? ADD : SUB;
                *++代码段 = (expr_type == CHAR) ? SC : SI;
                *++代码段 = PUSH;
                *++代码段 = IMM;
                *++代码段 = (expr_type > PTR) ? 求长度(整) : 求长度(字);
                *++代码段 = (标记流 == Inc) ? SUB : ADD;
                match(标记流);
            }
            另 如 (标记流 == Brak) {
                // array access var[xx] 数组访问var [xx]
                match(Brak);
                *++代码段 = PUSH;
                解析一个表达式(Assign);
                match(']');

                如 (tmp > PTR) { 
                    // pointer, `not 字 *` 指针，不是char *`
                    *++代码段 = PUSH;
                    *++代码段 = IMM;
                    *++代码段 = 求长度(整);
                    *++代码段 = MUL;
                }
                另 如 (tmp < PTR) {
                    printf("%d: pointer type expected 指针类型的预期\n", line);
                    exit(-1);
                }
                expr_type = tmp - PTR;
                *++代码段 = ADD;
                *++代码段 = (expr_type == CHAR) ? LC : LI;
            }
            另 {
                printf("%d: compiler error, 标记流 = %d\n", line, 标记流);
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
    // 6. 解析一个表达式; (解析一个表达式 end with semicolon)  6.表达; （表达式以分号结尾）

    整 *a, *b; // bess for branch control 对分支机构的控制

    如 (标记流 == If) {
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
        解析一个表达式(Assign);  // parse condition 解析条件
        match(')');

        // emit code for 如  为if发出代码
        *++代码段 = JZ;
        b = ++代码段;

        statement();         // parse statement 解析语句
        如 (标记流 == Else) { // parse 另
            match(Else);

            // emit code for JMP B
            *b = (整)(代码段 + 3);
            *++代码段 = JMP;
            b = ++代码段;

            statement();
        }

        *b = (整)(代码段 + 1);
    }
    另 如 (标记流 == While) {
        //
        // a:                     a:
        //    当 (<cond>)        <cond>
        //                          JZ b
        //     <statement>          <statement>
        //                          JMP a
        // b:                     b:
        match(While);

        a = 代码段 + 1;

        match('(');
        解析一个表达式(Assign);
        match(')');

        *++代码段 = JZ;
        b = ++代码段;

        statement();

        *++代码段 = JMP;
        *++代码段 = (整)a;
        *b = (整)(代码段 + 1);
    }
    另 如 (标记流 == '{') {
        // { <statement> ... }
        match('{');

        当 (标记流 != '}') {
            statement();
        }

        match('}');
    }
    另 如 (标记流 == Return) {
        // 返回 [解析一个表达式]; 返回表达式
        match(Return);

        如 (标记流 != ';') {
            解析一个表达式(Assign);
        }

        match(';');

        // emit code for 返回
        *++代码段 = LEV;
    }
    另 如 (标记流 == ';') {
        // empty statement
        match(';');
    }
    另 {
        // a = b; or function_call();
        解析一个表达式(Assign);
        match(';');
    }
}

空 enum_declaration() {
    // parse 枚举 [id] { a = 1, b = 3, ...} 解析枚举
    整 i;
    i = 0;
    当 (标记流 != '}') {
        如 (标记流 != Id) {
            printf("%d: bad 枚举 identifier %d\n", line, 标记流);
            exit(-1);
        }
        词法分析();
        如 (标记流 == Assign) {
            // like {a=10}
            词法分析();
            如 (标记流 != Num) {
                printf("%d: bad 枚举 initializer\n", line);
                exit(-1);
            }
            i = token_val;
            词法分析();
        }

        current_id[Class] = Num;
        current_id[Type] = INT;
        current_id[Value] = i++;

        如 (标记流 == ',') {
            词法分析();
        }
    }
}

空 function_parameter() {
    整 type;
    整 params;
    params = 0;
    当 (标记流 != ')') {
        // 整 name, ...
        type = INT;
        如 (标记流 == Int) {
            match(Int);
        } 另 如 (标记流 == Char) {
            type = CHAR;
            match(Char);
        }

        // pointer type
        当 (标记流 == Mul) {
            match(Mul);
            type = type + PTR;
        }

        // parameter name 参数名称
        如 (标记流 != Id) {
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

        如 (标记流 == ',') {
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

    整 pos_local; // position of local variables on the 栈. 堆栈上局部变量的位置。
    整 type;
    pos_local = index_of_bp;

    当 (标记流 == Int || 标记流 == Char) {
        // local variable declaration, just like global ones. 局部变量声明，就像全局变量一样。
        basetype = (标记流 == Int) ? INT : CHAR;
        match(标记流);

        当 (标记流 != ';') {
            type = basetype;
            当 (标记流 == Mul) {
                match(Mul);
                type = type + PTR;
            }

            如 (标记流 != Id) {
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

            如 (标记流 == ',') {
                match(',');
            }
        }
        match(';');
    }

    // save the 栈 size for local variables 保存本地变量的堆栈大小
    *++代码段 = ENT;
    *++代码段 = pos_local - index_of_bp;

    // statements
    当 (标记流 != '}') {
        statement();
    }

    // emit code for leaving the sub function 发出离开子功能的代码
    *++代码段 = LEV;
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
    当 (current_id[标记流]) {
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
    如 (标记流 == Enum) {
        // 枚举 [id] { a = 10, b = 20, ... }
        match(Enum);
        如 (标记流 != '{') {
            match(Id); // skip the [id] part
        }
        如 (标记流 == '{') {
            // parse the assign part 解析分配部分
            match('{');
            enum_declaration();
            match('}');
        }

        match(';');
        返回;
    }

    // parse type information 解析类型信息
    如 (标记流 == Int) {
        match(Int);
    }
    另 如 (标记流 == Char) {
        match(Char);
        basetype = CHAR;
    }

    // parse the comma seperated variable declaration. 分析逗号分隔的变量声明。
    当 (标记流 != ';' && 标记流 != '}') {
        type = basetype;
        // parse pointer type, note that there may exist `整 ****x;` 解析指针类型，注意可能存在`整 **** x;`
        当 (标记流 == Mul) {
            match(Mul);
            type = type + PTR;
        }

        如 (标记流 != Id) {
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

        如 (标记流 == '(') {
            current_id[Class] = Fun;
            current_id[Value] = (整)(代码段 + 1); // the memory address of function 函数的内存地址
            function_declaration();
        } 另 {
            // variable declaration
            current_id[Class] = Glo; // global variable
            current_id[Value] = (整)数据段; // assign memory address 分配内存地址
            数据段 = 数据段 + 求长度(整);
        }

        如 (标记流 == ',') {
            match(',');
        }
    }
    词法分析();
}

空 语法分析() {
    // get 词法分析 标记流 得到下一个令牌
    词法分析();
    当 (标记流 > 0) {
        global_declaration();
    }
}

整 虚拟机() {
    整 op, *tmp;
    cycle = 0;
    当 (1) {
        cycle ++;
        op = *程序计数器++; // get 词法分析 operation code 获取下一个操作代码

        // print debug info 打印调试信息
        如 (debug) {
            printf("%d> %.4s", cycle,
                   & "LEA ,IMM ,JMP ,CALL,JZ  ,JNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PUSH,"
                   "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
                   "OPEN,READ,CLOS,PRTF,MALC,MSET,MCMP,EXIT"[op * 5]);
            如 (op <= ADJ)
                printf(" %d\n", *程序计数器);
            另
                printf("\n");
        }
        如 (op == IMM)       {通用寄存器 = *程序计数器++;}                                     // load immediate value to 通用寄存器  将即时值加载到ax
        另 如 (op == LC)   {通用寄存器 = *(字 *)通用寄存器;}                               // load character to 通用寄存器, address in ax   加载字符到ax，地址在ax
        另 如 (op == LI)   {通用寄存器 = *(整 *)通用寄存器;}                                // load integer to 通用寄存器, address in 通用寄存器 将整数加载到ax，地址放在ax中
        另 如 (op == SC)   {通用寄存器 = *(字 *)*指针寄存器++ = 通用寄存器;}                       // save character to address, value in 通用寄存器, address on 栈 保存字符到地址，值在ax，堆栈上的地址
        另 如 (op == SI)   {*(整 *)*指针寄存器++ = 通用寄存器;}                             // save integer to address, value in 通用寄存器, address on 栈 将整数保存到地址，值为ax，堆栈上的地址
        另 如 (op == PUSH) {*--指针寄存器 = 通用寄存器;}                                     // push the value of 通用寄存器 onto the 栈 将ax的值推入堆栈
        另 如 (op == JMP)  {程序计数器 = (整 *)*程序计数器;}                                // jump to the address             跳转到地址
        另 如 (op == JZ)   {程序计数器 = 通用寄存器 ? 程序计数器 + 1 : (整 *)*程序计数器;}                   // jump 如 通用寄存器 is zero            如果ax为零，则跳转
        另 如 (op == JNZ)  {程序计数器 = 通用寄存器 ? (整 *)*程序计数器 : 程序计数器 + 1;}                   // jump 如 通用寄存器 is zero            如果ax为零，则跳转
        另 如 (op == CALL) {*--指针寄存器 = (整)(程序计数器+1); 程序计数器 = (整 *)*程序计数器;}           // call subroutine            调用子程序
        //另 如 (op == RET)  {程序计数器 = (整 *)*指针寄存器++;}                              // 返回 from subroutine;         子程序返回;
        另 如 (op == ENT)  {*--指针寄存器 = (整)基址指针; 基址指针 = 指针寄存器; 指针寄存器 = 指针寄存器 - *程序计数器++;}      // make new 栈 frame             使新的堆栈框架
        另 如 (op == ADJ)  {指针寄存器 = 指针寄存器 + *程序计数器++;}                                // add esp, <size>                 添加esp，<size>
        另 如 (op == LEV)  {指针寄存器 = 基址指针; 基址指针 = (整 *)*指针寄存器++; 程序计数器 = (整 *)*指针寄存器++;}  // restore call frame and          PC恢复呼叫帧和PC
        另 如 (op == LEA)  {通用寄存器 = (整)(基址指针 + *程序计数器++);}                         // load address for arguments.      加载参数的地址。

        另 如 (op == OR)  通用寄存器 = *指针寄存器++ | 通用寄存器;
        另 如 (op == XOR) 通用寄存器 = *指针寄存器++ ^ 通用寄存器;
        另 如 (op == AND) 通用寄存器 = *指针寄存器++ & 通用寄存器;
        另 如 (op == EQ)  通用寄存器 = *指针寄存器++ == 通用寄存器;
        另 如 (op == NE)  通用寄存器 = *指针寄存器++ != 通用寄存器;
        另 如 (op == LT)  通用寄存器 = *指针寄存器++ < 通用寄存器;
        另 如 (op == LE)  通用寄存器 = *指针寄存器++ <= 通用寄存器;
        另 如 (op == GT)  通用寄存器 = *指针寄存器++ >  通用寄存器;
        另 如 (op == GE)  通用寄存器 = *指针寄存器++ >= 通用寄存器;
        另 如 (op == SHL) 通用寄存器 = *指针寄存器++ << 通用寄存器;
        另 如 (op == SHR) 通用寄存器 = *指针寄存器++ >> 通用寄存器;
        另 如 (op == ADD) 通用寄存器 = *指针寄存器++ + 通用寄存器;
        另 如 (op == SUB) 通用寄存器 = *指针寄存器++ - 通用寄存器;
        另 如 (op == MUL) 通用寄存器 = *指针寄存器++ * 通用寄存器;
        另 如 (op == DIV) 通用寄存器 = *指针寄存器++ / 通用寄存器;
        另 如 (op == MOD) 通用寄存器 = *指针寄存器++ % 通用寄存器;

        另 如 (op == EXIT) { printf("exit(%d)", *指针寄存器); 返回 *指针寄存器;}
        另 如 (op == OPEN) { 通用寄存器 = open((字 *)指针寄存器[1], 指针寄存器[0]); }
        另 如 (op == CLOS) { 通用寄存器 = close(*指针寄存器);}
        另 如 (op == READ) { 通用寄存器 = read(指针寄存器[2], (字 *)指针寄存器[1], *指针寄存器); }
        另 如 (op == PRTF) { tmp = 指针寄存器 + 程序计数器[1]; 通用寄存器 = printf((字 *)tmp[-1], tmp[-2], tmp[-3], tmp[-4], tmp[-5], tmp[-6]); }
        另 如 (op == MALC) { 通用寄存器 = (整)malloc(*指针寄存器);}
        另 如 (op == MSET) { 通用寄存器 = (整)memset((字 *)指针寄存器[2], 指针寄存器[1], *指针寄存器);}
        另 如 (op == MCMP) { 通用寄存器 = memcmp((字 *)指针寄存器[2], (字 *)指针寄存器[1], *指针寄存器);}
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
    如 (!(代码段 = malloc(poolsize))) {
        printf("could not malloc(%d) for 代码段 area\n", poolsize);
        返回 -1;
    }
    如 (!(数据段 = malloc(poolsize))) {
        printf("could not malloc(%d) for 数据段 area\n", poolsize);
        返回 -1;
    }
    如 (!(栈 = malloc(poolsize))) {
        printf("could not malloc(%d) for 栈 area\n", poolsize);
        返回 -1;
    }
    如 (!(symbols = malloc(poolsize))) {
        printf("could not malloc(%d) for symbol table\n", poolsize);
        返回 -1;
    }

    memset(代码段, 0, poolsize);
    memset(数据段, 0, poolsize);
    memset(栈, 0, poolsize);
    memset(symbols, 0, poolsize);

    old_text = 代码段;

    src = "字 另 枚举 如 整 返回 求长度 当 "
          "open read close printf malloc memset memcmp exit 空 main";

     // add keywords to symbol table 添加关键字符号表
    i = Char;
    当 (i <= While) {
        词法分析();
        current_id[标记流] = i++;
    }

    // add library to symbol table 添加库到符号表
    i = OPEN;
    当 (i <= EXIT) {
        词法分析();
        current_id[Class] = Sys;
        current_id[Type] = INT;
        current_id[Value] = i++;
    }

    词法分析(); current_id[标记流] = Char; // handle 空 type 处理void类型
    词法分析(); idmain = current_id; // keep track of main 跟踪主要

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

    语法分析();

    如 (!(程序计数器 = (整 *)idmain[Value])) {
        printf("main() not defined\n");
        返回 -1;
    }

    // dump_text(); 转储文本
    如 (assembly) {
        // only for compile 仅用于编译
        返回 0;
    }

    // setup 栈 设置堆栈
    指针寄存器 = (整 *)((整)栈 + poolsize);
    *--指针寄存器 = EXIT; // call exit 如 main returns 如果主要返回，则调用exit
    *--指针寄存器 = PUSH; tmp = 指针寄存器;
    *--指针寄存器 = argc;
    *--指针寄存器 = (整)argv;
    *--指针寄存器 = (整)tmp;

    返回 虚拟机();
}
