#![feature(if_let_guard)]
use std::f64::consts::{E, PI, TAU};
use std::io::BufRead;
use std::{fmt, iter::Peekable};

mod lex;
use lex::{lex, Token};

// sexp
#[derive(Debug, Clone)]
enum S {
    Num(f64),
    Atom(String),
    Cons(Token, Vec<S>),
}

impl fmt::Display for S {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            S::Num(i) => write!(f, "{}", i),
            S::Atom(i) => write!(f, "{}", i),
            S::Cons(head, rest) => {
                write!(f, "({}", head)?;
                for s in rest {
                    write!(f, " {}", s)?
                }
                write!(f, ")")
            }
        }
    }
}

trait InfixBinding {
    fn infix_binding_power(&self) -> Option<(u8, u8)>;
}

struct Tokens<T: Iterator<Item = Token>> {
    tokens: Peekable<T>,
}

impl<T: Iterator<Item = Token>> Tokens<T> {
    fn from(tokens: T) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    fn expr_bp(&mut self, min_bp: u8) -> Option<S> {
        // prefix
        let mut lhs = match self.tokens.next()? {
            t if let Some(((), rbp)) = prefix_binding_power(&t) => {
                let rhs = self.expr_bp(rbp)?;
                S::Cons(t, vec![rhs])
            }
            Token::Num(num) => S::Num(num),
            Token::Name(name) => S::Atom(name),
            Token::Symbol(sym) if sym == "(" => {
                let lhs = self.expr_bp(0)?;
                // dbg!(&lhs);
                assert_eq!(self.tokens.next(), Some(Token::Symbol(String::from(")"))));
                lhs
            }
            t => panic!("bad prefix token: {:?}", t),
        };

        loop {
            let sym = match self.tokens.peek()? {
                Token::Eof => break,
                sym @ Token::Symbol(_) => sym.clone(),
                name @ Token::Name(_) => name.clone(),
                num @ Token::Num(_) => num.clone(),
                t => panic!("bad infix token: {:?}", t),
            };

            if let Some((l_bp, ())) = postfix_binding_power(&sym) {
                if l_bp < min_bp {
                    break;
                }
                self.tokens.next();
                lhs = S::Cons(sym, vec![lhs]);
                continue;
            }

            if let Some((l_bp, r_bp)) = infix_binding_power(&sym) {
                if l_bp < min_bp {
                    break;
                }
                self.tokens.next();
                let rhs = self.expr_bp(r_bp)?;
                lhs = S::Cons(sym, vec![lhs, rhs]);
                continue;
            }
            break;
        }

        Some(lhs)
    }
}

// TODO: fix bp
// ( 3, 4 ) Add, Sub
// ( 5, 6 ) Mul, Div, Mod
// ( 7, 8 ) Pow, Root
// ( 9, 10) Log, Perm, Comb
// (11, 12) Factorial
// (13, 14) everything else?

fn prefix_binding_power(sym: &Token) -> Option<((), u8)> {
    // dbg!(sym);
    let res = match sym {
        Token::Symbol(sym) => match sym.as_str() {
            "+" | "-" => ((), 9),
            _ => return None,
        },
        Token::Name(name) => match name.as_str() {
            "EXP" => ((), 9),
            "SQRT" => ((), 9),
            "CBRT" => ((), 9),
            "LN" => ((), 9),
            "SIN" => ((), 9),
            "COS" => ((), 9),
            "TAN" => ((), 9),
            "ASIN" => ((), 9),
            "SINH" => ((), 9),
            "COSH" => ((), 9),
            "TANH" => ((), 9),
            "ASINH" => ((), 9),
            "ACOSH" => ((), 9),
            "ATANH" => ((), 9),
            "ABS" => ((), 9),
            "RCL" => ((), u8::MAX), // capture only one
            _ => return None,
        },
        _ => return None,
    };
    Some(res)
}

fn infix_binding_power(sym: &Token) -> Option<(u8, u8)> {
    let res = match sym {
        Token::Symbol(sym) => match sym.as_str() {
            "^" | "**" => (7, 8),
            "*" | "/" | "%" => (5, 6),
            "+" | "-" => (3, 4),
            _ => return None,
        },
        Token::Name(name) => match name.as_str() {
            "ROOT" => (7, 8),
            "LOG" => (9, 10),
            "P" => (5, 6),
            "C" => (5, 6),
            _ => return None,
        },
        _ => return None,
    };
    Some(res)
}

fn postfix_binding_power(sym: &Token) -> Option<(u8, ())> {
    let res = match sym {
        Token::Symbol(sym) => match sym.as_str() {
            "!" => (9, ()),
            "!!" => (9, ()),
            _ => return None,
        },
        _ => return None,
    };
    Some(res)
}

fn to_int(f: f64) -> u64 {
    if f.fract() == 0.0 {
        f as u64
    } else {
        panic!("{f} is not an integer")
    }
}

fn factorial(num: u64) -> u64 {
    (1..num).product()
}

fn combinations(n: u64, k: u64) -> u64 {
    (1..k).map(|i| (n + 1 - i) / i).product()
}

fn permutations(l: u64, r: u64) -> u64 {
    assert!(l > r);
    factorial(l) / (l - r)
}

#[derive(Debug, Default)]
struct Interpreter {
    mem: [f64; 10],
}

impl Interpreter {
    fn new() -> Self {
        Self::default()
    }

    fn ans(&self) -> f64 {
        self.mem[0]
    }

    fn recall(&self, nth: f64) -> f64 {
        let nth = to_int(nth);
        assert!(1 <= nth && nth <= 9, "Recall must be between 1 and 9");
        self.mem[(nth as usize + 1)]
    }

    fn functions(&self, name: &str, args: &[f64]) -> f64 {
        match (name, args) {
            ("+", [val]) => *val,
            ("+", [lhs, rhs]) => lhs + rhs,
            ("-", [val]) => -val,
            ("-", [lhs, rhs]) => lhs - rhs,
            ("*", [lhs, rhs]) => lhs * rhs,
            ("/", [lhs, rhs]) => lhs / rhs,
            ("%", [lhs, rhs]) => lhs % rhs,
            ("EXP", [val]) => val.exp(),
            ("^" | "**", [lhs, rhs]) => lhs.powf(*rhs),
            ("!", [val]) => factorial(to_int(*val)) as f64,
            // ("!!", [val]) => Natural::double_factorial(u64(val)).into(),
            ("SQRT", [val]) => val.sqrt(),
            ("CBRT", [val]) => val.cbrt(),
            // ("ROOT", [lhs, rhs]) => lhs,
            ("LN", [val]) => val.ln(),
            ("LOG", [lhs, rhs]) => lhs.log(*rhs),
            ("PI", []) => PI,
            ("E", []) => E,
            ("TAU", []) => TAU,
            ("SIN", [val]) => val.sin(),
            ("COS", [val]) => val.cos(),
            ("TAN", [val]) => val.tan(),
            ("ASIN", [val]) => val.asin(),
            ("ACOS", [val]) => val.acos(),
            ("ATAN", [val]) => val.atan(),
            ("SINH", [val]) => val.sinh(),
            ("COSH", [val]) => val.cosh(),
            ("TANH", [val]) => val.tanh(),
            ("ASINH", [val]) => val.asinh(),
            ("ACOSH", [val]) => val.acosh(),
            ("ATANH", [val]) => val.atanh(),
            ("P", [lhs, rhs]) => permutations(to_int(*lhs), to_int(*rhs)) as f64,
            ("C", [lhs, rhs]) => combinations(to_int(*lhs), to_int(*rhs)) as f64,
            ("ABS", [val]) => val.abs(),
            ("ANS", []) => self.ans(),
            ("RCL", [val]) => self.recall(*val),
            (name, args) => todo!("('{name} {:?})", args),
        }
    }

    fn evaluate_expr(&self, expr: &S) -> f64 {
        match expr {
            S::Num(num) => *num,
            S::Cons(Token::Name(name) | Token::Symbol(name), args) => self.functions(
                &name,
                &args
                    .iter()
                    .map(|arg| self.evaluate_expr(arg))
                    .collect::<Vec<_>>(),
            ),
            S::Atom(name) => self.functions(name, &[]),
            s => panic!("Unexpected Expression: {s}"),
        }
    }

    fn evaluate(&mut self, expr: &S) -> f64 {
        let res = self.evaluate_expr(expr);
        self.mem.rotate_right(1);
        self.mem[0] = res;
        res
    }
}

fn main() {
    let mut interpreter = Interpreter::new();

    for line in std::io::stdin().lock().lines() {
        let line = line.unwrap();

        let lexed = lex(line.chars());
        let mut tokens = Tokens::from(lexed);
        let expr = tokens.expr_bp(0).expect("invalid expression?");
        let res = interpreter.evaluate(&expr);
        println!("{}", expr);
        println!("> {}", res);
    }
}
