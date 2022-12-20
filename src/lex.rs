use itertools::Itertools;
// use malachite::Integer;
use std::{fmt::Display, str::Chars};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Num(f64),
    Name(String),
    Symbol(String),
    Space,
    Eof,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Num(num) => write!(f, "{}", num),
            Token::Name(name) => write!(f, "'{}", name),
            Token::Symbol(name) => write!(f, "'{}", name),
            Token::Space => write!(f, " "),
            Token::Eof => write!(f, "EOF"),
        }
    }
}

pub const fn is_symbol(char: char) -> bool {
    matches!(
        char,
        '+' | '-' | '*' | '/' | '=' | '(' | ')' | '%' | '^' | '!'
    )
}

pub fn lex(chars: Chars) -> impl Iterator<Item = Token> + '_ {
    let mut chars = chars.peekable();

    std::iter::from_fn(move || {
        let char = chars.peek()?;

        match char {
            // skip whitespace
            char if char.is_whitespace() => {
                chars
                    .by_ref()
                    .peeking_take_while(|ch| ch.is_whitespace())
                    .for_each(drop);
                return Some(Token::Space);
            }
            char if char.is_alphabetic() => {
                // name
                let name: String = chars
                    .by_ref()
                    .peeking_take_while(|ch| ch.is_alphabetic())
                    .collect();

                return Some(Token::Name(name.to_uppercase()));
            }
            char if char.is_ascii_digit() => {
                // num, radix parsing not included
                let num: usize = chars
                    .by_ref()
                    .peeking_take_while(|ch| ch.is_ascii_digit())
                    .fold(0, |acc, ch| {
                        (acc * 10) + ch.to_digit(10).expect("Invalid Digit") as usize
                    });

                return Some(Token::Num(num as f64));
            }
            char if is_symbol(*char) => {
                // symbol
                let symbol: String = chars
                    .by_ref()
                    .peeking_take_while(|&ch| is_symbol(ch))
                    .collect();

                return Some(Token::Symbol(symbol));
            }
            t => panic!("invalid token: {}", t),
        }
    })
    .filter(|tok| !matches!(tok, Token::Space))
    .chain(std::iter::once(Token::Eof))
}
