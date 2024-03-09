use std::iter::Peekable;
use std::str::Chars;

// Define a trait for token types
pub trait TokenType: std::fmt::Debug {}

#[derive(Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    Number(i64),
    Operator(char),
    // Add more token types as needed
}

impl TokenType for Token {}

#[derive(Debug)]
pub enum LexerError {
    InvalidCharacter(char),
    UnexpectedEndOfInput,
    InvalidNumber,
    // Add more error types as needed
}

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input: input.chars().peekable(),
        }
    }

    fn consume_while<F>(&mut self, condition: F) -> String
    where
        F: Fn(char) -> bool,
    {
        let mut result = String::new();
        while let Some(&c) = self.input.peek() {
            if condition(c) {
                result.push(c);
                self.input.next();
            } else {
                break;
            }
        }
        result
    }

    fn is_ident_start(c: char) -> bool {
        let _ = c.is_ascii_alphabetic() || c == '_';
        c.is_ascii_alphanumeric() || c == '_'
    }

    fn is_ident_part(c: char) -> bool {
        c.is_ascii_alphanumeric() || c == '_'
    }

    fn tokenize_identifier(&mut self) -> Token {
        let ident = self.consume_while(Self::is_ident_part);
        Token::Identifier(ident)
    }

    fn tokenize_number(&mut self) -> Result<Token, LexerError> {
        let number_str = self.consume_while(|c| c.is_digit(10));
        if let Some(c) = number_str.chars().next() {
            if c.is_digit(10) {
                if let Ok(number) = number_str.parse::<i64>() {
                    Ok(Token::Number(number))
                } else {
                    Err(LexerError::InvalidNumber)
                }
            } else {
                Err(LexerError::InvalidCharacter(c))
            }
        } else {
            Err(LexerError::UnexpectedEndOfInput)
        }
    }

    fn tokenize_operator(&mut self) -> Token {
        let op = self.input.next().unwrap();
        Token::Operator(op)
    }

    pub fn next_token(&mut self) -> Result<Option<Token>, LexerError> {
        while let Some(&c) = self.input.peek() {
            if c.is_whitespace() {
                self.input.next();
            } else {
                break;
            }
        }

        if let Some(&c) = self.input.peek() {
            match c {
                _ if Self::is_ident_start(c) => Ok(Some(self.tokenize_identifier())),
                _ if c.is_digit(10) => self.tokenize_number().map(Some),
                '+' | '-' | '*' | '/' | '=' => Ok(Some(self.tokenize_operator())),
                _ => Err(LexerError::InvalidCharacter(c)),
            }
        } else {
            Ok(None)
        }
    }
}

fn main() {
    let input = "x = 42 + y";
    let mut lexer = Lexer::new(input);

    while let Ok(Some(token)) = lexer.next_token() {
        println!("{:?}", token);
    }

    if let Err(error) = lexer.next_token() {
        match error {
            LexerError::InvalidCharacter(c) => {
                println!("Error: Invalid character '{}'", c);
            }
            LexerError::UnexpectedEndOfInput => {
                println!("Error: Unexpected end of input");
            }
            LexerError::InvalidNumber => {
                println!("Error: Invalid number");
            } // Add more error cases as needed
        }
    }
}
