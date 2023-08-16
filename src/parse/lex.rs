use crate::CodePosition;
use std::{ops::Range, iter::Peekable};
use logos::{Logos, SpannedIter};

#[derive(Clone, Debug, PartialEq)]
pub enum Atom
{
    Int(i32),
    Float(f32),
    Ident(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Op
{
    Assign,
    Plus,
    Minus,
    Times,
    By,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    BlockSep,
    ItemSep,
    ObjNav,
    CodeNav,
    Type,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Key
{
    Let,
    Fn,
    MapsTo,
}

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(skip r"[ \t\v\f\r]+")]
pub enum Token
{
    Eof,
    #[token("\n")]
    Newline,
    #[regex("\\-?[0-9]+", |lex| lex.slice().parse().ok().map(Atom::Int))]
    #[regex("\\-?[0-9]+\\.[0-9]+", |lex| lex.slice().parse().ok().map(Atom::Float))]
    #[regex("[a-zA-Z][a-zA-Z_0-9]*", |lex| (Atom::Ident(lex.slice().to_owned())))]
    Atom(Atom),
    #[token("=", |_| Op::Assign)]
    #[token("+", |_| Op::Plus)]
    #[token("-", |_| Op::Minus)]
    #[token("*", |_| Op::Times)]
    #[token("/", |_| Op::By)]
    #[token("(", |_| Op::LParen)]
    #[token(")", |_| Op::RParen)]
    #[token("[", |_| Op::LBracket)]
    #[token("]", |_| Op::RBracket)]
    #[token("{", |_| Op::LBrace)]
    #[token("}", |_| Op::RBrace)]
    #[token(";", |_| Op::BlockSep)]
    #[token(",", |_| Op::ItemSep)]
    #[token(".", |_| Op::ObjNav)]
    #[token("::", |_| Op::CodeNav)]
    #[token(":", |_| Op::Type)]
    Op(Op),
    #[token("let", |_| Key::Let)]
    #[token("fn", |_| Key::Fn)]
    #[token("->", |_| Key::MapsTo)]
    Key(Key),
}

pub type IterItem = Result<(Token, CodePosition), CodePosition>;

struct TokenIterInner<'source>
{
    tokens: SpannedIter<'source, Token>,
    line_number: usize,
    range_offset: usize
}

impl<'source> TokenIterInner<'source>
{
    fn code_position(&self, raw: Range<usize>) -> CodePosition
    {
        CodePosition { line: self.line_number, cols: Range { start: raw.start - self.range_offset + 1, end: raw.end - self.range_offset + 1 } }
    }
}

impl<'source> Iterator for TokenIterInner<'source>
{
    type Item = IterItem;

    fn next(&mut self) -> Option<Self::Item>
    {
        match self.tokens.next()
        {
            Some((Ok(Token::Newline), range)) =>
            {
                self.line_number += 1;
                self.range_offset = range.end;
                self.next()
            },
            Some((token, range)) =>
            {
                let pos = self.code_position(range);
                match token
                {
                    Ok(token) => Some(Ok((token, pos))),
                    Err(_) => Some(Err(pos))
                }
            },
            None => None
        }
    }
}

pub struct TokenIter<'source>(Peekable<TokenIterInner<'source>>);

impl<'source> TokenIter<'source>
{
    pub fn from(code: &'source str) -> Self
    {
        let iter = TokenIterInner
        {
            tokens: Token::lexer(code).spanned(),
            line_number: 1,
            range_offset: 0
        };
        Self(iter.peekable())
    }

    pub fn peek(&mut self) -> IterItem
    {
        self.0.peek().cloned().unwrap_or(Ok((Token::Eof, CodePosition::default())))
    }

    pub fn next(&mut self) -> IterItem
    {
        self.0.next().unwrap_or(Ok((Token::Eof, CodePosition::default())))
    }
}
