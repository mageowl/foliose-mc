use std::{iter::Peekable, str::Chars};

use crate::error::{Chunk, SourceError, SourcePos, Span};
use token::Token;

pub mod token;

const SEPERATORS: [char; 8] = [';', ',', '(', ')', '{', '}', '[', ']'];
const OPERATORS: [char; 10] = ['=', '+', '-', '/', '%', '>', '<', ':', '.', '#'];

pub struct TokenStream<'a> {
    source: Peekable<Chars<'a>>,
    pos: SourcePos,
    done: bool,
    peeked: Option<Option<<Self as Iterator>::Item>>,
}

impl<'a> From<&'a str> for TokenStream<'a> {
    fn from(value: &'a str) -> Self {
        Self {
            source: value.chars().peekable(),
            pos: SourcePos { ln: 1, col: 1 },
            done: false,
            peeked: None,
        }
    }
}

impl TokenStream<'_> {
    pub fn expect_next(&mut self) -> Result<Chunk<Token>, SourceError> {
        self.next().unwrap_or_else(|| {
            Err(SourceError {
                span: Span::new(self.pos, self.pos),
                message: "Unexpected end of file.".to_string(),
            })
        })
    }

    pub fn is_done(&self) -> bool {
        self.done
    }

    pub fn get_pos(&self) -> SourcePos {
        self.pos
    }

    pub fn peek(&mut self) -> Option<&<Self as Iterator>::Item> {
        if self.peeked.is_none() {
            self.peeked = Some(self.next());
        }

        // Will never panic: if it was None, gets set to Some above.
        self.peeked.as_ref().unwrap().as_ref()
    }

    pub fn peek_token(&mut self) -> Option<&Token> {
        self.peek().map(|r| r.as_ref().ok().map(|c| &c.data))?
    }

    pub fn next_raw(&mut self) -> Result<char, SourceError> {
        self.source.next().ok_or_else(|| SourceError {
            span: Span::new(self.pos, self.pos),
            message: "Unexpected end of file.".to_string(),
        })
    }
}

enum Context {
    Program,
    Comment,
    String,
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Result<Chunk<Token>, SourceError>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(v) = self.peeked.take() {
            return v;
        }

        if self.done {
            return None;
        }

        let mut start = self.pos;
        let mut chunk = String::new();
        let mut context = Context::Program;

        while let Some(char) = self.source.next() {
            match context {
                Context::Program => {
                    match char {
                        '\n' | '\t' | '\r' | ' ' => {
                            if !chunk.is_empty() {
                                break;
                            }
                        }
                        c if OPERATORS.contains(&c) => {
                            chunk.push(char);
                            if self.source.peek().is_some_and(|c| OPERATORS.contains(c)) {
                                self.pos.col += 1;
                                chunk.push(self.source.next().unwrap());
                            }

                            if chunk == "//" {
                                context = Context::Comment;
                                chunk = String::new();
                            } else {
                                break;
                            }
                        }
                        c if SEPERATORS.contains(&c) => {
                            chunk.push(char);
                            break;
                        }
                        '"' => {
                            context = Context::String;
                        }
                        '@' => {
                            self.pos.col += 1;
                            let sel_char = if let Some(c) = self.source.next() {
                                c
                            } else {
                                return Some(Err(SourceError {
                                    span: Span::new(self.pos, self.pos),
                                    message: "Unexpected end of file.".to_string(),
                                }));
                            };

                            return Some(Ok(Chunk {
                                span: Span::new(start, self.pos),
                                data: Token::Selector(sel_char),
                            }));
                        }
                        _ => {
                            chunk.push(char);
                        }
                    }

                    if self
                        .source
                        .peek()
                        .is_some_and(|c| OPERATORS.contains(c) || SEPERATORS.contains(c))
                        && !chunk.is_empty()
                    {
                        break;
                    }
                }
                Context::String => match char {
                    '"' => {
                        return Some(Ok(Chunk {
                            span: Span::new(start, self.pos),
                            data: Token::String(chunk),
                        }))
                    }
                    _ => chunk.push(char),
                },
                Context::Comment => match char {
                    '\n' => {
                        context = Context::Program;
                        start = self.pos;
                    }
                    _ => (),
                },
            }

            if char == '\n' {
                self.pos.ln += 1;
                self.pos.col = 1;
                start = self.pos;
            } else {
                self.pos.col += 1;
            }
        }

        if self.source.peek().is_none() {
            self.done = true;
        }

        if chunk.is_empty() {
            None
        } else {
            let token = chunk.try_into().map_err(|message| SourceError {
                span: Span::new(start, self.pos),
                message,
            });
            Some(token.map(|token| Chunk {
                span: Span::new(start, self.pos),
                data: token,
            }))
        }
    }
}
