use std::collections::HashMap;

use crate::{
    error::{Chunk, SourceError, Span},
    lexer::{
        token::{Token, CONDITIONS},
        TokenStream,
    },
};

pub trait Parsable: Sized {
    fn parse_from(src: &mut TokenStream) -> Result<Chunk<Self>, SourceError>;
}

impl Parsable for String {
    fn parse_from(src: &mut TokenStream) -> Result<Chunk<Self>, SourceError> {
        let chunk =
            src.expect_next("Trying to match identifier, but reached the end of the file.")?;
        if let Token::Ident(data) = chunk.data {
            Ok(Chunk {
                span: chunk.span,
                data,
            })
        } else {
            Err(SourceError {
                span: chunk.span,
                message: format!("Expected an identifier, got {:?} instead.", chunk.data),
            })
        }
    }
}

impl TokenStream<'_> {
    pub fn parse<T: Parsable>(&mut self) -> Result<Chunk<T>, SourceError> {
        T::parse_from(self)
    }
}

macro_rules! parse_symbol {
    ($s:ident, $t:pat, $msg:literal) => {
        let chunk = $s.expect_next("Trying to match $t, but reached the end of the file.")?;
        match chunk.data {
            $t => (),
            _ => {
                return Err(SourceError {
                    span: chunk.span,
                    message: format!($msg, chunk.data),
                })
            }
        }
    };
}

macro_rules! parse_group {
    ($s:ident, $t:pat) => {{
        let mut block = Vec::new();
        while !$s.is_done() {
            if let Some($t) = $s.peek_token() {
                break;
            }

            block.push($s.parse()?);
        }
        $s.expect_next("Trying to match $t, but reached the end of the file.")?;

        block
    }};
}

#[derive(Debug)]
pub struct Program {
    body: Vec<Chunk<ModuleBody>>,
}

impl Parsable for Program {
    fn parse_from(src: &mut TokenStream) -> Result<Chunk<Program>, SourceError> {
        let start = src.get_pos();
        let mut body = Vec::new();

        while !src.is_done() {
            body.push(src.parse()?);
        }

        Ok(Chunk {
            span: Span::new(start, src.get_pos()),
            data: Self { body },
        })
    }
}

#[derive(Debug)]
pub enum ModuleBody {
    Module {
        name: Chunk<String>,
        instructions: Vec<Chunk<String>>,
        body: Vec<Chunk<Box<ModuleBody>>>,
    },
    Function {
        name: Chunk<String>,
        instructions: Vec<Chunk<String>>,
        args: Vec<Chunk<FnArg>>,
        body: Vec<Chunk<Statement>>,
    },
    Use(Chunk<String>),
}

impl Parsable for ModuleBody {
    fn parse_from(src: &mut TokenStream) -> Result<Chunk<ModuleBody>, SourceError> {
        let next =
            src.expect_next("Trying to read module body, but reached the end of the file.")?;
        match next.data {
            Token::KeywordModule => {
                let name = src.parse()?;
                parse_symbol!(
                    src,
                    Token::BraceOpen,
                    "Expected an opening brace ('{{') for module, but instead got {:?}."
                );
                let body = parse_group!(src, Token::BraceClose)
                    .into_iter()
                    .map(|b| b.map(Box::new))
                    .collect();

                Ok(Chunk {
                    span: Span::new(next.span.start, src.get_pos()),
                    data: Self::Module {
                        name,
                        body,
                        instructions: Vec::new(),
                    },
                })
            }
            Token::KeywordFn => {
                let name = src.parse()?;

                parse_symbol!(
                    src,
                    Token::ParenOpen,
                    "Expected arguments ('('), but instead got {:?}."
                );
                let args = parse_group!(src, Token::ParenClose);

                parse_symbol!(
                    src,
                    Token::BraceOpen,
                    "Expected an opening brace ('{{') for function body, but instead got {:?}."
                );
                let body = parse_group!(src, Token::BraceClose);

                Ok(Chunk {
                    span: Span::new(next.span.start, src.get_pos()),
                    data: Self::Function {
                        name,
                        args,
                        body,
                        instructions: Vec::new(),
                    },
                })
            }
            Token::KeywordUse => {
                let data = Self::Use(src.parse()?);
                parse_symbol!(
                    src,
                    Token::Terminator,
                    "Expected a semicolon to end use statement, but instead got {:?}."
                );

                Ok(Chunk {
                    span: Span::new(next.span.start, src.get_pos()),
                    data,
                })
            }
            Token::OpInstruction => {
                parse_symbol!(
                    src,
                    Token::BracketOpen,
                    "Expected the start of an instruction ('['), but got {:?} instead."
                );

                let mut inst = String::new();
                loop {
                    let char = src.next_raw()?;
                    if char == ']' {
                        break;
                    } else {
                        inst.push(char);
                    }
                }
                let inst = Chunk {
                    span: Span::new(next.span.start, src.get_pos()),
                    data: inst,
                };

                let mut next = src.parse()?;
                match &mut next.data {
                    ModuleBody::Function { instructions, .. } => instructions.push(inst),
                    ModuleBody::Module { instructions, .. } => instructions.push(inst),
                    _ => {
                        return Err(next
                            .span
                            .error(format!("Cannot apply an instruction to {:?}.", next.data)))
                    }
                }
                Ok(next)
            }
            _ => Err(next.span.error(format!(
                "Unexpected token {:?} in module body. Only resources, submodules, and imports can be defined inside a module.", 
                next.data
            ))),
        }
    }
}

#[derive(Debug)]
pub struct FnArg {
    name: Chunk<String>,
    data_type: Chunk<String>,
    default: Option<Chunk<Expression>>,
}

impl Parsable for FnArg {
    fn parse_from(src: &mut TokenStream) -> Result<Chunk<FnArg>, SourceError> {
        let name = src.parse()?;

        parse_symbol!(
            src,
            Token::OpColon,
            "Arguments must have defined type, and so expected ':', but instead got {:?}."
        );
        let data_type = src.parse()?;

        let default = if let Some(Token::OpAssign) = src.peek_token() {
            src.next();
            Some(src.parse()?)
        } else {
            None
        };

        Ok(Chunk {
            span: Span::new(name.span.start, src.get_pos()),
            data: Self {
                name,
                data_type,
                default,
            },
        })
    }
}

#[derive(Debug)]
pub enum AssignmentOperator {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
}

impl Parsable for AssignmentOperator {
    fn parse_from(src: &mut TokenStream) -> Result<Chunk<Self>, SourceError> {
        let token = src
            .expect_next("Trying to match assignment operator, but reached the end of the file.")?;
        match token.data {
            Token::OpAssign => Ok(token.span.chunk(Self::Assign)),
            Token::OpAddAssign => Ok(token.span.chunk(Self::AddAssign)),
            Token::OpSubtractAssign => Ok(token.span.chunk(Self::SubAssign)),
            Token::OpMultiplyAssign => Ok(token.span.chunk(Self::MulAssign)),
            Token::OpDivideAssign => Ok(token.span.chunk(Self::DivAssign)),
            t => Err(token.span.error(format!(
                "Expected assignment operator, but instead got {:?}.",
                t
            ))),
        }
    }
}

#[derive(Debug)]
pub enum Statement {
    VariableDefinition {
        name: Chunk<String>,
        value: Chunk<Expression>,
    },
    Assignment {
        var: Chunk<Expression>,
        value: Chunk<Expression>,
        op: Chunk<AssignmentOperator>,
    },
    OrphanedExpr(Expression),
}

impl Parsable for Statement {
    fn parse_from(src: &mut TokenStream) -> Result<Chunk<Statement>, SourceError> {
        let c = match src.peek_token() {
            Some(Token::KeywordLet) => {
                let next = src.expect_next(
                    "Trying to match variable definition, but reached the end of the file.",
                )?;
                let name = src.parse()?;
                parse_symbol!(
                    src,
                    Token::OpAssign,
                    "Expected assignment operator for variable definition, instead got {:?}"
                );
                let value = src.parse()?;

                Ok(Chunk {
                    span: Span::new(next.span.start, src.get_pos()),
                    data: Self::VariableDefinition { name, value },
                })
            }

            Some(t) if CONDITIONS.contains(t) => todo!(),

            Some(_) => {
                let expr = src.parse()?;

                match src.peek_token() {
                    Some(
                        Token::OpAssign
                        | Token::OpAddAssign
                        | Token::OpSubtractAssign
                        | Token::OpMultiplyAssign
                        | Token::OpDivideAssign,
                    ) => {
                        let op = src.parse()?;
                        let value = src.parse()?;
                        Ok(Chunk {
                            span: Span::new(expr.span.start, value.span.end),
                            data: Self::Assignment {
                                var: expr,
                                value,
                                op,
                            },
                        })
                    }
                    _ => Ok(expr.map(|x| Self::OrphanedExpr(x))),
                }
            }

            None => Err(SourceError {
                span: Span::new(src.get_pos(), src.get_pos()),
                message: "Unexpected end of file.".to_string(),
            }),
        }?;

        parse_symbol!(
            src,
            Token::Terminator,
            "Expected a semicolon (terminator) to end a statement, but got {:?} instead."
        );
        Ok(c)
    }
}

#[derive(Debug)]
pub enum Expression {
    ResourcePath(Chunk<String>, Chunk<String>),
    ConstantInt(isize),
    ConstantFloat(f64),
    ConstantString(String),
    ConstantBoolean(bool),

    Selector {
        sel_type: char,
        criteria: String,
    },

    Variable(String),
    Access(Chunk<Box<Expression>>, Chunk<Box<Expression>>),
    FnCall(Chunk<String>, Chunk<Box<FnArgs>>),

    Conditional {
        conditions: Vec<Chunk<Condition>>,
        block: Vec<Chunk<Statement>>,
        else_block: Vec<Chunk<Statement>>,
    },

    RelativePos(f64, f64, f64),
    DirectionalPos(f64, f64, f64),
    StaticPos(f64, f64, f64),

    Sum(Chunk<Box<Expression>>, Chunk<Box<Expression>>),
    Difference(Chunk<Box<Expression>>, Chunk<Box<Expression>>),
    Product(Chunk<Box<Expression>>, Chunk<Box<Expression>>),
    Quotient(Chunk<Box<Expression>>, Chunk<Box<Expression>>),
    Remainder(Chunk<Box<Expression>>, Chunk<Box<Expression>>),

    IsEq(Chunk<Box<Expression>>, Chunk<Box<Expression>>),
    IsLt(Chunk<Box<Expression>>, Chunk<Box<Expression>>),
    IsGt(Chunk<Box<Expression>>, Chunk<Box<Expression>>),
}

fn parse_static_pos(
    src: &mut TokenStream,
    x: f64,
    next: Chunk<Token>,
) -> Result<Chunk<Expression>, SourceError> {
    let yt = src.expect_next(
        "Trying to match Y component of static vector, but reached the end of the file.",
    )?;
    let y = match yt.data {
        Token::Integer(n) => n as f64,
        Token::Float(n) => n,
        _ => {
            return Err(next.span.error(format!(
                "Expected second Y component of relative vector, instead got {:?}.",
                next.data
            )))
        }
    };
    let zt = src.expect_next(
        "Trying to match Z component of relative vector, but reached the end of the file.",
    )?;
    let z = match zt.data {
        Token::Integer(n) => n as f64,
        Token::Float(n) => n,
        _ => {
            return Err(next.span.error(format!(
                "Expected third Z component of relative vector, instead got {:?}.",
                next.data
            )));
        }
    };

    Ok(Chunk {
        data: Expression::StaticPos(x, y, z),
        span: Span::new(next.span.start, zt.span.end),
    })
}

impl Parsable for Expression {
    fn parse_from(src: &mut TokenStream) -> Result<Chunk<Expression>, SourceError> {
        let next =
            src.expect_next("Trying to match expression, but reached the end of the file.")?;
        let lhs = match next.data {
            Token::Integer(i) => {
                if let Some(Token::Integer(_) | Token::Float(_)) = src.peek_token() {
                    parse_static_pos(src, i as f64, next)
                } else {
                    Ok(next.span.chunk(Self::ConstantInt(i)))
                }
            }
            Token::Float(f) => {
                if let Some(Token::Integer(_) | Token::Float(_)) = src.peek_token() {
                    parse_static_pos(src, f, next)
                } else {
                    Ok(next.span.chunk(Self::ConstantFloat(f)))
                }
            }
            Token::String(s) => Ok(next.span.chunk(Self::ConstantString(s))),

            Token::True => Ok(next.span.chunk(Self::ConstantBoolean(true))),
            Token::False => Ok(next.span.chunk(Self::ConstantBoolean(false))),

            Token::Selector(sel_type) => {
                let mut criteria = String::new();

                if let Some(Token::BracketOpen) = src.peek_token() {
                    src.next();
                    loop {
                        let next_char = src.next_raw()?;
                        if next_char == ']' {
                            break;
                        } else {
                            criteria.push(next_char)
                        }
                    }
                }

                Ok(Chunk {
                    span: Span::new(next.span.start, src.get_pos()),
                    data: Self::Selector { sel_type, criteria },
                })
            }

            Token::Ident(name) => {
                let name = next.span.chunk(name);
                match src.peek_token() {
                    Some(Token::OpColon) => {
                        src.next();
                        let data = Self::ResourcePath(name, src.parse()?);
                        Ok(Chunk {
                            span: Span::new(next.span.start, src.get_pos()),
                            data,
                        })
                    }
                    Some(Token::ParenOpen) => Ok(Chunk {
                        data: Self::FnCall(name, src.parse()?.map(Box::new)),
                        span: Span::new(next.span.start, src.get_pos()),
                    }),
                    _ => Ok(name.span.chunk(Self::Variable(name.data))),
                }
            }

            Token::OpColon => Ok(Chunk {
                data: Self::ResourcePath(
                    Chunk {
                        data: "minecraft".to_string(),
                        span: next.span,
                    },
                    src.parse()?,
                ),
                span: Span::new(next.span.start, src.get_pos()),
            }),

            Token::PosRelative(x) => {
                let yt = src.expect_next("Trying to match Y component of relative vector, but reached the end of the file.")?;
                let Token::PosRelative(y) = yt.data else {
                    return Err(next.span.error(format!(
                        "Expected second Y component of relative vector, instead got {:?}.",
                        next.data
                    )));
                };
                let zt = src.expect_next("Trying to match Z component of relative vector, but reached the end of the file.")?;
                let Token::PosRelative(z) = zt.data else {
                    return Err(next.span.error(format!(
                        "Expected third Z component of relative vector, instead got {:?}.",
                        next.data
                    )));
                };

                Ok(Chunk {
                    data: Self::RelativePos(x, y, z),
                    span: Span::new(next.span.start, zt.span.end),
                })
            }

            Token::PosDirectional(x) => {
                let yt = src.expect_next("Trying to match Y component of directional vector, but reached the end of the file.")?;
                let Token::PosDirectional(y) = yt.data else {
                    return Err(next.span.error(format!(
                        "Expected second Y component of directional vector, instead got {:?}.",
                        next.data
                    )));
                };
                let zt = src.expect_next("Trying to match Z component of directional vector, but reached the end of the file.")?;
                let Token::PosDirectional(z) = zt.data else {
                    return Err(next.span.error(format!(
                        "Expected third Z component of directional vector, instead got {:?}.",
                        next.data
                    )));
                };

                Ok(Chunk {
                    data: Self::DirectionalPos(x, y, z),
                    span: Span::new(next.span.start, zt.span.end),
                })
            }

            token => Err(SourceError {
                span: next.span,
                message: format!("Expected expression, instead got {:?}.", token),
            }),
        }?;

        match src.peek_token() {
            Some(Token::OpAdd) => {
                src.next();
                let rhs = src.parse()?;
                Ok(Chunk {
                    span: Span::new(lhs.span.start, rhs.span.end),
                    data: Self::Sum(lhs.map(Box::new), rhs.map(Box::new)),
                })
            }
            Some(Token::OpSubtract) => {
                src.next();
                let rhs = src.parse()?;
                Ok(Chunk {
                    span: Span::new(lhs.span.start, rhs.span.end),
                    data: Self::Difference(lhs.map(Box::new), rhs.map(Box::new)),
                })
            }
            Some(Token::OpMultiply) => {
                src.next();
                let rhs = src.parse()?;
                Ok(Chunk {
                    span: Span::new(lhs.span.start, rhs.span.end),
                    data: Self::Product(lhs.map(Box::new), rhs.map(Box::new)),
                })
            }
            Some(Token::OpDivide) => {
                src.next();
                let rhs = src.parse()?;
                Ok(Chunk {
                    span: Span::new(lhs.span.start, rhs.span.end),
                    data: Self::Quotient(lhs.map(Box::new), rhs.map(Box::new)),
                })
            }
            Some(Token::OpModulus) => {
                src.next();
                let rhs = src.parse()?;
                Ok(Chunk {
                    span: Span::new(lhs.span.start, rhs.span.end),
                    data: Self::Remainder(lhs.map(Box::new), rhs.map(Box::new)),
                })
            }

            Some(Token::OpAccess) => {
                src.next();
                let rhs = src.parse()?;

                fn transform(
                    a: Chunk<Box<Expression>>,
                    rhs: Chunk<Box<Expression>>,
                ) -> Chunk<Expression> {
                    match *rhs.data {
                        Expression::Access(b, c) => Chunk {
                            span: Span::new(a.span.start, rhs.span.end),
                            data: Expression::Access(transform(a, b).map(Box::new), c),
                        },
                        data => Chunk {
                            span: Span::new(a.span.start, rhs.span.end),
                            data: Expression::Access(a, rhs.span.chunk(Box::new(data))),
                        },
                    }
                }

                Ok(transform(lhs.map(Box::new), rhs.map(Box::new)))
            }

            _ => Ok(lhs),
        }
    }
}

#[derive(Debug)]
pub enum Condition {
    If(Expression),
    As(Expression),
    At(Expression),
}

#[derive(Debug)]
pub struct FnArgs {
    pos_args: Vec<Chunk<Expression>>,
    named_args: HashMap<String, Chunk<Expression>>,
}

impl Parsable for FnArgs {
    fn parse_from(src: &mut TokenStream) -> Result<Chunk<FnArgs>, SourceError> {
        let start = src.get_pos();
        src.next(); // Skip open parenthisis.

        let mut named_args = HashMap::new();
        let mut pos_args = Vec::new();
        let mut pos_done = false;

        while !src.is_done() {
            if let Some(Token::ParenClose) = src.peek_token() {
                break;
            }

            let arg = src.parse()?;
            match arg.data {
                Arg::Named(name, value) => {
                    pos_done = true;

                    // make error message first, so its not moved.
                    let message = format!("Argument '{}' already defined.", name.data);
                    if let Some(_) = named_args.insert(name.data, value) {
                        return Err(SourceError {
                            span: arg.span,
                            message,
                        });
                    }
                }
                Arg::Positional(value) => {
                    if pos_done {
                        return Err(SourceError {
                            span: arg.span,
                            message: "Positional arguments cannot come after named ones."
                                .to_string(),
                        });
                    }
                    pos_args.push(arg.span.chunk(value))
                }
            }
        }

        let paren_end = src.expect_next("Trying to match closing parenthisis for arguments (')'), but reached the end of the file.")?;

        Ok(Chunk {
            span: Span::new(start, paren_end.span.end),
            data: Self {
                pos_args,
                named_args,
            },
        })
    }
}

#[derive(Debug)]
pub enum Arg {
    Named(Chunk<String>, Chunk<Expression>),
    Positional(Expression),
}

impl Parsable for Arg {
    fn parse_from(src: &mut TokenStream) -> Result<Chunk<Arg>, SourceError> {
        let lhs = src.parse()?;

        if let Some(Token::OpAssign) = src.peek_token() {
            todo!()
        } else {
            Ok(lhs.map(|x| Self::Positional(x)))
        }
    }
}
