use super::OPERATORS;

pub const CONDITIONS: [Token; 3] = [Token::ConditionAs, Token::ConditionIf, Token::ConditionAt];

#[derive(Debug, PartialEq)]
pub enum Token {
    KeywordModule,
    KeywordFn,
    KeywordLet,
    KeywordUse,
    KeywordDo,
    KeywordElse,

    ConditionAt,
    ConditionAs,
    ConditionIf,

    BraceOpen,
    BraceClose,
    ParenOpen,
    ParenClose,
    BracketOpen,
    BracketClose,

    OpAssign,
    OpAddAssign,
    OpAdd,
    OpSubtractAssign,
    OpSubtract,
    OpMultiplyAssign,
    OpMultiply,
    OpDivisionAssign,
    OpDivision,
    OpModulus,
    OpLessThan,
    OpLessThanEq,
    OpGreaterThan,
    OpGreaterThanEq,
    OpColon,
    OpAccess,
    OpInstruction,

    PosRelative(f64),
    PosDirectional(f64),

    Ident(String),
    String(String),
    Integer(isize),
    Float(f64),
    Selector(char),

    True,
    False,

    Terminator,
    ArgSeperator,
}

impl TryFrom<String> for Token {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        match value.as_str() {
            "module" => Ok(Token::KeywordModule),
            "fn" => Ok(Token::KeywordFn),
            "let" => Ok(Token::KeywordLet),
            "use" => Ok(Token::KeywordUse),
            "do" => Ok(Token::KeywordDo),
            "else" => Ok(Token::KeywordElse),

            "at" => Ok(Token::ConditionAt),
            "as" => Ok(Token::ConditionAs),
            "if" => Ok(Token::ConditionIf),

            "{" => Ok(Token::BraceOpen),
            "}" => Ok(Token::BraceClose),
            "(" => Ok(Token::ParenOpen),
            ")" => Ok(Token::ParenClose),
            "[" => Ok(Token::BracketOpen),
            "]" => Ok(Token::BracketClose),

            "=" => Ok(Token::OpAssign),
            "+=" => Ok(Token::OpAddAssign),
            "+" => Ok(Token::OpAdd),
            "-=" => Ok(Token::OpSubtractAssign),
            "-" => Ok(Token::OpSubtract),
            "*=" => Ok(Token::OpMultiplyAssign),
            "*" => Ok(Token::OpMultiply),
            "/=" => Ok(Token::OpDivisionAssign),
            "/" => Ok(Token::OpDivision),
            "%" => Ok(Token::OpModulus),
            "<" => Ok(Token::OpLessThan),
            "<=" => Ok(Token::OpLessThanEq),
            ">" => Ok(Token::OpGreaterThan),
            ">=" => Ok(Token::OpGreaterThanEq),
            ":" => Ok(Token::OpColon),
            "." => Ok(Token::OpAccess),
            "#" => Ok(Token::OpInstruction),

            ";" => Ok(Token::Terminator),
            "," => Ok(Token::ArgSeperator),


            op if op.chars().all(|c| OPERATORS.contains(&c)) => {
                Err(format!("Unexpected symbol '{}'.", value))
            }

            num if num.chars().next().is_some_and(|c| c.is_ascii_digit()) => {
                let value = value.replace("_", "");
                if value.contains(".") {
                    Ok(Self::Float(value.parse().map_err(|_| {
                        format!(
                            "Invalid float '{num}'. Identifiers can't start with numbers.",
                            num = value
                        )
                    })?))
                } else {
                    Ok(Self::Integer(value.parse().map_err(|_| {
                        format!(
                            "Invalid number '{num}'. Identifiers can't start with numbers.",
                            num = value
                        )
                    })?))
                }
            }

            "~" => Ok(Self::PosRelative(0.)),
            pos if pos.chars().next().unwrap() == '~' => {
                Ok(Self::PosRelative(value.strip_prefix("~").unwrap().parse().map_err(|_| {
                    format!(
                        "Invalid relative coordinate '{num}'.",
                        num = value
                    )
                })?))
            }

            "^" => Ok(Self::PosDirectional(0.)),
            pos if pos.chars().next().unwrap() == '^' => {
                Ok(Self::PosDirectional(value.strip_prefix("^").unwrap().parse().map_err(|_| {
                    format!(
                        "Invalid directional coordinate '{num}'.",
                        num = value
                    )
                })?))
            },

            "true" => Ok(Self::True),
            "false" => Ok(Self::False),

            ident
                if ident
                    .chars()
                    .all(|c| matches!(c, 'A'..='Z' | 'a'..='z' | '0'..='9' | '_')) =>
            {
                Ok(Self::Ident(value))
            }
            _ => {
                Err(format!("Invalid identifier '{ident}'. Identifiers must only contain A-Z, a-z, 0-9, and underscores.", ident=value))
            }
        }
    }
}
