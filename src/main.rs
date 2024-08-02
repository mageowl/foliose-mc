use std::fs;

use clap::{command, Command};
use error::Chunk;
use lexer::TokenStream;
use parser::Program;

mod error;
mod lexer;
mod parser;

fn main() {
    let cmd = get_command();
    let matches = cmd.get_matches();

    match matches.subcommand() {
        Some(("test", subcmd)) => {
            let source_path: &str = subcmd.get_one::<String>("file").unwrap();
            let source = fs::read_to_string(source_path).expect("Failed to open file");

            let mut source = TokenStream::from(source.as_str());
            let tree: Chunk<Program> = source.parse().unwrap();
            dbg!(tree);
        }
        _ => todo!(),
    }
}

#[cfg(debug_assertions)]
fn get_command() -> Command {
    use clap::Arg;

    command!()
        .subcommand(Command::new("init").about("Setup new datapack with Foliose"))
        .subcommand(
            Command::new("build")
                .about("Build project in current directory")
                .alias("b"),
        )
        .subcommand(
            Command::new("test")
                .about("Compile single .fol file.")
                .arg(Arg::new("file")),
        )
        .arg_required_else_help(true)
}

#[cfg(not(debug_assertions))]
fn get_command() -> Command {
    command!()
        .subcommand(Command::new("init").about("Setup new datapack with Foliose"))
        .subcommand(
            Command::new("build")
                .about("Build project in current directory")
                .alias("b"),
        )
        .arg_required_else_help(true)
}
