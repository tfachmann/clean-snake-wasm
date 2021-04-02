use serde::{Deserialize, Serialize};
use std::time::Duration;

type Score = u32;
type Position = u32;
type Name = String;

#[derive(Debug, Deserialize, Serialize)]
pub enum ClientMessage {
    RequestHighscore(Score),
    SubmitEntry {
        name: String,
        score: u32,
        elapsed_time: Duration,
        snake_length: u32,
        changed_directions: u32,
        passed_through_walls: u32,
    },
}

impl ClientMessage {
    pub fn class_name(&self) -> &str {
        match self {
            ClientMessage::RequestHighscore(_) => "RequestHighscore",
            ClientMessage::SubmitEntry {
                name: _,
                score: _,
                elapsed_time: _,
                snake_length: _,
                changed_directions: _,
                passed_through_walls: _,
            } => "SubmitEntry",
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub enum ServerMessage {
    Highscore {
        others: Vec<(Name, Score)>,
        you: (Position, Score),
    },
}

impl ServerMessage {
    pub fn class_name(&self) -> &str {
        match self {
            ServerMessage::Highscore { others: _, you: _ } => "Highscore",
        }
    }
}
