use serde::{Serialize, Deserialize};

type Score = u32;
type Position = u32;
type Name = String;

#[derive(Debug, Deserialize, Serialize)]
pub enum ClientMessage {
    RequestHighscore(Score),
    SubmitName(Name, Score),
}

impl ClientMessage {
    pub fn class_name(&self) -> &str {
        match self {
            ClientMessage::RequestHighscore(_) => "RequestHighscore",
            ClientMessage::SubmitName(_, _) => "SubmitName",
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub enum ServerMessage {
    Highscore {
        others: Vec<(Name, Score)>,
        you: (Position, Score),
    }
}

impl ServerMessage {
    pub fn class_name(&self) -> &str {
        match self {
            ServerMessage::Highscore { others: _, you: _ } => { "Highscore" }
        }
    }
}
