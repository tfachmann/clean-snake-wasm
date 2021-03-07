use serde::{Serialize, Deserialize};

type Score = u32;
type Position = u32;
type Name = String;

#[derive(Debug, Deserialize, Serialize)]
pub enum ClientMessage {
    RequestHighscore(Score),
    SubmitName(Name, Score),
}

#[derive(Debug, Deserialize, Serialize)]
pub enum ServerMessage {
    Highscore {
        others: Vec<(Name, Score)>,
        you: (Position, Score),
    }
}
