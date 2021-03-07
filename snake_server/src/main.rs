use async_tungstenite::{tungstenite::Message, WebSocketStream};
use env_logger::Env;
use futures::{sink::SinkExt, stream::StreamExt};
use log::{debug, error, info, warn};
use serde::{Deserialize, Serialize};
use smol::Async;
use snake_common::{ClientMessage, ServerMessage};
use std::fs::File;
use std::io::{Read, Write};
use std::net::{SocketAddr, TcpListener, TcpStream};
use thiserror::Error;

#[derive(Debug, Serialize, Deserialize)]
struct HighScoreEntry {
    name: String,
    score: u32,
}

impl HighScoreEntry {
    fn new(name: &str, score: u32) -> Self {
        Self {
            name: name.to_string(),
            score,
        }
    }
}

fn read_highscore_list() -> Vec<HighScoreEntry> {
    match File::open("highscore_list") {
        Ok(mut file) => {
            let mut buffer = Vec::<u8>::new();
            match file.read_to_end(&mut buffer) {
                Ok(_) => match bincode::deserialize(&buffer[..]) {
                    Ok(data) => data,
                    Err(_) => {
                        error!("Could not deserialize, creating new data");
                        vec![]
                    }
                },
                Err(e) => {
                    error!("Could not read file content ({}), creating new data", e);
                    vec![]
                }
            }
        }
        Err(_) => {
            warn!("File doesn't exist yet, will create it at the next write...");
            vec![]
        }
    }
}

#[derive(Error, Debug)]
pub enum WriteError {
    /// Represents a failure to read from input.
    #[error("Serialization error")]
    IFSError(#[from] std::boxed::Box<bincode::ErrorKind>),

    /// Represents all other cases of `std::io::Error`.
    #[error(transparent)]
    IOError(#[from] std::io::Error),
}

fn write_highscore_list(data: &Vec<HighScoreEntry>) -> Result<(), WriteError> {
    let mut file = File::create("highscore_list")?;
    let bytes = bincode::serialize(&data)?;
    file.write_all(&bytes)?;
    Ok(())
}

async fn request_highscore(
    score: u32,
    stream: &mut WebSocketStream<Async<TcpStream>>,
    highscore_vec: &mut Vec<HighScoreEntry>,
) {
    // return entries
    let resp = ServerMessage::Highscore {
        others: {
            let slice = if highscore_vec.len() < 10 {
                &highscore_vec
            } else {
                &highscore_vec[highscore_vec.len() - 10..]
            };
            slice
                .iter()
                .rev()
                .map(|el| (el.name.clone(), el.score))
                .collect()
        },
        you: {
            let mut position = highscore_vec.len();
            for (idx, el) in highscore_vec.iter().rev().enumerate() {
                if score > el.score {
                    position = idx;
                    break;
                }
            }
            (position as u32, score)
        },
    };
    match bincode::serialize(&resp) {
        Err(e) => error!(
            "Could not serialize Highscore response {}, no message sent",
            e
        ),
        Ok(encoded) => {
            debug!("Sucessfully encoded message");
            match stream.send(Message::Binary(encoded)).await {
                Ok(_) => debug!("Sucessfully sent response message"),
                Err(e) => error!("Could not send response: {}", e),
            }
        }
    }
}

async fn submit_entry(name: String, score: u32, highscore_vec: &mut Vec<HighScoreEntry>) {
    info!("Submitting Name: {} with score: {}", name, score);
    highscore_vec.push(HighScoreEntry::new(&name, score));
    highscore_vec.sort_by_key(|el| el.score);
    match write_highscore_list(&highscore_vec) {
        Ok(_) => info!("Sucessfully submitted {}", &name),
        Err(e) => error!("Could not write file: {}", e),
    }
}

async fn read_stream(
    mut stream: WebSocketStream<Async<TcpStream>>,
    highscore_vec: &mut Vec<HighScoreEntry>,
) {
    while let Some(Ok(Message::Binary(t))) = stream.next().await {
        match bincode::deserialize::<ClientMessage>(&t) {
            Err(e) => error!("Could not Deserialize msg: {}", e),
            Ok(msg) => {
                info!(
                    "Received and deserialized msg of type `{}`",
                    msg.class_name()
                );
                match msg {
                    ClientMessage::RequestHighscore(score) => {
                        request_highscore(score, &mut stream, highscore_vec).await
                    }
                    ClientMessage::SubmitName(name, score) => {
                        submit_entry(name, score, highscore_vec).await
                    }
                }
            }
        }
    }
}

pub fn main() {
    env_logger::from_env(Env::default().default_filter_or("snake_server=INFO")).init();
    let addr = "127.0.0.1:8090";

    let mut highscore_vec: Vec<HighScoreEntry> = read_highscore_list();

    smol::block_on(async {
        info!("Listening on: {}", addr);

        let socket_addr: SocketAddr = addr.parse().expect("Unable to parse socket address");
        let listener = Async::<TcpListener>::bind(socket_addr).expect("Could not create listener");

        while let Ok((stream, addr)) = listener.accept().await {
            info!("Got connection from {}", addr);
            match async_tungstenite::accept_async(stream).await {
                Err(e) => {
                    error!("Could not get stream: {}", e);
                }
                Ok(ws_stream) => {
                    info!("Reading incomming stream...");
                    read_stream(ws_stream, &mut highscore_vec).await;
                }
            }
        }
    });
}
