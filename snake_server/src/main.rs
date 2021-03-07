use async_tungstenite::tungstenite::Message;
use env_logger::Env;
use futures::{sink::SinkExt, stream::StreamExt};
use log::{info, warn};
use serde::{Deserialize, Serialize};
use smol::Async;
use snake_common::{ClientMessage, ServerMessage};
use std::fs::File;
use std::io::{Read, Write};
use std::net::{SocketAddr, TcpListener};

#[derive(Debug, Serialize, Deserialize)]
struct HighScoreEntry {
    name: String,
    score: u32,
}

impl HighScoreEntry {
    fn new(name: String, score: u32) -> Self {
        Self { name, score }
    }
}

fn read_highscore_list() -> Vec<HighScoreEntry> {
    match File::open("highscore_list") {
        Ok(mut file) => {
            let mut buffer = Vec::<u8>::new();
            file.read_to_end(&mut buffer).unwrap();
            bincode::deserialize(&buffer[..]).unwrap_or(vec![])
        }
        Err(_) => {
            warn!("File doesn't exist yet, creating it...");
            File::create("highscore_list").expect("Could not create file");
            vec![]
        }
    }
}

fn write_highscore_list(data: &Vec<HighScoreEntry>) {
    let mut file = File::create("highscore_list").expect("could not open file");
    let bytes = bincode::serialize(&data).expect("Could not serialize data");
    file.write_all(&bytes).expect("Could not write file");
}

pub fn main() {
    env_logger::from_env(Env::default().default_filter_or("snake_server=INFO")).init();
    let addr = "127.0.0.1:8090";

    let mut highscore_vec: Vec<HighScoreEntry> = read_highscore_list();
    println!("{:?}", highscore_vec);

    smol::block_on(async {
        info!("Listening on: {}", addr);

        let socket_addr: SocketAddr = addr.parse().expect("Unable to parse socket address");
        let listener = Async::<TcpListener>::bind(socket_addr).expect("Could not create listener");

        while let Ok((stream, addr)) = listener.accept().await {
            info!("Got connection from {}", addr);
            let mut ws_stream = async_tungstenite::accept_async(stream)
                .await
                .expect("Oh, could not get message");
            while let Some(Ok(Message::Binary(t))) = ws_stream.next().await {
                let msg =
                    bincode::deserialize::<ClientMessage>(&t).expect("Could not deserialize msg");
                match msg {
                    ClientMessage::RequestHighscore(score) => {
                        info!("Requesting Highscore: {}", score);
                        // return entries
                        let resp = ServerMessage::Highscore {
                            others: {
                                let slice = if highscore_vec.len() < 10 {
                                    &highscore_vec
                                }
                                else {
                                    &highscore_vec[highscore_vec.len() - 10..]
                                };
                                slice.iter().rev().map(|el| (el.name.clone(), el.score)).collect()
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
                            }
                        };
                        let encoded = bincode::serialize(&resp)
                            .expect("Could not serialize Highscore response");
                        ws_stream
                            .send(Message::Binary(encoded))
                            .await
                            .expect("Highscore could not be delivered");
                    }
                    ClientMessage::SubmitName(name, score) => {
                        info!("Submitting Name: {} with score: {}", name, score);
                        highscore_vec.push(HighScoreEntry::new(name, score));
                        highscore_vec.sort_by_key(|el| el.score);
                        write_highscore_list(&highscore_vec);
                    }
                }
            }
        }
    });
}
