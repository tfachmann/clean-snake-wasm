use lazy_static;
use wasm_bindgen::convert::FromWasmAbi;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;

use rand::{thread_rng, Rng};
use snake_common::{ClientMessage, ServerMessage};
use std::collections::VecDeque;
use std::rc::Rc;
use std::sync::Mutex;
use web_sys::{
    Blob, Document, Element, EventTarget, FileReader, HtmlButtonElement, HtmlInputElement, InputEvent, KeyboardEvent,
    MessageEvent, ProgressEvent, Text, TouchEvent, WebSocket, Window,
};

type JsResult<T> = Result<T, JsValue>;
type JsError = Result<(), JsValue>;
type JsClosure<T> = Closure<dyn FnMut(T) -> JsError>;
type Pos = (f32, f32);
type GridSize = (usize, usize);
type GridPoint = (usize, usize);

macro_rules! console_log {
    ($($t:tt)*) => (web_sys::console::log_1(&format!($($t)*).into()))
}

trait DocExt {
    fn create_svg_element(&self, t: &str) -> JsResult<Element>;
}

impl DocExt for Document {
    fn create_svg_element(&self, t: &str) -> JsResult<Element> {
        self.create_element_ns(Some("http://www.w3.org/2000/svg"), t)
    }
}

#[derive(PartialEq)]
enum Direction {
    Left,
    Down,
    Up,
    Right,
}

struct Snake {
    length: usize,
    direction: Direction,
    pos: GridPoint,
    grid_size: GridSize,
    lock: bool,

    continuous_borders: bool,
}

impl Snake {
    fn new(grid_size: GridSize, start_pos: GridPoint) -> Self {
        Self {
            length: 9,
            direction: Direction::Right,
            pos: start_pos,
            grid_size,
            continuous_borders: true,
            lock: false,
        }
    }

    fn set_direction(&mut self, direction: Direction) {
        if !self.lock {
            match self.direction {
                Direction::Left => {
                    if direction != Direction::Right {
                        self.direction = direction
                    }
                }
                Direction::Down => {
                    if direction != Direction::Up {
                        self.direction = direction
                    }
                }
                Direction::Up => {
                    if direction != Direction::Down {
                        self.direction = direction
                    }
                }
                Direction::Right => {
                    if direction != Direction::Left {
                        self.direction = direction
                    }
                }
            }
            self.lock = true;
        }
    }

    fn do_move(&mut self) -> GridPoint {
        let mut pos_new = match self.direction {
            Direction::Left => (
                self.pos.0.checked_sub(1).unwrap_or(self.grid_size.0 - 1),
                self.pos.1,
            ),
            Direction::Down => (self.pos.0, self.pos.1 + 1),
            Direction::Up => (
                self.pos.0,
                self.pos.1.checked_sub(1).unwrap_or(self.grid_size.1 - 1),
            ),
            Direction::Right => (self.pos.0 + 1, self.pos.1),
        };
        if self.continuous_borders {
            if pos_new.0 == self.grid_size.0 {
                pos_new.0 = 0;
            }
            if pos_new.1 == self.grid_size.1 {
                pos_new.1 = 0;
            }
        }
        self.pos = pos_new;
        self.lock = false;
        pos_new
    }

    fn do_eat(&mut self) {
        self.length += 1;
    }
}

#[derive(Clone, PartialEq)]
enum GridField {
    SnakeHead,
    Snake,
    Food,
    Empty,
}

struct Grid {
    field: Vec<Vec<GridField>>,
    snake: Snake,
    snake_stack: VecDeque<GridPoint>,

    score: usize,
    move_count: usize,
    perfect_score: usize,
}

impl Grid {
    fn new(grid_size: GridSize) -> Grid {
        let field = vec![vec![GridField::Empty; grid_size.1]; grid_size.0];
        let mut snake_stack = VecDeque::with_capacity(grid_size.0 * grid_size.1);
        snake_stack.push_front((6, 10));
        snake_stack.push_front((7, 10));
        snake_stack.push_front((8, 10));
        snake_stack.push_front((9, 10));
        snake_stack.push_front((10, 10));
        snake_stack.push_front((11, 10));
        snake_stack.push_front((12, 10));
        snake_stack.push_front((13, 10));
        snake_stack.push_front((14, 10));
        Grid {
            field,
            snake: Snake::new(grid_size, (8, 10)),
            snake_stack,
            score: 0,
            move_count: 0,
            perfect_score: 0,
        }
    }

    fn do_move(&mut self) -> Result<(), String> {
        let (x_prev, y_prev) = self.snake.pos;
        self.field[x_prev][y_prev] = GridField::Snake;
        let (x, y) = self.snake.do_move();
        match self.field[x][y] {
            GridField::Snake => {
                if let Some((x_end, y_end)) = self.snake_stack.pop_back() {
                    if x != x_end || y != y_end {
                        Err("GameOver".to_string())
                    } else {
                        self.field[x_end][y_end] = GridField::Empty;
                        self.snake_stack.push_front((x, y));
                        self.field[x][y] = GridField::SnakeHead;
                        self.move_count += 1;
                        Ok(())
                    }
                } else {
                    Err("GameOver".to_string())
                }
            }
            GridField::Food => {
                // check for perfect score
                console_log!(
                    "perfect: {}; actual: {}",
                    self.perfect_score,
                    self.move_count
                );
                // you don't have to be "PERFECT" to get perfect_score
                self.score += if (self.move_count + 1) <= (self.perfect_score + 2) {
                    3
                } else {
                    1
                };
                self.snake.do_eat();
                self.snake_stack.push_front((x, y));
                self.field[x][y] = GridField::SnakeHead;
                let (x_food, y_food) = self.spawn_food();
                // calculate perfect score
                let min = |a, b| -> usize {
                    if a < b {
                        a
                    } else {
                        b
                    }
                };

                self.perfect_score = {
                    let x_diff = x_food.checked_sub(x).unwrap_or(x - x_food);
                    let y_diff = y_food.checked_sub(y).unwrap_or(y - y_food);
                    min(x_diff, self.field.len() - x_diff)
                        + min(y_diff, self.field[0].len() - y_diff)
                };
                self.move_count = 0;
                Ok(())
            }
            GridField::Empty => {
                if let Some((x, y)) = self.snake_stack.pop_back() {
                    self.field[x][y] = GridField::Empty;
                }
                self.snake_stack.push_front((x, y));
                self.field[x][y] = GridField::SnakeHead;
                self.move_count += 1;
                Ok(())
            }
            GridField::SnakeHead => Ok(()),
        }
    }

    fn spawn_food(&mut self) -> (usize, usize) {
        let mut rng = thread_rng();
        let mut x = rng.gen_range(0..self.field.len());
        let mut y = rng.gen_range(0..self.field[0].len());
        while self.field[x][y] != GridField::Empty {
            x = rng.gen_range(0..self.field.len());
            y = rng.gen_range(0..self.field[0].len());
        }
        self.field[x][y] = GridField::Food;
        return (x, y);
    }

    fn score(&self) -> usize {
        self.score
    }
}

struct Board {
    doc: Document,

    grid_drawer: Element,
    overlay: Element,
    text_score: Text,
    text_score_comment: Text,
    rect_size: Pos,
    touch: bool,

    grid: Grid,
}

impl Board {
    fn new(doc: &Document, touch: bool) -> JsResult<Board> {
        doc.get_element_by_id("submit_score_wrapper")
            .expect("Could not find submit_score_wrapper")
            .set_attribute("class", "hidden")?;
        let grid_drawer = doc
            .get_element_by_id("game_content")
            .expect("Could not find game_content");
        grid_drawer.set_inner_html("");

        let overlay = doc
            .get_element_by_id("game_overlay")
            .expect("Could not find game_overlay");
        overlay.set_inner_html("");

        let game_misc = doc
            .get_element_by_id("game_misc")
            .expect("Could not find game_misc");
        game_misc.set_inner_html("");

        let score_svg = doc.create_svg_element("text")?;
        score_svg.set_attribute("x", "320")?;
        score_svg.set_attribute("y", "20")?;
        score_svg.set_attribute("transform", "scale(0.5, 0.5)")?;
        let title_score = doc.create_text_node("Score: ");
        let text_score = doc.create_text_node("0");
        score_svg.append_child(&title_score)?;
        score_svg.append_child(&text_score)?;

        let score_comment_svg = doc.create_svg_element("text")?;
        score_comment_svg.set_attribute("x", "150")?;
        score_comment_svg.set_attribute("y", "20")?;
        score_comment_svg.set_attribute("transform", "scale(0.5, 0.5)")?;
        let text_score_comment = doc.create_text_node("");
        score_comment_svg.append_child(&text_score_comment)?;

        game_misc.append_child(&score_svg)?;
        game_misc.append_child(&score_comment_svg)?;

        let rect_size = (5., 5.);
        let mut grid = Grid::new((40, 28));
        grid.spawn_food();

        if touch {
            let touch_div = doc
                .get_element_by_id("touch_controls_wrapper")
                .expect("Could not find touch_controls");
            touch_div.set_attribute("class", "visible")?;

            set_event_cb(
                &doc.get_element_by_id("move_left")
                    .expect("Could not find move_left"),
                "touchstart",
                move |event: TouchEvent| HANDLE.lock().unwrap().move_left(event),
            )
            .forget();

            set_event_cb(
                &doc.get_element_by_id("move_down")
                    .expect("Could not find move_down"),
                "touchstart",
                move |event: TouchEvent| HANDLE.lock().unwrap().move_down(event),
            )
            .forget();

            set_event_cb(
                &doc.get_element_by_id("move_up")
                    .expect("Could not find move_up"),
                "touchstart",
                move |event: TouchEvent| HANDLE.lock().unwrap().move_up(event),
            )
            .forget();

            set_event_cb(
                &doc.get_element_by_id("move_right")
                    .expect("Could not find move_right"),
                "touchstart",
                move |event: TouchEvent| HANDLE.lock().unwrap().move_right(event),
            )
            .forget();
        }

        console_log!("finish init board");
        Ok(Board {
            doc: doc.clone(),
            grid_drawer,
            overlay,
            text_score,
            text_score_comment,
            rect_size,
            touch,
            grid,
        })
    }

    fn on_keydown(&mut self, event: KeyboardEvent) -> JsError {
        match event.key().as_str() {
            "ArrowLeft" | "h" | "a" => self.grid.snake.set_direction(Direction::Left),
            "ArrowDown" | "j" | "s" => self.grid.snake.set_direction(Direction::Down),
            "ArrowUp" | "k" | "w" => self.grid.snake.set_direction(Direction::Up),
            "ArrowRight" | "l" | "d" => self.grid.snake.set_direction(Direction::Right),
            _ => (),
        }
        Ok(())
    }

    fn draw(&mut self) -> JsError {
        // update score
        let score = if let Ok(num) = self.text_score.data().parse::<usize>() {
            let score = self.grid.score();
            match score - num {
                1 => self.text_score_comment.set_data(""),
                3 => self.text_score_comment.set_data("Perfect! (+2)"),
                _ => (),
            };
            score
        } else {
            self.grid.score()
        };
        self.text_score.set_data(&score.to_string());

        // remove all childs
        self.grid_drawer.set_inner_html("");
        for (x, row) in self.grid.field.iter().enumerate() {
            for (y, field) in row.iter().enumerate() {
                let x_px = x as f32 * self.rect_size.0;
                let y_px = y as f32 * self.rect_size.1;
                match field {
                    GridField::SnakeHead => {
                        let snake = self.doc.create_svg_element("rect")?;
                        snake.class_list().add_1("snake_head")?;
                        snake.set_attribute("width", &self.rect_size.0.to_string())?;
                        snake.set_attribute("height", &self.rect_size.1.to_string())?;
                        snake.set_attribute("x", &x_px.to_string())?;
                        snake.set_attribute("y", &y_px.to_string())?;
                        self.grid_drawer.append_child(&snake)?;
                    }
                    GridField::Snake => {
                        let snake = self.doc.create_svg_element("rect")?;
                        snake.class_list().add_1("snake")?;
                        snake.set_attribute("width", &self.rect_size.0.to_string())?;
                        snake.set_attribute("height", &self.rect_size.1.to_string())?;
                        snake.set_attribute("x", &x_px.to_string())?;
                        snake.set_attribute("y", &y_px.to_string())?;
                        self.grid_drawer.append_child(&snake)?;
                    }
                    GridField::Food => {
                        let food = self.doc.create_svg_element("rect")?;
                        food.class_list().add_1("food")?;
                        food.set_attribute("width", &self.rect_size.0.to_string())?;
                        food.set_attribute("height", &self.rect_size.1.to_string())?;
                        food.set_attribute("x", &x_px.to_string())?;
                        food.set_attribute("y", &y_px.to_string())?;
                        self.grid_drawer.append_child(&food)?;
                    }
                    GridField::Empty => (),
                }
            }
        }
        Ok(())
    }

    fn draw_gameover(&mut self) -> JsError {
        self.text_score_comment.set_data("");
        let text_svg = self.doc.create_svg_element("text")?;
        text_svg.set_attribute("x", "65")?;
        text_svg.set_attribute("y", "15")?;
        let text = self.doc.create_text_node("Game Over");
        text_svg.append_child(&text)?;

        let text_svg2 = self.doc.create_svg_element("text")?;
        text_svg2.set_attribute("x", "115")?;
        text_svg2.set_attribute("y", "250")?;
        let text2 = self.doc.create_text_node(match self.touch {
            true => "Touch `Right` to play again",
            false => "Press `Esc` to play again",
        });
        text_svg2.append_child(&text2)?;
        text_svg2.set_attribute("transform", "scale(0.5, 0.5)")?;

        self.overlay.append_child(&text_svg)?;
        self.overlay.append_child(&text_svg2)?;
        Ok(())
    }
}

#[derive(Clone)]
struct Base {
    doc: Document,
    ws: WebSocket,
    touch: bool,
}

impl Base {
    fn send(&self, msg: ClientMessage) -> JsError {
        let encoded = bincode::serialize(&msg)
            .map_err(|e| JsValue::from_str(&format!("Could not encode: {}", e)))?;
        self.ws.send_with_u8_array(&encoded[..])
    }
}

struct Playing {
    base: Rc<Base>,
    window: Rc<Window>,
    board: Board,

    handle_id: i32,
}

impl Playing {
    fn new(base: Rc<Base>, window: Rc<Window>) -> JsResult<Playing> {
        let board = Board::new(&base.doc, base.touch)?;

        // game ticks
        let cb = Closure::wrap(Box::new(move || {
            HANDLE
                .lock()
                .unwrap()
                .game_tick()
                .expect("Could not update game");
        }) as Box<dyn Fn()>);

        let handle_id = window.set_interval_with_callback_and_timeout_and_arguments_0(
            cb.as_ref().unchecked_ref(),
            50,
        )?;
        cb.forget();

        Ok(Playing {
            base,
            window,
            board,
            handle_id,
        })
    }

    fn on_keydown(&mut self, event: KeyboardEvent) -> JsError {
        self.board.on_keydown(event)
    }

    fn stop_game(&mut self) -> JsError {
        self.board
            .draw_gameover()
            .expect("Could not draw gameover view");
        self.base.send(ClientMessage::RequestHighscore(
            self.board.grid.score as u32,
        ))?;
        self.window.clear_interval_with_handle(self.handle_id);
        Ok(())
    }

    fn on_start_game(&self) -> JsResult<StartGame> {
        Ok(StartGame::new(
            self.base.clone(),
            self.window.clone(),
            self.board.grid.score,
        ))
    }
}

struct StartGame {
    base: Rc<Base>,
    window: Rc<Window>,
    overlay: Element,
    score: usize,
    submit_button: HtmlButtonElement,
    input_name: HtmlInputElement,
    input_val_before: String,
    already_submitted: bool,
}

impl StartGame {
    fn new(base: Rc<Base>, window: Rc<Window>, score: usize) -> StartGame {
        let overlay = base
            .doc
            .get_element_by_id("game_overlay")
            .expect("Could not find game_overlay");

        let submit_button = base
            .doc
            .get_element_by_id("submit_score")
            .expect("Could not find submit_score")
            .dyn_into::<HtmlButtonElement>()
            .expect("Not an HtmlButtonElement");
        submit_button.set_disabled(false);

        base.doc
            .get_element_by_id("submit_score_wrapper")
            .expect("Could not find submit_score_wrapper")
            .set_attribute("class", "visible")
            .expect("Could not set class");

        let input_name = base
            .doc
            .get_element_by_id("input_name")
            .expect("Could not find input_name")
            .dyn_into::<HtmlInputElement>()
            .expect("Could not convert");

        // TODO: only when connection exists
        set_event_cb(&input_name, "input", move |event: InputEvent| {
            HANDLE.lock().unwrap().on_input_name(event)
        })
        .forget();

        let cb = Closure::wrap(Box::new(move || HANDLE.lock().unwrap().on_submit_score())
            as Box<dyn FnMut() -> JsError>);
        &submit_button
            .set_onclick(Some(cb.as_ref().unchecked_ref()));
        cb.forget();

        StartGame {
            base,
            window,
            overlay,
            score,
            submit_button,
            input_name,
            input_val_before: String::new(),
            already_submitted: false,
        }
    }

    fn on_start_game(&self) -> JsResult<Playing> {
        Ok(Playing::new(self.base.clone(), self.window.clone())?)
    }

    fn draw_highscore(&mut self, others: &Vec<(String, u32)>, you: &(u32, u32)) -> JsError {
        fn count_digits(mut number: u32) -> u32 {
            let mut digits: u32 = 1;
            while number > 9 {
                number = (number as f64 / 10.0) as u32;
                digits += 1
            }
            return digits;
        }
        let create_text_element =
            |width: usize, height: usize, text: &str, class: &str| -> Result<Element, JsValue> {
                let text_element = self.base.doc.create_svg_element("text")?;
                text_element.set_attribute("x", &width.to_string())?;
                text_element.set_attribute("y", &height.to_string())?;
                text_element.set_attribute("transform", "scale(0.4, 0.4)")?;
                text_element.set_attribute("class", class)?;
                let text_name = self.base.doc.create_text_node(text);
                text_element.append_child(&text_name)?;
                return Ok(text_element);
            };
        let fill_winner = |height: usize, your_pos: u32, your_score: u32| -> JsError {
            let text_place = create_text_element(
                130 - 10 * count_digits(your_pos) as usize,
                height + 25,
                &format!("{}.", your_pos),
                "you",
            )?;
            let text_name = create_text_element(140, height + 25, "YOU", "you")?;
            let text_score = create_text_element(350, height + 25, &your_score.to_string(), "you")?;
            self.overlay.append_child(&text_place)?;
            self.overlay.append_child(&text_name)?;
            self.overlay.append_child(&text_score)?;
            Ok(())
        };

        let rect = self.base.doc.create_svg_element("rect")?;
        rect.set_attribute("class", "highscore_background")?;
        rect.set_attribute("width", "310")?;
        rect.set_attribute("height", "225")?;
        rect.set_attribute("x", "90")?;
        rect.set_attribute("y", "50")?;
        rect.set_attribute("transform", "scale(0.4, 0.4)")?;
        self.overlay.append_child(&rect)?;

        let (your_pos, your_score) = you;
        let mut i = 1;
        let mut you_inserted = false;
        if *your_pos == 0 {
            // you are the best
            let height = 65 + 10;
            fill_winner(height - 25, *your_pos + 1, *your_score)?;
            let dot = create_text_element(160, height + 10, ".", "others")?;
            let dot2 = create_text_element(160, height + 14, ".", "others")?;
            let dot3 = create_text_element(160, height + 18, ".", "others")?;
            let dot4 = create_text_element(160, height + 22, ".", "others")?;
            self.overlay.append_child(&dot)?;
            self.overlay.append_child(&dot2)?;
            self.overlay.append_child(&dot3)?;
            self.overlay.append_child(&dot4)?;
            i += 1;
            you_inserted = true;
        }
        for (name, score) in others {
            // only show the 10 best (including yourself)
            if i == 11 {
                continue;
            }
            let height = (65 + i * 18 + (15 * you_inserted as u32)) as usize;
            let text_place = create_text_element(
                130 - 10 * count_digits(i as u32) as usize,
                height,
                &format!("{}.", i),
                "others",
            )?;
            let text_name = create_text_element(140, height, name, "others")?;
            let text_score = create_text_element(350, height, &score.to_string(), "others")?;

            self.overlay.append_child(&text_place)?;
            self.overlay.append_child(&text_name)?;
            self.overlay.append_child(&text_score)?;
            if !you_inserted {
                if *your_pos == i as u32 {
                    i += 1;
                    // you are within the 10 best
                    fill_winner(height, *your_pos + 1, *your_score)?;
                    let dot = create_text_element(160, height + 7, ".", "others")?;
                    let dot2 = create_text_element(160, height + 10, ".", "others")?;
                    self.overlay.append_child(&dot)?;
                    self.overlay.append_child(&dot2)?;

                    if *your_pos != others.len() as u32 {
                        let dot3 = create_text_element(160, height + 30, ".", "others")?;
                        let dot4 = create_text_element(160, height + 33, ".", "others")?;
                        self.overlay.append_child(&dot3)?;
                        self.overlay.append_child(&dot4)?;
                    }
                    you_inserted = true;
                }
            }
            i += 1
        }
        if !you_inserted {
            // you are outside the best 10
            let height = 65 + others.len() * 18;
            fill_winner(height, *your_pos + 1, *your_score)?;
            let dot = create_text_element(160, height + 7, ".", "others")?;
            let dot2 = create_text_element(160, height + 10, ".", "others")?;
            self.overlay.append_child(&dot)?;
            self.overlay.append_child(&dot2)?;
        }

        Ok(())
    }

    fn check_name(&self, name: &str) -> bool {
        if name.len() > 20 {
            false
        } else if name.contains("<") || name.contains(">") {
            false
        } else {
            true
        }
    }

    fn submit_score(&mut self) -> JsError {
        if !self.already_submitted {
            let name = self.input_name.value();
            if self.check_name(&name) && !name.is_empty() {
                let msg = ClientMessage::SubmitName(name, self.score as u32);
                self.base.send(msg)?;
                self.already_submitted = true;
                self.submit_button.set_inner_html("Success!");
                self.submit_button.set_disabled(true);
            } else {
                // TODO: notify user
            }
        }
        Ok(())
    }

    fn on_input_name_changed(&mut self) -> JsError {
        let name = self.input_name.value();
        if self.check_name(&name) {
            self.input_name.set_value(&name);
            self.input_val_before = name;
        } else {
            self.input_name.set_value(&self.input_val_before);
        }
        Ok(())
    }
}

enum State {
    Playing(Playing),
    StartGame(StartGame),
    Empty,
}

impl State {
    fn on_keydown(&mut self, event: KeyboardEvent) -> JsError {
        Ok(match self {
            State::Playing(s) => s.on_keydown(event)?,
            State::StartGame(_s) => {
                if event.key().as_str() == "Escape" {
                    // Transition to Playing
                    let s = std::mem::replace(self, State::Empty);
                    match s {
                        State::StartGame(s) => *self = State::Playing(s.on_start_game()?),
                        _ => panic!("Invalid state"),
                    }
                }
            }
            _ => (),
        })
    }

    fn move_left(&mut self, _event: TouchEvent) -> JsError {
        Ok(match self {
            State::Playing(s) => s.board.grid.snake.set_direction(Direction::Left),
            State::StartGame(_s) => (),
            _ => (),
        })
    }

    fn move_down(&mut self, _event: TouchEvent) -> JsError {
        Ok(match self {
            State::Playing(s) => s.board.grid.snake.set_direction(Direction::Down),
            State::StartGame(_s) => (),
            _ => (),
        })
    }

    fn move_up(&mut self, _event: TouchEvent) -> JsError {
        Ok(match self {
            State::Playing(s) => s.board.grid.snake.set_direction(Direction::Up),
            State::StartGame(_s) => (),
            _ => (),
        })
    }

    fn move_right(&mut self, _event: TouchEvent) -> JsError {
        Ok(match self {
            State::Playing(s) => s.board.grid.snake.set_direction(Direction::Right),
            State::StartGame(_s) => {
                let s = std::mem::replace(self, State::Empty);
                match s {
                    State::StartGame(s) => *self = State::Playing(s.on_start_game()?),
                    _ => panic!("Invalid state"),
                }
            }
            _ => (),
        })
    }

    fn game_tick(&mut self) -> JsError {
        match self {
            State::Playing(s) => {
                match s.board.grid.do_move() {
                    Ok(_) => s.board.draw()?,
                    Err(_) => {
                        s.stop_game()?;
                        // Transition to StartGame
                        let s = std::mem::replace(self, State::Empty);
                        match s {
                            State::Playing(s) => *self = State::StartGame(s.on_start_game()?),
                            _ => panic!("Invalid state"),
                        }
                    }
                }
            }
            State::Empty => (),
            State::StartGame(_) => (),
        }
        Ok(())
    }

    fn received_highscore(&mut self, others: &Vec<(String, u32)>, you: &(u32, u32)) -> JsError {
        match self {
            State::Playing(_) => (),
            State::StartGame(s) => {
                s.draw_highscore(others, you)?;
            }
            State::Empty => (),
        }
        Ok(())
    }

    fn on_submit_score(&mut self) -> JsError {
        console_log!("on_submit_score...");
        match self {
            State::Playing(_) => (),
            State::StartGame(s) => {
                s.submit_score()?;
            }
            State::Empty => (),
        }
        Ok(())
    }

    fn on_input_name(&mut self, _event: InputEvent) -> JsError {
        match self {
            State::Playing(_) => (),
            State::StartGame(s) => s.on_input_name_changed()?,
            State::Empty => (),
        }
        Ok(())
    }
}

unsafe impl Send for State {
    /* YOLO */
}

lazy_static::lazy_static! {
    static ref HANDLE: Mutex<State> = Mutex::new(State::Empty);
}

// Boilerplate to wrap and bind a callback.
// The resulting callback must be stored for as long as it may be used.
#[must_use]
fn build_cb<F, T>(f: F) -> JsClosure<T>
where
    F: FnMut(T) -> JsError + 'static,
    T: FromWasmAbi + 'static,
{
    Closure::wrap(Box::new(f) as Box<dyn FnMut(T) -> JsError>)
}

#[must_use]
fn set_event_cb<E, F, T>(obj: &E, name: &str, f: F) -> JsClosure<T>
where
    E: JsCast + Clone + std::fmt::Debug,
    F: FnMut(T) -> JsError + 'static,
    T: FromWasmAbi + 'static,
{
    let cb = build_cb(f);
    let target = obj
        .dyn_ref::<EventTarget>()
        .expect("Could not convert into `EventTarget`");
    target
        .add_event_listener_with_callback(name, cb.as_ref().unchecked_ref())
        .expect("Could not add event listener");
    cb
}

fn on_message(msg: ServerMessage) -> JsError {
    console_log!("Got message {:?}", msg);

    let mut state = HANDLE.lock().unwrap();

    match msg {
        ServerMessage::Highscore { others, you } => state.received_highscore(&others, &you),
    }
}

#[wasm_bindgen(start)]
pub fn main() -> JsError {
    console_log!("Started Main!");
    let window = web_sys::window().expect("no global `window` exists");

    let doc = window.document().expect("should have a document on window");
    let location = doc.location().expect("Could not get doc location");
    let hostname = location.hostname()?;
    let (ws_protocol, ws_port) = if location.protocol()? == "https:" {
        ("wss", 8091)
    } else {
        ("ws", 8090)
    };
    let hostname = format!("{}://{}:{}", ws_protocol, hostname, ws_port);

    let ws = WebSocket::new(&hostname)?;
    console_log!("ws ready_state: {}", ws.ready_state());

    let on_decoded_cb = Closure::wrap(Box::new(move |e: ProgressEvent| {
        let target = e.target().expect("Could not get target");
        let reader: FileReader = target.dyn_into().expect("Could not cast");
        let result = reader.result().expect("Could not get result");
        let buf = js_sys::Uint8Array::new(&result);
        let mut data = vec![0; buf.length() as usize];
        buf.copy_to(&mut data[..]);
        let msg = bincode::deserialize(&data[..])
            .map_err(|e| JsValue::from_str(&format!("Failed to deserialize: {}", e)))
            .expect("Could not decode message");
        on_message(msg).expect("Message decoding failed")
    }) as Box<dyn FnMut(ProgressEvent)>);
    set_event_cb(&ws, "message", move |e: MessageEvent| {
        let blob = e.data().dyn_into::<Blob>()?;
        let fr = FileReader::new()?;
        fr.add_event_listener_with_callback("load", &on_decoded_cb.as_ref().unchecked_ref())?;
        fr.read_as_array_buffer(&blob)?;
        Ok(())
    })
    .forget();

    let mut base = Base {
        doc,
        ws,
        touch: false,
    };

    set_event_cb(&base.doc, "keydown", move |event: KeyboardEvent| {
        HANDLE.lock().unwrap().on_keydown(event)
    })
    .forget();

    match base.doc.create_event("TouchEvent") {
        Ok(_) => {
            base.touch = true;
        }
        Err(_) => (),
    };

    *HANDLE.lock().unwrap() = State::Playing(Playing::new(Rc::new(base), Rc::new(window))?);
    Ok(())
}
