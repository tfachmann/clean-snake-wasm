# clean-snake-wasm

Yet another snake application written in Rust and [WebAssembly](https://webassembly.org).
Rendering is (naively) done in an `<svg>` element. Greatly inspired by [mketeer/pont](https://github.com/mkeeter/pont), which was used as basis to get into using the `web-sys` crate.

## How to Play it

You can move the snake with any of those keys:
```
a, s, w, d
h, j, k, l
ArrowLeft, ArrowDown, ArrowUp, ArrowLeft
```

It detects whether you are on mobile and provides buttons accordingly.

So far, everything is done on client side, but a global high score / ranking is WIP

## Building

```
git clone https://github.com/tfachmann/clean-snake-wasm
cd clean-snake-wasm
wasm-pack build --target web
```

## Hosting it yourself

You can use any server for that, for example python:
```
python -m http.server 8080
```

It will then run on http://localhost:8080

[Demo](https://www.tfachmann.com/snake_wasm)
