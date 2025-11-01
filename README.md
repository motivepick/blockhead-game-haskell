# Blockhead Game

## Configuring IntelliJ IDEA

Tested with macOS 15.6.1 (24G90), IntelliJ IDEA 2025.2.4 (Ultimate Edition), Haskell LSP 1.4.4.

1. Install [GHCup](https://www.haskell.org/ghcup/) with
   `curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh`.
2. Run `ghcup tui`, then install **and set** the recommended HLS. You should see **two check marks** next to HLS.
3. Also install **and set** GHC 9.8.4. You can choose a different version, but it must be [supported by the currently set HLS](https://github.com/haskell/ghcup-hs/blob/36073910cf7e133700f23c5e1d0538f22cbb4989/lib-tui/GHCup/Brick/Widgets/Tutorial.hs#L70C60-L70C90) (marked as `hls-powered`).
   There should be also a snapshot with this version in https://www.stackage.org/snapshots.
4. Run `stack new haskell-sandbox`, then `cd haskell-sandbox`.
5. In `stack.yaml`, set the `resolver` to the snapshot version that matches the GHC version you've installed with GHCup. For example, for GHC 9.8.4, you can use [this snapshot](https://www.stackage.org/lts-23.27): `resolver: lts-23.27`.
6. Run `stack clean`.
7. Run `stack build`.
8. Open the `haskell-sandbox` folder in IntelliJ IDEA, the plugin should work now.

## Running The Game

```shell
stack build --exec blockhead-game-exe
```

## Running The Unit Tests

```shell
stack test
```

## Loading A Module

```shell
stack ghci --ghci-options src/Lib.hs
```

Type `:quit` to exit once done.

## Building And Running With Docker

```shell
docker build -t blockhead-game .
docker run -p 8080:8080 -d blockhead-game
```

## API

### Create New Field

#### Request

```shell
curl -v http://localhost:8080/api/field/5
```

#### Response

```json
[
  ".....",
  ".....",
  "БАЛДА",
  ".....",
  "....."
]
```

### Ask Computer To Make A Move

#### Request

```shell
curl -v -d '{ "field": [ ".....", ".....", "БАЛДА", ".....", "....." ], "usedWords": [ "БАЛДА" ],"difficulty":"Medium" }' http://localhost:8080/api/move-requests
```

#### Response

```json
{
  "cell": [
    3,
    1
  ],
  "letter": "Ф",
  "path": [
    [
      3,
      1
    ],
    [
      2,
      1
    ],
    [
      2,
      2
    ],
    [
      2,
      3
    ],
    [
      2,
      4
    ]
  ],
  "success": true,
  "updatedField": [
    ".....",
    ".....",
    "БАЛДА",
    ".Ф...",
    "....."
  ],
  "word": "ФАЛДА"
}
```

### Request

For DFS testing. It is possible to find word "ПЛАКАТИСТ" which consists of 9 letters if `difficulty` is set to `Hard`.

```shell
curl -v -d '{"field":[".....","АЛП..","КИСТЬ",".ТКАЖ","....."],"usedWords":["КИСТЬ","","КИСТА","","КИСКА","","ТАКТИК","","ТАКТИКА","","КАЛИТКА","","ЛИСТАЖ","","ПЛИТКА",""],"difficulty":"Hard"}' http://localhost:8080/api/move-requests
```

### Request

There are empty cells but computer cannot find any word.

```shell
curl -v -d '{"field":["ЬТЗСР","ЭСАПО","ГОГОТ","ТФАРТ",".ЬРТЫ"],"usedWords":["ГОГОТ","ТОГА","ФАГОТ","АГОГЭ","СОФА","САГА","САПОГ","ТОПАЗ","ЗАПОР","ЗАГАР","СПОРА","СПОРТ","ПОРТЫ","РОПОТ","РОПОТ","ТРОПА","ГАРЬ","ГОСТ","ПАСТЬ","РАФТ"],"difficulty":"Medium"}' http://localhost:8080/api/move-requests 
```

### Request

There are empty cells but computer cannot find any word, another example.

```shell
curl -v -d '{"field":["ТЬ.ЬН","ОНВДА","ЗАВОЗ","АГАЙЮ","ВАНСО"],"usedWords":["ЗАВОЗ","НАВОЗ","ЗАВОД","ЗАВОДЬ","ВДОВА","ДОЗА","НАВОЙ","НАДОЙ","ЗОНА","АЗОТ","ГАЗОН","ВАЗА","НАВАГА","ВАЗОН","ГАВАНЬ","НАГАН","АГАВА","АВАНС","ЮЗ","СОЮЗ"],"difficulty":"Medium"}' http://localhost:8080/api/move-requests
```

### Request

There are no empty cells.

```shell
curl -v -d '{ "field": [ "ТОНЯЯ", "ЕГТЫН", "БАЛДА", "ЕФЛАХ", "ДОПТА" ], "usedWords": [ "БАЛДА", "ФАЛДА", "БАЛЛ", "БАЛТ", "ГАЛЛ", "БЕГА", "БАЛЛАДА", "БАГЕТ", "АЛЛАХ", "НАХАЛ", "АЛТЫН", "ФАГОТ", "БЕТОН", "ПЛАХА", "ПТАХА", "ПЛАТА", "ОПЛАТА", "ДОПЛАТА", "ДЕБЕТ", "ТОНЯ", "ДЫНЯ" ],"difficulty":"Medium" }' http://localhost:8080/api/move-requests
```
