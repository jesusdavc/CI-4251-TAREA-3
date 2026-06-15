# Tarea 3 Funcional

This is a small Haskell project for working with a Pokémon JSON dataset.

## Files

- `Sol.hs` — main library module with data types, JSON parsing, optics queries, and helper functions.
- `Main.hs` — executable entrypoint that loads `pokemon.json` and prints summary information.
- `pokemon.json` — example dataset.
- `tarea3.cabal` — Cabal package description.

## Requirements

- GHC 9.6.x (or compatible)
- Cabal

## Build and run

From the project root:

```bash
cabal v2-build
cabal v2-run sol
```

## Notes

- The project uses Cabal to manage dependencies, so you do not need to add package flags manually.
- If the program reports a failure loading `pokemon.json`, make sure the file exists in the project root.
- `Sol.hs` defines `loadPokeSet` and the JSON decoders for the dataset.
# CI-4251-TAREA-3
