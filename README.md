# dot-analysis

Analysing dependency graphs produced by Agda

## Installation

```
cabal install .
```

## Quick usage

1. Install [agda](https://agda.readthedocs.io/en/latest/getting-started/installation.html).

2. Install [graphviz](https://graphviz.org/download/).

3. Generate a `dot` file for an `agda` file -
    ```
    agda -i. --dependency-graph={file_name}.dot {agda_file_name}.agda`
    ```

4. Run `dot-analysis` -
    ```
    dot-analysis [--arity] [--weight] [--top] FILE
    ```
