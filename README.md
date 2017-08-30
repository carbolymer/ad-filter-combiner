# Ad Filters Combiner

## What's this?
That's simple CLI application which loads uBlock Origin's filters list, downloads them and combines them into one file.

## How to run?
1. Download & install [Stack](https://www.haskellstack.org/).
2. Run `stack build`.
3. Download and replace ublock filters list ( https://github.com/gorhill/uBlock/blob/master/assets/ublock/filter-lists.json ) in `filter-lists.json` file. You can use already existing file if you want (may be outdated)
4. Run `stack exec ad-filter-combiner`
5. Combined filters list will be in `rules.txt` file
