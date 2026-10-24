# Flip 7 Companion

A phone-first score and card-counting companion for Flip 7, written in Elm. Static files only, no backend, no storage: refresh and everything resets.

## Build

Needs Elm 0.19.1 or newer.

    ./build.sh

(or `elm make src/Main.elm --optimize --output=elm.js`)

## Host

Upload `index.html` and `elm.js` to the same folder on your site. Nothing else is required. The page loads two fonts from Google Fonts and falls back to system fonts without them.

## Rules the app uses

- First player to reach 200 or more at the end of a round wins; if several do, the highest total wins.
- Tapping a card you already logged takes it back (or tap its chip in the hand). Press Busted, then pick the card that busted that player: their round score drops to 0 and that card is counted as seen. Undo reverses a bust.
- ×2 doubles number cards only. +2 to +10 are added after doubling.
- Seven different numbers in a hand scores a +15 Flip 7 bonus.
- Card counting follows the real pile: cards are not reshuffled between rounds. Each round's cards go to the discards, and only when the draw pile runs dry are the earlier rounds' discards shuffled back in (cards in front of players stay out). Log every hand each round to keep the count honest.
